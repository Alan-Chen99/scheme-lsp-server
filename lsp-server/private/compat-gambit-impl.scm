(define $server-name
  "Gambit LSP server")

  ;;; Ignored for now
(define $tcp-read-timeout (make-parameter #f))
(define root-path (make-parameter #f))

(define ($initialize-lsp-server! r-path)
  (root-path r-path)
  (write-log 'info
             (format "initializing LSP server with root ~a"
                     r-path))
  (module-search-order-add! r-path)
  ;; Disabled for now, since too slow on large projects (ex. gambit itself)
  ;;(generate-meta-data! r-path)
  #f)

(define $server-capabilities
  '((definitionProvider . ())))

(define ($tcp-listen port-number)
  (open-tcp-server port-number))

(define ($tcp-accept listener)
  (let ((p (read listener)))
    (values p p)))

(define ($tcp-close listener)
  (close-port listener))

(define ($tcp-connect tcp-address tcp-port-number)
  (let ((p (open-tcp-client tcp-port-number)))
    (values p p)))

(define ($apropos-list module prefix)
  (write-log 'debug (format "$apropos-list ~a ~a" module prefix))
  (fetch-apropos prefix))

(define (procedure-module-prefix identifier)
  (let* ((idn (symbol->string identifier))
         (ap-lst ($apropos-list #f idn))
         (match (assoc idn ap-lst)))
    (and match
         (cdr match))))

(define (fetch-location identifier)
  (write-log 'debug
             (format "fetch-location ~s" identifier))

  (let ((proc (##global-var-ref
               (##make-global-var identifier))))
    (if (and (not (##unbound? proc)) (procedure? proc))
        (let ((loc (##procedure-locat proc)))
          (if (not loc)
              '()
              (let* ((container (##locat-container loc))
                     (file-path (and loc (##container->path container)))
                     (pos (and loc (##locat-position loc)))
                     (file-pos (and pos (##position->filepos pos)))
                     (line-number (and file-pos (##filepos-line file-pos)))
                     (char-number (or (and file-pos (##filepos-col file-pos))
                                      0)))
                (if (and file-path line-number)
                    `(((uri . ,file-path)
                       (range . ((start . ((line . ,line-number)
                                           (character . ,char-number)))
                                 (end . ((line . ,line-number)
                                         (character . ,(+ char-number
                                                          (string-length
                                                           (symbol->string identifier))))))))))
                    '()))))
        '())))

;; TODO move this to geiser?
(define ($get-definition-locations mod-name identifier)
  (let* ((res (fetch-location identifier))
         (internal-res
          (if (null? res)
              (let ((mod-prefix (procedure-module-prefix identifier)))
                (if mod-prefix
                    (fetch-location (string->symbol
                                     (format "~a~a"
                                             mod-prefix
                                             identifier)))
                    '()))
              res)))
    (if (not (null? internal-res))
        internal-res
        (fetch-definition-locations identifier))))

(define (lsp-server-dependency? mod-name)
  (define pred
    (if (symbol? mod-name)
        (lambda (dep-name)
          (eq? mod-name (car dep-name)))
        (lambda (dep-name)
          (equal? mod-name dep-name))))
  (find pred
        '((_geiser)
          (irregex)
          (json-rpc)
          (json-rpc private)
          (json-rpc lolevel)
          (json-rpc gambit)
          (lsp-server)
          (lsp-server private adapter)
          (lsp-server private document)
          (lsp-server private gambit)
          (lsp-server private compat)
          (lsp-server private parse)
          (lsp-server private util)
          (lsp-server private trie)
          (srfi 1)
          (srfi 13)
          (srfi 14)
          (srfi 28)
          (srfi 69))))

(define (compile-and-import-if-needed file-path)
  (call/cc
   (lambda (k)
     (with-exception-handler
      (lambda (condition)
        (write-log 'error
                   (format "Error compiling file ~a: ~a"
                           file-path
                           (cond ((error-object? condition)
                                  (error-object-message condition))
                                 (else condition))))
        (k #f))
      (lambda ()
        (let ((mod-name (parse-library-name-from-file file-path)))
          (cond ((and mod-name
                      (not (lsp-server-dependency? mod-name)))
                 (write-log 'info (format "Importing module ~s" mod-name))
                 (eval `(import ,mod-name)))
                (else
                 (write-log 'debug
                            (format "Ignoring LSP-server dependency ~a"
                                    mod-name))
                 #f))))))))

(define ($open-file! file-path text)
  (generate-meta-data! file-path)
  (let* ((ldef (find-library-definition-file file-path))
         (file-to-compile (or ldef file-path)))
    (when (and ldef (not (string=? ldef file-path)))
      (generate-meta-data! ldef))
    (compile-and-import-if-needed file-path))
  
  #f)

(define ($save-file! file-path text)
  (generate-meta-data! file-path)
  (call/cc
   (lambda (k)
     (with-exception-handler
      (lambda (condition)
        (write-log 'error
                   (format "Error loading file ~a: ~a"
                           file-path
                           condition))
        (k #f))
      (lambda ()
        (let ((mod-name (parse-library-name-from-file file-path)))
          (if (not (lsp-server-dependency? mod-name))
              (load file-path)
              (write-log 'info
                         (format "Skip reload of lsp dependency: ~a"
                                 mod-name))))))))
  #f)

(define ($fetch-documentation mod-name identifier)
  #f)

(define ($fetch-signature mod-name identifier)
  (write-log 'debug
             (format "$fetch-signature ~s ~s"
                     mod-name
                     identifier))

  (or (lsp-geiser-signature identifier)
      (let ((mod-prefix (procedure-module-prefix identifier)))
        (or (and mod-prefix
                 (lsp-geiser-signature (string->symbol
                                        (format "~a~a"
                                                mod-prefix
                                                identifier))))
            (fetch-signature mod-name identifier)))))

(define namespace-regex
  (irregex '(: "\""
               (submatch (+ graphic))
               "\""
               whitespace
               "namespace:")))

(define (parse-namespace line)
  (let ((m (irregex-match namespace-regex line)))
    (and m
         (irregex-match-substring m 1))))

(define (fetch-apropos prefix)
  (let ((outp (open-output-string))
        (valid-entry (char-set-complement (char-set #\, #\space))))
    (apropos prefix outp)
    (let ((text (get-output-string outp)))
      (let ((inp (open-input-string text)))
        (let loop ((line (read-line inp))
                   (cur-mod #f)
                   (result '()))
          (cond ((eof-object? line)
                 (close-input-port inp)
                 (close-output-port outp)
                 (reverse result))
                ((parse-namespace line)
                 => (lambda (cur-mod)
                      (loop (read-line inp)
                            cur-mod
                            result)))
                (else
                 (let* ((entries (string-tokenize line valid-entry))
                        (suggestions (filter (lambda (e)
                                               (string-prefix? prefix e))
                                             entries)))
                   (loop (read-line inp)
                         cur-mod
                         (append (map (lambda (s)
                                        (cons s cur-mod))
                                      suggestions)
                                 result))))))))))

(define error-line-regex
  (irregex '(: "*** ERROR IN "
               (: #\" (submatch (+ any)) #\")
               #\@
               (submatch (+ num))
               #\.
               (submatch (+ num))
               " -- "
               (* space)
               (submatch (+ any)))))

(define (externally-compile-file file-path proc)
  ;; TODO: check return code
  (let* ((search-path (if (root-path)
                          (format "-:search=~a" (root-path))
                          "."))
         (ldef-path (find-library-definition-file file-path))
         (path-to-compile (or ldef-path file-path))
         (p (##open-input-process (list path: "gsi"
                                        arguments: (list search-path
                                                         path-to-compile)))))
    (write-log 'debug (format "externally-compile-file: compiled ~a"
                              path-to-compile))
    (let loop ((line (read-line p))
               (diags '()))
      (write-log 'debug (format "compile command line: ~a" line))
      (cond ((eof-object? line)
             (reverse diags))
            ((proc line) => (lambda (diag)
                              (loop (read-line p)
                                    (cons diag diags))))
            (else (loop (read-line p)
                        diags))))))

(define (parse-compiler-output str)
  (let ((m (irregex-search error-line-regex str)))
    (guard
        (condition
         (else #f))
      (if m
          (make-diagnostic "gsi"
                           (irregex-match-substring m 1)
                           (max (- (string->number (irregex-match-substring m 2)) 1)
                                0)
                           (max (- (string->number (irregex-match-substring m 3)) 1)
                                0)
                           (irregex-match-substring m 4))
          #f))))

(define ($compute-diagnostics file-path)
  (cond ((externally-compile-file file-path parse-compiler-output)
           => (lambda (diags)
                (let ((matching-diags
                       (filter (lambda (d)
                                 (let ((fname (diagnostic-file-path d)))
                                   (and fname
                                        (string=? (get-absolute-pathname file-path)
                                                  (get-absolute-pathname fname)))))
                               diags)))
                  matching-diags)))
          (else
           '())))
