(define $server-name
  "Gambit LSP server")

  ;;; Ignored for now
(define $tcp-read-timeout (make-parameter #f))

(define ($initialize-lsp-server! root-path)
  (write-log 'info
             (format "initializing LSP server with root ~a"
                     root-path))
  (module-search-order-add! root-path)
  ;; Disabled for now, since too slow on large projects (ex. gambit itself)
  ;;(generate-meta-data! root-path)
  #f)

(define $server-capabilities
  '((definitionProvider . ())
    (diagnosticsProvider . ((interFileDiagnostics . #t)
                            (workspaceDiagnostics . #t)))))

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

(define (parse-location str)
  ;;; we consume contents starting at the string end. Gambit's displayed
  ;;; location is of the form <filename>@<line-num>:<char-num>
  (guard
      (condition
       (else #f))
    (let loop ((i (- (string-length str) 1))
               (char-num #f)
               (line-num #f)
               (acc ""))
      (cond ((= i 0)
             (list acc line-num char-num))
            (else
             (let ((c (string-ref str i)))
               (cond ((not char-num)
                      (case c
                        ((#\:)
                         (loop (- i 1)
                               (string->number acc)
                               line-num
                               ""))
                        (else (loop (- i 1)
                                    char-num
                                    line-num
                                    (string-append (string c) acc)))))
                     ((not line-num)
                      (case c
                        ((#\@)
                         (loop (- i 1)
                               char-num
                               (string->number acc)
                               ""))
                        (else (loop (- i 1)
                                    char-num
                                    line-num
                                    (string-append (string c) acc)))))
                     (else
                      (loop (- i 1)
                            char-num
                            line-num
                            (string-append (string c) acc))))))))))

(define (parse-continuation-location c)
  (let ((str (with-output-to-string
               (lambda ()
                 (display-continuation-backtrace c)))))
    (write-log 'info (format "parse-continuation-location str~s"
                             str))
    (with-input-from-string str
      (lambda ()
        (let loop ((line (read-line))
                   (res '()))
          (if (eof-object? line)
              (reverse res)
              (let ((contents (string-split line " ")))
                (write-log 'info (format "parse-continuation-location line: ~s, contents: ~s"
                                         line
                                         contents))
                (if (< (length contents) 3)
                    (loop (read-line))
                    (let ((loc (parse-location (list-ref contents 2))))
                      (if (and loc
                               (not (null? loc)))
                          (loop (read-line)
                                (cons loc res))
                          (loop (read-line) res)))))))))))

(define (send-diagnostics file-path k condition)
  (let ((frames (parse-continuation-location k))
        (msg (condition->string condition)))
    (write-log 'debug (format "send-diagnostics frames: ~s, msg: ~s, length: ~s"
                              frames
                              msg
                              (length frames)))
    ;; (for-each (lambda (f)
    ;;             (write-log 'debug (format "frame ~a" f)))
    ;;           frames)
    ;; (when (> (length diags) 2)
    ;;   (let ((line-num (list-ref diags 1))
    ;;         (char-num (list-ref diags 2)))
    ;;     (json-rpc-send-notification
    ;;      (server-out-port)
    ;;      "textDocument/publishDiagnostics"
    ;;      `((uri . ,file-path)
    ;;        (diagnostics . #(((message . ,msg)
    ;;                          (source . "gsi")
    ;;                          (range . ((start . ((line . ,line-num)
    ;;                                              (character . ,char-num)))
    ;;                                    (end . ((line . ,(and line-num (+ line-num 1)))
    ;;                                            (character . ,(and char-num (+ char-num 1))))))))))))
        
        
    ;;     ))
    ))

(define (compile-and-import-if-needed file-path)
  (continuation-capture
   (lambda (k)
     (with-exception-handler
         (lambda (condition)
           (write-log 'error
                      (format "Error compiling file ~a: ~a"
                              file-path
                              (condition->string condition)))
           (send-diagnostics file-path k condition)
           #f)
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
  (compile-and-import-if-needed file-path)
  #f)

(define ($save-file! file-path text)
  (generate-meta-data! file-path)
  (continuation-capture
   (lambda (k)
     (with-exception-handler
      (lambda (condition)
        (write-log 'error
                   (format "Error loading file ~a: ~a"
                           file-path
                           (condition->string condition)))
        (send-diagnostics file-path k condition)
        #f)
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
