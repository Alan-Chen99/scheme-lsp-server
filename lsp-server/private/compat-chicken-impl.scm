

(define chicken-source-path
  (or (get-environment-variable "CHICKEN_SOURCE_PATH") ""))

(define root-path
  (make-parameter #f))

(define $tcp-read-timeout tcp-read-timeout)

(define $server-name
  "CHICKEN LSP server")

(define (pick-port)
  (+ (pseudo-random-integer 2000)
     8001))

;;; Initialize LSP server to manage project at ROOT (a string). Used
;;; for implementation-specific side effects only.
(define ($initialize-lsp-server! root)
  (root-path (if (and root (not (equal? root 'null)))
                 root
                 "."))
  (verify-repository)     ; needed to use chicken-doc in compiled code
  (init-module-egg-table!)
  (generate-meta-data! chicken-source-path)
  (generate-meta-data! (root-path))

  #t)

;;; An alist with implementation-specific server capabilities. See:
;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
;;; Note: These capabilities are joined to the implementation independent
;;; mandatory-capabilities (see server.scm).
(define $server-capabilities
  `((definitionProvider . ())))

(define $tcp-accept tcp-accept)

(define $tcp-close tcp-close)

(define $tcp-connect tcp-connect)

(define $tcp-listen tcp-listen)

;;; Return completion suggestions for PREFIX (a symbol).
;;; MODULE is ignored for now, but belongs to the API, since other
;;; implementations (e.g. guile) use it.
;;; A suggestion is a pair of strings (identifier . library)
(define ($apropos-list module prefix)
  (let ((tracked-completions (list-completions prefix)))
    (append
     tracked-completions
     (let ((suggestions
            (apropos-information-list prefix #:macros? #t #:imported? #f)))

       (write-log 'debug
                  (format "apropos suggestions for ~a: ~a"
                          prefix
                          suggestions))
       (append
        (fold (lambda (s acc)
                (let* ((mod-id-pair (car s))
                       (mod (let ((fst (car mod-id-pair)))
                              (if (eqv? fst '||)
                                  #f
                                  (map string->symbol
                                       (string-split (symbol->string fst) ".")))))
                       (id (cdr mod-id-pair))
                       (type (cdr s)))
                  (if (string-prefix? prefix (symbol->string id))
                      (cons (cons (symbol->string id)
                                  (module-name->chicken-string mod))
                            acc)
                      acc)))
              '()
              suggestions))))))

;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
;;; MODULE (a symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi-1) 'map
(define ($fetch-documentation mod-name identifier)
  (define match (alist-ref identifier (geiser-autodoc identifier)))
  (define module (and match
                      (alist-ref "module" match equal?)))
  (define egg (or (module-egg module) module))
  (write-log 'debug
             (format "$fetch-documentation identifier: ~a, match: ~a, module: ~a, egg: ~a"
                     identifier
                     match
                     module
                     egg))
  (if (and egg (not (null? egg)))
      (let ((doc-path
             (append (if (list? egg)
                         egg
                         (list egg))
                     (list identifier))))
        (write-log 'debug
                   (format "looking up doc-path: ~a" doc-path))
        (with-output-to-string
          (lambda ()
            (guard (condition
                    (else
                     (write-log 'debug
                                (format "#fetch-documentation: documentation not found: (~a ~a)"
                                        egg
                                        identifier))
                     #f))
             (describe (lookup-node doc-path))))))
      #f))

; Return the signature (a string) for IDENTIFIER (a symbol) in MODULE (a
; symbol). Return #f if nothing found.
; Example call: $fetch-documentation '(srfi 1) 'map
(define ($fetch-signature module identifier)
  ;; first check chicken-doc info, if not available use custom fetch-signature
  (let ((cdoc-nodes (match-nodes identifier)))
    (if (not (null? cdoc-nodes))
        (node-signature (car cdoc-nodes))
        (fetch-signature module identifier))))

(define (file-is-egg-definition? file-path)
  (string=? (pathname-extension file-path)
            "egg"))

(define (file-supported? file-path)
  (not (file-is-egg-definition? file-path)))

(define (load-or-import file-path)
  (guard
   (condition
    (else (write-log 'error
                     (format "Can't load file ~a: ~a"
                             file-path
                             (with-output-to-string
                               (lambda ()
                                 (print-error-message condition)))))))
   (let ((mod-name (parse-library-name-from-file file-path)))

     (cond ((not (file-supported? file-path))
            (write-log 'debug
                       (format "ignoring unsupported file: ~a~%"
                               file-path)))
           ((not mod-name)
            (lsp-geiser-load-file file-path))
           (else (lsp-geiser-load-file file-path)
                 (eval `(import ,mod-name)))))))


(define (generate-meta-data-if-supported! file-path text)
  (cond ((file-supported? file-path)
         (generate-meta-data! file-path)
         (when text
           (let ((meta-data (parse-file file-path text)))
             (when meta-data
               (let ((imports (source-meta-data-imports meta-data)))
                 (write-log 'debug
                            (format "processing imports ~s~%"
                                    imports))
                 (for-each (lambda (imp)
                             (let ((imp-path (get-module-path imp)))
                               (when imp-path
                                 (generate-meta-data! imp-path))))
                           imports))))))
        (else
         (write-log 'debug
                    (format "ignoring unsupported file: ~a~%"
                            file-path)))))

;;; Action to execute when FILE-PATH is opened. TEXT contain the file
;;; content, which is sent by the client in a didOpen request.
;;; Used for side effects only.
(define ($open-file! file-path text)
  (generate-meta-data-if-supported! file-path text)
  #f)

;;; Action to execute when FILE-PATH is saved. Used for side effects only.
(define ($save-file! file-path text)
  (generate-meta-data-if-supported! file-path text)
  #f)

(define (join-definition-tables! left right)
  (for-each
   (lambda (k)
     (let ((left-locations (hash-table-ref/default left k '()))
           (right-locations (hash-table-ref right k)))
       (if (not (null? left-locations))
           (let ((updated-locations
                  (fold (lambda (right-loc acc)
                          (let* ((right-loc-path (car right-loc))
                                 (acc-loc (assoc right-loc-path
                                                 acc)))
                            (if acc-loc
                                (cons right-loc
                                      (alist-delete right-loc-path acc))
                                (cons right-loc acc))))
                        left-locations
                        right-locations)))
             (hash-table-set! left k updated-locations))
           (hash-table-set! left k right-locations))))
   (hash-table-keys right))
  left)

;;; Return a list of locations found for IDENTIFIER (a symbol).
;;; Each location is represented by an alist
;;; '((url . "file:///<path>")
;;;   (range . ((start . ((line  . <line number>)
;;;                       (character . <character number))
;;;             (end . ((line  . <line number>)
;;;                     (character . <character number))))
;;;
(define ($get-definition-locations mod-name identifier)
  (fetch-definition-locations identifier))

(define (module-name->chicken-string mod-name)
  (cond ((not mod-name)
         #f)
        ((list? mod-name)
         (let ((lib-parts (map symbol->string mod-name)))
           (string-join lib-parts ".")))
        (else (symbol->string mod-name))))

(define (spawn-repl-server port-num)
  (nrepl port-num))

(define error-line-regex
  (irregex '(: "Error: "
               (submatch (+ any)))))

(define in-file-regex
  (irregex '(: (* whitespace)
               "In file `"
               (submatch (+ any))
               #\')))

(define on-line-regex
  (irregex '(or (: (* whitespace)
                   "On line "
                   (submatch (+ num)))
                (: (* any)
                   "on line "
                   (submatch (+ num))))))

(define (compile-and-collect-output file-path)
  ;; Wrap module to get more information, if needed
  (let* ((module-arg (if (find-library-definition-file file-path)
                         ""
                         "-m some-module"))
         (cmd (format "~a -R r7rs ~a -to-stdout ~a 2>&1 > /dev/null"
                      (or (chicken-program-path)
                          "csc")
                      module-arg
                      file-path)))
    (write-log 'debug
               (format "compile-and-collect-output with command: ~s"
                       cmd))
    (with-input-from-pipe cmd
                          (lambda ()
                            (let loop ((line (read-line))
                                       (res '()))
                              (if (eof-object? line)
                                  (reverse res)
                                  (loop (read-line)
                                        (cons line res))))))))

(define (externally-compile-file file-path)
  ;; TODO: check return code
  (define ldef-path (find-library-definition-file file-path))
  (define path-to-compile (or ldef-path file-path))
  (define lines (compile-and-collect-output path-to-compile))
  (write-log 'debug (format "externally-compile-file: compiled ~s."
                            path-to-compile))
  
  (let loop ((rem lines)
             (msg "")
             (filename #f)
             (diags '()))
    (if (null? rem)
        (reverse diags)
        (let ((line (car rem)))
          (cond
           ((irregex-search error-line-regex line)
            => (lambda (m)
                 (write-log 'debug
                            (format "Error line: ~s" line))
                 (loop (cdr rem)
                       (irregex-match-substring m 1)
                       #f
                       diags)))
           ((irregex-search in-file-regex line)
            => (lambda (m)
                 (write-log 'debug
                            (format "In-file line: ~s" line))
                 (loop (cdr rem)
                       msg
                       (irregex-match-substring m 1)
                       diags)))
           ((irregex-search on-line-regex line)
            => (lambda (m)
                 (write-log 'debug
                            (format "On-line line: ~s" line))
                 (let ((line-num
                        (max (- (string->number
                                 (or (irregex-match-substring m 1)
                                     (irregex-match-substring m 2)))
                                1)
                             0)))
                   (cond ((and filename
                               line-num)
                          (loop (cdr rem)
                                ""
                                filename
                                (cons (make-diagnostic "csc"
                                                       filename
                                                       line-num
                                                       0
                                                       msg)
                                      diags)))
                         (else
                          (write-log 'info
                                     (format "Diagnostics: could not find location for message ~s. Ignoring it."
                                             msg))
                          (loop (cdr rem)
                                ""
                                #f
                                diags))))))
           ((string=? line "")
            (loop (cdr rem)
                  msg
                  filename
                  diags))
           (else
            (loop (cdr rem)
                  (string-append msg ": " line)
                  filename
                  diags)))))))

(define ($compute-diagnostics file-path)
  (define abs-file-path
    (get-absolute-pathname file-path))
  (let ((diags (externally-compile-file file-path)))
    (write-log 'debug (format "$compute-diagnostics ~s got: ~s"
                              file-path
                              diags))
    (filter (lambda (d)
              (let ((res (string=? (get-absolute-pathname
                                    (diagnostic-file-path d))
                                   abs-file-path)))

                (write-log 'debug
                           (format "comparing ~s with ~s: ~s~%"
                                   (get-absolute-pathname
                                    (diagnostic-file-path d))
                                   abs-file-path
                                   res))
                res))
            diags)))

