(cond-expand
 (gambit
  (define geiser-completions geiser:module-completions)
  (define geiser-autodoc geiser:autodoc)
  (define geiser-symbol-documentation (lambda (x) (error "not implemented")))
  (define geiser-symbol-location (lambda (x) (error "not implemented")))
  (define geiser-symbol-module (lambda (x) (error "not implemented"))))
 (else))

(define (lsp-geiser-completions prefix)
  (geiser-completions prefix))

(define (lsp-geiser-signature identifier)
  (define autodoc-result (geiser-autodoc (list identifier)))
  (if (or (null? autodoc-result)
          (null? (car autodoc-result)))
      #f
      (let* ((doc (alist-ref/default identifier autodoc-result '()))
             (args (alist-ref/default "args" doc '()))
             (required-args
              (map stringify
                   (if (not (null? args))
                       (alist-ref/default "required" (car args) '())
                       '())))
             (optional-args
              (map stringify
                   (if (not (null? args))
                       (alist-ref/default "optional" (car args) '())
                       '())))
             (key-args
              (map (lambda (kw-pair)
                     (format "~a" kw-pair))
                   (if (not (null? args))
                       (alist-ref/default "key" (car args) '())
                       '())))
             (raw-module-name (alist-ref "module" doc))
             (module-name (cond ((and raw-module-name
                                      (list? raw-module-name)
                                      (null? raw-module-name))
                                 #f)
                                (else raw-module-name))))
        (if (null? doc)
            #f
            (format "(~a~a ~a)"
                    (if module-name
                        (format "~a:" module-name)
                        "")
                    identifier
                    (string-append
                     (string-join
                      (map (lambda (req-arg)
                             (format "~a" req-arg))
                           required-args)
                      " ")
                     (if (not (null? optional-args))
                         " "
                         "")
                     (apply string-append
                            (map (lambda (opt-arg)
                                   (format "(~a)" opt-arg))
                                 optional-args))
                     (if (not (null? key-args))
                         " "
                         "")
                     (string-join
                      (map (lambda (key-arg)
                             (format "~a" key-arg))
                           key-args)
                      " ")))))))

(define (lsp-geiser-documentation identifier)
  (define doc (geiser-symbol-documentation identifier))
  (if (and doc (list? doc))
      (alist-ref "docstring" doc)
      #f))

(define (lsp-geiser-symbol-location module identifier)
  (define loc (geiser-symbol-location identifier))

  (define file (and loc
                    (not (null? loc))
                    (let ((val (alist-ref "file" loc)))
                      (and (not (null? val))
                           val))))
  (define line (and loc
                    (not (null? loc))
                    (let ((val (alist-ref "line" loc)))
                      (and (not (null? val))
                           val))))
  (define file-path (cond ((and file (not (null? file)))
                           (format "file://~a"
                                   (if (absolute-pathname? file)
                                       file
                                       (get-absolute-pathname file))))
                          (module (cond-expand
                                   (guile (let* ((mod-name (geiser-symbol-module identifier))
                                                 (mod-path (and mod-name
                                                                (geiser-module-path mod-name))))
                                            (and mod-path
                                                 (format "file://~a"
                                                         (if (absolute-pathname? mod-path)
                                                             mod-path
                                                             (get-absolute-pathname mod-path))))))
                                   (else #f)))
                          (else #f)))
  (write-log 'debug
             (format "lsp-geiser-symbol-location loc: ~a, file: ~a, line: ~a, file-path: ~a"
                     loc file line file-path))

  (cond ((and line (< line 0))
         (write-log 'warning "lsp-geiser-symbol-location: line number is negative")
         '())
        ((and file-path line)
         `((uri . ,file-path)
           (range . ((start . ((line . ,(- line 1))
                               (character . 0)))
                     (end . ((line . ,line)
                             (character . 0)))))))
        (else '())))

(define (lsp-geiser-compile-file file-path)
  (write-log 'debug (format "lsp-geiser-compile-file: ~a" file-path))
  (cond-expand
   (gambit (compile-file file-path))
   (guile (ge:compile-file file-path))
   (chicken (geiser-compile-file file-path))))

(define (lsp-geiser-load-file file-path)
  (cond-expand
   ((or gambit guile)
    (define load-file-fn ge:load-file))
   (chicken (define load-file-fn geiser-load-file)))
  (call/cc
   (lambda (k)
     (with-exception-handler
      (lambda (condition)
        (write-log 'error
                   (format "load-file ~a error: ~a"
                           file-path
                           condition))
        (k #f))
      (lambda () (load-file-fn file-path))))))

(define (lsp-geiser-module-path module-name)
  (geiser-module-path module-name))
