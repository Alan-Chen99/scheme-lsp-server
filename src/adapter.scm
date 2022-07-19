(define (lsp-geiser-completions prefix)
  (geiser-completions prefix))

(define (lsp-geiser-signature identifier)
  (define doc (alist-ref/default identifier (geiser-autodoc (list identifier)) '()))
  (define args (alist-ref/default "args" doc '()))
  (define required-args
    (map stringify
         (if (not (null? args))
             (alist-ref/default "required" (car args) '())
             '())))
  (define optional-args
    (map stringify
         (if (not (null? args))
             (alist-ref/default "optional" (car args) '())
             '())))
  (define key-args
    (map (lambda (kw-pair)
           (format "~a" kw-pair))
         (if (not (null? args))
             (alist-ref/default "key" (car args) '())
             '())))
  (define module-name (alist-ref "module" doc))
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
                   " ")))))

(define (lsp-geiser-documentation identifier)
  (define doc (geiser-symbol-documentation identifier))
  (if (and doc (list? doc))
      (alist-ref "docstring" doc)
      #f))

(define (lsp-geiser-symbol-location identifier)
  (define loc (geiser-symbol-location identifier))
  (define file (and loc (alist-ref "file" loc)))
  (define line (and loc (alist-ref "line" loc)))
  (define file-path (if (and file (not (null? file)))
                        (format "file://~a" file)
                        #f))
  (cond ((and line (< line 0))
         (write-log 'error
                    "lsp-geiser-symbol-location: line is negative")
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
   (guile (ge:compile-file file-path))
   (chicken (geiser-compile-file file-path))))

(define (lsp-geiser-load-file file-path)
  (cond-expand
   (guile (ge:load-file file-path))
   (chicken (geiser-load-file file-path))))
