(define (lsp-geiser-completions prefix)
  (completions prefix))

(define (lsp-geiser-signature identifier)
  (define doc (alist-ref/default identifier (autodoc (list identifier)) '()))
  (define args (alist-ref/default "args" doc '()))
  (define required-args
    (map symbol->string
         (if (not (null? args))
             (alist-ref/default "required" (car args) '())
             '())))
  (define optional-args
    (map symbol->string
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
  (define doc (symbol-documentation identifier))
  (if (and doc (list? doc))
      (alist-ref "docstring" doc)
      #f))

(define (lsp-geiser-symbol-location identifier)
  (define loc (symbol-location identifier))
  (define file (and loc (alist-ref "file" loc)))
  (define line (and loc (alist-ref "line" loc)))
  (if loc
      `((uri . ,(string-append "file://" file))
        (range . ((start . ((line . ,line)
                            (character . 0)))
                  (end . ((line . ,line)
                          (character . 0))))))
      '()))
