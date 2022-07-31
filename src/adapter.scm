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

(define (lsp-geiser-symbol-location module identifier)
  (define loc (geiser-symbol-location identifier))
  (write-log 'debug (format "lsp-geiser-symbol-location loc: ~s" loc))

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
                          (module (format "file://~a"
                                          (let ((mod-path (geiser-module-path module)))
                                            (if (absolute-pathname? mod-path)
                                                mod-path
                                                (get-absolute-pathname mod-path)))))
                          (else #f)))
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
   (guile (define load-file-fn ge:load-file))
   (chicken (define load-file-fn geiser-load-file)))
  (guard
   (condition (#t (write-log 'error
                             (format "load-file ~a error: ~a"
                                     file-path
                                     condition))))
   (load-file-fn file-path)))
