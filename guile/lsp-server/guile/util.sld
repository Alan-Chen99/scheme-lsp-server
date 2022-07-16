(define-module (lsp-server guile util)

#:export (alist-ref
          alist-ref/default
          current-directory
          create-directory

          absolute-pathname?
          pathname-directory
          pathname-join
          get-absolute-pathname
          hash-table-join!)

#:use-module ((scheme base) #:select (assoc guard))
#:use-module (srfi srfi-69)
#:declarative? #f
)

(define (alist-ref key lst)
  (define res (assoc key lst))
  (if res
      (cdr res)
      #f))

(define (alist-ref/default key lst default)
  (define res (assoc key lst))
  (if res
      (cdr res)
      default))

(define current-directory getcwd)

(define (pathname-join dir-name file-name)
  (string-concatenate (list dir-name
                            file-name-separator-string
                            file-name)))

(define (pathname-directory path)
  (dirname path))

(define absolute-pathname? absolute-file-name?)

(define (get-absolute-pathname path)
  (guard
   (condition
    (#t #f))
   (canonicalize-path path)))

(define (hash-table-join! ht other-ht)
  (hash-table-fold
   other-ht (lambda (k v ign) (hash-table-set! ht k v)) #f)
  ht)

(define (create-directory path)
  (define parts
    (filter (lambda (p)
              (not (string=? p "")))
            (string-split path
                          (string->char-set
                           file-name-separator-string))))
  (display parts)
  (when (null? parts)
    (error "invalid path" path))
  (let loop ((directory-name (if (absolute-pathname? path)
                                 (string-append
                                  file-name-separator-string
                                  (car parts))
                                 (car parts)))
             (rest-parts (cdr parts)))
    (when (not (file-exists? directory-name))
      (mkdir directory-name))
    (if (null? rest-parts)
        #t
        (loop (string-append directory-name
                             file-name-separator-string
                             (car rest-parts))
              (cdr rest-parts)))))
