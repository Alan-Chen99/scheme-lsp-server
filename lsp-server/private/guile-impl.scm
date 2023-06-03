;;; copied over from srfi-133
(define (vector-fold kons knil vec1 . o)
  (let ((len (vector-length vec1)))
    (if (null? o)
        (let lp ((i 0)
                 (acc knil))
          (if (>= i len)
              acc
              (lp (+ i 1)
                  (kons acc (vector-ref vec1 i)))))
        (let lp ((i 0)
                 (acc knil))
          (if (>= i len)
              acc
              (lp (+ i 1)
                  (apply kons acc (vector-ref vec1 i)
                         (map (lambda (v)
                                (vector-ref v i))
                              o))))))))

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

(define (get-module-path mod)
  #f)

(define (pathname-join dir-name file-name)
  (string-concatenate (list dir-name
                            file-name-separator-string
                            file-name)))

(define (pathname-directory path)
  (dirname path))

(define (pathname-base path)
  (basename path))

(define (pathname-strip-extension path)
  (define len (string-length path))

  (if (= len 0)
      path
      (let loop ((i (- len 1)))
        (if (= i 0)
            path
            (let ((c (string-ref path i)))
               (cond ((file-name-separator? c)
                      path)
                     ((char=? c #\.)
                      (if (or (= i (- len 1))
                              (= i 0)
                              (file-name-separator?
                               (string-ref path (- i 1))))
                          path
                          (string-take path i)))
                     (else (loop (- i 1)))))))))

(define absolute-pathname? absolute-file-name?)

(define (get-absolute-pathname path)
  (if (absolute-file-name? path)
      path
      (let ((base-path
             (find (lambda (load-path)
                     (file-exists? (string-append load-path
                                                  file-name-separator-string
                                                  path)))
                   %load-path)))
        (guard
            (condition
             (#t (write-log 'error
                            (format "error getting absolute path of ~s: ~a"
                                    path
                                    condition))
                 #f))
          (if base-path
              (canonicalize-path (string-append base-path
                                                file-name-separator-string
                                                path))
              #f)))))

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

(define (intersperse lst delim)
  (let loop ((remaining lst)
             (result '()))
    (cond ((null? remaining)
           (reverse result))
          ((null? (cdr remaining))
           (reverse (cons (car remaining) result)))
          (else
           (loop (cdr remaining)
                 (cons delim
                       (cons (car remaining)
                             result)))))))

(define (condition->string exc)
  (format "~a" exc))
