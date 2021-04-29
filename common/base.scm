(define-record-type <apropos-info>
  (make-apropos-info module name type object)
  apropos-info?
  (module apropos-info-module)
  (name apropos-info-name)
  (type apropos-info-type)
  (object apropos-info-object))

(cond-expand
 (guile
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
                               result))))))))
 (else))

(define (join-module-name mod)
  (if mod
      (apply string-append (append '("(")
                                   (intersperse (map symbol->string mod)
                                                " ")
                                   '(")")))
      #f))

(define (split-module-name mod)
  (map string->symbol
       (string-split
        (substring mod
                   1
                   (- (string-length mod) 1))
        " ")))
