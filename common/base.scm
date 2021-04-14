(define-record-type <apropos-info>
  (make-apropos-info module name type object)
  apropos-info?
  (module apropos-info-module)
  (name apropos-info-name)
  (type apropos-info-type)
  (object apropos-info-object))

(define (join-module-name mod)
  (define $intersperse
    (cond-expand (chicken intersperse)
                 (guile string-join)))
  (apply string-append (append '("(")
                               ($intersperse (map symbol->string mod)
                                             " ")
                               '(")"))))

(define (split-module-name mod)
  (map string->symbol
       (string-split
        (substring mod
                   1
                   (- (string-length mod) 1))
        " ")))
