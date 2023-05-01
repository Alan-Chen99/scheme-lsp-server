(import (srfi 1)
        (srfi 69))

(cond-expand
 ((or gambit guile)
  (define (func x)
    x))
 (chicken (define (func x)
            x))
 (else))

(define (func2 x . args)
  (cons x args))

(include "sample-3-included.scm")
