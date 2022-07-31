(import (srfi srfi-1)
        (srfi srfi-69))

(cond-expand
 (guile (define (func x)
          x))
 (chicken (define (func x)
            x))
 (else))

(define (func2 x . args)
  (cons x args))

(include "sample-3-included.scm")
