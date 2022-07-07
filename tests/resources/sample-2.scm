(import (srfi srfi-1)
        (srfi srfi-69))

(cond-expand
 (guile (define (func x)
          x))
 (chicken (define (func x)
            x))
 (else))

(include "sample-3-included.scm")
