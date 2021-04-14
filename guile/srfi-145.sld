(define-module (srfi srfi-145)
#:export (assume)
#:use-module (scheme base))

(define-syntax assume
  (syntax-rules ()
    ((assume expression message ...)
     (or expression
         (fatal-error "invalid assumption" (quote expression) (list message ...))))
    ((assume . _)
     (syntax-error "invalid assume syntax"))))

(cond-expand
 (debug
  (begin
    (define fatal-error error)))
 (else
  (begin
    (define (fatal-error message . objs)
      (car 0)))))
