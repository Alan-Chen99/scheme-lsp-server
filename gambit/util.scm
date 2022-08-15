(define-library (lsp-server gambit util)

(export alist-ref
        alist-ref/default
        irregex
        irregex-match
        irregex-match-substring
        irregex-match-start-index
        irregex-search
        vector-fold
        with-input-from-string)

(import (scheme base)
        (gambit))

(include "../irregex.scm")

(begin
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

  (define (with-input-from-string str thunk)
    (define p (open-input-string str))
    (dynamic-wind
        (lambda () #t)
        (lambda ()
          (parameterize ((current-input-port p))
            (thunk)))
        (lambda () (close-input-port p))))

  (define (alist-ref key lst)
    (define res (assoc key lst))
    (if res
        (cdr res)
        #f))

  (define (alist-ref/default key lst default)
    (define res (assoc key lst))
    (if res
        (cdr res)
        default))))
