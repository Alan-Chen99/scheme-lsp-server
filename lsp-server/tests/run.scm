(import (chicken base)
        (srfi 1)
        test)

(import (lsp-server private)
        (lsp-server document))

(test-group "Line manipulation"
  (test '()
    (delete-lines '("a" "b" "c" "d") 0 3))
  (test '("a" "b" "d")
    (delete-lines '("a" "b" "c" "d") 2 2))
  (test '()
    (delete-lines '("a") 0 0)))


(test-group "Range update"
  (define document
    (string-append "(define (f x)\n"
                   "  (cond ((= x 0) 1)\n"
                   "        ((= x 1) 1)\n"
                   "        (else (* x (f (- x 1))))))\n"))

  (test (string-append "(define (g x)\n"
                       "  (cond ((= x 0) 1)\n"
                       "        ((= x 1) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 0 9 0 10 1)
                   "g")
                  document))

  (test (string-append "(define (f x)\n"
                       "  (cond ((= x 0) 1)\n"
                       "\n"
                       "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 0 2 19 19)
                   "")
                  document))

  (test (string-append "(define (f x)\n"
                       "  (cond ((= x 0) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 0 3 0 20)
                   "")
                  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        ((= x 2) 2)\n"
         "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 19 2 19 0)
                   "\n        ((= x 2) 2)")
                  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "\n"
         "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 19 2 19 0)
                   "\n")
                  document))

  (test (string-append "        ((= x 1) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 0 0 2 0 34)
                   "")
                  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n"
         "\n"          )
    (apply-change (make-change-contents
                   (make-range 3 34 3 34 0)
                   "\n")
                  document))

  (test (string-append
         "i(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 0 0 0 0 0)
                   "i")
                  document))

  (test (string-append "(define (f x)\n"
                       "blacond ((= x 0) 1)\n"
                       "        ((= x 1) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 1 0 1 3 3)
                   "bla")
                  document))

  (test (string-append "i\n" "bla\n")
    (apply-change (make-change-contents
                   (make-range 0 0 0 0 0)
                   "i")
                  (string-append "\n" "bla\n")))

  (test (string-append
         "(define (f x)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 1 0 2 0 20)
                   "")
                  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 0 1 19 1)
                   "")
                  document))

  #;
  (test '("(define (f x)\n"
          "  (cond ((= x 0) 1)\n"
          "(+ 2\n"
          "   3)        ((= x 1) 1)\n"
          "        (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 0 2 5 10)
                   '("(+ 2\n   3)"))
                  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "(+ 2\n"
         "   3)   (else (* x (f (- x 1))))))\n")
    (apply-change (make-change-contents
                   (make-range 2 0 3 5 10)
                   (string-append "(+ 2\n" "   3)"))
                  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n"
         "(")
    (apply-change (make-change-contents
                   (make-range 4 0 4 0 0)
                   "(")
                  document))

  (test ""
    (apply-change (make-change-contents
                   (make-range 0 0 4 0 88)
                   "")
                  document)))


(test-group "append-lines[*]"
    (test '("ab\n")
      (append-lines '("a" "b\n")))

    (test '("a\n" "b\n")
      (append-lines '("a\n" "b\n")))

    (test '()
      (append-lines '()))

    (test '("a")
      (append-lines '("a")))

    (test '()
      (append-lines '("")))

    (test '("a\n" "b\n" "c\n")
      (append-lines* '("a\n" "b\n") '("c\n")))

    (test '("ac\n")
      (append-lines* '("a") '("c\n")))

    (test '("ac\n" "b")
      (append-lines* '("a") '("c\n") '("b"))))


(test-group "document insertion/expansion/contraction"
  (test "a     b"
    (document-expand "ab" 1 5))

  (test "a123   b"
    (document-insert "a   b" "123" 1))

  (test "a12345   b"
    (document-insert "a   b" "12345" 1))

  (test "abe"
    (document-contract "abcde" 2 4)))

(test-group "line/char->pos"
  (test 2
    (line/char->pos "a\nb\nc" 1 0))

  (test 0
    (line/char->pos "a\nb\nc" 0 0))

  (test 5
    (line/char->pos "0123\n56" 1 0))

  (test 6
    (line/char->pos "0123\n56" 1 1)))
