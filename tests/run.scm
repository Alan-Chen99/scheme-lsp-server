(import (chicken base)
        (chicken format)
        (srfi 1)
        (srfi 69)
        test)

(import (lsp-server)
        (lsp-server chicken)
        (lsp-server private)
        (lsp-server document)
        (lsp-server tags))


(test-group "(lsp-server document): document main functions"
  (define a-doc (string->document "abc\ndef\ng"))
  (test 6
    (line/char->pos a-doc 1 2))
  (test 0
    (line/char->pos a-doc 0 0))
  (test 2
    (line/char->pos a-doc 0 2))
  (test 8
    (line/char->pos a-doc 2 0))
  (let ((offsets (compute-lines-offsets "abc\ndef\ng")))
    (test 2
      (vector-length offsets))
    (test 3 (vector-ref offsets 0))
    (test 7 (vector-ref offsets 1)))

  (test 2 (line/char->pos (string->document "1\n\n3\n") 1 0)))


(test-group "(lsp-server document): document-copy"
  (let ((doc (lsp-server.document#document-copy
              (string->document "abc\ndef\n12\n34") 0 4)))
    (test "abc\n" (document-contents doc))
    (test #(3) (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-copy
              (string->document "") 0 0)))
    (test "" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-copy
              (string->document "1\n\n3\n")
              0 2)))
    (test "1\n" (document-contents doc))
    (test #(1) (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-copy
              (string->document "1\n\n3\n")
              2)))
    (test "\n3\n" (document-contents doc))
    (test #(0 2) (lsp-server.document#document-lines-offsets doc))))

(test-group "(lsp-server document): document-append"
  (let ((doc (lsp-server.document#document-append
              (string->document "abc\ndef\n")
              (string->document "12\n34"))))
    (test "abc\ndef\n12\n34"
      (document-contents doc))
    (test #(3 7 10)
      (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-append
              (string->document "abc\ndef\n")
              (string->document "12"))))
    (test "abc\ndef\n12"
      (document-contents doc))
    (test #(3 7)
      (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-append
              (string->document "abc")
              (string->document "def"))))
    (test "abcdef"
      (document-contents doc))
    (test #()
      (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-append
              (string->document "abc\n")
              (string->document "def"))))
    (test "abc\ndef"
      (document-contents doc))
    (test #(3)
      (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server.document#document-append
              (string->document "abc\n")
              (string->document "\ndef\n"))))
    (test "abc\n\ndef\n"
      (document-contents doc))
    (test #(3 4 8)
      (lsp-server.document#document-lines-offsets doc))))

(test-group "(lsp-server document): document-contract"
  (let ((doc (document-contract (string->document "ab\nde")
                                2 4)))
    (test "abe" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (document-contract (string->document "a\nb")
                                1 2)))
    (test "ab" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (document-contract (string->document "abcde")
                                2 4)))
    (test "abe" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (document-contract (string->document "abcde")
                                3 4)))
    (test "abce" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc))))

(test-group "(lsp-server document): document-insert"
  (let* ((doc (document-insert (string->document "ab\n")
                               "123\n45"
                               3)))
    (test "ab\n123\n45" (document-contents doc))
    (test #(2 6) (lsp-server.document#document-lines-offsets doc)))

  (let* ((doc (document-insert (string->document "ab\ncd")
                               "\n\n"
                               3)))
    (test "ab\n\n\ncd" (document-contents doc))
    (test #(2 3 4) (lsp-server.document#document-lines-offsets doc)))

  (let* ((doc (document-insert (string->document "ab\ncd\ne")
                               "12"
                               4)))
    (test "ab\nc12d\ne" (document-contents doc))
    (test #(2 7) (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (document-insert (string->document "a   b")
                              "123" 1)))
    (test "a123   b" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (document-insert (string->document "a   b")
                              "12345" 1)))
    (test "a12345   b" (document-contents doc))
    (test #() (lsp-server.document#document-lines-offsets doc)))

  (test "ab12345"
    (document-contents (document-insert (string->document "ab")
                                        "12345" 2))))



(test-group "(lsp-server document): string->document"
  (test
      #(1 2 4)
    (lsp-server.document#document-lines-offsets (string->document "1\n\n3\n"))))


(test-group "line/char->pos"
  (test 2
    (line/char->pos (string->document "a\nb\nc") 1 0))

  (test 0
    (line/char->pos (string->document "a\nb\nc") 0 0))

  (test 5
    (line/char->pos (string->document "0123\n56") 1 0))

  (test 6
    (line/char->pos (string->document "0123\n56") 1 1))

  (test 7
    (line/char->pos (string->document "0123\n56") 1 2))

  (test 4
    (line/char->pos (string->document "0123\n56") 0 4)))

(test-group "(lsp-server tag): tag generation"
  (test "x"
    (car (parse-definition-line "define x")))

  (test "func"
    (car (parse-definition-line "(define   (func x)")))

  (test "my-macro"
    (car (parse-definition-line "(define-syntax my-macro")))

  (test "var"
    (car (parse-definition-line "(define\n var")))

  (test "var"
    (car (parse-definition-line "(define var (+ x 1))")))

  (test "var"
    (car (parse-definition-line "(set! var (+ x 1))")))

  (test #f
    (parse-definition-line "defin var"))

  (test #f
    (parse-definition-line "definevar"))

  (test #f
    (parse-definition-line "(define(var")))

(test-group "(lsp-server document): range update"
  (define document
    (string->document
     (string-append "(define (f x)\n"
                    "  (cond ((= x 0) 1)\n"
                    "        ((= x 1) 1)\n"
                    "        (else (* x (f (- x 1))))))\n")))

  (test (string-append "(define (g x)\n"
                       "  (cond ((= x 0) 1)\n"
                       "        ((= x 1) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 0 9 0 10 1)
       "g")
      document)))

  (test (string-append "(define (f x)\n"
                       "  (cond ((= x 0) 1)\n"
                       "\n"
                       "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 2 0 2 19 19)
       "")
      document)))

  (test (string-append "(define (f x)\n"
                       "  (cond ((= x 0) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 2 0 3 0 20)
       "")
      document)))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        ((= x 2) 2)\n"
         "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 2 19 2 19 0)
       "\n        ((= x 2) 2)")
      document)))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "\n"
         "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 2 19 2 19 0)
       "\n")
      document)))

  (test (string-append "        ((= x 1) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 0 0 2 0 34)
       "")
      document)))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n"
         "\n"          )
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 3 34 3 34 0)
       "\n")
      document)))

  (test (string-append
         "i(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 0 0 0 0 0)
       "i")
      document)))

  (test (string-append "(define (f x)\n"
                       "blacond ((= x 0) 1)\n"
                       "        ((= x 1) 1)\n"
                       "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 1 0 1 3 3)
       "bla")
      document)))

  (test (string-append "i\n" "bla\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 0 0 0 0 0)
       "i")
      (string->document
       (string-append "\n" "bla\n")))))

  (test (string-append
         "(define (f x)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 1 0 2 0 20)
       "")
      document)))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 2 0 1 19 1)
       "")
      document)))

  #;
  (test '("(define (f x)\n"
  "  (cond ((= x 0) 1)\n"
  "(+ 2\n"
  "   3)        ((= x 1) 1)\n"
  "        (else (* x (f (- x 1))))))\n")
  (lsp-server#apply-change (lsp-server#make-change-contents
  (lsp-server#make-range 2 0 2 5 10)
  '("(+ 2\n   3)"))
  document))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "(+ 2\n"
         "   3)   (else (* x (f (- x 1))))))\n")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 2 0 3 5 10)
       (string-append "(+ 2\n" "   3)"))
      document)))

  (test (string-append
         "(define (f x)\n"
         "  (cond ((= x 0) 1)\n"
         "        ((= x 1) 1)\n"
         "        (else (* x (f (- x 1))))))\n"
         "(")
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 4 0 4 0 0)
       "(")
      document)))

  (test ""
    (document-contents
     (lsp-server#apply-change
      (lsp-server#make-change-contents
       (lsp-server#make-range 0 0 4 0 88)
       "")
      document)))

  (let ((doc (lsp-server#apply-change
              (lsp-server#make-change-contents
               (lsp-server#make-range 1 0 2 0 1)
               "")
              (string->document "1\n\n3\n"))))
    (test "1\n3\n" (document-contents doc))
    (test #(1 3) (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server#apply-change
              (lsp-server#make-change-contents
               (lsp-server#make-range 1 0 1 0 0)
               "\n")
              (string->document "1\n3\n"))))
    (test "1\n\n3\n" (document-contents doc))
    (test #(1 2 4) (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server#apply-change
              (lsp-server#make-change-contents
               (lsp-server#make-range 1 0 1 0 0)
               "\n")
              (string->document "1\n\n3\n"))))
    (test "1\n\n\n3\n" (document-contents doc))
    (test #(1 2 3 5) (lsp-server.document#document-lines-offsets doc)))

  (let ((doc (lsp-server#apply-change
              (lsp-server#make-change-contents
               (lsp-server#make-range 2 0 3 0 1)
               "")
              (string->document "1\n\n\n3\n"))))
    (test "1\n\n3\n" (document-contents doc))
    (test #(1 2 4) (lsp-server.document#document-lines-offsets doc))))

(test-group "(lsp-server document): line manipulation"
  (test '()
    (delete-lines '("a" "b" "c" "d") 0 3))
  (test '("a" "b" "d")
    (delete-lines '("a" "b" "c" "d") 2 2))
  (test '()
    (delete-lines '("a") 0 0)))
