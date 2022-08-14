(cond-expand
 (guile (import (srfi srfi-1)
                (srfi srfi-18)
                (srfi srfi-28)
                (srfi srfi-69)
                (srfi srfi-64)))
 (else (import (srfi 1)
               (srfi 18)
               (srfi 28)
               (srfi 69)
               (srfi 64))))

(import (json-rpc)
        (json-rpc lolevel))

(import (lsp-server)
        (lsp-server private)
        (lsp-server document))

(cond-expand
 (chicken (import (scheme base)
                  (chicken base)
                  (chicken format)
                  (only (chicken keyword) keyword?)
                  (only (chicken port) with-input-from-string)
                  (only (srfi 130) string-replace)
                  (lsp-server chicken)
                  (lsp-server trie)))
 (guile (import (except (scheme base)
                        cond-expand
                        include
                        error
                        raise
                        map)
                (lsp-server guile))))

;; (include "../lsp-server/file.scm")
;; (include "../lsp-server/server.scm")

;; (include "parse-tests.scm")
;; (include "trie-tests.scm")

(cond-expand
 (chicken (import-for-syntax (only (scheme base) string->symbol)
                             (only (srfi 13) string-join)
                             (only (chicken string) conc ->string))
          (define-syntax @@
            (er-macro-transformer
             (lambda (form rename compare?)
               (let* ((lib-name (cadr form))
                      (identifier (caddr form))
                      (%eval (rename 'eval))
                      (%string->symbol (rename 'string->symbol))
                      (%string-append (rename 'string-append))
                      (%string-join (rename 'string-join))
                      (lib-name-str (if (pair? lib-name)
                                        (string-join (map ->string lib-name) ".")
                                        (->string lib-name)))
                      (internal-name-str (conc lib-name-str
                                               "#"
                                               identifier))
                      (internal-name (string->symbol internal-name-str)))
                 internal-name)))))
 (else))

(define document-append (@@ (lsp-server document) document-append))
(define document-copy (@@ (lsp-server document) document-copy))
(define document-newline-positions (@@ (lsp-server document) document-newline-positions))
(define line/char->pos (@@ (lsp-server document) line/char->pos))

(define parse-definition-line (@@ (lsp-server parse) parse-definition-line))

(define apply-change (@@ (lsp-server) apply-change))
(define make-change-contents (@@ (lsp-server) make-change-contents))
(define make-range (@@ (lsp-server) make-range))

(test-begin "lsp-server tests")

(test-group "(lsp-server document): document main functions"
  (define a-doc (string->document "abc\ndef\ng"))
  (test-equal 6
              (line/char->pos a-doc 1 2))
  (test-equal 0
              (line/char->pos a-doc 0 0))
  (test-equal 2
              (line/char->pos a-doc 0 2))
  (test-equal 8
              (line/char->pos a-doc 2 0))
  (test-equal 2 (line/char->pos (string->document "1\n\n3\n") 1 0)))


(test-group "(lsp-server document): document-copy"
  (let ((doc (document-copy
              (string->document "abc\ndef\n12\n34") 0 4)))
    (test-equal "abc\n" (document-contents doc))
    (test-equal #(3) (document-newline-positions doc)))

  (let ((doc (document-copy
              (string->document "") 0 0)))
    (test-equal "" (document-contents doc))
    (test-equal #() (document-newline-positions doc)))

  (let ((doc (document-copy
              (string->document "1\n\n3\n")
              0 2)))
    (test-equal "1\n" (document-contents doc))
    (test-equal #(1) (document-newline-positions doc)))

  (let ((doc (document-copy
              (string->document "1\n\n3\n")
              2)))
    (test-equal "\n3\n" (document-contents doc))
    (test-equal #(0 2) (document-newline-positions doc))))

(test-group "(lsp-server document): document-append"
  (let ((doc (document-append
              (string->document "abc\ndef\n")
              (string->document "12\n34"))))
    (test-equal "abc\ndef\n12\n34"
                (document-contents doc))
    (test-equal #(3 7 10)
                (document-newline-positions doc)))

  (let ((doc (document-append
              (string->document "abc\ndef\n")
              (string->document "12"))))
    (test-equal "abc\ndef\n12"
                (document-contents doc))
    (test-equal #(3 7)
                (document-newline-positions doc)))

  (let ((doc (document-append
              (string->document "abc")
              (string->document "def"))))
    (test-equal "abcdef"
                (document-contents doc))
    (test-equal #()
                (document-newline-positions doc)))

  (let ((doc (document-append
              (string->document "abc\n")
              (string->document "def"))))
    (test-equal "abc\ndef"
                (document-contents doc))
    (test-equal #(3)
                (document-newline-positions doc)))

  (let ((doc (document-append
              (string->document "abc\n")
              (string->document "\ndef\n"))))
    (test-equal "abc\n\ndef\n"
                (document-contents doc))
    (test-equal #(3 4 8)
                (document-newline-positions doc))))

(test-group "(lsp-server document): document-contract"
  (let ((doc (document-contract (string->document "ab\nde")
                                2 4)))
    (test-equal "abe" (document-contents doc))
    (test-equal #() (document-newline-positions doc)))

  (let ((doc (document-contract (string->document "a\nb")
                                1 2)))
    (test-equal "ab" (document-contents doc))
    (test-equal #() (document-newline-positions doc)))

  (let ((doc (document-contract (string->document "abcde")
                                2 4)))
    (test-equal "abe" (document-contents doc))
    (test-equal #() (document-newline-positions doc)))

  (let ((doc (document-contract (string->document "abcde")
                                3 4)))
    (test-equal "abce" (document-contents doc))
    (test-equal #() (document-newline-positions doc))))

(test-group "(lsp-server document): document-insert"
  (let* ((doc (document-insert (string->document "ab\n")
                               "123\n45"
                               3)))
    (test-equal "ab\n123\n45" (document-contents doc))
    (test-equal #(2 6) (document-newline-positions doc)))

  (let* ((doc (document-insert (string->document "ab\ncd")
                               "\n\n"
                               3)))
    (test-equal "ab\n\n\ncd" (document-contents doc))
    (test-equal #(2 3 4) (document-newline-positions doc)))

  (let* ((doc (document-insert (string->document "ab\ncd\ne")
                               "12"
                               4)))
    (test-equal "ab\nc12d\ne" (document-contents doc))
    (test-equal #(2 7) (document-newline-positions doc)))

  (let ((doc (document-insert (string->document "a   b")
                              "123" 1)))
    (test-equal "a123   b" (document-contents doc))
    (test-equal #() (document-newline-positions doc)))

  (let ((doc (document-insert (string->document "a   b")
                              "12345" 1)))
    (test-equal "a12345   b" (document-contents doc))
    (test-equal #() (document-newline-positions doc)))

  (test-equal "ab12345"
              (document-contents (document-insert (string->document "ab")
                                                  "12345" 2))))



(test-group "(lsp-server document): string->document"
  (test-equal
      #(1 2 4)
    (document-newline-positions (string->document "1\n\n3\n"))))


(test-group "line/char->pos"
  (test-equal 2
              (line/char->pos (string->document "a\nb\nc") 1 0))

  (test-equal 0
              (line/char->pos (string->document "a\nb\nc") 0 0))

  (test-equal 5
              (line/char->pos (string->document "0123\n56") 1 0))

  (test-equal 6
              (line/char->pos (string->document "0123\n56") 1 1))

  (test-equal 7
              (line/char->pos (string->document "0123\n56") 1 2))

  (test-equal 4
              (line/char->pos (string->document "0123\n56") 0 4)))

(cond-expand
 (chicken
  (test-group "(lsp-server tag): tag generation"
    (test-equal "x"
                (car (parse-definition-line "define x")))

    (test-equal "func"
                (car (parse-definition-line "(define   (func x)")))

    (test-equal "my-macro"
                (car (parse-definition-line "(define-syntax my-macro")))

    (test-equal "var"
                (car (parse-definition-line "(define\n var")))

    (test-equal "var"
                (car (parse-definition-line "(define var (+ x 1))")))

    (test-equal "var"
                (car (parse-definition-line "(set! var (+ x 1))")))

    (test-equal #f
                (parse-definition-line "defin var"))

    (test-equal #f
                (parse-definition-line "definevar"))

    (test-equal #f
                (parse-definition-line "(define(var"))))
 (else))

(test-group "(lsp-server document): range update"
  (define document
    (string->document
     (string-append "(define (f x)\n"
                    "  (cond ((= x 0) 1)\n"
                    "        ((= x 1) 1)\n"
                    "        (else (* x (f (- x 1))))))\n")))

  (test-equal (string-append "(define (g x)\n"
                             "  (cond ((= x 0) 1)\n"
                             "        ((= x 1) 1)\n"
                             "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 0 9 0 10 1)
                 "g")
                document)))

  (test-equal (string-append "(define (f x)\n"
                             "  (cond ((= x 0) 1)\n"
                             "\n"
                             "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 2 0 2 19 19)
                 "")
                document)))

  (test-equal (string-append "(define (f x)\n"
                             "  (cond ((= x 0) 1)\n"
                             "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 2 0 3 0 20)
                 "")
                document)))

  (test-equal (string-append
               "(define (f x)\n"
               "  (cond ((= x 0) 1)\n"
               "        ((= x 1) 1)\n"
               "        ((= x 2) 2)\n"
               "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 2 19 2 19 0)
                 "\n        ((= x 2) 2)")
                document)))

  (test-equal (string-append
               "(define (f x)\n"
               "  (cond ((= x 0) 1)\n"
               "        ((= x 1) 1)\n"
               "\n"
               "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 2 19 2 19 0)
                 "\n")
                document)))

  (test-equal (string-append "        ((= x 1) 1)\n"
                             "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 0 0 2 0 34)
                 "")
                document)))

  (test-equal (string-append
               "(define (f x)\n"
               "  (cond ((= x 0) 1)\n"
               "        ((= x 1) 1)\n"
               "        (else (* x (f (- x 1))))))\n"
               "\n"          )
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 3 34 3 34 0)
                 "\n")
                document)))

  (test-equal (string-append
               "i(define (f x)\n"
               "  (cond ((= x 0) 1)\n"
               "        ((= x 1) 1)\n"
               "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 0 0 0 0 0)
                 "i")
                document)))

  (test-equal (string-append "(define (f x)\n"
                             "blacond ((= x 0) 1)\n"
                             "        ((= x 1) 1)\n"
                             "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 1 0 1 3 3)
                 "bla")
                document)))

  (test-equal (string-append "i\n" "bla\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 0 0 0 0 0)
                 "i")
                (string->document
                 (string-append "\n" "bla\n")))))

  (test-equal (string-append
               "(define (f x)\n"
               "        ((= x 1) 1)\n"
               "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 1 0 2 0 20)
                 "")
                document)))

  (test-equal (string-append
               "(define (f x)\n"
               "  (cond ((= x 0) 1)        ((= x 1) 1)\n"
               "        (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 2 0 1 19 1)
                 "")
                document)))

  #;
  (test-equal '("(define (f x)\n"
  "  (cond ((= x 0) 1)\n"
  "(+ 2\n"
  "   3)        ((= x 1) 1)\n"
  "        (else (* x (f (- x 1))))))\n")
  (apply-change (make-change-contents
  (make-range 2 0 2 5 10)
  '("(+ 2\n   3)"))
  document))

  (test-equal (string-append
               "(define (f x)\n"
               "  (cond ((= x 0) 1)\n"
               "(+ 2\n"
               "   3)   (else (* x (f (- x 1))))))\n")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 2 0 3 5 10)
                 (string-append "(+ 2\n" "   3)"))
                document)))

  (test-equal (string-append
               "(define (f x)\n"
               "  (cond ((= x 0) 1)\n"
               "        ((= x 1) 1)\n"
               "        (else (* x (f (- x 1))))))\n"
               "(")
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 4 0 4 0 0)
                 "(")
                document)))

  (test-equal ""
              (document-contents
               (apply-change
                (make-change-contents
                 (make-range 0 0 4 0 88)
                 "")
                document)))

  (let ((doc (apply-change
              (make-change-contents
               (make-range 1 0 2 0 1)
               "")
              (string->document "1\n\n3\n"))))
    (test-equal "1\n3\n" (document-contents doc))
    (test-equal #(1 3) (document-newline-positions doc)))

  (let ((doc (apply-change
              (make-change-contents
               (make-range 1 0 1 0 0)
               "\n")
              (string->document "1\n3\n"))))
    (test-equal "1\n\n3\n" (document-contents doc))
    (test-equal #(1 2 4) (document-newline-positions doc)))

  (let ((doc (apply-change
              (make-change-contents
               (make-range 1 0 1 0 0)
               "\n")
              (string->document "1\n\n3\n"))))
    (test-equal "1\n\n\n3\n" (document-contents doc))
    (test-equal #(1 2 3 5) (document-newline-positions doc)))

  (let ((doc (apply-change
              (make-change-contents
               (make-range 2 0 3 0 1)
               "")
              (string->document "1\n\n\n3\n"))))
    (test-equal "1\n\n3\n" (document-contents doc))
    (test-equal #(1 2 4) (document-newline-positions doc))))

(test-group "(lsp-server document): line manipulation"
  (test-equal '()
              (delete-lines '("a" "b" "c" "d") 0 3))
  (test-equal '("a" "b" "d")
              (delete-lines '("a" "b" "c" "d") 2 2))
  (test-equal '()
              (delete-lines '("a") 0 0)))

(test-end)

