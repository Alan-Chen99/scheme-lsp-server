(cond-expand
 (guile (import (except (scheme base)
                        cond-expand
                        include
                        map
                        error)
                (srfi srfi-1)
                (only (srfi srfi-13) string-contains)
                (srfi srfi-28)
                (srfi srfi-64)
                (except (srfi srfi-69) hash-table-merge!)
                (ice-9 ftw)
                (lsp-server private)
                (lsp-server guile)
                (lsp-server guile util)
                (lsp-server trie)))
 (chicken (import (srfi 1)
                  (only (srfi 13) string-contains)
                  (srfi 28)
                  (srfi 64)
                  (srfi 69)
                  (scheme base)
                  (only (scheme file) with-input-from-file)
                  (scheme read)
                  (scheme)
                  (chicken irregex)
                  (chicken file)
                  (chicken file posix)
                  (lsp-server chicken)
                  (lsp-server chicken util)
                  (lsp-server private))))
(cond-expand
 (chicken
  (define hash-table-join! hash-table-merge!)
  (define (library-available? x) #t))
 (else))
(import (lsp-server parse))
(cond-expand
 (guile (include "../src/irregex.scm"))
 (else))

(include "../src/parse.scm")

(test-begin "cond-expand parse tests")

(cond-expand
 (guile (test-assert (cond-expand-clause-satisfied? '(guile #t))))
 (chicken (test-assert (cond-expand-clause-satisfied? '(chicken #t)))))

(cond-expand
 (guile (test-assert (cond-expand-clause-satisfied?
                      '((or gambit guile) #t))))
 (chicken (test-assert (cond-expand-clause-satisfied?
                        '((or chibi chicken) #t)))))

(cond-expand
 (guile (test-assert (cond-expand-clause-satisfied?
                      '((library (srfi srfi-64)) #t))))
 (chicken (test-assert (cond-expand-clause-satisfied?
                        '((library (srfi 64)) #t)))))

(cond-expand
 (guile (test-assert (not (cond-expand-clause-satisfied?
                           '((not guile) #t)))))
 (chicken (test-assert (not (cond-expand-clause-satisfied?
                             '((not chicken) #t))))))

(test-equal (cond-expand (guile '(begin (import (lsp-server guile))))
                         (chicken '(begin (import (lsp-server chicken))))
                         (else))
            (cond-expand-find-satisfied-clause
             '(cond-expand (guile (import (lsp-server guile)))
                           (chicken (import (lsp-server chicken)))
                           (else))))

(test-equal (cond-expand ((or gambit guile) '(begin "gambit or guile"))
                         ((or chibi chicken) '(begin "chibi or chicken"))
                         (else))
            (cond-expand-find-satisfied-clause
             '(cond-expand ((or gambit guile) "gambit or guile")
                           ((or chicken chibi) "chibi or chicken")
                           (else))))
(test-end "cond-expand parse tests")

(test-begin "parse tests")

(test-assert (tagged-expression? '(import ...) 'import))

(test-assert (procedure-definition-form? '(define (f x) x)))

(test-assert (not (procedure-definition-form? '(define f x))))

(test-assert (procedure-definition-form? '(define f (lambda (x) x))))

(test-eq 'f (procedure-definition-name '(define f (lambda (x) x))))

(test-eq 'f (procedure-definition-name '(define (f x) x)))

(test-equal '(x y) (procedure-definition-arguments '(define (f x y) x)))

(test-equal '(x y) (procedure-definition-arguments '(define f (lambda (x y) x))))

(test-equal '(() (x y))
            (procedure-definition-arguments
             '(define f (case-lambda (() (f 1 2))
                                     ((x y) x)))))

(test-equal '(= . rest)
            (procedure-definition-arguments
             '(define (lset-union = . rest)
                ;; Likewise, allow memq / memv to be used if possible.
                #t)))

(test-equal "my func"
            (procedure-definition-docstring
             '(define (f x) "my func" x)))

(test-equal "my func"
            (procedure-definition-docstring
             '(define f (lambda (x) "my func" x))))

(test-equal "my func"
            (procedure-definition-docstring
             '(define f (case-lambda "my func" (() 1) ((x) x)))))

(let ((res (parse-expression
            '(begin (import (srfi 1) (srfi 69) utf-8)
                    (define (f x) x)
                    (define g (lambda (x y) (+ x y))))
            (make-parse-context #f '(my lib)))))
  (test-equal '((srfi 1) (srfi 69) utf-8) (source-meta-data-imports res))
  (test-equal 2 (hash-table-size (source-meta-data-procedure-info-table res)))
  (test-equal '(my lib) (source-meta-data-library-name res)))

(let ((res (parse-expression
            '(define-library (my lib)
                (export f g)
                (import (srfi 1) (except (srfi 69) make-hash-table))
                (begin (define (f x) x))
                (define g (lambda (x y) (+ x y))))
             (make-parse-context #f #f))))
  (test-equal '((srfi 1) (srfi 69)) (source-meta-data-imports res))
  (test-equal 2 (hash-table-size (source-meta-data-procedure-info-table res)))
  (test-equal '(my lib) (source-meta-data-library-name res)))

(let ((res (parse-expression
            '(define-module (my lib)
                #:export (f g)
                #:use-module (srfi 1)
                #:use-module ((srfi 69) #:select (hash-table-walk)))
            (make-parse-context #f #f))))
  (let ((imports (source-meta-data-imports res)))
    (test-assert (member '(srfi 1) imports))
    (test-assert (member '(srfi 69) imports)))
  (test-equal '(my lib) (source-meta-data-library-name res)))

(let ((res (parse-expression
            '(cond-expand (guile (import (system vm program))
                                 (define (f x) x))
                          (chicken (import (apropos-api))
                                   (define (f x) x)
                                   (define (g x y) x))
                          (else))
            (make-parse-context #f #f))))
  (cond-expand
   (chicken (test-equal '((apropos-api)) (source-meta-data-imports res))
            (test-equal 2 (hash-table-size (source-meta-data-procedure-info-table res))))
   (guile (test-equal '((system vm program)) (source-meta-data-imports res))
          (test-equal 1 (hash-table-size (source-meta-data-procedure-info-table res))))))

(let ((res (parse-file "resources/sample-1.scm")))
  (test-equal 2 (length (source-meta-data-imports res)))
  (test-equal 2 (hash-table-size
                 (source-meta-data-procedure-info-table res)))
  (test-equal 3 (procedure-info-line
                 (hash-table-ref (source-meta-data-procedure-info-table res) 'f)))
  (test-equal '(x)
              (procedure-info-arguments
               (hash-table-ref (source-meta-data-procedure-info-table res) 'f)))
  (test-equal 6 (procedure-info-line
                 (hash-table-ref (source-meta-data-procedure-info-table res) 'g)))
  (test-equal '(x y)
              (procedure-info-arguments
               (hash-table-ref (source-meta-data-procedure-info-table res) 'g))))

(parameterize ((identifier-to-source-meta-data-table (make-hash-table)))
  (parse-and-update-table! "resources/sample-1.scm")
  (test-assert (lset-intersection equal?
                                  (hash-table-keys (identifier-to-source-meta-data-table))
                                  '(f g)))
  (test-assert (any (lambda (k)
                      (string-contains k "sample-1.scm"))
                    (hash-table-keys
                     (hash-table-ref
                      (identifier-to-source-meta-data-table) 'f))))
  (test-equal "(f x)" (fetch-signature #f 'f)))

(parameterize ((identifier-to-source-meta-data-table (make-hash-table))
               (source-path-timestamps (make-hash-table)))
  (generate-meta-data! "resources/sample-2.scm")
  (test-equal "(func2 x . args)"
              (fetch-signature #f 'func2))
  (test-assert (lset-intersection equal?
                                  (hash-table-keys (identifier-to-source-meta-data-table))
                                  '(func included-func)))
  (test-assert (any (lambda (k)
                      (string-contains k "sample-2.scm"))
                    (hash-table-keys
                     (hash-table-ref
                      (identifier-to-source-meta-data-table) 'func))))
  (test-assert (any (lambda (k)
                      (string-contains k
                                       "sample-3-included.scm"))
                    (hash-table-keys
                     (hash-table-ref
                      (identifier-to-source-meta-data-table) 'included-func))))
  (test-assert (every absolute-pathname?
                      (hash-table-keys
                       (hash-table-ref
                        (identifier-to-source-meta-data-table) 'func))))
  (test-assert (every absolute-pathname?
                      (hash-table-keys
                       (hash-table-ref
                        (identifier-to-source-meta-data-table) 'included-func))))
  (with-output-to-file "/tmp/out.log"
    (lambda () (display (hash-table-keys (source-path-timestamps)))))
  (test-assert (and (any (lambda (k)
                           (string-contains k "sample-2.scm"))
                         (hash-table-keys (source-path-timestamps)))
                    (any (lambda (k)
                           (string-contains k "sample-3-included.scm"))
                         (hash-table-keys (source-path-timestamps)))))
  (test-assert (every absolute-pathname?
                      (hash-table-keys (source-path-timestamps)))))

(parameterize ((identifier-to-source-meta-data-table (make-hash-table))
               (source-path-timestamps (make-hash-table)))
  (generate-meta-data! "resources")
  (test-assert (hash-table-exists? (identifier-to-source-meta-data-table)
                                   'f))
  (test-assert (hash-table-exists? (identifier-to-source-meta-data-table)
                                   'g))
  (test-assert (hash-table-exists? (identifier-to-source-meta-data-table)
                                   'func))
  (test-assert (every absolute-pathname?
                      (hash-table-keys (source-path-timestamps)))))

(cond-expand
 (guile (test-equal '(my lib)
                    (parse-library-name-from-file "resources/sample-guile-lib.scm")))
 (r7rs (test-equal '(my lib)
                   (parse-library-name-from-file "resources/sample-r7rs-lib.scm"))))

(let ((meta-data (parse-file "../guile/lsp-server/parse.sld")))
  (test-assert (member '(srfi srfi-1)
                       (source-meta-data-imports meta-data)))
  (test-assert (member '(ice-9 ftw)
                       (source-meta-data-imports meta-data)))
  (test-assert (member '(scheme file)
                       (source-meta-data-imports meta-data))))

(test-end "parse tests")