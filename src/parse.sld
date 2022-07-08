(define-library (lsp-server parse)

(export collect-meta-data-from-file
        generate-meta-data!
        fetch-definition-locations)

(import (srfi 1)
        (srfi 28)
        (srfi 69)
        (scheme base)
        (only (scheme file) with-input-from-file)
        (scheme read)
        (scheme)
        (chicken irregex)
        (chicken file)
        (chicken file posix)
        (lsp-server chicken)
        (lsp-server private))

(begin
  (include "parse.scm")
  (define hash-table-join! hash-table-merge!)
  (define (library-available? x) #t)))
