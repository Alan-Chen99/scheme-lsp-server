(define-library (lsp-server parse)

(export generate-meta-data!
        fetch-definition-locations
        fetch-documentation
        fetch-signature
        file-already-parsed?
        list-completions)

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
        (only (chicken keyword) keyword?)
        (lsp-server chicken util)
        (lsp-server private)
        (lsp-server trie))

(begin
  (include "parse.scm")
  (define hash-table-join! hash-table-merge!)
  (define (library-available? x) #t)))
