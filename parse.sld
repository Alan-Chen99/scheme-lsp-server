(define-library (lsp-server parse)

(export generate-meta-data!
        fetch-definition-locations
        fetch-documentation
        fetch-signature
        file-already-parsed?
        list-completions
        parse-library-name-from-file)

(import (only (srfi 1) any caadr every filter find fold)
        (only (srfi 14) char-set)
        (only (srfi 13) string-trim-right)
        (srfi 28)
        (srfi 69)
        (only (scheme file) with-input-from-file)
        (scheme read)
        (lsp-server private)
        (lsp-server trie))

(cond-expand
 (chicken (import (scheme base)
                  (scheme)
                  (chicken irregex)
                  (chicken file)
                  (chicken file posix)
                  (only (chicken keyword) keyword?)
                  (lsp-server chicken util)))
 (gambit (import (scheme base)
                 (lsp-server gambit util))))

(include "parse.scm")

(begin
  (define hash-table-join! hash-table-merge!)
  (define (library-available? x) #t)))
