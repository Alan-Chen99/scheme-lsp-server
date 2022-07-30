(define-module (lsp-server parse)

#:export (fetch-signature
          generate-meta-data!
          fetch-definition-locations
          fetch-documentation
          file-already-parsed?
          list-completions
          parse-file
          parse-library-name-from-file
          source-meta-data-imports
          irregex
          irregex-search)

#:use-module (srfi srfi-1)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module ((scheme base) #:select (define-record-type
                                      features
                                      guard
                                      read-line))
#:use-module ((scheme file) #:select (with-input-from-file))
#:use-module (scheme read)
#:use-module (lsp-server private)
#:use-module (lsp-server trie)
#:use-module (lsp-server guile util)
#:use-module (system vm program)
#:use-module (ice-9 ftw)

#:declarative? #f)

(include "../irregex.scm")
(include "parse.scm")
