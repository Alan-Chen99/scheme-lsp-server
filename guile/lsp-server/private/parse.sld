(define-module (lsp-server private parse)

#:export (fetch-signature
          generate-meta-data!
          fetch-definition-locations
          fetch-documentation
          file-already-parsed?
          list-completions
          parse-file
          parse-library-name-from-file
          source-meta-data-imports)

#:re-export (irregex
             irregex-search)

#:use-module (srfi srfi-1)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module ((scheme base) #:select (define-record-type
                                      error-object?
                                      error-object-message
                                      features
                                      guard
                                      read-line))
#:use-module ((scheme file) #:select (with-input-from-file))
#:use-module (scheme read)
#:use-module (lsp-server private util)
#:use-module (lsp-server private compat)
#:use-module (lsp-server private guile)
#:use-module (lsp-server private trie)
#:use-module (system vm program)
#:use-module (ice-9 ftw)

#:declarative? #f)
