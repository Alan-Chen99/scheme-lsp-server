(define-module (lsp-server parse)

#:export (collect-meta-data-from-file
          fetch-signature
          generate-meta-data!
          fetch-definition-locations
          fetch-documentation
          file-already-parsed?
          apropos-list)

#:use-module (srfi srfi-1)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module ((scheme base) #:select (define-record-type features read-line))
#:use-module ((scheme file) #:select (with-input-from-file))
#:use-module (scheme read)
#:use-module (lsp-server guile)
#:use-module (lsp-server private)
#:use-module (lsp-server trie)
#:use-module (system vm program)
#:use-module (ice-9 ftw)

#:declarative? #f)

(include "../irregex.scm")
(include "parse.scm")
