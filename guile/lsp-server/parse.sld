(define-module (lsp-server parse)

#:export (collect-meta-data-from-file)

#:use-module (srfi srfi-1)
#:use-module ((srfi srfi-69) #:select
              (alist->hash-table
               hash-table-fold
               hash-table-keys
               hash-table-ref/default
               hash-table-set!
               hash-table-size))
#:use-module (scheme base)
#:use-module ((scheme file) #:select (with-input-from-file))
#:use-module (scheme read)
#:use-module (lsp-server guile)
#:use-module (system vm program)

#:declarative? #f)

(include "../irregex.scm")
(include "parse.scm")
