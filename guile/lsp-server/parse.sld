(define-module (lsp-server parse)

#:export (collect-meta-data-from-file)

#:use-module (srfi srfi-1)
#:use-module (scheme base)
#:use-module ((scheme file) #:select (with-input-from-file))
#:use-module (scheme read)
#:use-module (lsp-server guile)
#:use-module (system vm program)

#:declarative? #f)

(load-from-path "irregex.scm")
(include "parse.scm")
