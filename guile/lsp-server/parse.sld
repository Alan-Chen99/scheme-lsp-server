(define-module (lsp-server parse)

#:export (collect-meta-data-from-file
          generate-meta-data!
          get-definition-locations)

#:use-module (srfi srfi-1)
#:use-module (srfi srfi-69)
#:use-module (scheme base)
#:use-module ((scheme file) #:select (with-input-from-file))
#:use-module (scheme read)
#:use-module (lsp-server guile)
#:use-module (system vm program)
#:use-module (ice-9 ftw)

#:declarative? #f)

(include "../irregex.scm")
(include "parse.scm")
