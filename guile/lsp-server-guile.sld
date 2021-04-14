(define-module (lsp-server)

#:export (start-lsp-server)

#:use-module (scheme base)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-9)
#:use-module (srfi srfi-18)
#:use-module (srfi srfi-69)
#:use-module (srfi srfi-145)
#:use-module (srfi srfi-180)

#:use-module (json-rpc)
#:use-module (ice-9 format)
#:use-module (ice-9 session)

#:declarative? #f)

(include "../common/basic-log.scm")
(include "../common/base.scm")
(include "../common/util.scm")
(include "guile.scm")
(include "../common/file.scm")
(include "../common/server.scm")



