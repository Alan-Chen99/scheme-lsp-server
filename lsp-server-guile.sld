(define-module (lsp-server)

#:export (lsp-server-log-level
          start-lsp-server
          start-lsp-server/background)

#:use-module (scheme base)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-8) ;; receive
#:use-module (srfi srfi-9)
#:use-module (srfi srfi-18)
#:use-module (srfi srfi-28) ;; simple-format
#:use-module (srfi srfi-69)
#:use-module (srfi srfi-145)
#:use-module (srfi srfi-180) ;; json

#:use-module (json-rpc)
#:use-module (json-rpc lolevel)
#:use-module (ice-9 documentation)
#:use-module (ice-9 session)

#:declarative? #f)

(include "common/basic-log.scm")
(include "common/base.scm")
(include "common/util.scm")
(include "guile/guile.scm")
(include "common/file.scm")
(include "common/server.scm")



