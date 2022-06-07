(define-module (lsp-server)

#:export (lsp-server-log-level
          lsp-command-server-start
          lsp-server-request-connection
          lsp-server-start
          lsp-server-start/background
          lsp-server-start/stdio)

#:use-module (scheme base)
#:use-module (scheme file)
#:use-module (scheme load)
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-8) ;; receive
#:use-module (srfi srfi-13)
#:use-module (srfi srfi-18)
#:use-module (srfi srfi-28) ;; simple-format
#:use-module (srfi srfi-69)

#:use-module (json-rpc)
#:use-module (json-rpc lolevel)
#:use-module (ice-9 documentation)
#:use-module (ice-9 session)

#:use-module (system vm program)

#:use-module (lsp-server guile)
#:use-module (lsp-server document)
#:use-module (lsp-server private)

#:declarative? #f)

(include "lsp-server/file.scm")
(include "lsp-server/server.scm")
