(define-module (lsp-server)

#:export (lsp-server-log-level
          lsp-spawner-start
          lsp-server-start/stdio
          lsp-server-start/tcp
          lsp-server-version)

#:re-export (lsp-server-log-file)

#:use-module ((scheme base)
              #:select (define-record-type
                        guard
                        let-values))
#:use-module (scheme file)
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

#:use-module (lsp-server guile util)
#:use-module (lsp-server guile)
#:use-module (lsp-server document)
#:use-module (lsp-server parse)
#:use-module (lsp-server private)

#:declarative? #f)

(include "lsp-server/file.scm")
(include "lsp-server/server.scm")
