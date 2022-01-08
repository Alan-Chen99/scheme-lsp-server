(define-library (lsp-server)

(export lsp-server-log-level
        start-lsp-server
        start-lsp-server/background
        start-lsp-loop)

(import (scheme base)
        (scheme file)
        (scheme write)
        (srfi srfi-1)
        (srfi srfi-8) ;; receive
        (srfi srfi-9)
        (srfi srfi-13)
        (srfi srfi-18)
        (srfi srfi-28) ;; simple-format
        (srfi srfi-69)

        (json-rpc)
        (json-rpc lolevel)
        (ice-9 documentation)
        (ice-9 session)

        (system vm program)

        (lsp-server guile)
        (lsp-server document)
        (lsp-server private))

(begin
  (include "lsp-server/file.scm")
  (include "lsp-server/server.scm")))
