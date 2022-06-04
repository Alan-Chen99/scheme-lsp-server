(define-library (lsp-server)

(export lsp-server-log-level
        lsp-server-request-connection
        lsp-command-server-start
        start-lsp-server
        start-lsp-server/background
        start-lsp-loop)

(import (except (chicken io) read-string)
        (only (chicken base) alist-ref)
        (chicken tcp)
        apropos
        medea
        r7rs
        scheme
        (scheme file)
        (scheme load)
        (only (scheme process-context)
              exit
              command-line)
        (scheme write)
        (srfi 1)
        (srfi 18)
        (srfi 28)
        (srfi 69)
        (srfi 130) ;; string-upcase
        (only (srfi 13) string-tokenize)
        utf8

        (json-rpc)
        (json-rpc lolevel)

        (lsp-server document)
        (lsp-server private))

(cond-expand
 (chicken (import (chicken format)
                  (lsp-server chicken)
                  (only (chicken base) case-lambda)))
 (else))

(include "file.scm")
(include "server.scm"))
