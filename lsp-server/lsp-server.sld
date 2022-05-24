(define-library (lsp-server)

(export lsp-server-log-level
        lsp-command-server-start
        start-lsp-server
        start-lsp-server/background
        start-lsp-loop
        start-lsp-server-full)

(import (except (chicken io) read-string)
        (only (chicken base) alist-ref)
        (chicken tcp)
        apropos
        medea
        r7rs
        (scheme base)
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
 (chicken (import (chicken format))
          (import (lsp-server chicken)))
 (else))

(include "file.scm")
(include "server.scm"))
