(define-library (lsp-server)

(export lsp-server-log-level
        start-lsp-server
        start-lsp-server/background
        start-lsp-loop)

(import (except (chicken io) read-string)
        (only (chicken base) alist-ref)
        (chicken tcp)
        apropos
        medea
        r7rs
        (scheme base)
        (scheme file)
        (scheme load)
        (scheme write)
        (srfi 1)
        (srfi 18)
        (srfi 28)
        (srfi 69)
        (srfi 130) ;; string-upcase
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
