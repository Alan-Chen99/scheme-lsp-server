(define-library (lsp-server)

(export lsp-server-log-level
        start-lsp-server
        start-lsp-server/background
        start-lsp-loop)

(import (chicken base)
        (chicken condition)
        (except (chicken io) read-string)
        (chicken port)
        (chicken process)
        (chicken process-context)
        (chicken string)
        (chicken tcp)
        apropos
        medea
        r7rs
        scheme
        srfi-1
        srfi-130 ;; string-upcase
        srfi-18
        srfi-28
        srfi-69

        (json-rpc)
        (json-rpc lolevel)

        (lsp-server document)
        (lsp-server private))

(cond-expand
 (chicken (import (lsp-server chicken)))
 (else))

(include "file.scm")
(include "server.scm"))
