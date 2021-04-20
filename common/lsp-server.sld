(define-library (lsp-server)

(export lsp-server-log-level
        start-lsp-server
        start-lsp-server/background)

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
        srfi-13 ;; string-upcase
        srfi-18
        srfi-28
        srfi-69

        (json-rpc)
        (json-rpc lolevel))

(include "basic-log.scm")
(include "util.scm")
(cond-expand
  (chicken (include "../chicken/chicken.scm"))
  (else))
(include "file.scm")
(include "server.scm"))
