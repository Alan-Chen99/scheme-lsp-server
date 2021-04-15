(define-library (lsp-server)

(export start-lsp-server)

(import (chicken base)
        (chicken condition)
        (chicken format)
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
        srfi-69

        (json-rpc lolevel))

(include "basic-log.scm")
(include "util.scm")
(cond-expand
  (chicken (include "../chicken/chicken.scm"))
  (else))
(include "file.scm")
(include "server.scm"))
