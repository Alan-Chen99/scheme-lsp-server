(define-library (lsp-server)

(export lsp-server-log-file
        lsp-server-log-level
        lsp-server-start/stdio
        lsp-server-start/tcp
        lsp-server-version)

(cond-expand
 (chicken (import scheme
                  (chicken format)
                  (lsp-server chicken)
                  (only (chicken base) case-lambda)
                  (only (chicken condition) print-error-message)
                  r7rs

                  (except (chicken io) read-string)
                  (only (chicken base) alist-ref)
                  (only (srfi 133) vector-fold)
                  (chicken tcp)
                  apropos
                  (srfi 18)
                  (srfi 180)
                  utf8))
 (gambit (import (except (gambit) string-upcase)
                 (lsp-server gambit util)
                 (lsp-server gambit))))


(import (scheme base)
        (scheme case-lambda)
        (scheme file)
        (scheme load)
        (only (scheme process-context)
              exit
              command-line)
        (scheme write)
        (only (srfi 14) char-set)
        (srfi 28)
        (srfi 69)
        (only (srfi 13)
              string-fold
              string-take
              string-tokenize
              string-upcase)
        (json-rpc)
        (json-rpc lolevel)

        (lsp-server document)
        (lsp-server parse)
        (lsp-server private))

(include "file.scm")
(include "server.scm"))
