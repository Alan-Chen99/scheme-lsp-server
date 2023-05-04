(define-library (lsp-server)

(export lsp-server-log-file
        lsp-server-log-level
        lsp-server-start/stdio
        lsp-server-start/tcp
        lsp-server-version)

(cond-expand
 (chicken (import scheme
                  (chicken format)
                  (lsp-server private chicken)
                  (only (chicken base) case-lambda)
                  (only (chicken condition) print-error-message)
                  r7rs

                  (except (chicken io) read-string)
                  (only (chicken base) alist-ref)
                  (only (srfi 133) vector-fold)
                  (only (chicken file) file-exists?)
                  (chicken tcp)
                  apropos
                  (only (srfi 13)
                      string-fold
                      string-take
                      string-tokenize
                      string-upcase)
                  (srfi 18)
                  (srfi 180)
                  utf8))
 (gambit (import (only (scheme base)
                      define-record-type
                      guard
                      let-values)
                 (rename (except (gambit) with-exception-handler)
                         (r7rs-with-exception-handler with-exception-handler))
                 (lsp-server private gambit)
                 (only (srfi 13)
                       string-fold
                       string-tokenize))
         (include "~~lib/_gambit#.scm"))
 (guile (import (only (scheme base)
                      define-record-type
                      guard
                      let-values)
                (scheme write)
                (srfi 1)
                (srfi 8)
                (only (srfi 13)
                      string-fold
                      string-take
                      string-tokenize
                      string-upcase)
                (srfi 18)
                (srfi 43)
                (ice-9 documentation)
                (ice-9 session)
                (system vm program)
                (lsp-server private adapter))))

(import (scheme case-lambda)
        (scheme file)
        (scheme load)
        (only (scheme process-context)
              exit
              command-line)
        (scheme write)
        (only (srfi 14) char-set)
        (srfi 28)
        (srfi 69)
        (json-rpc)
        (json-rpc lolevel)

        (lsp-server private file)
        (lsp-server private util)
        (lsp-server private document)
        (lsp-server private parse)
        (lsp-server private compat)
        )

(include "lsp-server-impl.scm"))
