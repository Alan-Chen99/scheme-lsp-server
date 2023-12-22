(define-library (lsp-server)

(export lsp-server-log-file
        lsp-server-log-level
        lsp-server-start/stdio
        lsp-server-start/tcp
        lsp-server-version)

(cond-expand
 (chicken (import (except (scheme base)
                          string-length string-ref string-set! make-string string substring
                          string->list list->string string-fill! write-char read-char)
                  (chicken format)
                  (lsp-server private chicken)
                  (only (chicken base) case-lambda)
                  (only (chicken condition) print-error-message)

                  (except (chicken io)
                          read-string write-string read-token)
                  (only (chicken base) alist-ref)
                  (only (srfi 133) vector-fold)
                  (only (chicken file) file-exists?)
                  (chicken tcp)
                  apropos
                  (only (utf8-srfi-13)
                        string-fold
                        string-take
                        string-tokenize
                        string-upcase)
                  (srfi 18)
                  (srfi 180)
                  (only utf8 string-length make-string string-set!)
))
 (gambit (import (only (scheme base)
                      define-record-type
                      let-values)
                 (rename (except (gambit) guard)
                         (r7rs-guard guard))
                 (lsp-server private gambit)
                 (only (srfi 13)
                       string-fold
                       string-take))
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

        (lsp-server private diagnostics)
        (lsp-server private file)
        (lsp-server private util)
        (lsp-server private document)
        (lsp-server private parse)
        (lsp-server private compat)
        )

(include "lsp-server-impl.scm"))
