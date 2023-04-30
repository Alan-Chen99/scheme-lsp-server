(define-library (lsp-server private file)

(export file-table
        file-table-mutex
        read-file!
        read-text!
        update-file!
        free-file!
        get-word-under-cursor
        parse-change-contents
        apply-change
        apply-all-changes
        invert-range
        normalize-range)

(cond-expand
 (chicken (import (except scheme
                          string-length string-ref string-set!
                          make-string string substring string->list
                          list->string string-fill! write-char
                          read-char display)
                  (chicken format)
                  (lsp-server private chicken)
                  (only (chicken base) case-lambda)
                  (only (chicken condition) print-error-message)
                  r7rs

                  (except (chicken io)
                          read-string
                          write-string
                          read-token)
                  (only (chicken base) alist-ref)
                  (only (chicken port) call-with-input-string)
                  (only (srfi 133) vector-fold)
                  (chicken tcp)
                  apropos
                  (only (srfi 18)
                        make-mutex
                        mutex-lock!
                        mutex-unlock!)
                  (srfi 180)
                  utf8))
 (gambit (import (gambit)
                 (scheme write)
                 (lsp-server private gambit))
         (include "~~lib/_gambit#.scm"))
 (guile (import (only (scheme base)
                      define-record-type
                      guard
                      let-values)
                (scheme write)
                (srfi 1)
                (srfi 8)
                (only (srfi 18)
                        make-mutex
                        mutex-lock!
                        mutex-unlock!)
                (only (lsp-server private guile) vector-fold))))

(import (scheme file)
        (srfi 28)
        (srfi 69)

        (lsp-server private util)
        (lsp-server private document)
        (lsp-server private parse)
        (lsp-server private compat)
        )

(include "file-impl.scm"))
