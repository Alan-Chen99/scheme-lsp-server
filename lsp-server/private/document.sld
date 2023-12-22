(define-library (lsp-server private document)

(export make-document
        read-document
        document-contents
        document-length
        document-insert
        document-contract
        document-num-lines
        line/char->pos
        string->document)

(import (scheme case-lambda)
        (scheme file)
        (srfi 28)
        (srfi 69)
        (lsp-server private util))

(cond-expand
 (chicken (import (except (scheme base)
                          string-length string-ref string-set! make-string string substring
                          string->list list->string string-fill! write-char read-char)
                  (only (utf8-srfi-13) string-replace)
                  (only (chicken port) with-input-from-string)
                  (only utf8 display read-char string string-length)))
 (gambit (import (gambit)
                 (lsp-server private gambit)
                 (only (srfi 13) string-replace))
         (include "~~lib/_gambit#.scm"))
 (guile (import (only (scheme base)
                      (scheme write)
                      (only (srfi 13) string-replace)
                      define-record-type
                      vector-append
                      vector-copy
                      vector-map))))

(include "document-impl.scm"))

