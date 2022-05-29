(define-library (lsp-server document)

(export make-document
        read-document
        document-contents
        document-length
        document-insert
        document-contract
        document-num-lines
        document-take
        document-take-right
        document-expand
        line/char->pos
        string->document
        compute-lines-offsets)

(import (scheme base)
        (scheme file)
        (scheme write)
        (lsp-server private))

(cond-expand
 (guile (import (srfi srfi-13)
                (srfi srfi-28)
                (srfi srfi-69)))
 (else (import (only (chicken port) with-input-from-string)
               (srfi 28)
               (srfi 69)
               (srfi 130))))
(include "document.scm"))

