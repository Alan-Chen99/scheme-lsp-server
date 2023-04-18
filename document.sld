(define-library (lsp-server document)

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
        (scheme write)
        (srfi 28)
        (srfi 69)
        (only (srfi 13) string-replace)
        (lsp-server private))

(cond-expand
 (chicken (import (r7rs)
                  (scheme base)
                  (only (chicken port) with-input-from-string)
                  (utf8)))
 (gambit (import (lsp-server gambit util)
                 (except (scheme base) vector-copy))
         (include "~~lib/_gambit#.scm")))

(include "document.scm"))

