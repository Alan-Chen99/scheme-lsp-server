(define-module (lsp-server document)

#:export (make-document
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

#:use-module ((scheme base)
              #:select (define-record-type
                         vector-append
                         vector-copy
                         vector-map))
#:use-module (scheme file)
#:use-module (scheme write)
#:use-module (lsp-server private)
#:use-module (srfi srfi-13)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)

#:declarative? #f)

(load "document.scm")
