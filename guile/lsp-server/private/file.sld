(define-module (lsp-server private file)

#:export ( file-table
           file-table-mutex
           get-word-in-document
           get-word-under-cursor
           read-file!
           read-text!
           update-file!
           free-file!
           parse-change-contents
           apply-change
           apply-all-changes
           invert-range
           normalize-range)

#:re-export (lsp-server-log-file)

#:use-module ((scheme base)
              #:select (define-record-type
                        guard
                        let-values))
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-8) ;; receive
#:use-module ((srfi srfi-18) #:select (make-mutex mutex-lock! mutex-unlock!))
#:use-module (srfi srfi-28) ;; simple-format
#:use-module (srfi srfi-69)

#:use-module (lsp-server private document)
#:use-module (lsp-server private parse)
#:use-module (lsp-server private util)
#:use-module (lsp-server private compat)
#:use-module ((lsp-server private guile)
              #:select (alist-ref vector-fold))

#:declarative? #f)

