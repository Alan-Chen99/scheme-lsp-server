(define-module (lsp-server private diagnostics)

#:export (clear-diagnostics

          alist->diagnostic

          make-diagnostic
          diagnostic-char-number
          diagnostic-file-path
          diagnostic-line-number
          diagnostic-message
          diagnostic? 

          send-diagnostics)

#:use-module (lsp-server private document)
#:use-module (lsp-server private file)
#:use-module (lsp-server private guile)
#:use-module (lsp-server private util)
#:use-module (json-rpc)
#:use-module (scheme base)
#:use-module ((srfi srfi-1) #:select (filter))
#:use-module ((srfi srfi-13) #:select (string-contains))
#:use-module (srfi srfi-28)


#:declarative? #f)
