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

#:declarative? #f)
