(define-library (lsp-server private diagnostics)

(export clear-diagnostics

        alist->diagnostic

        make-diagnostic
        diagnostic-char-number
        diagnostic-file-path
        diagnostic-line-number
        diagnostic-message
        diagnostic-source
        diagnostic-source-set!
        diagnostic? 
        send-diagnostics)

(import (lsp-server private document)
        (lsp-server private file)
        (lsp-server private util)
        (json-rpc)
        (scheme base))

(cond-expand
 (chicken (import (lsp-server private chicken)))
 (gambit (import (lsp-server private gambit)))
 (guile (import (lsp-server private guile)))))
