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
        (scheme base)
        (only (srfi 1) filter)
        (only (srfi 13) string-contains)
        (srfi 28))

(cond-expand
 (chicken (import (lsp-server private chicken)))
 (gambit (import (lsp-server private gambit)))
 (guile (import (lsp-server private guile)))
 (else))

(begin (include "diagnostics-impl.scm")))
