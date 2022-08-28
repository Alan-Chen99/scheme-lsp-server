(define-library (lsp-server adapter)

(export lsp-geiser-completions
        lsp-geiser-documentation
        lsp-geiser-signature
        lsp-geiser-symbol-location
        lsp-geiser-compile-file
        lsp-geiser-load-file)

(import (scheme base)
        (lsp-server private)
        (lsp-server parse)
        (srfi 28)
        (only (srfi 13) string-join))

(cond-expand
 (chicken (import (lsp-server chicken util)
                  (geiser)
                  (scheme)))
 (gambit (import (_geiser)
                 (lsp-server gambit util)))
 (else))

(begin
  (include "adapter.scm")))
