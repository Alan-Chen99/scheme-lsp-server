(define-library (lsp-server adapter)

(export lsp-geiser-completions
        lsp-geiser-documentation
        lsp-geiser-signature
        lsp-geiser-symbol-location
        lsp-geiser-compile-file)

(import (scheme base)
        (scheme)
        geiser
        (lsp-server private)
        (lsp-server parse)
        (lsp-server chicken util)
        (srfi 28)
        (only (srfi 13) string-join))

(begin
  (include "adapter.scm")))
