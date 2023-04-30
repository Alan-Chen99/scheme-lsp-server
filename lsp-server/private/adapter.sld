(define-library (lsp-server private adapter)

(export lsp-geiser-completions
        lsp-geiser-documentation
        lsp-geiser-signature
        lsp-geiser-symbol-location
        lsp-geiser-compile-file
        lsp-geiser-load-file
        lsp-geiser-module-path)

(import (scheme base)
        (lsp-server private util)
        (srfi 28)
        (only (srfi 13) string-join))

(cond-expand
 (chicken (import (geiser)
                  (scheme)
                  (lsp-server private chicken)))
 (gambit (import (_geiser)
                 (lsp-server private gambit)))
 (guile (import (prefix (geiser completion) geiser-)
                (geiser evaluation)
                (prefix (geiser modules) geiser-)
                (prefix (geiser doc) geiser-)
                (prefix (geiser xref) geiser-)
                (lsp-server private guile)))
 (else))

(begin
  (include "adapter-impl.scm")))
