(define-library (lsp-server private adapter)

(export lsp-geiser-completions
        lsp-geiser-documentation
        lsp-geiser-signature
        lsp-geiser-symbol-location
        lsp-geiser-compile-file
        lsp-geiser-load-file
        lsp-geiser-module-path)

(import (lsp-server private util)
        (srfi 28)
        (only (srfi 13) string-join))

(cond-expand
 (chicken (import (scheme base)
                  (geiser)
                  (scheme)
                  (lsp-server private chicken)))
 (gambit (import (except (scheme base) with-exception-handler)
                 (rename (only (gambit) with-exception-catcher)
                         (with-exception-catcher with-exception-handler))
                 (_geiser)
                 (lsp-server private gambit)))
 (guile (import (scheme base)
                (prefix (geiser completion) geiser-)
                (geiser evaluation)
                (prefix (geiser modules) geiser-)
                (prefix (geiser doc) geiser-)
                (prefix (geiser xref) geiser-)
                (lsp-server private guile)))
 (else))

(begin
  (include "adapter-impl.scm")))
