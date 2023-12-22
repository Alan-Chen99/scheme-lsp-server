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
 (chicken (import (except (r7rs)
                          string-length string-ref string-set! make-string string substring
                          string->list list->string string-fill! write-char read-char)
                  (geiser)
                  (lsp-server private chicken)))
 (gambit (import (except (scheme base) with-exception-handler)
                 (rename (only (gambit) r7rs-guard)
                         (r7rs-guard guard))
                 (_geiser)
                 (lsp-server private gambit)))
 (guile (import (scheme base)
                (prefix (lsp-server geiser completion) geiser-)
                (lsp-server geiser evaluation)
                (prefix (lsp-server geiser modules) geiser-)
                (prefix (lsp-server geiser doc) geiser-)
                (prefix (lsp-server geiser xref) geiser-)
                (lsp-server private guile)))
 (else))

(begin
  (include "adapter-impl.scm")))
