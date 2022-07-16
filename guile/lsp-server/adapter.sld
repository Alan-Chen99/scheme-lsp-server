(define-module (lsp-server adapter)

#:export (lsp-geiser-completions
          lsp-geiser-documentation
          lsp-geiser-signature
          lsp-geiser-symbol-location)

#:use-module (geiser completion)
#:use-module (geiser doc)
#:use-module (geiser xref)
#:use-module (lsp-server guile util)
#:use-module (srfi srfi-28)
#:declarative? #f)

(include "adapter.scm")
