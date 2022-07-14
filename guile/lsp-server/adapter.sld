(define-module (lsp-server adapter)

#:export (geiser-completions)

#:use-module (geiser completion)
#:use-module (geiser doc)
#:use-module (geiser xref)
#:declarative? #f)

(include "adapter.scm")
