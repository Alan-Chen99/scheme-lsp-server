(define-module (lsp-server adapter)

#:export (lsp-geiser-completions
          lsp-geiser-documentation
          lsp-geiser-signature
          lsp-geiser-symbol-location
          lsp-geiser-compile-file
          lsp-geiser-load-file)

#:use-module ((scheme base) #:select (guard))
#:use-module ((geiser completion) #:prefix geiser-)
#:use-module (geiser evaluation)
#:use-module ((geiser modules) #:prefix geiser-)
#:use-module ((geiser doc) #:prefix geiser-)
#:use-module ((geiser xref) #:prefix geiser-)
#:use-module (lsp-server guile util)
#:use-module (lsp-server parse)
#:use-module (lsp-server private)
#:use-module (srfi srfi-28)
#:declarative? #f)

(include "adapter.scm")
