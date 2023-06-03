(define-module (lsp-server private guile)

#:export (alist-ref
          alist-ref/default
          current-directory
          create-directory

          absolute-pathname?
          condition->string
          get-module-path
          pathname-directory
          pathname-base
          pathname-join
          pathname-strip-extension
          get-absolute-pathname
          hash-table-join!
          intersperse

          vector-fold)

#:re-export (uri-decode
             irregex
             irregex-match
             irregex-match-start-index
             irregex-match-substring
             irregex-search)

#:use-module (guile)
#:use-module ((scheme base) #:select (guard))
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module ((lsp-server private util) #:select (write-log))
#:use-module (web uri)
#:use-module (rx irregex)
#:declarative? #f
)
