(define-module (lsp-server private compat)

#:export ($apropos-list
          $compute-diagnostics
          $open-file!
          $save-file!
          $fetch-documentation
          $fetch-signature
          $get-definition-locations
          $initialize-lsp-server!
          $server-capabilities
          $server-name
          $tcp-accept
          $tcp-close
          $tcp-connect
          $tcp-listen
          $tcp-read-timeout
          spawn-repl-server)

#:use-module (ice-9 popen)
#:use-module ((scheme base)
              #:select (define-record-type let-values guard read-line))
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module ((srfi srfi-13)
              #:select (string-join string-concatenate))
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module (lsp-server geiser modules)
#:use-module (ice-9 documentation)
#:use-module (ice-9 ftw)
#:use-module (ice-9 optargs)
#:use-module (ice-9 session)
#:use-module (system base compile)
#:use-module (system vm program)
#:use-module (system repl server)
#:use-module (json-rpc)
#:use-module (json-rpc lolevel)
#:use-module (lsp-server private diagnostics)
#:use-module (lsp-server private document)
#:use-module (lsp-server private file)
#:use-module (lsp-server private util)
#:use-module (lsp-server private adapter)
#:use-module (lsp-server private parse)
#:use-module (lsp-server private guile)

#:declarative? #f)
