(define-module (lsp-server private)

#:export (make-apropos-info
          apropos-info-module
          apropos-info-name
          apropos-info-type
          apropos-info-object

          make-editor-word
          editor-word-text
          editor-word-end-char
          editor-word-end-line
          editor-word-start-char
          editor-word-start-line

          intersperse
          join-module-name
          split-module-name

          alist-ref*
          get-root-path
          get-uri-path
          parse-uri

          identifier-char?
          symbols->string
          hash-table-merge-updating!

          write-log
          lsp-server-log-file
          log-level
          satisfies-log-level?

          $string-split

          delete-lines)

#:use-module ((scheme base)
              #:select (define-record-type
                        flush-output-port))
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)


#:declarative? #f)

(include "private.scm")
