(define-library (lsp-server private util)

(export make-apropos-info
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

        module-name->string
        split-module-name

        alist-ref*
        get-root-path
        get-uri-path
        parse-uri

        identifier-char?
        symbols->string
        hash-table-merge-updating!
        stringify

        write-log
        lsp-server-log-file
        log-level
        satisfies-log-level?

        delete-lines
        string-lines

        compose

        server-out-port)

(import (scheme char)
        (json-rpc))

(cond-expand
 (chicken (import (only (chicken base) intersperse)
                  (except (scheme base)
                          string-length string-ref string-set! make-string string substring
                          string->list list->string string-fill! write-char read-char)
                  (only (srfi 1)
                        take
                        take-right
                        fold)
                  (srfi 28)
                  (srfi 69)
                  (only (utf8-srfi-13)
                        string-join
                        string-tokenize)
                  (only utf8
                        list->string
                        substring
                        string-length
                        string->list)
                  (lsp-server private chicken)))
 (gambit (import (scheme base)
                 (only (srfi 1)
                       take
                       take-right
                       fold)
                 (srfi 28)
                 (srfi 69)
                 (only (srfi 13)
                       string-join
                       string-tokenize)
                 (chibi uri)
                 (lsp-server private gambit)))
 (guile
  (import
   (scheme base)
   (srfi 1)
   (srfi 28)
   (srfi 69)
   (srfi 13)
   (only (scheme base)
         define-record-type
         flush-output-port)
   (scheme write)
   (lsp-server private guile))))

(include "util-impl.scm"))
