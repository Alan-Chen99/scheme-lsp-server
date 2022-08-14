(define-library (lsp-server private)

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

        $string-split

        delete-lines

        compose
        flatmap)

(import scheme
        (scheme base)
        (scheme char)
        (scheme write))

(cond-expand
 (guile (import
         (srfi srfi-1)
         (srfi srfi-28)
         (srfi srfi-69)
         (srfi srfi-13)))
 (else (import
        (srfi 1)
        (srfi 28)
        (srfi 69)
        (srfi 130))))

(cond-expand
 (chicken (import (only (chicken base) intersperse)
                  (lsp-server chicken util)
                  r7rs))
 (else))

(include "private.scm"))
