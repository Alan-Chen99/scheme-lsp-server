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

        compose

        server-out-port)

(import (scheme base)
        (scheme char)
        (scheme write)
        (json-rpc))

(cond-expand
 (chicken (import (scheme)))
 (else))

(cond-expand
 (guile (import
         (srfi 1)
         (srfi 28)
         (srfi 69)
         (srfi 13)
         (only (scheme base)
               define-record-type
               flush-output-port)
         (scheme write)))
 (else (import
        (only (srfi 1)
              take
              take-right
              fold)
        (srfi 28)
        (srfi 69)
        (only (srfi 13)
              string-join
              string-tokenize))))

(cond-expand
 (chicken (import (only (chicken base) intersperse)
                  (lsp-server private chicken)
                  r7rs))
 (gambit (import (chibi uri)
                 (lsp-server private gambit)))
 (guile (import (lsp-server private guile))))

(include "util-impl.scm"))
