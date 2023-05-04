(define-library (lsp-server private compat)

(export $apropos-list
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
        $tcp-read-timeout)

(import (lsp-server private util)
        (lsp-server private adapter)
        (lsp-server private parse)
        (json-rpc lolevel))

(import (scheme write)
        (srfi 28)
        (srfi 69))

(cond-expand
 (chicken
  (import (apropos)
          (chicken base)
          (chicken condition)
          (chicken file)
          (chicken format)
          (chicken io)
          (chicken irregex)
          (chicken pathname)
          (chicken port)
          (chicken process)
          (chicken process-context)
          (chicken random)
          (only (chicken string) string-intersperse)
          (chicken tcp)
          nrepl
          chicken-doc
          r7rs
          scheme
          (srfi 1)
          (only (srfi 13) string-join string-concatenate)
          (srfi 18)
          (srfi 28)
          (srfi 69)
          (srfi 130)
          (srfi 180)
          (uri-generic)

          (geiser)
          (lsp-server private chicken))
  (include "compat-chicken-impl.scm"))
 (gambit
  (import (rename (except (gambit) with-exception-handler)
                  (with-exception-catcher with-exception-handler))
          (only (srfi 1) find)
          (only (srfi 13) string-prefix? string-tokenize)
          (only (srfi 14) char-set char-set-complement)
          (srfi 28)
          (lsp-server private adapter)
          (lsp-server private gambit))
  (include "compat-gambit-impl.scm"))
 (guile
  (import (only (scheme base)
                define-record-type
                read-line
                guard)
          (geiser modules)
          (ice-9 documentation)
          (ice-9 ftw)
          (ice-9 optargs)
          (ice-9 session)
          (srfi 1)
          (only (srfi 13) string-join string-concatenate)
          (system vm program)
          (system repl server))
  (include "compat-guile-impl.scm"))
 (else)))
