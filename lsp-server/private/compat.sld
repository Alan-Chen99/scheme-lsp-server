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
        spawn-repl-server
        $tcp-accept
        $tcp-close
        $tcp-connect
        $tcp-listen
        $tcp-read-timeout
        get-module-path
        pathname-directory
        pathname-join)

(import (lsp-server private)
        (lsp-server parse)
        (lsp-server adapter)
        (json-rpc lolevel))

(import (srfi 1)
        (only (srfi 13) string-join string-concatenate)
        (scheme write)
        (srfi 28)
        (srfi 69))

(cond-expand
 (chicken
  (import (apropos)
          (chicken base)
          (chicken condition)
          (chicken file)
          (chicken foreign)
          (chicken format)
          (chicken io)
          (chicken irregex)
          (chicken pathname)
          (chicken platform)
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
          (srfi 18)
          (srfi 28)
          (srfi 69)
          (srfi 130)
          (srfi 180)
          (uri-generic)

          (geiser))
  (include "compat-chicken-impl.scm"))
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
          (system vm program)
          (system repl server))
  (include "compat-guile-impl.scm"))
 (else)))
