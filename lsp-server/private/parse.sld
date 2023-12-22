(define-library (lsp-server private parse)

(export generate-meta-data!
        fetch-definition-locations
        fetch-documentation
        fetch-signature
        file-already-parsed?
        list-completions
        parse-file
        file-library-info
        file-library-name
        find-library-definition-file
        parse-library-name-from-file
        source-meta-data-imports
        source-meta-data-library-name
        source-meta-data-procedure-info-table)

(import (only (srfi 1) any every filter find fold append-map)
        (srfi 28)
        (srfi 69)
        (only (scheme file) with-input-from-file)
        (scheme read)
        (lsp-server private util)
        (lsp-server private trie))

(cond-expand
 (chicken (import (except (scheme base)
                          string-length string-ref string-set! make-string string substring
                          string->list list->string string-fill! write-char read-char)
                  (only (scheme) caddr)
                  (only (utf8-srfi-14) char-set)
                  (only (utf8-srfi-13) string-contains string-trim-right string-prefix?)
                  (chicken condition)
                  (chicken irregex)
                  (chicken file)
                  (chicken port)
                  (chicken file posix)
                  (only (chicken keyword) keyword?)
                  (only utf8 list->string read-char)

                  (lsp-server private chicken)))
 (gambit (import (except (scheme base) guard with-exception-handler)
                 (scheme write)
                 (only (srfi 14) char-set)
                 (only (srfi 13) string-contains string-trim-right string-prefix?)

                 (rename (only (gambit)
                               caddr
                               file-exists?
                               file-last-modification-time
                               keyword?
                               path-extension
                               r7rs-guard
                               time->seconds
                               with-exception-catcher)
                         (r7rs-guard guard)
                         (with-exception-catcher with-exception-handler))
                 (github.com/ashinn/irregex)
                 (lsp-server private gambit)))
 (guile (import (only (scheme base)
                      define
                      define-record-type
                      error-object?
                      error-object-message
                      features
                      guard
                      read-line)
                (only (scheme file) with-input-from-file)
                (scheme write)
                (only (srfi 14) char-set)
                (only (srfi 13) string-contains string-trim-right string-prefix?)

                (scheme read)
                (system vm program)
                (ice-9 ftw)
                (lsp-server private guile))))

(include "parse-impl.scm")

(begin
  (define hash-table-join! hash-table-merge!)
  (define (library-available? x) #t)))
