(define-library (lsp-server tags)

(export parse-definition-line
        generate-tags!
        get-definition-locations
        make-tag-info
        parse-and-insert-tags!
        tag-info-character
        tag-info-name
        tag-info-line)

(import r7rs
        (scheme base)
        (scheme file)
        (scheme write)
        (srfi 1)
        (srfi 28)
        (srfi 69)
        (chicken file)
        (chicken irregex)
        (only (chicken file posix) directory?)

        (lsp-server private))

(include "tags.scm"))
