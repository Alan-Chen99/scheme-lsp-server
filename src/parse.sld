(define-library (lsp-server parse)

(export collect-meta-data-from-file)

(import (srfi 1)
        (scheme base)
        (only (scheme file) with-input-from-file)
        (scheme read))

(begin
  (include "parse.scm")))
