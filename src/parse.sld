(define-library (lsp-server parse)

(export collect-meta-data-from-file)

(import (srfi 1)
        (scheme base)
        (only (scheme file) with-input-from-file)
        (scheme read))

(cond-expand
 (guile (import (lsp-server guile)
                (system vm program)))
 (chicken))

(include "parse.scm"))
