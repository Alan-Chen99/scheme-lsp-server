(define-library (lsp-server parse)

(export collect-imports-from-expression
        collect-imports-from-file)

(import (srfi 1)
        (scheme base))

(cond-expand
 (guile (import (lsp-server guile)
                (system vm program)))
 (chicken))

(include "parse.scm"))
