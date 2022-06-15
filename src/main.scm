(import (lsp-server)
        (scheme process-context))

(define (main args)
  (define debug-level
    (if (> (length args) 1)
        (cadr args)
        'error))
  (parameterize ((lsp-server-log-level debug-level))
    (lsp-server-start/stdio)))

(main (command-line))
