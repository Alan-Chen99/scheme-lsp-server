(import (lsp-server)
        (scheme process-context))

(define (main args)
  (parameterize ((lsp-server-log-level 'debug))
    (start-lsp-server (string->number (cadr args)))))

(main (command-line))
