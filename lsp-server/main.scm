(import (lsp-server)
        (scheme process-context))

(define (main args)
  (define lsp-port-num (string->number (cadr args)))
  (define command-port-num (string->number (caddr args)))
  (define repl-port-num (string->number (cadddr args)))

  (parameterize ((lsp-server-log-level 'debug))
    (start-lsp-server-full lsp-port-num command-port-num repl-port-num)))

(main (command-line))
