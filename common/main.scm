(import (scheme base)
        (lsp-server)
        (srfi-18))

(define (main args)
  (display "Scheme LSP server started")
  (newline)
  (flush-output-port)
  (thread-join! (start-lsp-server)))
;(main '())

