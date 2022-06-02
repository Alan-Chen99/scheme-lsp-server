#! /usr/local/bin/csi -ss

(import (lsp-server)
        (chicken tcp)
        (scheme process-context))

(define (main args)
  (let ((command-port-number (string->number (list-ref args 0)))
        (lsp-error-port-number (string->number (list-ref args 1)))
        (lsp-port-number (string->number (list-ref args 2))))

    (parameterize ((tcp-read-timeout #f)
                   (tcp-write-timeout #f)
                   (tcp-accept-timeout #f)
                   (lsp-server-log-level 'debug))
      (lsp-server-request-connection command-port-number
                                     lsp-port-number
                                     lsp-error-port-number))))
