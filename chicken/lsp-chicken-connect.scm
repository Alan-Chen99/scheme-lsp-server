#! /usr/local/bin/csi -ss

(import (lsp-server)
        (lsp-server chicken)
        (only (lsp-server private) write-log)

        (chicken tcp)
        (scheme process-context)
        (srfi 28))

(define (main args)
  (let ((command-port-number (string->number (list-ref args 0)))
        (lsp-port-number (string->number (list-ref args 1)))
        (lsp-error-port-number (string->number (list-ref args 2)))
        (log-level (string->symbol (list-ref args 3))))

    (write-log 'debug
               (format
                "Requesting through command port ~a main connection at ~a and error connection at ~a "
                command-port-number
                lsp-port-number
                lsp-error-port-number))
    (parameterize ((tcp-read-timeout #f)
                   (tcp-write-timeout #f)
                   (tcp-accept-timeout #f)
                   (lsp-server-log-level log-level))
      (lsp-server-request-connection command-port-number
                                     lsp-port-number
                                     lsp-error-port-number))))

(main (cdr (command-line)))
