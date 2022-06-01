(import (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 18)
        (srfi 28))

(define (main args)
  (define command-port-num (string->number (list-ref args 0)))
  (define lsp-error-port-num (string->number (list-ref args 1)))
  (define lsp-port-num (string->number (list-ref args 2)))
  (display (format "Started lsp-...-connect on command port ~a, error port ~a and lsp port ~a~%"
                   command-port-num
                   lsp-error-port-num
                   lsp-port-num)
           (current-error-port))
  (flush-output-port (current-error-port))

  (let-values (((inp outp) ($tcp-connect "127.0.0.1"
                                         command-port-num)))
    (display (format "requesting new LSP connection at port ~a~%"
                     lsp-port-num)
             (current-error-port))
    (display (format "sending command: spawn-lsp-server ~a ~a~%"
                     lsp-port-num
                     lsp-error-port-num)
             (current-error-port)
    (display (format "spawn-lsp-server ~a ~a~%"
                     lsp-port-num
                     lsp-error-port-num)
             outp)
    (flush-output-port outp)
    (display (format "LSP connection successfull.~%")
             (current-error-port))

    (let ((listener ($tcp-listen lsp-error-port-num)))
      (display "Listening for incomming error messages\n"
               (current-error-port))
      (let-values (((in-err-port out-err-port)
                    ($tcp-accept listener)))
        (let loop ((msg (read-line in-err-port)))
          (if (eof-object? msg)
              #f
              (begin
                (display (format "RECEIVED: ~a\n" msg) (current-error-port))
                (newline (current-error-port))
                (flush-output-port (current-error-port))
                (loop (read-line in-err-port))))))))))
