(define $server-name
  "guile lsp server")

(define $server-capabilities
  `((completionProvider . ())
    (textDocumentSync . 1)))

(define ($tcp-listen server-port)
  (define sock (socket PF_INET SOCK_STREAM 0))
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
  (bind sock (make-socket-address AF_INET INADDR_LOOPBACK server-port))
  (listen sock 20)
  sock)

(define ($tcp-accept listener)
  (define res (accept listener))
  (define port (car res))
  (values port port))

(define ($apropos-list identifier)
  (apropos-fold (lambda (mod name obj acc)
                  (cons (make-apropos-info (module-name mod)
                                           name
                                           #f
                                           obj)
                        acc))
                '()
                identifier
                (apropos-fold-accessible (current-module))))

(define ($fetch-documentation module identifier)
  #f)

(define ($fetch-signature module identifier)
  #f)

(define* (read-lines #:optional (port #f))
  (define p (or port (current-input-port)))
  (let loop ((res '())) 
    (let ((line (read-line p)))
      (if (eof-object? line)
          (reverse res)
          (loop (cons line res))))))
