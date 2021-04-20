(define $server-name
  "guile lsp server")

(define ($initialize-lsp-server)
  #f)

(define $server-capabilities
  `((completionProvider . ((resolveProvider . #t)))
    (signatureHelpProvider . ())
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
                (if (symbol? identifier)
                    (symbol->string identifier)
                    identifier)
                (apropos-fold-accessible (current-module))))

(define ($fetch-documentation module-name identifier)
  (define mod (resolve-module module-name))
  (define obj (module-ref mod identifier))
  (if (and obj (procedure? obj))
      (format "~a~%~a"
              (build-procedure-signature module-name identifier obj)
              (let ((doc (object-documentation obj)))
                (if doc
                    (string-append doc "\n")
                    "")))
      #f))

(define ($fetch-signature module-name identifier)
  (define mod (resolve-module module-name))
  (define obj (module-ref mod identifier))
  (if (and obj (procedure? obj))
      (format "~a~%"
              (build-procedure-signature module-name identifier obj))
      #f))

(define (build-procedure-signature module name proc-obj)
  (define args (procedure-arguments proc-obj))
  (define required (alist-ref 'required args))
  (define optional (alist-ref 'optional args))
  (define keyword (alist-ref 'keyword args))
  (define rest (alist-ref 'rest args))

  (format "~a" `(,name
                 ,@required
                 ,@(if optional (map list optional) '())
                 ,@(map car keyword))))

(define* (read-lines #:optional (port #f))
  (define p (or port (current-input-port)))
  (let loop ((res '()))
    (let ((line (read-line p)))
      (if (eof-object? line)
          (reverse res)
          (loop (cons line res))))))

;; TODO implement the same interface as in SRFI-152
(define (string-split str delim-str . args)
  (if (string-null? delim-str)
      #f
      (let ((delim (if (equal? delim-str "\r\n")
                       #\newline
                       (string-ref delim-str 0))))
        ((@ (guile) string-split) str delim))))
