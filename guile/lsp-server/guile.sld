(define-module (lsp-server guile)

#:export ($apropos-list
          $open-file!
          $save-file!
          $fetch-documentation
          $get-definition-locations
          $initialize-lsp-server!
          $server-capabilities
          $server-name
          $tcp-accept
          $tcp-connect
          $tcp-listen
          $tcp-read-timeout
          spawn-repl-server
          library-available?
          alist-ref
          get-module-path
          hash-table-join!
          pathname-directory
          pathname-join)

#:use-module ((scheme base)
              #:select (read-line guard))
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module ((srfi srfi-13)
              #:select (string-join string-concatenate))
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module (ice-9 documentation)
#:use-module (ice-9 optargs)
#:use-module (ice-9 session)
#:use-module (system vm program)
#:use-module (system repl server)
#:use-module (lsp-server private)

#:declarative? #f)

(define root-path (make-parameter #f))
(define current-path (make-parameter #f))

;;; Ignored for now
(define $tcp-read-timeout (make-parameter #f))

(define $server-name
  "Guile LSP server")

;;; Initialize LSP server to manage project at ROOT (a string). Used
;;; for implementation-specific side effects only. Empty for now.
(define ($initialize-lsp-server! root)
  ;; (root-path
  ;;  (if (and root (not (equal? root 'null)))
  ;;      root
  ;;      "."))
  #f)

;;; An alist with implementation-specific server capabilities. See:
;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
;;; Note: These capabilities are joined to the implementation independent
;;; mandatory-capabilities (see server.scm).
(define $server-capabilities
  `((definitionProvider . ())))

;;; Return a socket listening on PORT-NUMBER at the localhost.
(define ($tcp-listen port-number)
  (define sock (socket PF_INET SOCK_STREAM 0))
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
  (bind sock (make-socket-address AF_INET INADDR_LOOPBACK port-number))
  (listen sock 20)
  sock)

;;; Waits for incoming connections on LISTENER and return two values to
;;; communicate with the client: an input port and an output port.
(define ($tcp-accept listener)
  (define res (accept listener))
  (define port (car res))
  (values port port))

;;; Connects to a listening server on TCP-ADDRESS and TCP-PORT.
(define ($tcp-connect tcp-address tcp-port)
  (define sock (socket PF_INET SOCK_STREAM 0))
  (define addr (inet-pton AF_INET tcp-address))
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
  (connect sock (make-socket-address AF_INET addr tcp-port))
  (values sock sock))

;;; Return apropos instances of all functions matching IDENTIFIER (a symbol).
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
  #f)

(define ($get-definition-locations identifier)
  #f)

(define (alist-ref key lst)
  (define res (assoc key lst))
  (if res
      (cdr res)
      #f))

(define* (read-lines #:optional (port #f))
  (define p (or port (current-input-port)))
  (let loop ((res '()))
    (let ((line (read-line p)))
      (if (eof-object? line)
          (reverse res)
          (loop (cons line res))))))

(define (symbol->object sym)
  (if (and (symbol? sym)
           (module-defined? (current-module) sym))
      (module-ref (current-module) sym)
      #f))

(define (find-absolute-path path)
  (define base-path
    (find (lambda (load-path)
            (file-exists? (string-append load-path "/" path)))
          (append (filter values (list (root-path) (current-path)))
                  %load-path)))
  (if base-path
      (canonicalize-path (string-append base-path "/" path))
      path))

(define (pathname-join dir-name file-name)
  (string-concatenate (list dir-name
                            file-name-separator-string
                            file-name)))

(define (pathname-directory path)
  (dirname path))

(define (get-module-path module-name)
  (define num-parts (if (list? module-name)
                        (length module-name)
                        1))
  (define file-path-without-extension
    (cond ((> num-parts 1)
          (let* ((dir-part (drop-right module-name 1))
                 (dir-path
                  (string-join (map stringify dir-part)
                               file-name-separator-string)))
            (pathname-join dir-path
                           (stringify
                            (last module-name)))))
          ((= num-parts 1)
           (stringify (if (list? module-name)
                          (car module-name)
                          module-name)))
          (else (write-log 'error
                           (format "invalid module name ~a"
                                   module-name))
                #f)))
  (if (not file-path-without-extension)
      #f
      (let loop ((dirs %load-path))
        (if (null? dirs)
            #f
            (let* ((dir (car dirs))
                   (full-path-without-extension
                    (pathname-join dir file-path-without-extension))
                   (scm-file (string-append full-path-without-extension
                                            ".scm"))
                   (sld-file (string-append full-path-without-extension
                                            ".sld"))
                   (ss-file (string-append full-path-without-extension
                                           ".ss"))
                   (file-found
                    (find file-exists? (list sld-file scm-file ss-file))))
              (if file-found
                  file-found
                  (loop (cdr dirs))))))))

(define (spawn-repl-server port-num)
  (define sock (make-tcp-server-socket #:port port-num))
  (run-server sock))

(define (library-available? library-name)
  (resolve-module library-name #f #f #:ensure #f))

;;; hash-table-merge! had a bug and was only fixed on a recent Guile
;;; version, we want to support older versions too though.
(define (hash-table-join! ht other-ht)
  "Add all key/value pairs from OTHER-HT to HT, overriding HT's
mappings where present.  Return HT."
  (hash-table-fold
   other-ht (lambda (k v ign) (hash-table-set! ht k v)) #f)
  ht)
