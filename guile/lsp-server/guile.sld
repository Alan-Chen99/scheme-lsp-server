(define-module (lsp-server guile)

#:export ($apropos-list
          $open-file!
          $save-file!
          $fetch-documentation
          $fetch-signature
          $get-definition-locations
          $initialize-lsp-server!
          $server-capabilities
          $server-name
          $tcp-accept
          $tcp-connect
          $tcp-listen
          $tcp-read-timeout
          library-available?
          alist-ref)

#:use-module ((scheme base)
              #:select (read-line guard))
#:use-module (scheme write)
#:use-module (srfi srfi-1)
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

;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
;;; MODULE (a symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi-1) 'map
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

;;; Return the signature (a string) for IDENTIFIER (a symbol) in MODULE (a
;;; symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi 1) 'map
(define ($fetch-signature module-name identifier)
  (define mod (resolve-module module-name))
  (define obj (module-ref mod identifier))
  (if (and obj (procedure? obj))
      (format "~a~%"
              (build-procedure-signature module-name identifier obj))
      #f))

;;; Return a list of locations found for IDENTIFIER (a symbol).
;;; Each location is represented by an alist
;;; '((url . "file:///<path>")
;;;   (range . ((start . ((line  . <line number>)
;;;                       (character . <character number))
;;;             (end . ((line  . <line number>)
;;;                     (character . <character number))))
;;;
(define ($get-definition-locations identifier)
  (write-log 'debug
             (format "$get-definition-locations ~a" identifier))
  (define obj (symbol->object (string->symbol identifier)))
  (write-log 'debug (format "obj: ~a" obj))
  (if (procedure? obj)
      (begin
        (write-log 'debug "in")
        (let ((program (program-source obj 0)))
          (write-log 'debug
                     (format "program: ~a" program))
          (if program
              (let ((file-path (source:file program)))

                (write-log 'debug (format "file-path: ~a" file-path))
                (if file-path
                    (let ((file-abs-path (if (absolute-file-name? file-path)
                                             file-path
                                             (find-absolute-path file-path))))
                      (write-log 'debug (format "file-abs-path: ~a" file-abs-path))
                      (write-log 'debug (format "line: ~a" (source:line program)))
                      (write-log 'debug (format "column: ~a" (source:column program)))
                      ;; TODO return all matches (see chicken.scm)
                      (let ((ans (list
                                  `((uri . ,(string-append "file://" file-abs-path))
                                    (range . ((start . ((line . ,(source:line program))
                                                        (character . ,(source:column program))))
                                              (end . ((line . ,(source:line program))
                                                      (character . ,(+ (source:column program)
                                                                       (string-length identifier)))))))))))
                        (write-log 'debug (format "responding with: ~a" ans))
                        ans))
                    (begin
                      (write-log 'debug
                                 (format "definition does not have a source file: ~a"
                                         file-path))
                      '())))
              '())))
      (begin
        (write-log 'debug
                   (format "definition not found: ~a" identifier))
        '())))

(define (alist-ref key lst)
  (define res (assoc key lst))
  (if res
      (cdr res)
      #f))

(define ($open-file! file-path)
  (write-log 'debug (format "opening file in guile: ~a" file-path))
  ;;(load-protected file-path)
  ;; (current-path (if file-path
  ;;                        (dirname file-path)
  ;;                        #f))
  #f)

(define ($save-file! file-path)
  ;;(load-protected file-path)
  #f)

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

(define (load-protected path)
  (guard (condition
          (#t (write-log 'warning
                         (format "error loading file ~a" path))))
         (load path)))

(define (library-available? library-name)
  (resolve-module library-name #f #f #:ensure #f))

