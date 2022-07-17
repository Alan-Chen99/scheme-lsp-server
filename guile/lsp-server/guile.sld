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
          spawn-repl-server
)

#:use-module ((scheme base)
              #:select (define-record-type read-line guard))
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
#:use-module (lsp-server parse)
#:use-module (lsp-server private)
#:use-module (lsp-server adapter)
#:use-module (lsp-server guile util)

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
  ;; (define boot-module-path (get-module-path '(ice-9 boot-9)))
  ;; (when boot-module-path
  ;;   (generate-meta-data! boot-module-path))
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
  (lsp-geiser-completions identifier))

;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
;;; MODULE (a symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi-1) 'map
(define ($fetch-documentation identifier)
  (lsp-geiser-documentation identifier))

;;; Return the signature (a string) for IDENTIFIER (a symbol) in MODULE (a
;;; symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi 1) 'map
(define ($fetch-signature identifier)
  (lsp-geiser-signature identifier))

;;; Return a list of locations found for IDENTIFIER (a symbol).
;;; Each location is represented by an alist
;;; '((url . "file:///<path>")
;;;   (range . ((start . ((line  . <line number>)
;;;                       (character . <character number))
;;;             (end . ((line  . <line number>)
;;;                     (character . <character number))))
;;;
(define ($get-definition-locations identifier)
  (define loc
    (lsp-geiser-symbol-location (if (symbol? identifier)
                                    identifier
                                    (string->symbol identifier))))
  (if (null? loc)
      loc
      (list loc)))

(define (file-in-load-path? file-path)
  (any (lambda (dir)
         (string-prefix? dir file-path))
       %load-path))

(define ($open-file! file-path)
  (guard
   (condition
    (#t (write-log 'error (format "Can't compile file ~a: ~a"
                                  file-path
                                  condition))))
   (let ((lib-name (parse-library-name-from-file file-path)))

     (when (or (not lib-name)
               (not (resolve-module lib-name #t #:ensure #f)))
       (lsp-geiser-compile-file file-path))))
  #f)

(define ($save-file! file-path)
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

(define (spawn-repl-server port-num)
  (define sock (make-tcp-server-socket #:port port-num))
  (run-server sock))


(define (symbol->object mod sym)
  (if (and (symbol? sym)
           (module-defined? mod sym))
      (module-ref mod sym)
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

