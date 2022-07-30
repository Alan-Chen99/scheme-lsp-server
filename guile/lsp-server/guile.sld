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
#:use-module (geiser modules)
#:use-module (ice-9 documentation)
#:use-module (ice-9 optargs)
#:use-module (ice-9 session)
#:use-module (system vm program)
#:use-module (system repl server)
#:use-module (json-rpc lolevel)
#:use-module (lsp-server parse)
#:use-module (lsp-server private)
#:use-module (lsp-server adapter)
#:use-module (lsp-server guile util)

#:declarative? #f)

(define root-path (make-parameter #f))
(define current-path (make-parameter #f))

(define scheme-file-regex
  (irregex '(: (* any)
               (or ".scm"
                   ".sld"
                   ".ss")
               eol)))

;;; Ignored for now
(define $tcp-read-timeout (make-parameter #f))

(define $server-name
  "Guile LSP server")

;;; Initialize LSP server to manage project at ROOT (a string). Used
;;; for implementation-specific side effects only. Empty for now.
(define ($initialize-lsp-server! root)
  (when (not (eq? root 'null))
    (add-to-load-path root))
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

(define (add-modules-to-symbols lst)
  (map (lambda (s)
         (cons s (module-name->string
                  (symbol-module (if (string? s)
                                     (string->symbol s)
                                     s)))))
       lst))

;;; Return completion suggestions for PREFIX (a symbol).
;;; MODULE may be used to give a hint where to search for suggestions, besides
;;; of the current module (probably guile-user).
;;; A suggestion is a pair of strings (identifier . library)
(define ($apropos-list module prefix)
  (define lst (add-modules-to-symbols (lsp-geiser-completions prefix)))
  (define extra-lst (if module
                        (execute-in-module
                         module
                         (lambda ()
                           (add-modules-to-symbols (lsp-geiser-completions prefix))))
                        '()))
  (lset-union equal? lst extra-lst))

;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
;;; MODULE (a symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi-1) 'map
(define ($fetch-documentation module identifier)
  (or (lsp-geiser-documentation identifier)
      (execute-in-module module
                         (lambda ()
                           (lsp-geiser-documentation identifier)))))

;;; Return the signature (a string) for IDENTIFIER (a symbol) in MODULE (a
;;; symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi 1) 'map
(define ($fetch-signature module identifier)
  (or (lsp-geiser-signature identifier)
      (execute-in-module module (lambda ()
                                  (lsp-geiser-signature identifier)))))

;;; Return a list of locations found for IDENTIFIER (a symbol).
;;; Each location is represented by an alist
;;; '((url . "file:///<path>")
;;;   (range . ((start . ((line  . <line number>)
;;;                       (character . <character number))
;;;             (end . ((line  . <line number>)
;;;                     (character . <character number))))
;;;
(define ($get-definition-locations mod-name identifier)
  (write-log 'debug
             (format "$get-definition-locations ~a ~a"
                     mod-name
                     identifier))
  (define loc
    (lsp-geiser-symbol-location (if (symbol? identifier)
                                    identifier
                                    (string->symbol identifier))))
  (write-log 'debug
             (format "loc: ~a" loc))
  (cond ((and (or (not loc) (null? loc)) mod-name)
         (execute-in-module mod-name
                            (lambda ()
                              (let ((loc2
                                     (lsp-geiser-symbol-location
                                      (if (symbol? identifier)
                                          identifier
                                          (string->symbol identifier)))))
                                (if (null? loc2)
                                    '()
                                    (list loc2))))))
        ((or (not loc) (null? loc)) '())
        (else (list loc))))

(define (file-in-load-path? file-path)
  (any (lambda (dir)
         (string-prefix? dir file-path))
       %load-path))

(define (import-library-by-name mod-name)
  (write-log 'debug
             (format "importing module ~a" mod-name))
  (eval `(import ,mod-name) (interaction-environment))
  (let ((mod (resolve-module mod-name #t #:ensure #f)))
    (import-module-dependencies mod)))

(define (import-module-dependencies mod)
  (for-each (lambda (m)
              (let ((mod-name (module-name m)))
                (write-log 'debug
                           (format "importing library ~a" mod-name))
                (eval `(import ,mod-name)
                      (interaction-environment))))
            (module-uses mod)))

(define (compile-and-import-if-needed file-path)
  (guard
   (condition
    (#t (write-log 'error (format "Can't compile file ~a: ~a"
                                  file-path
                                  condition))))
   (let* ((mod-name (parse-library-name-from-file file-path))
          (mod (if mod-name
                   (resolve-module mod-name #t #:ensure #f)
                   #f)))
     (cond ((and mod-name (not mod))
            (write-log 'debug
                       (format "compile-and-import-if-needed: compiling ~a and importing ~a"
                               file-path
                               mod-name))
            (lsp-geiser-compile-file file-path)
            (import-library-by-name mod-name))
           ((and mod-name mod)
            (write-log 'debug
                       (format "compile-and-import-if-needed: importing ~a" mod-name))
            (import-library-by-name mod-name))
           (else
            (write-log 'debug
                       (format "compile-and-import-if-needed: ignoring file ~a" file-path))
            #f)))))

(define ($open-file! file-path)
  (compile-and-import-if-needed file-path)
  #f)

(define ($save-file! file-path)
  (define mod-name (parse-library-name-from-file file-path))
  (if mod-name
      (guard
       (condition (#t
                   (write-log 'error
                              (format "$save-file: error reloading module ~a: ~a"
                                      mod-name
                                      condition))
                   (raise-exception
                    (make-json-rpc-custom-error
                     'load-error
                     (format "error loading/import file ~a" file-path)))))
       (let ((mod (resolve-module mod-name #t #:ensure #f)))

         (reload-module mod)
         (import-library-by-name mod-name)
         #f))
      (lsp-geiser-load-file file-path)))


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

(define (execute-in-module module thunk)
  (if module
      (save-module-excursion
       (lambda ()
         (let ((mod (resolve-module module #f #:ensure #f)))
           (if mod
               (begin
                 (set-current-module (resolve-module module #f #:ensure #f))
                 (thunk))
               #f))))
      #f))
