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
#:use-module (lsp-server parse)
#:use-module (lsp-server private)
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
  (write-log 'debug
             (format "apropos-list ~s" identifier))
  ;;; apropos-fold crashes when searching, for instance, "*unspecified*"
  (guard
      (condition
       (#t (write-log 'error
                      (format "$apropos-list error fetching apropos for ~s"
                              identifier))
           '()))
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
                  apropos-fold-all)))

;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
;;; MODULE (a symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi-1) 'map
(define ($fetch-documentation module-name identifier)
  (write-log 'info (format "$fetch-documentation: ~s ~s"
                           module-name
                           identifier))
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
  (write-log 'debug
             (format "$fetch-signature ~s ~s" module-name identifier))
  (if module-name
      (let* ((mod (resolve-module module-name #f #:ensure #f))
             (obj (module-ref mod identifier)))
        (if (and obj (procedure? obj))
            (format "~a~%"
                    (build-procedure-signature module-name identifier obj))
            #f))
      #f))

;;; Return a list of locations found for IDENTIFIER (a symbol).
;;; Each location is represented by an alist
;;; '((url . "file:///<path>")
;;;   (range . ((start . ((line  . <line number>)
;;;                       (character . <character number))
;;;             (end . ((line  . <line number>)
;;;                     (character . <character number))))
;;;
(define ($get-definition-locations module identifier)
  (write-log 'debug
             (format "$get-definition-locations ~a ~a" module identifier))
  (define obj (symbol->object (resolve-module module #f #:ensure #f)
                              (string->symbol identifier)))
  (write-log 'debug (format "obj: ~a" obj))
  (if (procedure? obj)
      (begin
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

(define ($open-file! file-path)
  (define lib-name (parse-library-name-from-file file-path))
  (write-log 'info (format "lib resolved ~a" lib-name))
  (when (or (not lib-name)
            (not (resolve-module lib-name #f #:ensure #f)))
    (write-log 'info (format "parsing file ~a" file-path))
    (generate-meta-data! file-path))
  #f)

(define ($save-file! file-path)
  (generate-meta-data! file-path)
  ;; (define meta-data (parse-file file-path))
  ;; (define imports (source-meta-data-imports meta-data))
  ;; (for-each (lambda (imp)
  ;;             (resolve-module imp #t))
  ;;           imports)
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

