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
(define ($initialize-lsp-server! root-path)
  (write-log 'info (format "initializing LSP server with root ~a"
                           root-path))

  (when (not (eq? root-path 'null))
    (add-to-load-path root-path))
  #f)

;;; An alist with implementation-specific server capabilities. See:
;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
;;; Note: These capabilities are joined to the implementation independent
;;; mandatory-capabilities (see server.scm).
(define $server-capabilities
  `((definitionProvider . ())
    (diagnosticsProvider . ((interFileDiagnostics . #t)
                            (workspaceDiagnostics . #t)))))

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
(define ($tcp-connect tcp-address tcp-port-number)
  (define sock (socket PF_INET SOCK_STREAM 0))
  (define addr (inet-pton AF_INET tcp-address))
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
  (connect sock (make-socket-address AF_INET addr tcp-port-number))
  (values sock sock))

(define (add-modules-to-symbols lst)
  (fold (lambda (s acc)
          (if s
              (let ((mod (symbol-module (if (string? s)
                                            (string->symbol s)
                                            s))))
                (if mod
                    (cons (cons s (module-name->string mod))
                          acc)
                    acc))
              acc))
        '()
        lst))

;;; Return completion suggestions for PREFIX (a symbol).
;;; MODULE may be used to give a hint where to search for suggestions, besides
;;; of the current module (probably guile-user).
;;; A suggestion is a pair of strings (identifier . library)
(define ($apropos-list module prefix)
  (write-log 'debug (format "$apropos-list ~a ~a" module prefix))
  (define lst (add-modules-to-symbols (lsp-geiser-completions prefix)))
  (define extra-lst (if module
                        (or (execute-in-module
                             module
                             (lambda ()
                               (add-modules-to-symbols (lsp-geiser-completions prefix))))
                            '())
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
(define ($fetch-signature mod-name identifier)
  (write-log 'debug
             (format "$fetch-signature ~s ~s"
                     mod-name
                     identifier))

  (or (lsp-geiser-signature identifier)
      (execute-in-module mod-name (lambda ()
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
    (lsp-geiser-symbol-location mod-name
                                (if (symbol? identifier)
                                    identifier
                                    (string->symbol identifier))))
  (cond ((and (or (not loc) (null? loc)) mod-name)
         (execute-in-module mod-name
                            (lambda ()
                              (let ((loc2
                                     (lsp-geiser-symbol-location
                                      mod-name
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
  (write-log 'info
   (format "importing module ~a" mod-name))
  (guard
   (condition (#t (write-log 'warning
                   (format "Can't import module ~a: ~a"
                           mod-name
                           condition))))
   (eval `(import ,mod-name) (interaction-environment))
   (let ((mod (resolve-module mod-name #t #:ensure #f)))
     (import-module-dependencies mod))))

(define (import-module-dependencies mod)
  (for-each (lambda (m)
              (let ((mod-name (module-name m)))
                (write-log 'info
                 (format "importing library ~a" mod-name))
                (eval `(import ,mod-name)
                      (interaction-environment))))
            (module-uses mod)))


(define (compile-and-import-if-needed file-path)
  (guard
      (condition
       (else (write-log 'warning (format "Can't compile file ~a: ~a"
                                         file-path
                                         condition))
             #f
             ))

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

(define ($open-file! file-path text)
  (let* ((ldef (find-library-definition-file file-path))
         (file-to-compile (or ldef file-path)))
    (when (and ldef (not (string=? ldef file-path)))
      (compile-and-import-if-needed ldef))
    (compile-and-import-if-needed file-path))

  #f)

(define ($save-file! file-path text)
  (define mod-name (parse-library-name-from-file file-path text))
  (guard
      (condition
       (else
        (write-log 'warning
                   (format "$save-file: error reloading module ~a: ~a"
                           mod-name
                           condition))

        #f))

    (if mod-name
        (let ((mod (resolve-module mod-name #t #:ensure #f)))
          (write-log 'debug (format "$save-file!: reloading ~a~%"
                                    mod-name))

          (reload-module mod)
          (import-library-by-name mod-name)
          #f)
        (load file-path))))


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

;; ignored for now
(define $tcp-read-timeout (make-parameter #f))

(define ($tcp-listen tcp-port)
  (define sock (socket PF_INET SOCK_STREAM 0))
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
  (bind sock (make-socket-address AF_INET INADDR_LOOPBACK tcp-port))
  (listen sock 20)
  sock)

(define ($tcp-accept listener)
  (define res (accept listener))
  (define port (car res))
  (values port port))

(define ($tcp-connect tcp-address tcp-port)
  (define sock (socket PF_INET SOCK_STREAM 0))
  (define addr (inet-pton AF_INET tcp-address))
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
  (connect sock (make-socket-address AF_INET addr tcp-port))
  (values sock sock))

(define ($tcp-close conn)
  (close conn))

(define (parse-compiler-output str)
  (guard
      (condition
       (else #f))
    (let ((fields (string-split str #\:)))
      (if (> (length fields) 4)
          (let ((msg-type (string-trim (list-ref fields 3))))
            (and (or (string=? msg-type "warning")
                     (string=? msg-type "error"))
                 (make-diagnostic "guile"
                                  (car fields)
                                  (- (string->number (list-ref fields 1)) 1)
                                  (string->number (list-ref fields 2))
                                  (string-join (drop fields 3) ": "))))
          #f))))

(define (externally-compile-file file-path proc)
  ;; TODO: check return code
  (let* ((ldef-path (find-library-definition-file file-path))
         (path-to-compile (or ldef-path file-path))
         (p (open-input-pipe
             (format "/usr/bin/env guild compile --r7rs -L . ~a 2>&1" path-to-compile))))
    (write-log 'debug (format "externally-compile-file: compiled ~a"
                              path-to-compile))
    (let loop ((line (read-line p))
               (diags '()))
      (write-log 'debug (format "compile command line: ~a" line))
      (cond ((eof-object? line)
             (reverse diags))
            ((proc line) => (lambda (diag)
                              (loop (read-line p)
                                    (cons diag diags))))
            (else (loop (read-line p)
                        diags))))))

(define ($compute-diagnostics file-path)
  (cond ((externally-compile-file file-path parse-compiler-output)
           => (lambda (diags)
                (write-log 'debug
                           (format "diagnotics found: ~a"
                                   diags))
                (let ((matching-diags
                       (filter (lambda (d)
                                 (let ((fname (diagnostic-file-path d)))
                                   (and fname
                                        (string=? (get-absolute-pathname file-path)
                                                  (get-absolute-pathname fname)))))
                               diags)))
                  matching-diags)))
          (else
           '())))


