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


(define (send-diagnostics file-path line-num char-num msg)
  (let* ((doc (read-text! file-path))
         (word (get-word-at-position doc
                                     line-num
                                     char-num
                                     (lambda (c)
                                       (or (identifier-char? c)
                                           (char=? c #\()
                                           (char=? c #\{)
                                           (char=? c #\[)))
                                     (lambda (c)
                                       (or (identifier-char? c)
                                           (char=? c #\space)
                                           (char=? c #\))
                                           (char=? c #\})
                                           (char=? c #\]))))))
    (let* ((end (or (and word
                         (editor-word-end-char word))
                    79))
           (start (if (> (- end char-num) 1)
                      char-num
                      0)))
      (json-rpc-send-notification
       (server-out-port)
       "textDocument/publishDiagnostics"
       `((uri . ,file-path)
         (diagnostics . #(((message . ,msg)
                           (source . "interpreter")
                           (range . ((start . ((line . ,line-num)
                                               (character . ,start)))
                                     (end . ((line . ,line-num)
                                             (character . ,end))))))))))))
  #f)

(define (clear-diagnostics file-path)
  (json-rpc-send-notification
   (server-out-port)
   "textDocument/publishDiagnostics"
   `((uri . ,file-path)
     (diagnostics . #()))))

(define (compile-and-send-diagnostics file-path)
  (cond ((externally-compile-file file-path parse-compiler-output)
         => (lambda (diags)
              (write-log 'debug (format "compile-and-send-diagnostics: diagnostics found: ~a"
                                          diags))
              (let ((diag
                     (find (lambda (d)
                             (let ((fname (alist-ref 'filename d)))
                               (and fname
                                    (string-contains file-path fname))))
                           diags)))
                (if diag
                    (send-diagnostics (alist-ref 'filename diag)
                                    (alist-ref 'line-number diag)
                                    (alist-ref 'char-number diag)
                                    (alist-ref 'message diag))
                    (clear-diagnostics file-path)))))
        (else
         (clear-diagnostics file-path)))
  #f)

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
  (compile-and-import-if-needed file-path)
  (compile-and-send-diagnostics file-path)

  #f)

(define ($save-file! file-path text)
  (define mod-name (parse-library-name-from-file file-path))
  (guard
      (condition
       (else
        (write-log 'error
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
        (load file-path)))
  (compile-and-send-diagnostics file-path))


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
      (if (> (length fields) 3)
          `((filename . ,(car fields))
            (line-number . ,(- (string->number (cadr fields)) 1))
            (char-number . ,(string->number (caddr fields)))
            (message . ,(string-join (drop fields 3))))
          #f))))

(define (externally-compile-file file-path proc)
  ;; TODO: check return code
  (let ((p (open-input-pipe
            (format "guile --r7rs --no-auto-compile ~a 2>&1" file-path))))
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


