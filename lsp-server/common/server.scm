(define lsp-server-log-level (make-parameter 2))
(define lsp-server-state 'off)

;; (define (run)
;;   (json-rpc-loop (current-input-port) (current-output-port)))

(define (shutting-down?)
  (eqv? lsp-server-state 'shutdown))

(define-syntax define-handler
  (syntax-rules ()
    ((define-handler (handler params #:exit? exit?) body ...)
     (define (handler params)
       (when (and (shutting-down?) (not exit?))
         (raise (make-json-rpc-invalid-request-error
                 "Only exit request allowed after shutdown.")))
       (write-log 'debug
                  (format #f "Handler ~a called with params ~a"
                          'handler
                          params))
       body ...))
    ((define-handler (handler params) body ...)
     (define-handler (handler params #:exit? #f) body ...))))

(define-handler (initialize-handler params)
  ($initialize-lsp-server)
  (set! lsp-server-state 'on)
  `((capabilities . ,$server-capabilities)
    (serverInfo . ((name . ,$server-name)
                   (version . "0.1.0")))))

(define-handler (initialized-handler params)
  (write-log 'info
             "initialized")
  'null)

(define-handler (shutdown-handler params)
  (write-log 'info
             "shutting down")
  'null)

(define-handler (lsp-exit-handler params #:exit? #t)
  (write-log 'info "exiting")
  (json-rpc-exit)
  #f)

(define-handler (ignore-request params)
  (write-log 'debug (format "ignoring request. Params: ~a" params))
  #f)

(define-handler (text-document/definition params)
  (define word (get-word-under-cursor params))
  ($get-definition-location (string->symbol word)))

(define-handler (text-document/did-change params)
  (define file-path (get-uri-path params))
  (define changes
    (string-split
     (alist-ref 'text
                (vector-ref (alist-ref 'contentChanges params) 0))
     "\r\n"
     #t))
  (if file-path
      (begin (update-file! file-path changes)
             (write-log 'debug
                        (format "file contents updated: ~a"
                                file-path)))
      (write-log 'debug
                 (format "file-path not found: ~a"
                         file-path)))
  #f)

(define-handler (text-document/did-close params)
  (define file-path (get-uri-path params))
  (when (free-file! file-path)
    (write-log 'info "file closed" file-path))
  #f)

(define-handler (text-document/did-open params)
  (define file-path (get-uri-path params))
  (if file-path
      (begin (read-file! file-path)
             (write-log 'debug
                        (format "file contents read: ~a"
                                file-path)))
      (write-log 'debug
                        (format "file-path not found: ~a"
                                file-path)))
  #f)

(define-handler (text-document/did-save params)
  (define file-path (get-uri-path params))
  (write-log 'info "file saved.")
  #f)

(define-handler (text-document/completion params)
  (define word (get-word-under-cursor params))
  (write-log 'debug "getting completion suggestions for word " word)
  (if (or (not word)
          (< (string-length word) 3))
      'null
      (let ((suggestions ($apropos-list word)))
        (write-log 'debug (format "suggestions found: ~a~%"
                                  suggestions))
        `((items .
                 ,(list->vector
                   (map (lambda (ainfo)
                          (let* ((ainfo-module (apropos-info-module ainfo))
                                 (module-name
                                  (if ainfo-module
                                      (join-module-name ainfo-module)
                                      ""))
                                 (id-name
                                  (symbol->string
                                   (apropos-info-name ainfo)))
                                 (label (if ainfo-module
                                            (format "~a ~a"
                                                    module-name
                                                    id-name)
                                            (format "~a" id-name))))
                            `((label . ,label)
                              (insertText . ,id-name)
                              (data . ,(if ainfo-module
                                           `((identifier . ,id-name)
                                             (module . ,module-name))
                                           `((identifier . ,id-name)))))))
                        suggestions)))))))

(define-handler (completion-item/resolve params)
  (define id (string->symbol
              (alist-ref* '(data identifier) params)))
  (define mod (let ((m (alist-ref* '(data module) params)))
                (if m
                    (split-module-name m)
                    '())))
  (guard (condition
          (#t (begin
                (write-log 'warning
                           "error resolving "
                           mod
                           id)
                (if (> (lsp-server-log-level) 2)
                    (raise (make-json-rpc-internal-error
                            (format "Error resolving ~a ~a"
                                    mod
                                    id)))
                    #f))))
    (let ((doc ($fetch-documentation mod id)))
      (cons `(documentation . ,doc)
            params))))

(define-handler (text-document/signature-help params)
  (define cur-word (get-word-under-cursor params))
  (define matches
    (filter (lambda (ap)
              (string=? (symbol->string (apropos-info-name ap))
                        cur-word))
            ($apropos-list cur-word)))
  (if (null? matches)
      (begin
        (write-log 'info
                   (format "no signature found for: ~a" cur-word))
        'null)
      (guard
          (condition
           (#t (if (> (lsp-server-log-level) 2)
                   (raise (make-json-rpc-internal-error
                           (format "Error fetching signature of `~a`"
                                   cur-word)))
                   #f)))
        (let* ((ainfo (car matches))
               (signature
                ($fetch-signature (apropos-info-module ainfo)
                                  (apropos-info-name ainfo))))
          `((signatures . ,(vector `((label . ,signature)))))))))

(define-handler (custom/load-file params)
  (define file-path (get-uri-path params))
  (write-log 'debug "loading file: " file-path)
  (guard (condition
          (#t (write-log 'warning "error loading file")))
    (load file-path))
  #f)

(define (start-lsp-server tcp-port)
  (parameterize
      ((json-rpc-log-level (lsp-server-log-level))
       (log-level (lsp-server-log-level))
       (custom-error-codes '((definition-not-found-error . -32000)))
       (json-rpc-handler-table
        `(("initialize" . ,initialize-handler)
          ("initialized" . ,initialized-handler)
          ("textDocument/definition" . ,text-document/definition)
          ("textDocument/didChange" . ,text-document/did-change)
          ("textDocument/didClose" . ,text-document/did-close)
          ("textDocument/didOpen" . ,text-document/did-open)
          ("textDocument/didSave" . ,text-document/did-save)
          ("textDocument/completion" . ,text-document/completion)
          ("completionItem/resolve" . ,completion-item/resolve)
          ("textDocument/signatureHelp" . ,text-document/signature-help)
          ("$/cancelRequest" . ,ignore-request)
          ("exit" . ,lsp-exit-handler)
          ("shutdown" . ,shutdown-handler)
          ;; custom commands
          ("custom/loadFile" . ,custom/load-file))))
    (json-rpc-start-server/tcp tcp-port)))

(define (start-lsp-server/background tcp-port)
  (define thread
    (make-thread (lambda () (start-lsp-server tcp-port))))
  (thread-start! thread)
  thread)
