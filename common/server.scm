(define (run)
  (json-rpc-loop (current-input-port) (current-output-port)))

(define (run-tcp tcp-port)
  (cond-expand
    (chicken (tcp-read-timeout #f))
    (else))
  (write-log 'info
             (format #f "Server listening on port ~a" tcp-port))
  (define listener ($tcp-listen tcp-port))
  (define-values (in-port out-port)
    ($tcp-accept listener))
  (write-log 'info "client connected")

  (json-rpc-loop in-port out-port)

  (close-input-port in-port)
  (close-output-port out-port))

(define (ignore-request params)
  (write-log 'debug (format #f "ignoring request. Params: ~a" params))
  #f)

(define (text-document/did-change params)
  (define file-path (get-uri-path params))
  (define changes
    (cond-expand
     (chicken
      (string-split
       (alist-ref 'text
                  (vector-ref (alist-ref 'contentChanges params) 0))
       "\r\n"
       #t))
     (guile
      (string-split
       (alist-ref 'text
                  (vector-ref (alist-ref 'contentChanges params) 0))
       #\newline))))
  (if file-path
      (begin (update-file! file-path changes)
             (write-log 'debug
                        (format #f "file contents updated: ~a"
                                file-path)))
      (write-log 'debug
                 (format #f "file-path not found: ~a"
                         file-path)))
  #f)

(define (text-document/did-open params)
  (define file-path (get-uri-path params))
  (if file-path
      (begin (read-file! file-path)
             (write-log 'debug
                        (format #f "file contents read: ~a"
                                file-path)))
      (write-log 'debug
                        (format #f "file-path not found: ~a"
                                file-path)))
  #f)

(define (text-document/did-save params)
  (define file-path (get-uri-path params))
  (write-log 'info "file saved.")
  #f)

;; (define (text-document/completion params)
;;   (define file-path (get-uri-path params))
;;   (define lines (read-file! file-path))
;;   (define line-number (alist-ref* '(position line) params))
;;   (define char-number (alist-ref* '())))
(define (text-document/completion params)
  (define word (get-word-under-cursor params))
  (write-log 'debug "getting completion suggestions for word " word)
  (if (or (not word)
          (< (string-length word) 3))
      #f
      (let ((suggestions ($apropos-list word)))
        (write-log 'debug "suggestions found: ~a~%" suggestions)
        `((items .
                 ,(list->vector
                   (map (lambda (ainfo)
                          (let* ((module-name 
                                  (join-module-name
                                   (apropos-info-module ainfo)))
                                 (id-name
                                  (symbol->string
                                   (apropos-info-name ainfo)))
                                 (label (if module-name
                                            (format #f "~a ~a"
                                                    module-name
                                                    id-name)
                                            (format #f "~a" id-name))))
                            `((label . ,label)
                              (insertText . ,id-name)
                              (data . ((identifier . ,id-name)
                                       (module . ,module-name))))))
                        suggestions)))))))

(define (completion-item/resolve params)
  (define id (string->symbol
              (alist-ref* '(data identifier) params)))
  (define mod (let ((m (alist-ref* '(data module) params)))
                (if m
                    (split-module-name m)
                    '())))
  (define doc ($fetch-documentation mod id))
  (cons `(documentation . ,doc)
        params))

(define (text-document/signature-help params)
  (define cur-word (get-word-under-cursor params))
  (define matches
    (filter (lambda (ap)
              (string=? (symbol->string (apropos-info-name ap))
                        cur-word))
            ($apropos-list cur-word)))
  (if (null? matches)
      (begin
        (write-log 'info
                   (format #f "no signature found for: ~a" cur-word))
        #f)
      (let* ((ainfo (car matches))
             (signature
              ($fetch-signature (apropos-info-module ainfo)
                                (apropos-info-name ainfo))))
        `((signatures . ,(vector `((label . ,signature))))))))

(define (custom/load-file params)
  (define file-path (get-uri-path params))
  (write-log 'debug "loading file: " file-path)
  (guard (condition
          (#t (write-log 'warning "error loading file")))
    (load file-path))
  #f)

(define (start-lsp-server tcp-port . args)
  (define debug-level (if (null? args)
                          0
                          (car args)))
  (unless (number? debug-level)
    (error "Not a valid debug-level: " (car args)))
  (define thread
    (parameterize
        ((json-rpc-log-level debug-level)
         (json-string->scheme $json-string->scheme)
         (scheme->json-string $scheme->json-string))
      (make-thread
       (lambda ()
         (parameterize
             ((json-rpc-handler-table
               `(("initialize" .
                  ,(lambda (params)
                     `((capabilities . ,$server-capabilities)
                       (serverInfo . ((name . ,$server-name)
                                      (version . "0.0.1"))))))
                 ("shutdown" . ,(lambda (params)
                                  (write-log 'info
                                             "shutting down")
                                  #f))
                 ("initialized" . ,(lambda (params)
                                     (write-log 'info
                                                "initialized")
                                     #f))
                 ("textDocument/didChange" . ,text-document/did-change)
                 ("textDocument/didOpen" . ,text-document/did-open)
                 ("textDocument/didSave" . ,text-document/did-save)
                 ("textDocument/completion" . ,text-document/completion)
                 ("completionItem/resolve" . ,completion-item/resolve)
                 ("textDocument/signatureHelp" . ,text-document/signature-help)
                 ("$/cancelRequest" . ,ignore-request)
                 ("exit" . ,ignore-request)

;;; custom commands
                 ("custom/loadFile" . ,custom/load-file))))
           (run-tcp tcp-port))))))
  (thread-start! thread)
  thread)

;; (cond-expand ((or chicken-script compiling)
;;               (let ((thread (start-lsp-server)))
;;                (thread-join! thread)))
;;              (else))

