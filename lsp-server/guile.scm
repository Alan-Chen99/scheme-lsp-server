(define-module (lsp-server guile)

#:export ($apropos-list
          $open-file
          $save-file
          $fetch-documentation
          $fetch-signature
          $get-definition-locations
          $initialize-lsp-server
          $server-capabilities
          $server-name
          $tcp-listen
          $tcp-accept
          alist-ref)

#:use-module (scheme base)
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-28)
#:use-module (srfi srfi-69)
#:use-module (ice-9 documentation)
#:use-module (ice-9 optargs)
#:use-module (ice-9 session)
#:use-module (system vm program)
#:use-module (lsp-server private))

(define $server-name
  "guile lsp server")

(define ($initialize-lsp-server root-path)
  #f)

(define $server-capabilities
  `((definitionProvider . ())))

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

(define ($get-definition-locations identifier)
  (define obj (symbol->object (string->symbol identifier)))
  (define program (program-source obj 0))
  (if program
      (let* ((file-path (source:file program))
             (file-abs-path (if (absolute-file-name? file-path)
                                file-path
                                (find-absolute-path file-path))))
        ;; TODO return all matches (see chicken.scm)
        (list
         `((uri . ,(string-append "file://" file-abs-path))
           (range . ((start . ((line . ,(- (source:line program) 1))
                               (character . ,(- (source:column program)
                                                1))))
                     (end . ((line . ,(- (source:line program) 1))
                             (character . ,(+ (source:column program)
                                              (string-length identifier))))))))))
      '()))

(define (alist-ref key lst)
  (define res (assoc key lst))
  (if res
      (cdr res)
      #f))

(define ($open-file file-path)
  #f)

(define ($save-file file-path)
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
  (and (symbol? sym)
       (module-defined? (current-module) sym)
       (module-ref (current-module) sym)))

(define (find-absolute-path path)
  (define base-path
    (find (lambda (load-path)
            (file-exists? (string-append load-path "/" path)))
          %load-path))
  (if base-path
      (canonicalize-path (string-append base-path "/" path))
      #f))
