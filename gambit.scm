(define-library (lsp-server gambit)

(export $apropos-list
        $fetch-documentation
        $fetch-signature
        $get-definition-locations
        $initialize-lsp-server!
        $open-file!
        $save-file!
        $server-capabilities
        $server-name
        $tcp-accept
        $tcp-connect
        $tcp-listen
        $tcp-read-timeout)

(import (gambit)
        (only (srfi 13) string-tokenize string-prefix?)
        (only (srfi 14) char-set char-set-complement)
        (srfi 28)
        (lsp-server adapter)
        (lsp-server parse)
        (lsp-server private)
        (lsp-server gambit util))

(begin

  (define $server-name
    "Gambit LSP server")

  ;;; Ignored for now
  (define $tcp-read-timeout (make-parameter #f))

  (define ($initialize-lsp-server! root-path)
    (write-log 'info (format "initializing LSP server with root ~a"
                             root-path))
    (module-search-order-add! root-path)
    #f)

  (define $server-capabilities
    '((definitionProvider . ())))

  (define ($tcp-listen port-number)
    (open-tcp-server port-number))

  (define ($tcp-accept listener)
    (let ((p (read listener)))
      (values p p)))

  (define ($tcp-connect tcp-address tcp-port-number)
    (let ((p (open-tcp-client tcp-port-number)))
      (values p p)))

  (define ($apropos-list module prefix)
    (write-log 'debug (format "$apropos-list ~a ~a" module prefix))
    (fetch-apropos prefix))

  ;; TODO move this geiser
  (define ($get-definition-locations mod-name identifier)
    (define proc (if (symbol? identifier)
                     (guard
                         (condition (#t (write-log 'error
                                                   (format "procedure not found: ~a"
                                                           identifier))
                                        #f))
                       (eval identifier)) ; safe 'cause only a symbol
                     #f))
    (if proc
        (let ((loc (##procedure-locat proc)))
          (if (not loc)
              '()
              (let* ((file-path (##container->path (##locat-container loc)))
                     (pos (##position->filepos (##locat-position loc)))
                     (line-number (##filepos-line pos))
                     (char-number (##filepos-col pos)))
                `(((uri . ,file-path)
                   (range . ((start . ((line . ,line-number)
                                       (character . ,char-number)))
                             (end . ((line . ,line-number)
                                     (character . ,(+ char-number
                                                      (string-length
                                                       (symbol->string identifier)))))))))))))
        '()))

  (define (compile-and-import-if-needed file-path)
  (guard
   (condition
    (#t (write-log 'error (format "Can't compile file ~a: ~a"
                                  file-path
                                  (cond ((error-object? condition)
                                         (error-object-message condition))
                                        (else condition))))
        (raise condition)))
   (let ((mod-name (parse-library-name-from-file file-path)))
     (when mod-name
       ;;(write-log 'info (format "loading file ~a" file-path))
       ;;(load file-path)
       (write-log 'info (format "importing module ~a" mod-name))
       (eval `(import ,mod-name)))
     #f)))

  (define ($open-file! file-path)
    (compile-and-import-if-needed file-path)
    #f)

  (define ($save-file! file-path)
    (compile-and-import-if-needed file-path)
    #f)

  (define ($fetch-documentation mod-name identifier)
    #f)

  (define ($fetch-signature mod-name identifier)
    (write-log 'debug
               (format "$fetch-signature ~s ~s"
                       mod-name
                       identifier))

    (lsp-geiser-signature identifier))

  (define namespace-regex
    (irregex '(: "\""
                 (submatch (+ graphic))
                 "\""
                 whitespace
                 "namespace:")))

  (define (parse-namespace line)
    (let ((m (irregex-match namespace-regex line)))
      (and m
           (irregex-match-substring m 1))))

  (define (fetch-apropos prefix)
    (let ((outp (open-output-string))
          (valid-entry (char-set-complement (char-set #\, #\space))))
      (apropos prefix outp)
      (let ((text (get-output-string outp)))
        (let ((inp (open-input-string text)))
          (let loop ((line (read-line inp))
                     (cur-mod #f)
                     (result '()))
            (cond ((eof-object? line)
                   (close-input-port inp)
                   (close-output-port outp)
                   (reverse result))
                  ((parse-namespace line)
                      => (lambda (cur-mod)
                           (loop (read-line inp)
                                 cur-mod
                                 result)))
                  (else
                   (let* ((entries (string-tokenize line valid-entry))
                          (suggestions (filter (lambda (e)
                                                 (string-prefix? prefix e))
                                               entries)))
                     (loop (read-line inp)
                           cur-mod
                           (append (map (lambda (s)
                                          (cons s cur-mod))
                                        suggestions)
                                   result))))))))))))
