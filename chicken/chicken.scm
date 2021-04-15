(import (apropos)
        (chicken base)
        (chicken io)
        (chicken port)
        (chicken process)
        (chicken tcp)
        medea
        r7rs
        scheme)
(include "../common/base.scm")

(define $server-name
  "chicken lsp server")

(define $server-capabilities
  `((completionProvider . ((resolveProvider . #t)))
    (signatureHelpProvider . ())
    (textDocumentSync . 1)))

(define $tcp-listen tcp-listen)

(define $tcp-accept tcp-accept)

(define ($json-string->scheme j)
  (call-with-input-string j
    read-json))

(define ($scheme->json-string s)
  (json->string s))

(define ($apropos-list identifier)
  (define suggestions
    (apropos-information-list identifier #:macros? #t #:imported? #t))
  (map (lambda (s)
         (let* ((mod-id-pair (car s))
                (mod (let ((fst (car mod-id-pair)))
                       (if (eqv? fst '||)
                           #f
                           (map string->symbol
                                (string-split (symbol->string fst) ".")))))
                (id (cdr mod-id-pair))
                (type (cdr s)))
           (make-apropos-info mod id type #f)))
       suggestions))

(define ($fetch-documentation module identifier)
  (define local-module-name
    (map symbol->string module) " ")
  (define doc-path
    (append local-module-name
            (list (symbol->string identifier))))
  (define-values (in-port out-port pid)
    (process "chicken-doc" (cons "-i" doc-path)))
  (define lines (read-lines in-port))
  (apply string-append (intersperse lines "\n")))

(define ($fetch-signature module identifier)
  (if (not module)
      #f
      (let-values
          (((in-port out-port pid)
            (process "chicken-doc"
                     (append '("-s")
                             (map symbol->string module)
                             (list (symbol->string identifier))))))
        (let ((lines (read-lines in-port)))
          (if (null? lines)
              #f
              (car lines))))))
