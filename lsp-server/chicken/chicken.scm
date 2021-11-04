(import (apropos)
        (chicken base)
        (chicken io)
        (chicken irregex)
        (chicken port)
        (chicken process)
        (chicken tcp)
        chicken-doc
        medea
        r7rs
        scheme)
(include "../common/base.scm")

;;; A hash table mapping modules (extensions) to eggs. This is needed
;;; to fetch the correct documentation with chicken-doc
(define module-egg-mapping #f)

(define $server-name
  "chicken lsp server")

(define ($initialize-lsp-server)
  (set! module-egg-mapping (build-module-egg-mapping)))

(define $server-capabilities
  `((completionProvider . ((resolveProvider . #t)))
    (signatureHelpProvider . ())
    (textDocumentSync . 1)))

(define $tcp-listen tcp-listen)

(define $tcp-accept tcp-accept)

(define ($apropos-list identifier)
  (define suggestions
    (apropos-information-list identifier #:macros? #t))
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
  (define egg (or (module-egg module)
                  module))
  (define doc-path
    (append (if (list? egg)
                egg
                (list egg))
            (list identifier)))
  (begin
    (write-log 'debug
               (format #f "looking up doc-path: ~a" doc-path))
    (with-output-to-string
      (lambda ()
        (describe (lookup-node doc-path))))))

(define ($fetch-signature module identifier)
  (define egg (or (module-egg module)
                  (car module)))
  (if (not egg)
      #f
      (node-signature
       (lookup-node (list egg identifier)))))

(define ($get-definition-location identifier)
  (error "not implemented"))

(define (build-module-egg-mapping)
  (define-values (in out pid)
    (process "chicken-status" '("-c")))
  (define (egg-line? str)
    (irregex-match
     '(: (submatch (+ any)) (+ space) (+ #\.) (* any))
     str))
  (define (extension-line? str)
    (irregex-match
     '(: (+ space) "extension" (+ space)
         (submatch (+ (~ space))) (* space))
     str))
  (let loop ((line (read-line in))
             (table '())
             (cur-egg #f))
    (cond ((eof-object? line)
           (alist->hash-table table))
          ((egg-line? line) =>
           (lambda (m)
             (loop (read-line in)
                   table
                   (string->symbol (irregex-match-substring m 1)))))
          ((extension-line? line) =>
           (lambda (m)
             (loop (read-line in)
                   (let ((mod (map string->symbol
                                   (string-split (irregex-match-substring m 1)
                                                 "."))))

                     (cons (cons mod cur-egg)
                           table))
                   cur-egg)))
          (else (loop (read-line in)
                      table
                      cur-egg)))))

(define (module-egg mod)
  (hash-table-ref/default module-egg-mapping
                          mod
                          #f))
