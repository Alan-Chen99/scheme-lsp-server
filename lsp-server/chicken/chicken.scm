(import (apropos)
        (chicken base)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken pathname)
        (chicken platform)
        (chicken port)
        (chicken process)
        (chicken process-context)
        (chicken tcp)
        chicken-doc
        medea
        r7rs
        scheme
        srfi-69)
(include "../common/base.scm")

;;; A hash table mapping modules (extensions) to eggs. This is needed
;;; to fetch the correct documentation with chicken-doc
(define module-egg-mapping #f)


(define tags-table #f)
(define eggs-path
  (make-pathname (system-cache-directory) "chicken-install"))
(define chicken-source-path
  (or (get-environment-variable "CHICKEN_SRC") ""))
(define tags-path #f)
(define root-path #f)

(define $server-name
  "chicken lsp server")

(define ($initialize-lsp-server root)
  (set! root-path root)
  (set! tags-path (make-pathname root-path "lsp-server-tags"))
  (set! module-egg-mapping (build-module-egg-mapping))
  (generate-tags tags-path eggs-path chicken-source-path)
  (set! tags-table (parse-tags-file tags-path)))

(define $server-capabilities
  `((completionProvider . ((resolveProvider . #t)))
    (hoverProvider . #t)
    (signatureHelpProvider . ())
    (definitionProvider . ())
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

(define ($did-open file-path)
  (generate-tags tags-path file-path)
  (read-tags! tags-path))

(define ($did-save file-path)
  (generate-tags tags-path file-path)
  (read-tags! tags-path))

(define (generate-tags tags-file . dirs)
  (system
   (format "find ~a -type f -iname \"*.scm\" | etags --regex='/[ \\t]+([ \\t]*define[ \\t]+(?[ \\t]*\\([^ \\t)]+\\)/\\1/' -o ~a - > /dev/null"
           (string-intersperse dirs " ")
           tags-file)))

(define (parse-source-path line)
  (let ((fields (string-split line ",")))
    (unless (> (length fields) 1)
      (error "ill-formed TAGS source path" line))
    (car fields)))

(define (read-definitions src-path)
  (define regex
    (irregex '(: (* whitespace)
                 #\(
                 (or (: "define" (* any))
                     "set!")
                 (+ whitespace)
                 (? #\()
                 (submatch (+ (~ (or whitespace
                                     #\)))))
                 (? #\))
                 (* whitespace)
                 #\delete
                 (? (: (~ numeric) (* any) #\x1))
                 (submatch (+ numeric))
                 #\,
                 (+ numeric))))
  (let loop ((line (read-line))
             (res (make-hash-table)))
    (if (or (eof-object? line)
            (string-prefix? "\f" line))
        res
        (let ((submatches (irregex-match regex line)))
          (if (and submatches
                   (>= (irregex-match-num-submatches submatches)
                       2))
              (let ((identifier (irregex-match-substring submatches 1))
                    (line-number
                     (- (string->number
                         (irregex-match-substring submatches 2))
                        1)))
                (loop (read-line)
                      (begin (hash-table-set! res
                                              identifier
                                              (list src-path
                                                    line-number))
                             res)))
              (begin (write-log 'debug
                                (format  "skipping ill-formed TAGS line: ~a"
                                         line))
                     (loop (read-line) res)))))))


(define (parse-tags-file path)
  (with-input-from-file path
    (lambda ()
      (unless (string-prefix? "\f" (read-line))
        (error "parse-tags-file: ill-formed tags file (should start if \f)"))
      (let loop ((line (read-line))
                 (res (make-hash-table)))
        (if (eof-object? line)
            res
            (let* ((src-path (parse-source-path line))
                   (def-table (read-definitions src-path)))
              (loop (read-line)
                    (hash-table-merge res def-table))))))))

(define (read-tags! path)
  (define new-tags
    (parse-tags-file path))
  (define new-table
    (hash-table-merge-updating! tags-table new-tags))

  (write-log 'debug
             (format "new tags:~%~a"
                     (map (lambda (k)
                            (format "  ~a: ~a~%"
                                    k
                                    (hash-table-ref/default new-tags k #f)))
                          (hash-table-keys new-tags))))
  (set! tags-table new-table))

(define ($get-definition-location identifier)
  (define match (hash-table-ref/default tags-table identifier #f))
  (if match
      (let ((path (car match))
            (line-number (cadr match)))
        (write-log 'debug (format "identifier ~a found: path ~a, line ~a "
                                  identifier
                                  path
                                  line-number))
          `((uri . ,(string-append "file://" path))
            (range . ((start . ((line . ,line-number)
                                (character . 0)))
                      (end . ((line . ,line-number)
                              (character . 2)))))))
      'null))

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
