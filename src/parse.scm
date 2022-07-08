(define-record-type <procedure-info>
  (make-procedure-info name arguments line character)
  procedure-info?
  (name procedure-info-name)
  (arguments procedure-info-arguments)
  (line procedure-info-line set-procedure-info-line!)
  (character procedure-info-character set-procedure-info-character!))

(define-record-type <source-meta-data>
  (make-source-meta-data procedure-infos imports)
  source-meta-data?
  (procedure-infos source-meta-data-procedure-infos)
  (imports source-meta-data-imports))

(define-record-type <parse-context>
  (make-parse-context directory)
  parse-context?
  (directory parse-context-directory))

;;;; Parameters

(define identifier-to-source-meta-data-table
  (make-parameter (make-hash-table)))

(define source-path-timestamps
  (make-parameter (make-hash-table)))

;;;; Predicates

(define (tagged-expression? expr procedure)
  (and (list? expr)
       (not (null? expr))
       (eq? (car expr) procedure)))

(define (r7rs-library-definition-form? expr)
  (tagged-expression? expr 'define-library))

(define (chicken-library-definition-form? expr)
  (tagged-expression? expr 'module))

(define (guile-library-definition-form? expr)
  (tagged-expression? expr 'define-module))

(define (library-definition-form? expr)
  (cond-expand
   (chicken (or (r7rs-library-definition-form? expr)
                (chicken-library-definition-form? expr)))
   (guile (or (r7rs-library-definition-form? expr)
              (guile-library-definition-form? expr)))))

(define (import-form? expr)
  (or (tagged-expression? expr 'import)
      (tagged-expression? expr 'use-modules)))

(define (include-form? expr)
  (tagged-expression? expr 'include))

(define (load-form? expr)
  (tagged-expression? expr 'load))

(define (begin-form? expr)
  (tagged-expression? expr 'begin))

(define (lambda-form? expr)
  (tagged-expression? expr 'lambda))

(define (case-lambda-form? expr)
  (tagged-expression? expr 'case-lambda))

(define (procedure-definition-with-case-lambda? expr)
  (and (symbol? (cadr expr))
       (not (null? (cddr expr)))
       (case-lambda-form? (caddr expr))))

(define (procedure-definition-with-lambda? expr)
  (and (symbol? (cadr expr))
       (not (null? (cddr expr)))
       (lambda-form? (caddr expr))))

(define (procedure-definition-with-parenthesis? expr)
  (and (pair? (cadr expr))
       (not (null? (cddr expr)))))

;; TODO support set!
(define (procedure-definition-form? expr)
  (and (or (tagged-expression? expr 'define)
           (tagged-expression? expr 'define*))
       (not (null? (cdr expr)))
       (or (procedure-definition-with-case-lambda? expr)
           (procedure-definition-with-lambda? expr)
           (procedure-definition-with-parenthesis? expr))))

(define (cond-expand-form? expr)
  (tagged-expression? expr 'cond-expand))

(define (cond-expand-clause-satisfied? clause)
  (if (or (not (list? clause))
          (null? clause))
      #f
      (let ((predicate (car clause)))
        (cond ((and (symbol? predicate)
                    (eq? predicate 'else))
               #t)
              ((symbol? predicate)
               (memq predicate (features)))
              ((and (list? predicate)
                    (not (null? predicate)))
               (case (car predicate)
                 ((and) (every (compose cond-expand-clause-satisfied? list) (cdr predicate)))
                 ((or) (any (compose cond-expand-clause-satisfied? list) (cdr predicate)))
                 ((library) (and (not (null? (cdr predicate)))
                                 (library-available? (cadr predicate))))
                 ((not) (not (cond-expand-clause-satisfied? (cdr predicate))))
                 ((else) #t)
                 (else (memq (car predicate) (features)))))
              (else (error "unknown predicate " predicate))))))

;;;; Syntax accessors

(define (case-lambda-arguments expr)
  (reverse (fold (lambda (clause acc)
                   (if (and (pair? clause)
                            (not (null? clause)))
                       (cons (car clause)
                             acc)
                       acc))
                 '()
                 (cdr expr))))

(define (lambda-arguments expr)
  (cadr expr))

(define (procedure-definition-name expr)
  (cond ((procedure-definition-with-parenthesis? expr)
         (caadr expr))
        ((procedure-definition-with-lambda? expr)
         (cadr expr))))

(define (procedure-definition-arguments expr)
  (cond ((procedure-definition-with-parenthesis? expr)
         (cdr (cadr expr)))
        ((procedure-definition-with-case-lambda? expr)
         (case-lambda-arguments (caddr expr)))
        ((procedure-definition-with-lambda? expr)
         (lambda-arguments (caddr expr)))))

;;;; Main procedures

(define (parse-guile-module expression)
  (let loop ((expr (cddr expression)) ;; skip module name for now
             (previous-keyword #f)
             (imports '()))
    (cond ((null? expr)
           (make-source-meta-data (make-hash-table)
                                  (reverse imports)))
          ((keyword? (car expr))
           (loop (cdr expr)
                 (car expr)
                 imports))
          ((eq? previous-keyword #:use-module)
           (loop (cdr expr)
                 #f
                 (if (and (not (null? expr))
                          (list? (car expr))
                          (not (null? (car expr)))
                          (list? (caar expr)))
                     ;; handle clauses of the form (ignore #:select for now):
                     ;; #:use-module ((scheme file) #:select (with-input-from-file))
                     (cons (caar expr) imports)
                     (cons (car expr) imports))))
          (else
           (loop (cdr expr)
                 #f
                 imports)))))

(define (parse-r7rs-import-set expr)
  (cond ((symbol? expr) expr)
        ((null? expr) '())
        ((member (car expr)
                 '(only except prefix rename))
         (parse-r7rs-import-set (cadr expr)))
        (else expr)))

(define (parse-expression expression context)
  (let loop ((expr expression)
             (procedure-infos (make-hash-table))
             (imports '()))
    (cond ((not (list? expr))
           (make-source-meta-data procedure-infos
                                  (reverse imports)))
          ((null? expr)
           (make-source-meta-data procedure-infos
                                  (reverse imports)))
          ((guile-library-definition-form? expr)
           (let ((subform-meta-data (parse-guile-module expr)))
             (loop (cdr expr)
                   procedure-infos
                   (append (source-meta-data-imports subform-meta-data)
                           imports))))
          ((or (library-definition-form? expr)
               (begin-form? expr))
           (let ((subforms-meta-data
                  (map (lambda (e)
                         (parse-expression e context))
                       (cdr expr))))
             (loop (cdr expr)
                   (fold (lambda (sub-ht acc)
                           (hash-table-join! acc sub-ht))
                         procedure-infos
                         (map source-meta-data-procedure-infos
                              subforms-meta-data))
                   (append (flatmap source-meta-data-imports
                                    subforms-meta-data)
                           imports))))
          ((cond-expand-form? expr)
           (let* ((matching-clause (cond-expand-find-satisfied-clause expr))
                  (subform-meta-data
                   (parse-expression matching-clause context)))
             (loop (cdr expr)
                   (source-meta-data-procedure-infos subform-meta-data)
                   (source-meta-data-imports subform-meta-data))))
          ((import-form? expr)
           (loop (cdr expr)
                 procedure-infos
                 (append (map parse-r7rs-import-set (cdr expr))
                         imports)))
          ((procedure-definition-form? expr)
           (loop (cdr expr)
                 (let ((proc-name (procedure-definition-name expr)))
                   (hash-table-set! procedure-infos
                                    proc-name
                                    (make-procedure-info proc-name
                                                         (procedure-definition-arguments expr)
                                                         #f
                                                         #f))
                   procedure-infos)
                 imports))
          ((or (include-form? expr)
               (load-form? expr))
           (when (and (not (null? (cdr expr)))
                      (string? (cadr expr)))
             (generate-meta-data!
              (pathname-join (parse-context-directory context)
                             (cadr expr))))
           (make-source-meta-data procedure-infos
                                  imports))
          (else (loop (cdr expr)
                      procedure-infos
                      imports)))))

(define (cond-expand-find-satisfied-clause expr)
  (cons 'begin (cdr (find cond-expand-clause-satisfied?
                          (cdr expr)))))

(define (merge-meta-data lst)
  (fold (lambda (m acc)
          (make-source-meta-data (hash-table-join!
                                  (source-meta-data-procedure-infos m)
                                  (source-meta-data-procedure-infos acc))
                                 (append (source-meta-data-imports m)
                                         (source-meta-data-imports acc))))
        (make-source-meta-data (make-hash-table) '())
        lst))

(define (print-meta-data meta-data)
  (write-log 'debug (format "imports: ~s" (source-meta-data-imports meta-data)))
  (write-log 'debug "procedure-infos: ")
  (let ((pinfos (source-meta-data-procedure-infos meta-data)))
    (hash-table-walk pinfos
                     (lambda (pname pinfo)
                       (write-log 'debug (format "\t~s" pname))))))

(define (print-source-meta-data-table)
  (hash-table-walk
   (identifier-to-source-meta-data-table)
   (lambda (identifier source-meta-data-table)
     (write-log 'debug
                (format "~s: " identifier))
     (hash-table-walk
      source-meta-data-table
      (lambda (source-path identifier-pinfo-table)
        (write-log 'debug
                   (format "\t~s: " source-path)))))))

(define definition-regex
  (irregex '(: (* whitespace)
               (* #\()
               (: (or "define" "define-syntax" "set!")
                  (+ whitespace)
                  (? #\()
                  (submatch (+ (~ (or whitespace
                                      #\))))))
               (* any))))

(define (parse-definition-line line)
  (let ((submatches (irregex-match definition-regex line)))
    (if submatches
        (cons (irregex-match-substring submatches 1)
              (irregex-match-start-index submatches 1))
        #f)))

(define (collect-procedure-locations procedure-infos filename)
  (define symbol-location-map
    (with-input-from-file filename
      (lambda ()
        (let loop ((line (read-line))
                   (line-number 1)
                   (results '()))
          (cond ((eof-object? line)
                 (alist->hash-table results))
                (else
                 (let ((parse-result (parse-definition-line line)))
                   (if parse-result
                       (let ((parsed-symbol (car parse-result))
                             (line-offset (cdr parse-result)))
                         (loop (read-line)
                               (+ line-number 1)
                               (cons (list (string->symbol parsed-symbol)
                                           (- line-number 1)
                                           line-offset)
                                     results)))
                       (loop (read-line)
                             (+ line-number 1)
                             results)))))))))
  (hash-table-fold procedure-infos
                   (lambda (identifier pinfo acc)
                     (let* ((pname (procedure-info-name pinfo))
                            (loc (hash-table-ref/default symbol-location-map
                                                         pname
                                                         #f)))
                       (hash-table-set! acc
                                        identifier
                                        (if loc
                                            (make-procedure-info
                                             pname
                                             (procedure-info-arguments pinfo)
                                             (list-ref loc 0)
                                             (list-ref loc 1))
                                            pinfo))
                       acc))
                   (make-hash-table)))

(define (collect-meta-data-from-file filename)
  (define meta-data-without-location
    (with-input-from-file filename
      (lambda ()
        (let loop ((expr (read))
                   (meta-data '()))
          (if (eof-object? expr)
              (merge-meta-data meta-data)
              (loop (read)
                    (cons (parse-expression expr (make-parse-context
                                                  (pathname-directory filename)))
                          meta-data)))))))
  (make-source-meta-data
   (collect-procedure-locations
    (source-meta-data-procedure-infos meta-data-without-location)
    filename)
   (source-meta-data-imports meta-data-without-location)))

(define (update-identifier-to-source-meta-data-table! source-path meta-data)
  (hash-table-walk
   (source-meta-data-procedure-infos meta-data)
   (lambda (identifier pinfo)
     (hash-table-update!/default (identifier-to-source-meta-data-table)
                                 identifier
                                 (lambda (v)
                                   (begin (hash-table-set! v
                                                           source-path
                                                           pinfo)
                                          v))
                                 (alist->hash-table
                                  `((,source-path . ,pinfo)))))))

(define (parse-and-update-table! source-path)
  (write-log 'debug
             (format "parse-and-update-table!: ~s~%" source-path))
  (define meta-data (collect-meta-data-from-file source-path))
  (write-log 'debug
             "parse-and-update-table!: collected meta data:\n")
  (print-meta-data meta-data)
  (update-identifier-to-source-meta-data-table! source-path meta-data)
  (write-log 'debug "parse-and-update-table!: current meta data table state:\n")
  (print-source-meta-data-table)
  (for-each (lambda (path)
              (let ((module-path (get-module-path path)))
                (when module-path
                  (generate-meta-data! module-path))))
            (source-meta-data-imports meta-data)))

(cond-expand
 (guile (define (generate-meta-data! . files)
          (write-log 'debug
                     (format "generate-meta-data! for files ~a" files))
          (for-each
           (lambda (f)
             (ftw f
                  (lambda (filename statinfo flag)
                    (when (eq? flag 'regular)
                      (let ((old-time-stamp (hash-table-ref/default
                                             (source-path-timestamps)
                                             filename
                                             #f)))
                        (when (or (not old-time-stamp)
                                  (< old-time-stamp
                                     (stat:mtime statinfo)))
                          (begin
                            (hash-table-set! (source-path-timestamps)
                                             filename
                                             (stat:mtime statinfo))
                            (parse-and-update-table! filename)))))
                    #t)))
           (filter (lambda (f)
                     (not (string=? f "")))
                   files))))
 (chicken
  (define (generate-meta-data! . files)
    (write-log 'debug
               (format "generate-meta-data! for files ~a" files))
    (for-each
     (lambda (f)
       (guard
        (condition
         (#t (write-log 'warning
                        (format "generate-tags: can't read file ~a"
                                f))))
        (if (directory? f)
            (let ((files (find-files f
                                     #:test (irregex
                                             '(: (* any)
                                                 (or ".scm"
                                                     ".sld"
                                                     ".ss"))))))
              (for-each
               (lambda (filename)
                 (let* ((stats (file-stat filename))
                        (mtime (vector-ref stats 8))
                        (old-time-stamp (hash-table-ref/default
                                         (source-path-timestamps) filename #f)))
                   (when (or (not old-time-stamp)
                             (> mtime old-time-stamp))
                     (begin
                       (hash-table-set! (source-path-timestamps)
                                        filename
                                        mtime)
                       (parse-and-update-table! filename)))))
               files))
            (parse-and-update-table! f))))
     (filter (lambda (f)
               (not (string=? f "")))
             files)))))

(define (fetch-definition-locations identifier)
  (write-log 'debug
             (format "fetch-definition-locations: ~a (~a)"
                     identifier
                     (if (symbol? identifier)
                         'symbol
                         'string)))
  (define locations
    (hash-table->alist
     (hash-table-ref/default (identifier-to-source-meta-data-table)
                             (string->symbol identifier)
                             (make-hash-table))))
  (if (not (null? locations))
      (begin
        (write-log 'debug
                   (format "locations for identifier ~a found: ~a"
                           identifier
                           locations))
        (map (lambda (loc)
               (let* ((path (car loc))
                      (pinfo (cdr loc))
                      (line-number (procedure-info-line pinfo))
                      (char-number (procedure-info-character pinfo)))
                 (write-log 'debug (format "identifier ~a found: path ~a, line ~a, char ~a "
                                           identifier
                                           path
                                           line-number
                                           char-number))
                 `((uri . ,(string-append "file://" path))
                   (range . ((start . ((line . ,line-number)
                                       (character . ,char-number)))
                             (end . ((line . ,line-number)
                                     (character . ,char-number))))))))
             locations))
      '()))

(define (pinfo-signature pinfo)
  (define name (procedure-info-name pinfo))
  (define args (procedure-info-arguments pinfo))

  (format "~a" (cons name args)))

(define (fetch-signature identifier)
  (define id
    (if (symbol? identifier)
        identifier
        (string->symbol identifier)))
  (define source-meta-data-table
    (hash-table-ref/default
     (identifier-to-source-meta-data-table)
     id
     #f))
  (define pinfos
    (if source-meta-data-table
        (hash-table-values source-meta-data-table)
        '()))
  (define pinfo
    (if (not (null? pinfos))
        (car pinfos)
        #f))
  (if pinfo
      (pinfo-signature pinfo)
      #f))

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (flatmap proc lst)
  (fold append '() (map proc lst)))
