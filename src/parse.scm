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
  (tagged-expression? expr 'import))

(define (begin-form? expr)
  (tagged-expression? expr 'begin))

(define (lambda-form? expr)
  (tagged-expression? expr 'lambda))

(define (procedure-definition-with-lambda? expr)
  (and (symbol? (cadr expr))
       (not (null? (cddr expr)))
       (lambda-form? (caddr expr))))

(define (procedure-definition-with-parenthesis? expr)
  (and (list? (cadr expr))
       (not (null? (cddr expr)))))

;; TODO implement case-lambda and set!
(define (procedure-definition-form? expr)
  (and (tagged-expression? expr 'define)
       (not (null? (cdr expr)))
       (or (procedure-definition-with-lambda? expr)
           (procedure-definition-with-parenthesis? expr))))

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
        ((procedure-definition-with-lambda? expr)
         (lambda-arguments (caddr expr)))))

(define (cond-expand-form? expr)
  (tagged-expression? expr 'cond-expand))

(define (collect-meta-data-from-expression expression)
  (let loop ((expr expression)
             (procedure-infos '())
             (imports '()))
    (cond ((null? expr)
           (make-source-meta-data (reverse procedure-infos) (reverse imports)))
          ((or (library-definition-form? expr)
               (begin-form? expr))
           (let ((subforms-meta-data
                  (map collect-meta-data-from-expression (cdr expr))))
             (loop (cdr expr)
                   (append (flatmap source-meta-data-procedure-infos
                                    subforms-meta-data)
                           procedure-infos)
                   (append (flatmap source-meta-data-imports
                                    subforms-meta-data)
                           imports))))
          ((cond-expand-form? expr)
           (let* ((matching-clause (cond-expand-find-satisfied-clause expr))
                  (subform-meta-data
                   (collect-meta-data-from-expression matching-clause)))
             (loop (cdr expr)
                   (source-meta-data-procedure-infos subform-meta-data)
                   (source-meta-data-imports subform-meta-data))))
          ((import-form? expr)
           (loop (cdr expr)
                 procedure-infos
                 (append (cdr expr) imports)))
          ((procedure-definition-form? expr)
           (loop (cdr expr)
                 (cons (make-procedure-info (procedure-definition-name expr)
                                            (procedure-definition-arguments expr)
                                            #f
                                            #f)
                       procedure-infos)
                 imports))
          (else (loop (cdr expr)
                      procedure-infos
                      imports)))))

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

(define (cond-expand-find-satisfied-clause expr)
  (cons 'begin (cdr (find cond-expand-clause-satisfied?
                          (cdr expr)))))

(define (merge-meta-data lst)
  (fold (lambda (m acc)
          (make-source-meta-data (append (source-meta-data-procedure-infos m)
                                         (source-meta-data-procedure-infos acc))
                                 (append (source-meta-data-imports m)
                                         (source-meta-data-imports acc))))
        (make-source-meta-data '() '())
        lst))


(define (collect-meta-data-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((expr (read))
                 (meta-data '()))
        (if (eof-object? expr)
            (merge-meta-data meta-data)
            (loop (read)
                  (cons (collect-meta-data-from-expression expr)
                        meta-data)))))))

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (flatmap proc lst)
  (fold append '() (map proc lst)))
