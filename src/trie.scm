(define-record-type <trie-node>
  (make-trie-node children terminal? value)
  trie-node?
  (children trie-node-children)
  (terminal? trie-node-terminal? set-trie-node-terminal!)
  (value trie-node-value set-trie-node-value!))

(define (make-trie)
  (make-trie-node (make-hash-table) #f #f))

(define trie? trie-node?)

(define (trie-insert! trie key value)
  (define len (string-length key))
  (let loop ((idx 0)
             (current-node trie))
    (if (= idx len)
        (begin
          (set-trie-node-value! current-node value)
          (set-trie-node-terminal! current-node #t)
          trie)
        (let* ((char (string-ref key idx))
               (children (trie-node-children current-node))
               (existing-node (hash-table-ref/default children
                                                      char
                                                      #f))
               (next-node (or existing-node
                              (make-trie-node (make-hash-table)
                                              #f
                                              value))))
          (hash-table-set! children char next-node)
          (loop (+ idx 1) next-node)))))

(define (insert-sorted lst key)
  (let loop ((rem lst)
             (left '()))
    (cond ((null? rem)
           (reverse (cons key left)))
          ((string>? (car rem) key)
           (append (reverse left)
                   (cons key rem)))
          (else
           (loop (cdr rem)
                 (cons (car rem)
                       left))))))

(define (trie-keys trie)
  (define words-found '())
  (let loop ((current-node trie)
             (chars '()))
    (let ((children (trie-node-children current-node)))
      (if (trie-node-terminal? current-node)
          (let ((word (list->string (reverse chars))))
            (set! words-found (insert-sorted words-found word))
            (if (= (hash-table-size children) 0)
                #t
                (hash-table-walk children
                                 (lambda (char node)
                                   (loop node
                                         (cons char chars))))))
          (hash-table-walk children
                           (lambda (char node)
                             (loop node (cons char chars)))))))
  words-found)


(define (trie-words-with-prefix trie prefix)
  (define len (string-length prefix))
  (let loop ((idx 0)
             (current-node trie))
    (if (= idx len)
        (map (lambda (w)
               (string-append prefix w))
             (trie-keys current-node))
        (let* ((char (string-ref prefix idx))
               (next-node (hash-table-ref/default
                           (trie-node-children current-node)
                           char
                           #f)))
          (if next-node
              (loop (+ idx 1)
                    next-node)
              '())))))

(define (trie->alist trie)
  (define children (trie-node-children trie))
  (define terminal? (trie-node-terminal? trie))
  (define value (trie-node-value trie))
  `((children . ,(hash-table-fold children
                                  (lambda (k v acc)
                                    (cons `(,k . ,(trie->alist v))
                                          acc))
                                  '()))
    (terminal? . ,terminal?)
    (value . ,value)))

(define (alist->trie alist)
  (unless (not (null? alist))
    (error "alist->trie: invalid alist-encoded trie: " alist))
  (define children (assoc 'children alist))
  (define terminal? (assoc 'terminal? alist))
  (define value (assoc 'value alist))

  (when (or (not children)
            (not terminal?)
            (not value))
    (error "alist->trie: missing information to build trie: " alist))

  (make-trie-node (alist->hash-table
                   (fold (lambda (child-pair acc)
                           (let ((char (car child-pair))
                                 (node (cdr child-pair)))
                             (cons (cons char (alist->trie node))
                                   acc)))
                         '()
                         (cdr children)))
                  (cdr terminal?)
                  (cdr value)))
