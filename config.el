;; (put 'Expr 'scheme-indent-function 1)
;; (put 'Pointer 'scheme-indent-function 1)
;; (put 'TypeSpecList 'scheme-indent-function 1)

(let ((lst '(test-group
             test
             guard
             lambda*
             call-with-input-string
             with-db-connection
             with-input-from-file
             with-input-from-request
             with-input-from-string
             with-current-output-port)))
  (dolist (tag lst)
    (put tag 'scheme-indent-function 1)))

(let ((lst '(loop)))
  (dolist (tag lst)
    (put tag 'lisp-indent-function 1)))
