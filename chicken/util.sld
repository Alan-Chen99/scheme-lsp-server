(define-library (lsp-server chicken util)

(export absolute-pathname?
        current-directory
        get-module-path
        get-absolute-pathname
        pathname-directory
        pathname-join)

(import (scheme base)
        (chicken pathname)
        (chicken process-context))

(begin
  (define (get-module-path module-name)
    #f)
  (define pathname-join make-pathname)

  (define (get-absolute-pathname path)
    (if (absolute-pathname? path)
        path
        (pathname-join (current-directory) path))))
)
