(define-library (lsp-server chicken util)

(export get-module-path
        pathname-directory
        pathname-join)

(import (scheme base)
        (chicken pathname))

(begin
  (define (get-module-path module-name)
    #f)
  (define pathname-join make-pathname))

)
