(import (lsp-server))

(cond-expand
 (guile (import (except (scheme base)
                        assoc
                        cond-expand
                        member)
                (srfi srfi-18)
                (srfi srfi-28)
                (lsp-server guile)))
 (chicken (import (scheme base)
                  (scheme char) ;; string-downcase
                  (scheme process-context)
                  (srfi 18)
                  (srfi 28)
                  (lsp-server chicken))))

(cond-expand (guile (import (only (srfi srfi-1) any)))
             (else (import (only (srfi 1) any))))

(define implementation-name
  (cond-expand
   (chicken "CHICKEN")
   (guile "Guile")
   (else (error "Your Scheme implementation is not supported (yet)."))))

(define (print-usage)
  (define program-name
    (string-append (string-downcase implementation-name)
                   "-lsp-server"))

  (display (string-append "usage: "
                          program-name
                          " [--log-level <log-level>] [--listen <port-num>] [--version]."))
  (newline)
  (display (string-append "Example: " program-name " --log-level debug"))
  (newline)
  (display "Arguments: ")
  (newline)
  (display "    --log-level <level>: one of [error, warning, info, debug]. Default: error.")
  (newline)
  (display "    --listen <port-num>: port to listen to and spawn repl upon connection.")
  (newline)
  (display "    --help (or -h):    display this help and exit.")
  (newline)
  (display "    --version (or -v): display version information and exit.")
  (newline))

(define (print-version)
  (display (string-append implementation-name " LSP Server"))
  (newline)
  (display (string-append "Version " lsp-server-version))
  (newline)
  (display "Copyright (C) 2022 Ricardo Gabriel Herdt")
  (newline))

(define (valid-log-level? str)
  (any (lambda (log-level)
         (string=? log-level str))
       '("error" "warning" "info" "debug")))


(define (process-args arguments)
  (let loop ((args arguments)
             (options '()))
    (cond ((null? args)
           options)
          ((or (member "-v" args)
               (member "--version" args))
           (print-version)
           (exit))
          ((or (member "-h" args)
               (member "--help" args))
           (print-usage)
           (exit))
          ((and (>= (length args) 2)
                (string=? (car args) "--log-level")
                (valid-log-level? (cadr args)))
           (loop (cddr args)
                 (cons `(log-level . ,(string->symbol (cadr args)))
                       options)))
          ((and (>= (length args) 2)
                (string=? (car args) "--listen")
                (string->number (cadr args)))
           => (lambda (port-num)
                (loop (cddr args)
                      (cons `(listen . ,port-num) options))))
          (else (print-usage)
                (exit)))))

(define (main args)
  (define options (process-args args))
  (display "options: ")
  (display options)
  (newline)
  (define log-level (let ((log-level-option (assoc 'log-level options)))
                      (if log-level-option
                          (cdr log-level-option)
                          'error)))
  (define listen-port-num
    (let ((listen-option (assoc 'listen options)))
      (if listen-option
          (cdr listen-option)
          #f)))

  (if listen-port-num
      (thread-start!
       (make-thread
        (lambda () (spawn-repl-server listen-port-num))))
      #f)

  (parameterize ((lsp-server-log-level log-level))
    (lsp-server-start/stdio)))

(main (cdr (command-line)))
