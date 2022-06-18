(import (lsp-server))

(cond-expand
 (guile (import (except (scheme base)
                        assoc
                        cond-expand
                        member)))
 (else (import (scheme base)
               (scheme char) ;; string-downcase
               (scheme process-context))))

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
                          " [log-level] [--version]."))
  (newline)
  (display (string-append "Example: " program-name " debug"))
  (newline)
  (display "Arguments: ")
  (newline)
  (display "    log-level: one of [error, warning, info, debug]. Default: error.")
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

(define (process-args args)
  (define len (length args))
  (cond ((or (member "-v" args)
             (member "--version" args))
         (print-version)
         (exit))
        ((or (member "-h" args)
             (member "--help" args))
         (print-usage)
         (exit))
        ((or (> len 1)
             (and (= len 1)
                  (not (valid-log-level? (car args)))))
         (print-usage)
         (exit))
        ((= len 1)
         `((log-level . ,(string->symbol (car args)))))
        (else '((log-level . error)))))

(define (main args)
  (define options (process-args args))
  (define log-level (cdr (assoc 'log-level options)))
  (parameterize ((lsp-server-log-level log-level))
    (lsp-server-start/stdio)))

(main (cdr (command-line)))
