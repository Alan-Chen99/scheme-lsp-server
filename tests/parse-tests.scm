(import (scheme base)
        (srfi 1)
        (srfi 64)

        (lsp-server parse))
(cond-expand
 (guile (import (lsp-server guile)))
 (chicken (import (lsp-server chicken))))

(include "../src/parse.scm")

(test-begin "LSP server parse tests")

(test-assert (tagged-expression? '(import ...) 'import))

(cond-expand
 (guile (test-assert (cond-expand-clause-satisfied? '(guile #t))))
 (chicken (test-assert (cond-expand-clause-satisfied? '(chicken #t)))))

(cond-expand
 (guile (test-assert (cond-expand-clause-satisfied?
                      '((or gambit guile) #t))))
 (chicken (test-assert (cond-expand-clause-satisfied?
                        '((or chibi chicken) #t)))))

(cond-expand
 (guile (test-assert (cond-expand-clause-satisfied?
                      '((library (srfi srfi-64)) #t))))
 (chicken (test-assert (cond-expand-clause-satisfied?
                        '((library (srfi 64)) #t)))))

(test-equal (cond-expand (guile '(begin (import (lsp-server guile))))
                         (chicken '(begin (import (lsp-server chicken))))
                         (else))
            (cond-expand-find-satisfied-clause
             '(cond-expand (guile (import (lsp-server guile)))
                           (chicken (import (lsp-server chicken)))
                           (else))))

(test-equal (cond-expand ((or gambit guile) '(begin "gambit or guile"))
                         ((or chibi chicken) '(begin "chicken or chibi"))
                         (else))
            (cond-expand-find-satisfied-clause
             '(cond-expand ((or gambit guile) "gambit or guile")
                           ((or chicken chibi) "chibi or chicken")
                           (else))))

(test-end)
