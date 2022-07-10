(import (scheme base))

(cond-expand
 (guile (import (only (srfi srfi-1) every fold)
                (srfi srfi-64)
                (srfi srfi-69)
                (lsp-server guile util)))
 (else (import (only (srfi srfi-1) every fold)
               (srfi 64)
               (srfi 69))))

(include "../src/trie.scm")

(test-begin "trie tests")

(let ((trie (make-trie)))
  (trie-insert! trie "abcd" #f)
  (trie-insert! trie "bla" #f)
  (trie-insert! trie "abc" #f)
  (test-equal '("abc" "abcd" "bla") (trie-keys trie))
  (test-equal '("abc" "abcd") (trie-words-with-prefix trie "ab"))
  (test-equal '() (trie-words-with-prefix trie "ga")))

(test-end "trie tests")
