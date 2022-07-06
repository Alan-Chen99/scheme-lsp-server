(define-module (lsp-server trie)

#:export (make-trie trie? trie-insert! trie-keys trie-words-with-prefix)

#:use-module (scheme base)
#:use-module (srfi srfi-69))

(include "trie.scm")
