#! /usr/local/bin/csi -ss

(import (lsp-server chicken)
        (chicken tcp))

(parameterize ((tcp-read-timeout #f)
               (tcp-write-timeout #f)
               (tcp-accept-timeout #f))
  (include "../src/lsp-connect.scm")

  (main (cdr (command-line))))


