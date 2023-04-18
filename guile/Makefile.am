include guile.am

DEPENDENCIES =    \
  geiser/utils.scm          \
  geiser/modules.scm        \
  geiser/completion.scm     \
  geiser/doc.scm            \
  geiser/evaluation.scm     \
  geiser/xref.scm

LIBRARY_DEFINITIONS = \
  lsp-server/private/guile.sld \
  lsp-server/private.sld    \
  lsp-server/private/compat.sld \
  lsp-server/document.sld   \
  lsp-server/trie.sld       \
  lsp-server/parse.sld      \
  lsp-server/adapter.sld    \
  lsp-server.sld

SOURCES = \
  lsp-server/private-impl.scm    \
  lsp-server/private/compat-guile-impl.scm \
  lsp-server/private/guile-impl.scm \
  lsp-server/document-impl.scm   \
  lsp-server/trie-impl.scm       \
  lsp-server/parse-impl.scm      \
  lsp-server/adapter-impl.scm    \
  lsp-server.scm

EXTRA_DIST =            \
  README.md             \
  bootstrap             \
  configure
