include guile.am

DEPENDENCIES =    \
  geiser/utils.scm          \
  geiser/modules.scm        \
  geiser/completion.scm     \
  geiser/doc.scm            \
  geiser/evaluation.scm     \
  geiser/xref.scm

LIBRARY_DEFINITIONS = \
  lsp-server/private.sld    \
  lsp-server/document.sld   \
  lsp-server/trie.sld       \
  lsp-server/parse.sld      \
  lsp-server/adapter.sld    \
  lsp-server.sld

GUILE_SPECIFIC_SOURCES = \
  lsp-server/guile/util.scm \
  lsp-server/guile.scm


EXTRA_DIST =            \
  README.md             \
  bootstrap             \
  configure
