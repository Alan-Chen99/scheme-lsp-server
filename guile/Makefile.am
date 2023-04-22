include guile.am

DEPENDENCIES =    \
  geiser/utils.scm          \
  geiser/modules.scm        \
  geiser/completion.scm     \
  geiser/doc.scm            \
  geiser/evaluation.scm     \
  geiser/xref.scm

LIBRARY_DEFINITIONS = \
  lsp-server/private/util.sld  \
  lsp-server/private/guile.sld \
  lsp-server/private/trie.sld       \
  lsp-server/private/compat.sld \
  lsp-server/private/document.sld   \
  lsp-server/private/parse.sld      \
  lsp-server/private/adapter.sld    \
  lsp-server/private/file.sld  \
  lsp-server.sld

SOURCES = \
  lsp-server-impl.scm    \
  lsp-server/private/util-impl.scm    \
  lsp-server/private/guile-impl.scm \
  lsp-server/private/trie-impl.scm       \
  lsp-server/private/compat-guile-impl.scm \
  lsp-server/private/document-impl.scm   \
  lsp-server/private/parse-impl.scm      \
  lsp-server/private/adapter-impl.scm    \
  lsp-server/private/file-impl.scm    \
  lsp-server.scm

EXTRA_DIST =            \
  README.md             \
  bootstrap             \
  configure
