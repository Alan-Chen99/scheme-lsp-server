AC_INIT([guile-lsp-server], [0.2.2])
AC_CONFIG_SRCDIR([lsp-server.sld])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PKG([3.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-3.x installation.])
fi

AC_CONFIG_FILES([Makefile])

AC_CONFIG_LINKS([lsp-server/private/util-impl.scm:../lsp-server/private/util-impl.scm
                 lsp-server/private/document-impl.scm:../lsp-server/private/document-impl.scm
                 lsp-server/private/file-impl.scm:../lsp-server/private/file-impl.scm
                 lsp-server/main.scm:../main.scm
                 lsp-server-impl.scm:../lsp-server-impl.scm
                 lsp-server/private/trie-impl.scm:../lsp-server/private/trie-impl.scm
                 lsp-server/private/adapter-impl.scm:../lsp-server/private/adapter-impl.scm
                 lsp-server/private/parse-impl.scm:../lsp-server/private/parse-impl.scm
                 lsp-server/private/compat-guile-impl.scm:../lsp-server/private/compat-guile-impl.scm
                 lsp-server/private/guile-impl.scm:../lsp-server/private/guile-impl.scm

                 lsp-server/private/adapter.sld:lsp-server/private/adapter.sld
                 lsp-server/private/document.sld:lsp-server/private/document.sld
                 lsp-server/private/util.sld:lsp-server/private/util.sld
                 lsp-server/private/compat.sld:lsp-server/private/compat.sld
                 lsp-server/private/guile.sld:lsp-server/private/guile.sld
                 lsp-server/private/trie.sld:lsp-server/private/trie.sld
                 lsp-server/private/parse.sld:lsp-server/private/parse.sld
                 lsp-server.sld:lsp-server.sld

                 geiser/modules.scm:../geiser/guile/src/geiser/modules.scm
                 geiser/utils.scm:../geiser/guile/src/geiser/utils.scm
                 geiser/doc.scm:../geiser/guile/src/geiser/doc.scm
                 geiser/completion.scm:../geiser/guile/src/geiser/completion.scm
                 geiser/evaluation.scm:../geiser/guile/src/geiser/evaluation.scm
                 geiser/xref.scm:../geiser/guile/src/geiser/xref.scm

                 README.md:../README.md])

GUILE_MODULE_REQUIRED(json-rpc)

AC_OUTPUT

echo
echo "*** $PACKAGE $VERSION has been successfully configured ***"
echo
echo "$PACKAGE is using:"
echo
echo "    --prefix=$prefix --libdir=$LIBDIR"
echo
echo "If you want to install in Guile system's directory re-run with:"
echo
echo "    --prefix=$GUILE_PREFIX --libdir=$GUILE_LIBDIR"
echo
