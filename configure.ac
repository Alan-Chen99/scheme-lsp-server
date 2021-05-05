AC_INIT([guile-lsp-server], [0.1.0])
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
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

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
