AC_INIT([tetris], [0.01], erjoalgo@gmail.com)

: ${CFLAGS="-Ofast -DNDEBUG"}

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_FILES([Makefile src/Makefile data/Makefile])

AC_CHECK_LIB(ncurses, initscr)
dnl AM_CONDITIONAL([HAVE_LIBNCURSES], [test -n "$HAVE_LIBNCURSES"])
dnl AM_CONDITIONAL([HAVE_LIBNCURSES], [test "$HAVE_LIBNCURSES" -eq 1])
dnl AX_WITH_CURSES
AM_CONDITIONAL([USE_LIBNCURSES], [test "$ac_cv_lib_ncurses_initscr" = yes])

AM_INIT_AUTOMAKE([-W -Wall])

AC_CHECK_FUNCS([memmove])
AC_CHECK_FUNCS([memset])
AC_CHECK_HEADERS([float.h])
AC_CHECK_HEADERS([stdlib.h])
AC_CHECK_HEADERS([string.h])
AC_CHECK_HEADERS([unistd.h])
AC_C_INLINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC

AC_PROG_CC

AM_PROG_AR
LT_INIT
AC_CONFIG_MACRO_DIRS([m4])

AM_INIT_AUTOMAKE
AC_OUTPUT
