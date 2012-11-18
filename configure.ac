AC_INIT([setenv], [0])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsBase.h])

AC_CONFIG_HEADERS([include/HsSetEnvConfig.h])

# unsetenv
AC_CHECK_FUNCS([unsetenv])

###  POSIX.1003.1 unsetenv returns 0 or -1 (EINVAL), but older implementations
###  in common use return void.
AC_CACHE_CHECK([return type of unsetenv], fptools_cv_func_unsetenv_return_type,
  [AC_EGREP_HEADER(changequote(<, >)<void[      ]+unsetenv>changequote([, ]),
                   stdlib.h,
                   [fptools_cv_func_unsetenv_return_type=void],
                   [fptools_cv_func_unsetenv_return_type=int])])
case "$fptools_cv_func_unsetenv_return_type" in
  "void" )
    AC_DEFINE([UNSETENV_RETURNS_VOID], [1], [Define if stdlib.h declares unsetenv to return void.])
  ;;
esac

AC_OUTPUT
