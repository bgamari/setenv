#include "HsBase.h"

#ifdef HAVE_UNSETENV
#include <stdlib.h>

int __hsbase_unsetenv(const char *name) {
#ifdef UNSETENV_RETURNS_VOID
    unsetenv(name);
    return 0;
#else
    return unsetenv(name);
#endif
}
#endif
