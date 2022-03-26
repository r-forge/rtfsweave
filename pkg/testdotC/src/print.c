/* Create a C function to test .C intervace */

#include <stdio.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h>

static void
print123(void) {
  Rprintf("Hello from .C!\n");
}

/* Guided by https://svn.r-project.org/R/trunk/src/library/splines/src/splines.c  
 * and https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf
 */
#include <R_ext/Rdynload.h>

static const
R_CMethodDef cMethods[] = {
  {"print123", (DL_FUNC) &print123, 0},
  {NULL, NULL, 0}
};


void R_init_testdotC(DllInfo *dll) {
    R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
