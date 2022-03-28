/* Create a C function to test .C intervace */

#include <stdio.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h>

void check_rtf_brackets(const char **filename) {

  FILE *infile;

  if ((infile = fopen(filename[0], "r")) == NULL) {
    Rprintf("Cannot open '%s' to check brackets\n", filename[0]);
  } else {
    Rprintf("Able to open '%s' to check brackets\n", filename[0]);
  }

}

/* Guided by https://svn.r-project.org/R/trunk/src/library/splines/src/splines.c  
 * and https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf
 */

static const R_CMethodDef cMethods[] = {
  {"check_rtf_brackets", (DL_FUNC) &check_rtf_brackets, 1},
  {NULL, NULL, 0}
};


void R_init_testdotC(DllInfo *dll) {
    R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
