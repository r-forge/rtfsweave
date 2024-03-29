/* C function to be called from within R using the .C() interface. The
 * function reads in an RTF file character by character and checks that
 * the file has balanced curly brackets. (This function passes over
 * escaped curl brackets ('\{' and '\}') and so doesn't count them.)
*/

#include <stdio.h>
#include <string.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h>

#define MAXDEPTH 1000 /* How deeply nested we can go */

/* Arguments `filename` is the filename to check and
 * `status_code` is an early attempt at giving the user some status
 * information back. */

void check_rtf_brackets(const char **filename,
			int *status_code)
{

  FILE *infile;

  const char rtfstart[] = "{\\rtf1"; /* How an RTF file must start */
  
  int i;                /* Character counter and general index variable */
  int current;          /* Current character */
  int prior = '\0';     /* Prior character. Need to initialize early  */
  int rtf_complete = 0; /* Flag for whether RTF file completed */
  
  int cb = 0;           /* Curly bracket counter */
  int line = 1;         /* Line counter 1, 2, ... */
  int column = 1;       /* Column counter 1, 2, ... */
   
  int unclosed_line[MAXDEPTH]={0};   /* Array that tracks line of unclosed '{' */
  int unclosed_column[MAXDEPTH]={0}; /* Array that tracks column of unclosed '{' */

  /* Rprintf("This is the status_code[0] value: %d\n", *status_code);
     status_code[0] = column;
     Rprintf("This is the new status_code[0] value: %d\n", *status_code); */

  if ((infile = fopen(filename[0], "r")) == NULL) {
    Rprintf("Cannot open '%s' to check curly brackets\n", filename[0]);
    status_code[0] = 1;
    return;
  }

  /* Check that 'infile' starts with '{\rtf1' */

  for (i = 0; i < strlen(rtfstart); ++i) {
    current = getc(infile);
    if (current != rtfstart[i]) {
      Rprintf("First %d characters of RTF file must begin with '%s'. Ending early.\n",
              (int)strlen(rtfstart), /* strlen is type 'size_t', which is */
              rtfstart);             /* a long long unsigned int with GCC */
      status_code[0] = 2;            /* 11.2.0. But %llu not supported so */
      return;                        /* cast to an integer.               */
    }
  }
  /* To get this far, we have one opening cb */
  cb = 1;
  line = 1;
  column = strlen(rtfstart);
  unclosed_line[0] = 1;
  unclosed_column[0] = 1;
  
  /* Now read the rest of the file character by character */
  while ((current = getc(infile)) != EOF) {
    ++column;
    if (current == '\n') {
      ++line;
      column=0;
    } else if ((current == '{') & (prior != '\\')) {
      ++cb;
      if (cb >= MAXDEPTH) {
	Rprintf("Sorry. The maximum depth of %d curly brackets reached.\n",
		MAXDEPTH);
	status_code[0] = 3;
	return;
      }
      /* Record where this opening bracket is in case it's not later closed*/
      unclosed_line[cb - 1] = line;
      unclosed_column[cb - 1] = column;
    } else if ((current == '}') & (prior != '\\')) {
      --cb;
      /* If ever go negative it's an error */
      if (cb < 0) {
	Rprintf("Line %d, column %d: '}' without prior '{'.\nStopping processing.\n",
		line,
		column);
	status_code[0] = 4;
	return;
      } else if ((cb == 0) & (rtf_complete == 0)) { /* The RTF file is "complete" */
	rtf_complete = 1;
	Rprintf("RTF file is complete with closing '}' at line %d, column %d.\n",
		line,
		column);
	status_code[0] = 0;
      }
    }
    prior = current;
  }
  
  /* If we have unclosed brackets then print the locations */
  if (cb > 0) {
    for (i=0; i<= cb - 1; ++i) {
      if ((unclosed_line[i] == 1) & (unclosed_column[i] == 1)) {
	Rprintf("Opening '{' of RTF file at line 1, column 1 was never closed.\n");
	status_code[0] = 5;
      } else {
	Rprintf("Opening '{' of RTF file at line %d, column %d was never closed.\n",
		unclosed_line[i],
		unclosed_column[i]);
	status_code[0] = 6;
      }
    }
  }

  Rprintf("RTF file has %d lines in total.\n", line); 
  return;

}


/* What's below is guided by these references:
 * 
 * - http://cran.wustl.edu/doc/manuals/r-release/R-exts.html (Sec. 5.2)
 * - https://svn.r-project.org/R/trunk/src/library/splines/src/splines.c  
 * - https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf 
 * 
 */

static const R_CMethodDef cMethods[] = {
  {"check_rtf_brackets", (DL_FUNC) &check_rtf_brackets, 2},
  {NULL, NULL, 0}
};


void R_init_testdotC(DllInfo *dll) {
    R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
