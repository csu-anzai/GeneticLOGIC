/* io.h contain robust io routines */

#include <stdio.h>

/* getIntLtd() behaves differently on stdin and file */
/*   read from stdin until in the allowed range */
/*     no additional non-WS allowed before or after; non-interactive */
/*     (\n needed and is removed); return status code: */
/*   read from non-stdin open file in the range only (fail otherwise) */
/*     no additional non-WS allowed before; non-interactive */
/*   returned status codes */
/*   0 = successfully initialized result */
/*   1 = failed on eof */
/*   2 = failed on low/high range */
/*   3 = other fail */
extern int getIntLtd(FILE *fp, const char *prompt, int low, int high, 
                     int *result);

/* getDoubleLtd() behaves differently on stdin and file */
/*   read from stdin until in the allowed range */
/*     no additional non-WS allowed before or after; non-interactive */
/*     (\n needed and is removed); return status code: */
/*   read from non-stdin open file in the range only (fail otherwise) */
/*     no additional non-WS allowed before; non-interactive */
/*   returned status codes */
/*   0 = successfully initialized result */
/*   1 = failed on eof */
/*   2 = failed on low/high range */
/*   3 = other fail */
extern int getDoubleLtd(FILE *fp, const char *prompt, double low, double high, 
                 double *result);

/* getLUnsLtd() behaves differently on stdin and file */
/*   read from stdin until in the allowed range */
/*     no additional non-WS allowed before or after; non-interactive */
/*     (\n needed and is removed); return status code: */
/*   read from non-stdin open file in the range only (fail otherwise) */
/*     no additional non-WS allowed before; non-interactive */
/*   returned status codes */
/*   0 = successfully initialized result */
/*   1 = failed on eof */
/*   2 = failed on low/high range */
/*   3 = other fail */
extern int getLUnsLtd(FILE *fp, const char *prompt, long unsigned low, 
               long unsigned high, long unsigned *result);

/* getUnsLtd() behaves differently on stdin and file */
/*   read from stdin until in the allowed range */
/*     no additional non-WS allowed before or after; non-interactive */
/*     (\n needed and is removed); return status code: */
/*   read from non-stdin open file in the range only (fail otherwise) */
/*     no additional non-WS allowed before; non-interactive */
/*   returned status codes */
/*   0 = successfully initialized result */
/*   1 = failed on eof */
/*   2 = failed on low/high range */
/*   3 = other fail */
extern int getUnsLtd(FILE *fp, const char *prompt, unsigned low, 
              unsigned high, unsigned *result);

/* getStrLtd reads string into dest, behaves differently on stdin and */
/*   other file; dest must be of at least max+1 size, min >=1; */
/*   strings with more than max chars are truncated and removed from input */
/*   read a string from stdin */
/*     read only [min..max] characters (read until >=min) */
/*     consider only fisrt string per line */
/*     remove all remaining characters until and including \n */
/*     return read status: */
/*       0 = dest initialized with min..max characters, possibly chars lost */
/*       1 = dest not initialized, eof */
/*       2 = dest not initialized, improper range */
/*   read a string from non-stdin open file */
/*     read only [min..max] characters (fail if <min) */
/*     if string is longer than max, remove all remaining characters until WS */
/*     return read status: */
/*       0 = dest initialized with min..max characters, possibly chars lost */
/*       1 = dest not initialized, eof */
/*       2 = dest not initialized, improper range */
/*       3 = dest not initialized, other reading errors */
/*       4 = dest not initialized, too short */
extern int getStrLtd(FILE *fp, const char *prompt, char *dest, int min, 
                     int max);

/* read line from open file fp and put it into destination */
/* read only [1..max] characters (destination must be at least max+1 size */
/* remove all remaining characters until and including \n if input too long */
/* \n is not included in dest */
/* return read status: */
/*   0 = dest initialized with min..max characters, possibly characters lost */
/*   1 = dest not initialized, eof */
/*   2 = dest not initialized, improper range */
extern int getLineLtd(FILE *fp, const char *prompt, char *dest, int max);
