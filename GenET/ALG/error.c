/* error.c */

#include <stdio.h>
#include <stdlib.h>
#include "error.h"

#define BELL 7

void fail(char *message,char *file, int line)
{ fprintf(stderr,"\n\n\tEXECUTION ERROR: %s\n\t\tFILE %s @LINE %d%c%c%c\n",
          message,file,line,BELL,BELL,BELL);
  exit(1);
}
