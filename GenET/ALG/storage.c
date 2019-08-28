/* storage.c contains functions for standard storage allocation */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <malloc.h>
#include "main.h"
#include "error.h"
#include "storage.h"

boolean *makeBooleanVector(int length, boolean initial)
{ int i;
  boolean *b;                        /* boolean assumed to be a defined type */
  if (length==0) return(NULL);
  b=(boolean*)calloc((size_t)length,sizeof(boolean));
  if (b==NULL) FAIL("Memory allocation failed");
  for (i=0; i<length; i++)
    b[i]=initial;
  return(b);
}

int *makeIntVector(int length, int initial)
{ int i;
  int *b;
  if (length==0) return(NULL);
  b=(int*)calloc((size_t)length,sizeof(int));
  if (b==NULL) FAIL("Memory allocation failed");
  for (i=0; i<length; i++)
    b[i]=initial;
  return(b);
}

double *makeDoubleVector(int length, double initial)
{ int i; 
  double *b; 
  if (length==0) return(NULL);
  b=(double*)calloc((size_t)length,sizeof(double)); 
  if (b==NULL) FAIL("Memory allocation failed");
  for (i=0; i<length; i++) 
    b[i]=initial; 
  return(b); 
}

void *getStorage(int length, size_t bytes)
{ void *p;
  if (length==0) return(NULL);
  p=calloc((size_t)length,bytes);
  if (p==NULL)
    FAIL("Memory allocation failed");
  return(p);
}
