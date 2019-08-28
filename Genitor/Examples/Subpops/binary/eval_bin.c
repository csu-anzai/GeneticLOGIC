#include <stdio.h>
#include <math.h>
#include "gene.h"


/*
  This is a psuedo eval function for test purposes only.
  A string is fully optimized when all its elements are zeros
*/

float
bin_eval (string, len)
GENE_DATA string[];
int   len;
{
 int i;
 float val=0.0;

 for (i=0; i<len; i++)
	  if (string[i] =='1')
		 val = val + 1.0; 

 return (val);
}

