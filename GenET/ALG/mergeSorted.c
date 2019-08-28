/* mergeSort.c contains mergeSort for two arrays */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mergeSorted.h"

/* mergeSorted(A,A1,A2,S1,S2,elemSize,compr) merges sorted A1 and A2, of */
/*   sizes S1 and S2, into A.  Element size is elemSize */
/* NOTE: Assume A has proper size and A1, A2 are sorted */
/* NOTE: sorts in order according to compr(): if compr(A,B)<0 then A before B */
/* NOTE: put *A1 before *A2 if they == */
void mergeSorted(void *A, void *A1, void *A2, size_t S1, size_t S2, 
               size_t elemSize, int (*compr)(const void*, const void*))
{ size_t e=(size_t)elemSize;
  char *a1, *a2, *AA=(char*)A, *AA1=(char*)A1, *AA2=(char*)A2; 

  a1=AA1+S1*elemSize;
  a2=AA2+S2*elemSize;             /* a1 and a2 are end_addresses of A1 and A2*/
  if (S1<0 || S2<0)
  { fprintf(stderr,"\tmergeSort called with negative array size%c\n",7);
    exit(1);
  } 
  while (a1>AA1 || a2>AA2)
  { if (a1==AA1)                                           /* A1 all used up */
    { memcpy((void*)AA,(void*)AA2,(size_t)(a2-AA2));
      return;
    }
    if (a2==AA2)                                           /* A2 all used up */
    { memcpy((void*)AA,(void*)AA1,(size_t)(a1-AA1));
      return;
    }
  
    if (compr((void*)AA1,(void*)AA2)<=0)                     /* A1 before A2 */
    { memcpy((void*)AA,(void*)AA1,e);
      AA+=elemSize;
      AA1+=elemSize;
    }
    else                                                     /* A2 before A1 */
    { memcpy((void*)AA,(void*)AA2,e);
      AA+=elemSize;
      AA2+=elemSize;
    }
  }
}
