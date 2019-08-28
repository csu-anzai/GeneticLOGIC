/* mergeSort.h contains mergeSort for two arrays */

/* mergeSorted(A,A1,A2,S1,S2,compr) merges sorted A1 and A2, of sizes S1 and */
/*   S2, into A. Assume A has proper size. Element size is elemSize */
/* NOTE: sorts in order according to compr(): if compr(A,B)<0 then A before B */
void mergeSorted(void *A, void *A1, void *A2, size_t S1, size_t S2, 
               size_t elemSize, int (*compr)(const void*, const void*));
