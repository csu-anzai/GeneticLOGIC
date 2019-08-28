/* problem.h contains standard interface to a problem                       */

/* evalChrom(chrom) returns fitness of the chromosome                        */
extern double evalChrom(void *);

/* initChrom(chrom) is problem-specific but only for the given representation*/
/* fp is the input file pointer */
extern void initChrom(genericChromType *chrom,FILE *fp);

