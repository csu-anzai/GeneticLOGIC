/* representation.h includes standard interface to representation            */

/* setRepSpecifics() is used to set up any other information specific        */
/* to a representation                                                       */
extern void setRepSpecifics(FILE *input);

/* makeChrom() allocates storage for a chrom and initializes it by calling   */
/*   problem-specific function initChrom()                                   */
extern genericChromType *makeChrom(FILE *fp);

/* dspChrom() is provided if desired to display chromosomes */
extern void dspChrom(FILE *fp, void *c);
