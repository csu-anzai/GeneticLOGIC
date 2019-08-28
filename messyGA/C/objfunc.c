/*=================================================
       file : objfunc.c 
  
    purpose : calculate objective function value
 
  developed : 1991

     author : Kalyanmoy Deb
====================================================*/

#include "mga.ext"

double objfunc(rawchr)
unsigned *rawchr;
/*  evaluate the objective function value of chr  */
{
	register int j, k;
	double obj;
	int powerof2, sum, len;
	ALLELES subfunc_chrom[MAX_SIZE];
	FILE *fp, *fopen();

	/* for each subfunction  */
	for (k = 0,obj = 0.0; k < numsubfunc; k++)
	{
		len = str_length[table_id[k]];
		if (taborfunc[table_id[k]] == 1) {
			/* if a table */
			powerof2=power(2,len-1);
			/*   calculate objective function value  */
			for (j = 0, sum = 0; j < len; j++) {
				sum += powerof2 * pickallele(genesets[k][j], \
								rawchr);
				powerof2 /= 2;
			}
			obj += scale[k] * chromfitness[table_id[k]][sum];
		}
		else  /* if a function */
		{  
			for (j = 0; j < len; j++) /* get the binary string */
				subfunc_chrom[j] = pickallele(genesets[k][j], \
								rawchr);

             		/* use a function defined in functions.c file */
			obj += scale[k] * get_func_val(table_id[k], \
							subfunc_chrom,len);
		}
	}
	function_evaluations += 1.0; 
	return(obj);
}
