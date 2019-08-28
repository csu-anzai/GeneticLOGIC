#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function Liepin's Deception  *
	*********************************
*/

/*
* Author: Joseph Culberson
* Date: April 24, 1992

* Adapted from Eshelman FOGA 91


*/

double fdl(str)
/*
	Liepin's fully deceptive function
*/
unsigned char str[];
{
	int i,j, onecount, decpt_order;
	double sum;

	if (2 != OPTIONS.alphasize) {
		printf("Alphabet must be of size 2 for Liepin's deception\n");
		exit(1);
	}

	/* order of deception */
	decpt_order = 5;

	if (OPTIONS.parameters[0] < MAXDOUBLE) 
		decpt_order =(int) OPTIONS.parameters[0];

	if (0 != (OPTIONS.stringsize % decpt_order)) {
		printf("Stringsize must be multiple of decpt_order in Liepin's ");
		printf("deception\n");
		exit(1);
	}

	sum = (double) 0.0;

	for(i=0; i<OPTIONS.stringsize; i+= decpt_order){
		onecount = 0;
		for(j=0;j<decpt_order;j++) 
			if (str[i+j] == 1) onecount++;
		
		if (onecount == 0) sum +=
		  (double) 1.0 - ((double) 1.0 / ((double) decpt_order * 2.0));
		else if (onecount == decpt_order) sum += (double) 1.0;
		else sum +=
		  (double) 1.0 - 
		    (((double) 1.0 + (double) onecount) / (double) decpt_order);
	}

	return(sum);
}
