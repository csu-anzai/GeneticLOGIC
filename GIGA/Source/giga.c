#include "giga.h"

/*

	**********************  GIGA   **************************
	*		Main Control Routine			*
	*********************************************************

* Copyright
* Author: Joseph Culberson
* Date: April 24, 1992
* Permission is hereby granted to copy all or any part of
* this program for free distribution.  The author's name
* and this copyright notice must be included in any copy.
* This program is provided ``as is'' without any express or implied
* warranties.

* Comments and suggestions may be emailed to joe@cs.ualberta.ca
* but this implies no obligation on the part of the author.


* Users are encouraged to modify and extend this program,
* and such modifications should be commented with appropriate attribution.
* This program should not be considered a final product, but rather it is hoped
* that it will serve as a stimulus for further experimentation and development.

* Initial Version Created April 12, 1992.


*/

int main()
/*
	The main routine for GIGA
*/
{
	int i,idx,idy,exper;
	
	/* system initializations */
	optioninit();
	summaryinit();


	for(exper=1;exper<=OPTIONS.numexperiments;exper++){
		printf("\nExperiment Number %d\n\n",exper);

		resultinit();
		initpop();

		printf("Matings   \tEvaluations\tMinimum  \tMaximum\n");
		if (RESULTS.new_results == yes) {
			printf("%10d\t%10d\t%f\t%f\n",0,RESULTS.eval_count,
				RESULTS.min, RESULTS.max);
			summaryfirst(exper);
			RESULTS.new_results = no;
		}

		summaryupdate(0);
	
	
		for(i=1;i<=OPTIONS.maxiterations; i++) {
			p_select(&idx,&idy);
			if ( 0 == mate(idx,idy)) {
				if (OPTIONS.sortflag == yes) {
					sortadjust(idx,idy);
				}
			}
	
			if (RESULTS.new_results == yes) {
				printf("%10d\t%10d\t%f\t%f\n",i,
					RESULTS.eval_count,
					RESULTS.min, RESULTS.max);
				summaryfirst(exper);
				RESULTS.new_results = no;
			}
	
			if (OPTIONS.intermediate == yes)
				if (0 == (i % OPTIONS.frequency)) {
					printf("Matings    = %d ",i);
					printpop();
				}
			
			if (0 == (i % OPTIONS.collect_freq) )
				summaryupdate(i);
	
		}
		printf("%10d\t%10d\t%f\t%f (Final Results)\n",
			OPTIONS.maxiterations, RESULTS.eval_count, 
			RESULTS.min, RESULTS.max);
	
		if (OPTIONS.final == yes) {
			printf("Final Matings    = %d ",OPTIONS.maxiterations);
			printpop();
		}
		
		freepop();
	}

	/* Summary output */
	if (OPTIONS.numexperiments > 1)
		printsummary();
}
