#include "giga.h"

/*

        **********************  SUMMARY   ***********************
        *             Experiment Summary Routines               *
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

* Initial Version Created April 22, 1992.


*/

/* Summary table of data */
static	int
		recordcnt,  		/* which record to update   */
		maxrecord,		/* maximum records kept     */
		firstmin[MAXEXPER],	/* first occurence of min   */
		firstmax[MAXEXPER],	/* first occurence of max   */
		iterations[MAXCOLLECT]; /* number of iterations     */
static	double
		minvalue[MAXCOLLECT],	/* sum of min values	    */
		ssqmin[MAXCOLLECT],	/* sum of min values sqrd   */
		maxvalue[MAXCOLLECT],	/* sum of max values	    */
		ssqmax[MAXCOLLECT],	/* sum of max values sqrd   */
		firstminval[MAXEXPER],  /* value of smallest in expr*/
		firstmaxval[MAXEXPER];  /* value of largest in expr */


void summaryinit()
/*
	Initialize summary table
*/
{
	int i;

	maxrecord = recordcnt = 0;

	for(i=0;i<MAXCOLLECT;i++) {
		iterations[i]=0;
		minvalue[i] = maxvalue[i] = ssqmin[i] = ssqmax[i] = (double) 0.0;
	}

	for(i=0;i<MAXEXPER;i++) {
		firstmin[i] = firstmax[i] =0;
		firstminval[i] = MAXDOUBLE;
		firstmaxval[i] = -MAXDOUBLE;
	}
}

void summaryupdate(iter)
/*
	Update the summary for expected and variance
*/
int iter;
{
	if (iter == 0) recordcnt = 0;
	else recordcnt++;

	if (recordcnt < MAXCOLLECT) {
		if (maxrecord < recordcnt) maxrecord = recordcnt;
		iterations[recordcnt] = iter;
	
		minvalue[recordcnt] += RESULTS.min;
		ssqmin[recordcnt] += RESULTS.min * RESULTS.min;
		maxvalue[recordcnt] += RESULTS.max;
		ssqmax[recordcnt] += RESULTS.max * RESULTS.max;
	}
}

void summaryfirst(exper)
/*
	Record the smallest and largest and time of first passage 
	for each experiment.

	WARNING: first results are a bit suspect, since if set in initial
	population, the eval_count is for entire population.
	It is also just possible that both min and max are set in
	the same family, which could throw eval_count by up to family size.
*/
int exper;
{
   	if (exper < MAXEXPER) {
		if (firstminval[exper] > RESULTS.min) {
			firstminval[exper] = RESULTS.min;
			firstmin[exper] = RESULTS.eval_count;
		}
		if (firstmaxval[exper] < RESULTS.max) {
			firstmaxval[exper] = RESULTS.max;
			firstmax[exper] = RESULTS.eval_count;
		}
	}
}

void printsummary()
/*
	Print out the summary results
*/
{
	int i;
	double varmin, varmax;
	int totmin, totmax, numexp;
	double leastmin, greatestmax;
	int leastcnt, greatestcnt;

	printf("\n\n\t\tSummary\n\n");
	printf("Matings   \tAvg Min\t\tAvg Max\t\tVar Min \tVar Max\n");
	for(i=0;i<=maxrecord;i++) {
		printf("%7d  \t", iterations[i]);
		printf("%f\t", minvalue[i]/OPTIONS.numexperiments);
		printf("%f\t", maxvalue[i]/OPTIONS.numexperiments);
	
		/* use the sample variance formula */
		varmin = (ssqmin[i] - (minvalue[i]*minvalue[i]/
			OPTIONS.numexperiments)) /(OPTIONS.numexperiments-1);
		varmax = (ssqmax[i] - (maxvalue[i]*maxvalue[i]/
			OPTIONS.numexperiments)) /(OPTIONS.numexperiments-1);
	
		printf("%f\t",varmin);
		printf("%f\n",varmax);
	}
	
	printf("\n\tFirst Passage Info\n");
	if (OPTIONS.numexperiments >= MAXEXPER)
		numexp = MAXEXPER - 1;
	else numexp = OPTIONS.numexperiments;

	leastmin = MAXDOUBLE;
	leastcnt = MAXINT;
	greatestmax = -MAXDOUBLE;
	greatestcnt = MAXINT;
	totmin = totmax = 0;
	printf(" Exp #\t     Min \t  Evals  \t   Max   \t  Evals\n");
	for(i=1;i<=numexp;i++) {
		totmin += firstmin[i];
		totmax += firstmax[i];
		if (firstminval[i] < leastmin) {
			leastmin = firstminval[i];
			leastcnt = firstmin[i];
		}
		else if ((firstminval[i] == leastmin) && 
			(firstmin[i] < leastcnt))
			leastcnt = firstmin[i];

		if (firstmaxval[i] > greatestmax) {
			greatestmax = firstmaxval[i];
			greatestcnt = firstmax[i];
		}
		else if ((firstmaxval[i] == greatestmax) && 
			(firstmax[i] < greatestcnt))
			greatestcnt = firstmax[i];
		
		printf("%4d  %10f\t  %5d  \t%9f\t %5d\n",i,
			firstminval[i],firstmin[i],
			firstmaxval[i], firstmax[i]);
	}

	printf("\nAverage Evaluations to find a Min = %f\n",
		(double) totmin/numexp);
	printf("Average Evaluations to find a Max = %f\n",
		(double) totmax/numexp);
	printf("Overall Minimum found %f in %d minimum evaluations\n",
		leastmin,leastcnt); 
	printf("Overall Maximum found %f in %d minimum evaluations\n",
		greatestmax,greatestcnt); 
	totmin = totmax = 0;
	for(i=1;i<=numexp;i++) {
		if (firstminval[i] > leastmin) totmin++;
		if (firstmaxval[i] < greatestmax) totmax++;
	}

	printf("Failed on minimum %d out of %d times\n",totmin,numexp);
	printf("Failed on maximum %d out of %d times\n",totmax,numexp);
}
