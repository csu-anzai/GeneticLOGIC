#include "giga.h"

extern srandom();
/*

	********************** OPTIONINIT ***********************
	*	 	Option Input Routines			*
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

void optioninit()
/*
	Initialize the control options
*/
{
	char c[50];
	int seed;
	int i,rate;

	/************* GENERAL *********************/
	printf("Random Seed ");
	scanf("%d",&seed);
	printf("%d\n",seed);
	srandom(seed);

	/************** POPULATION ******************/
        printf("/* --- Population parameters --- */\n");
	printf("Population Size ");
	scanf("%d", &OPTIONS.popsize);
	printf("%d\n",OPTIONS.popsize);
	if ( ( OPTIONS.popsize < 2) || (OPTIONS.popsize > MAXPOP)) {
		printf("Out of range 2 to %d\nSee file giga.h\n",MAXPOP);
		exit(1);
	}

	printf("String Size ");
	scanf("%d", &OPTIONS.stringsize);
	printf("%d\n",OPTIONS.stringsize);
	if ( ( OPTIONS.stringsize < 2) || (OPTIONS.stringsize > MAXSTRING)) {
		printf("Out of range 2 to %d\nSee file giga.h\n",MAXSTRING);
		exit(1);
	}
		
	printf("Alphabet Size ");
	scanf("%d", &OPTIONS.alphasize);
	printf("%d\n",OPTIONS.alphasize);
	if ( ( OPTIONS.alphasize < 2) || (OPTIONS.alphasize > 256)) {
		printf("Out of range 2 to %d\n",256);
		exit(1);
	}

	printf("Population initialization type\n");
	printf("\t1 - Random\n");
	printf("\t2 - Random rotation\n");
	scanf("%d",&OPTIONS.popinit);
	printf("Choice is %d\n",OPTIONS.popinit);
	if ( (OPTIONS.popinit < 1) || (OPTIONS.popinit > 2)) {
		printf("Popinit must be 1 or 2\n");
		exit(1);
	}
	
	printf("Keep the population sorted (y/n) ");
        scanf("%s",c);
        printf("%s\n",c);
        if (c[0] == 'y') { 
		OPTIONS.sortflag = yes;
		OPTIONS.initsort = yes; /* implied */
	}
        else if (c[0] == 'n') {
		OPTIONS.sortflag = no;
	        printf("Sort the initial population (y/n) ");
	        scanf("%s",c);
        	printf("%s\n",c);
		if (c[0] == 'y') OPTIONS.initsort = yes;
		else if (c[0] == 'n') OPTIONS.initsort = no;
		else {
                	printf("Must be y or n\n");
                	exit(1);
		}
	}
        else {
                printf("Must be y or n\n");
                exit(1);
        }

	/************** MATING ******************/
        printf("\n/* --- Mating parameters --- */\n");
	printf("Use elitist mating (y/n) ");
	scanf("%s",c);
	printf("%s\n",c);
	if (c[0] == 'y') OPTIONS.elitist_mate = yes;
	else if (c[0] == 'n') OPTIONS.elitist_mate = no;
	else {
		printf("Must be y or n\n");
		exit(1);
	}

	printf("Epsilon for equality check ");
	scanf("%lf",&OPTIONS.epsilon);
	printf("%f\n",OPTIONS.epsilon);
	
	printf("Family Size ");
	scanf("%d", &OPTIONS.familysize);
	printf("%d\n",OPTIONS.familysize);

	printf("Which definition of best pair?\n");
	printf("\t1 - Maximum of pair\n");
	printf("\t2 - Minimum of pair\n");
	printf("\t3 - Maximum Difference\n");
	printf("\t4 - Minimum Difference (SQUISH)\n");
	scanf("%d",&OPTIONS.bestdef);
	printf("Choice is: %d\n",OPTIONS.bestdef);
	if ( (OPTIONS.bestdef< 1) || (OPTIONS.bestdef > 4)) {
		printf("Options are 1,2,3,4 only\n");
		exit(1);
	}

	/************** CROSSOVER ******************/
	printf("\n/* --- Crossover parameters --- */\n");
	printf("Which type of crossover?\n");
	printf("\t1 - One point crossover\n");
	printf("\t2 - Uniform (biased by swapbias)\n");
	printf("\t3 - One point with no repetition\n");
	printf("\t4 - Varying crossover\n");
	scanf("%d",&OPTIONS.crosstype);
	printf("Choice is: %d\n",OPTIONS.crosstype);
	if ( (OPTIONS.crosstype< 1) || (OPTIONS.crosstype > 4)) {
		printf("Options are 1,2,3 and 4 only\n");
		exit(1);
	}

	if ((OPTIONS.crosstype == 2) || (OPTIONS.crosstype == 4)) {
        	printf("Swap Bias (mod %d) ",SWAPBASIS);
        	scanf("%d", &OPTIONS.swapbias);
        	printf("%d\n",OPTIONS.swapbias);
        	if ((OPTIONS.alphasize < 0) || (OPTIONS.alphasize > SWAPBASIS)) {
                	printf("Out of range 0 to %d\n",SWAPBASIS);
                	exit(1);
        	}
	}

	if (OPTIONS.crosstype == 4) {
		printf("Cross Rate, Cross number (end with rate =0)\n");
		i = 0;
		OPTIONS.total_cross_freq = 0;
		rate = 1;
		while (rate > 0) {
			scanf("%d %d", &rate, &OPTIONS.crossnum[i]);
			printf("%d %d\n",rate,OPTIONS.crossnum[i]);
			OPTIONS.total_cross_freq += rate;
			OPTIONS.crossrate[i] = OPTIONS.total_cross_freq;
			i++;
		}
	}

	/************** SELECTION ******************/
	printf("\n/* --- Selection parameters --- */\n");
	printf("Which Selection type?\n");
	printf("\t1 - Unbiased adjacent pairs\n");
	printf("\t2 - Rotating adjacent pairs\n");
	printf("\t3 - Alternating adjacent pairs\n");
	scanf("%d",&OPTIONS.selecttype);
	printf("Choice is: %d\n",OPTIONS.selecttype);
	if ( (OPTIONS.selecttype< 1) || (OPTIONS.selecttype > 3)) {
		printf("Options are 1, 2 and 3 only\n");
		exit(1);
	}

	/************** TERMINATION ******************/
	printf("\n/* --- Termination parameters --- */\n");
	printf("Maximum Matings ");
	scanf("%d", &OPTIONS.maxiterations);
	printf("%d\n",OPTIONS.maxiterations);

	/************** REPORT ******************/
        printf("\n/* --- Report parameters --- */\n");
	printf("Print Initial population BEFORE sorting (y/n) ");
	scanf("%s",c);
	printf("%s\n",c);
	if (c[0] == 'y') OPTIONS.beforesort = yes;
	else if (c[0] == 'n') OPTIONS.beforesort = no;
	else {
		printf("Must be y or n\n");
		exit(1);
	}

	if (OPTIONS.initsort == yes) {
		printf("Print Initial population AFTER sorting (y/n) ");
		scanf("%s",c);
		printf("%s\n",c);
		if (c[0] == 'y') OPTIONS.aftersort = yes;
		else if (c[0] == 'n') OPTIONS.aftersort = no;
		else {
			printf("Must be y or n\n");
			exit(1);
		}
	}

	printf("Print INTERMEDIATE population(s) (y/n) ");
	scanf("%s",c);
	printf("%s\n",c);
	if (c[0] == 'y') OPTIONS.intermediate = yes;
	else if (c[0] == 'n') OPTIONS.intermediate = no;
	else {
		printf("Must be y or n\n");
		exit(1);
	}

	if (OPTIONS.intermediate == yes) {
		printf("Frequency for Intermediate Printing ");
		scanf("%d",&OPTIONS.frequency);
		printf("%d\n",OPTIONS.frequency);
	}

	printf("Print FINAL population (y/n) ");
	scanf("%s",c);
	printf("%s\n",c);
	if (c[0] == 'y') OPTIONS.final = yes;
	else if (c[0] == 'n') OPTIONS.final = no;
	else {
		printf("Must be y or n\n");
		exit(1);
	}

	/************** EXPERIMENT ******************/
        printf("\n/* --- Experiment parameters --- */\n");
	printf("Number of experiments ");
	scanf("%d",&OPTIONS.numexperiments);
	printf("%d\n",OPTIONS.numexperiments);
	if (OPTIONS.numexperiments <1) {
		printf("Too few experiments to bother with\n");
		exit(1);
	}
	if (OPTIONS.numexperiments >= MAXEXPER) {
		printf("WARNING: Too many experiments, first passage values ");
		printf("will not be kept for all of them\n");
	}

	printf("Statistics collection frequency ");
	scanf("%d",&OPTIONS.collect_freq);
	printf("%d\n",OPTIONS.collect_freq);
	if (OPTIONS.collect_freq < 1) {
		printf("Cannot collect that often\n");
		exit(1);
	}
	if ((OPTIONS.maxiterations / OPTIONS.collect_freq) >= (MAXCOLLECT -1)){
		printf("WARNING: ");
		printf("Too many collection points - some will be missed\n");
	}
		
	/************** FUNCTION ******************/
        printf("\n/* --- Function parameters --- */\n");
	printf("Which function?\n");
	printf("\t1 to 5 DeJong functions\n");
	printf("\t6 - 1's count\n\t7 - Add Characters\n");
	printf("\t8 - Absolute difference of 1's and 0's\n");
	printf("\t9 to 12 Deceptive attempts d1 to d4\n");
	printf("\t13 binary number\n");
	printf("\t14 Goldberg deception\n");
	printf("\t15 Liepin deception\n");
	printf("\t16 Halfway Deception d5\n");
	printf("\t17 Third-way Deception d6\n");
	scanf("%d",&OPTIONS.whichfnc);
	printf("Choice is %d\n",OPTIONS.whichfnc);

	printf("Interpret as gray code (y/n) ");
        scanf("%s",c);
        printf("%s\n",c);
        if (c[0] == 'y') OPTIONS.asgray = yes;
        else if (c[0] == 'n') OPTIONS.asgray = no;
        else {
                printf("Must be y or n\n");
                exit(1);
        }

	printf("Up to 10 double parameters to be passed to function\n");
	printf("\n\tENTER ^D TO END\n");
	i=0;
	while ((i<10) && (1==scanf("%lf",&OPTIONS.parameters[i]))) { 
		printf("%f\n",OPTIONS.parameters[i]);
		i++;
	}

	while (i<10) {
		OPTIONS.parameters[i] = MAXDOUBLE;
		i++;
	}

}

void resultinit()
/*
	initialize the results tables
*/
{
	RESULTS.max = -MAXDOUBLE;
	RESULTS.min = MAXDOUBLE;
	RESULTS.eval_count = 0;
	RESULTS.new_results = no;
}
