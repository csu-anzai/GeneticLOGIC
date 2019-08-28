
#include "giga.h"

extern double f00(), f01(), f02(), f03(),f04(),f05(),f06(),f07(),f08(),
	fdj1(), fdj2(), fdj3(), fdj4(), fdj5(),fbin(),fdgb(),fdl();
/*

	*********************** EVALUATE ************************
	*	Evaluation calls function (user defined)	*
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

void evaluate(ptr)
/*
	Get the value and assign it to the string.value
	Update RESULTS
*/
struct popmember_st *ptr;
{
	unsigned char *sptr;
	
	sptr = ptr->individual;

	switch (OPTIONS.whichfnc) {
	/* DeJong Functions */
	case 1: ptr->value = fdj1(sptr); break;
	case 2: ptr->value = fdj2(sptr); break;
	case 3: ptr->value = fdj3(sptr); break;
	case 4: ptr->value = fdj4(sptr); break;
	case 5: ptr->value = fdj5(sptr); break;

	/* Character count and addition */
	case 6: ptr->value = f00(sptr); break;
	case 7: ptr->value = f01(sptr); break;
	case 8: ptr->value = f02(sptr); break;

	/* attempts at deceptive functions */
	case 9: ptr->value = f03(sptr); break;
	case 10: ptr->value = f04(sptr); break;
	case 11: ptr->value = f05(sptr); break;
	case 12: ptr->value = f06(sptr); break;
	case 16: ptr->value = f07(sptr); break;
	case 17: ptr->value = f08(sptr); break;

	/* Binary number */
	case 13: ptr->value = fbin(sptr); break;

	/* Textbook deception */
	case 14: ptr->value = fdgb(sptr); break;
	case 15: ptr->value = fdl(sptr); break;

	/* illegal option */
	default: printf("Evaluate: No function %d\n",OPTIONS.whichfnc); exit(1);
	}

	if (ptr->value > RESULTS.max) {
		RESULTS.max = ptr->value;
		RESULTS.new_results = yes;
	}
	if (ptr->value < RESULTS.min) {
		RESULTS.min = ptr->value;
		RESULTS.new_results = yes;
	}
	RESULTS.eval_count++;
}


double chartodbl(buf,len,base)
/*
	Convert a string to binary representation
*/
unsigned char *buf;
int len, base;
{
	int i;
	double result;

	result = 0.0;

	for(i=0;i<len; i++ ) {
		result *= base;
		result += *buf;
		buf++;
	}
	return(result);
}

void ungray(in,out,len)
/*
	assume in is gray -> out is binary
*/
unsigned char in[],out[];
int len;
{
	int i;
	unsigned char replace;

	replace = 0;
	for(i=0;i<len;i++) {
		if (in[i] == 1) out[i] = 1-replace;
		else out[i] = replace;
		replace = out[i];
	}
}

