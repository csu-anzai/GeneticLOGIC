#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function f06 -  k char sets	*
	*********************************
*/

/*
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

double f06(str)
/*
	Attempt number 4 to be deceptive after failure of f03 f04 f05
	Allow zero values to have more errors to hide the highly
	correct ones. (i.e.only assign non-zero values to fully erroneous)

	USAGE: strings should be multiple of alphasize for best effect
*/
unsigned char str[];
{
	int i,j,c,t;

	c=0;
	for(i=0; i<OPTIONS.stringsize; i+=OPTIONS.alphasize) {
		t= 0;
		/* count 0's as correct characters */
		for(j=0;((j<OPTIONS.alphasize)&&( (j+i)<OPTIONS.stringsize));j++) 
			if (str[i+j] == 0) t++;
		if (t == OPTIONS.alphasize) c += OPTIONS.alphasize+1;
		if (t == 0) c += OPTIONS.alphasize;
	}
	return((double) c);
}
