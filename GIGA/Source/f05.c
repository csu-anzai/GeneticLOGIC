#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function f05 -  k char sets(3)*
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

* Initial Version Created April 17, 1992.

*/

double f05(str)
/*
	Attempt number 3 to be deceptive after failure of f03 and f04
	Allow zero values to have more errors to hide the highly
	correct ones. (i.e. div by 3 not 2)

	USAGE: strings should be multiple of alphasize for best effect
*/
unsigned char str[];
{
	int i,j,c,t;

	c=0;
	for(i=0; i<OPTIONS.stringsize; i+=OPTIONS.alphasize) {
		t= 0;
		/* count 0's aas correct characters */
		for(j=0;((j<OPTIONS.alphasize)&&( (j+i)<OPTIONS.stringsize));j++) 
			if (str[i+j] == 0) t++;
		if (t == OPTIONS.alphasize) c += OPTIONS.alphasize+1;
		if (t < (OPTIONS.alphasize/3)) {
			c += (OPTIONS.alphasize-t);
		}
	}
	return((double) c);
}
