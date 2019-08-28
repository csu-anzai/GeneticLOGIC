#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function f02 - abs(1's - 0's) *
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

double f02(str)
/*
	The absolute difference of number of 1's and 0's
*/
unsigned char str[];
{
	int i,c;

	c=0;
	for(i=0; i<OPTIONS.stringsize; i++) {
		if ( str[i] == 1) c++;
		if ( str[i] == 0) c--;
	}
	if (c<0) c = -c;
	return((double) c);
}

