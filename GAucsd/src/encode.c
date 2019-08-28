/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:	encode.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1990 (functions date back to 1981)
 *
 *  purpose:	functions to encode phenotypes into genotypes
 *
 */

#define   EXTERN
#include "global.h"


Itoc(n, outbuf, length)
/*
 * convert integer n into unpacked, binary coded char array
 */
register int n;
register char *outbuf;
int length;
{
	register int i;

	for (i = length - 1; i >= 0; i--)
	{
		outbuf[i] = (n & 1);
		n >>= 1;
	}
}


Gray(inbuf, outbuf, length)
/*
 * translate unpacked genes from binary to reflected Gray code
 */
char *inbuf, *outbuf;
register int length;
{
	register int i;
	register int last;

	last = 0;
	for (i = 0; i < length; i++)
	{
		outbuf[i] = (inbuf[i] != last);
		last = inbuf[i];
	}
}



Pack(charbuf, binbuf)
/*
 * convert unpacked genotype into left-justified packed form
 */
register char *charbuf;		/* unpacked array, one bit per char	*/
register char *binbuf;		/* packed representation of charbuf	*/
{
	register i, j;

	for (i = 0; i < Full; i++, binbuf++)
	{
		*binbuf = '\0';
		for (j = 0; j < CHAR_BIT; j++)
			if (*charbuf++)  *binbuf |= Bit[j];
	}

	if (Slop)
	{
		*binbuf = '\0';
		for (j = 0; j < Slop; j++)
			if (*charbuf++)  *binbuf |= Bit[j];
	}
}

/*** end of file ***/
