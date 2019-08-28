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
 *  file:	decode.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1990 (functions date back to 1981)
 *
 *  purpose:	functions to decode genotypes into phenotypes
 *
 */

#define   EXTERN
#include "global.h"


Unpack(binbuf, charbuf)
/*
 * convert left-justified packed genotypes to unpacked form
 */
register char *binbuf;		/* packed array		*/
register char *charbuf;		/* unpacked form	*/
{
	register i, j;

	for (i = 0; i < Full; i++, binbuf++)
		for (j = 0; j < CHAR_BIT; j++)
			*charbuf++ = ((*binbuf & Bit[j]) != 0);

	if (Slop)
		for (j = 0; j < Slop; j++)
			*charbuf++ = ((*binbuf & Bit[j]) != 0);
}


Degray(inbuf, outbuf, length)
/*
 * translate unpacked genes from reflected Gray code to binary
 */
char *inbuf, *outbuf;
register int length;
{
	register int i;
	register int last;

	last = 0;
	for (i = 0; i < length; i++)
	{
		if (inbuf[i])  outbuf[i] = !last;
		else outbuf[i] = last;
		last = outbuf[i];
	}
}


double Ctoi(buf, length)
/*
 * decode binary integer from char array into real value
 * 'buf' must be unpacked (contain only \000 and \001's)
 */
register char *buf;
register int length;
{
	register int i;
	register double answer = 0.0;
	extern char Aliasflag;
	extern double Rand();

	for (i = 0; i < length; i++)
	{
		answer *= 2.0;
		answer += *buf++;
	}
	if (Aliasflag) answer += Rand();
	return(answer);
}

/*** end of file ***/
