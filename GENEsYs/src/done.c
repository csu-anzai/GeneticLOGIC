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
 *
 *	$Id$
 *	$Log$
 *
 *
 *      file:  	done.c
 *
 *    author:   John J. Grefenstette
 *
 *   created:   apr 1984
 *
 *   purpose:  	test experiment-termination conditions.
 *
 *  modified:   7 feb 86
 *
 *		Thomas Baeck, 09 jul 90	
 *			simplified termination criterion
 */
 
#include "extern.h"
 
extern char Notermflag;		/* simplified termination criterion	*/

int 
Done()

{

    /*	modification for simplified termination, 	Ba 09 jul 90	*/

    if (Notermflag)		/* only Trials determines termination	*/
	return (Trials >= Totaltrials);

    return ((Trials >= Totaltrials) || 
	    (Lost   >= Length)      || 
	    (Spin   >= Maxspin));
}
 
/* end of file */
