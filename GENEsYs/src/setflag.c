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
 *	$Id: setflag.c,v 1.1 1992/06/12 11:35:48 baeck Exp $
 *	$Log: setflag.c,v $
 * Revision 1.1  1992/06/12  11:35:48  baeck
 * Initial revision
 *
 *
 *
 *  file:    	setflag.c
 *
 *  author:    	John J. Grefenstette
 *
 *  created:    apr 84
 *
 *  purpose:    set the option flags according to option string
 *        	in input file.
 *
 *  modified:    		28 mar 86
 *
 *		Thomas Baeck, 	09 jul 90 
 *			option `n' and flag `Notermflag' added for 
 *			simplified termination.
 *
 *		Thomas Baeck,   06 aug 90 
 *			option `p' and flag `Printpopflag' added for 
 *			decoded population dumps.
 *
 *		Thomas Baeck,	18 oct 90 
 *			option `k' and flag 'Proportionflag' added for
 *			best individual prop.
 *
 */


#include "extern.h"
 
void
SetFlg(c)
char 	c;

{
    	char 	msg[NSZ];	/* messages, filenames,...	*/

    	switch (c) {

    	case 'a' :	/* evaluate all structures */
	       	Allflag = 1;
	       	break;
	
	case 'e' :	/* elitist strategy */
		Eliteflag = 1;
		break;
	
	case 'i' :	/* read structures into initial population */
	        Initflag = 1;
	        break;
	
	case 'k' :	/* best statistics, Ba 19 oct 90 */
		Proportionflag = 1;
		break;
	
	case 'l' :	/* dump last generation */
	        Lastflag = 1;
	        break;
	
	case 'n' :	/* simplified termination criterion, Ba 09 jul 90 */
		Notermflag = 1;
		break;

	case 'm' :	/* collect all mutation rate data */
		MttFlg = 1;
		break;
	
	case 'p' :	/* dump decoded population, Ba 06 aug 90 */
		Printpopflag = 1;
		break;
	
	case 's' :	/* trace schema history */
	        Schemflag = 1;
	        break;
	
	case 't' :	/* trace each major function call */
	        Traceflag = 1;
	        break;

	case 'v':	/* collect all object variable data */
		VarFlg = 1;
		break;

	case 'z':	/* collect selection probabilities */
		PrbFlg = 1;
		break;

	default  :
		sprintf(msg, "Setflag: Wrong flag passed (%c)", c);
		Error(msg);
		break;
	}
	
} /* end setflag() */
	 
/*** end of file ***/
