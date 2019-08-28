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
 *  file:	setflag.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	apr 84
 *
 *  purpose:	set the option flags according to option string
 *		in input file.
 *
 *  modified:	28 mar 86
 */

#define   EXTERN
#include "global.h"


Setflag(c)
char c;
{
	switch (c) {
	case 'a' :
		Allflag = 1;
		break;
	case 'A' :
		Allflag = 1;
		Aliasflag = 1;
		break;
	case 'b' : 
		Bestflag = 1; 
		break;
	case 'c' : 
		Collectflag = 1; 
		Convflag = 1;
		break;
	case 'C' : 
		Collectflag = 1; 
		break;
	case 'd' : 
		Dumpflag = 1; 
		break;
	case 'e' :
		Eliteflag = 1;
		break;
	case 'i' :
		Initflag = 1;
		break;
	case 'l' :
		Logflag = 1;
		break;
	case 'L' :
		Lastflag = 1;
		break;
	case 'o' : 
		Onlnflag = 1; 
		break;
	case 'O' :
		Offlnflag = 1;
		break;
	case 'r' : 
		Restartflag = 1; 
		break;
	case 's' : 
		Schemflag = 1; 
		break;
	case 't' : 
		Traceflag = 1; 
		break;
	case 'u' : 
		Uniflag = 1; 
		break;
	}
}

/*** end of file ***/

