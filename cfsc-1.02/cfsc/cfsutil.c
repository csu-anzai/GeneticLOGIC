/*			CFSUTIL for the CFS-C Classifier System

This file, CFSUTIL.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file test and convert classifier/message strings
and to perform other "utility" jobs.

The subroutines in this file:

	InSrtCf 	Insert-sort classifier into strength-sorted classifier list.
	ReSrtCfs	Resort the classifier list.
	FindHighLowStrCf() Find Hi/Low strength classifiers

	IsMessage ( String )	-> TRUE if String is all 0's and 1's
	IsCondAct ( String )	-> TRUE if String is all 0, 1, or #'s.

	AMsgtoB 	Ascii Msg to Binary form
	BMsgtoA 	Binary Msg to ascii
	ACndtoB 	Ascii Condition to Binary form
	BCndtoA 	Binary Condition to ascii
	AActtoB 	Ascii Action to Binary form
	BActtoA 	Binary action to ascii

	Loci2Int ( ascii-loci, len )		returns integer form of message-loci
	Int2Loci ( int, start-loci, len )	convert int to message-string form (ascii).

	CalcSpec	Calculate the Specificity for a classifier.

	Mod_Msg |- modify msgs/cfs -- not implemented.
	Mod_Cf	|
**/

#include	"compiler.h"
#include	<math.h>
#include	<stdio.h>

#include	"utility.h" 
#include	"cfsio.ext" 	/* Input/output control variables */
#include	"core.ext"		/* Declare MsnNode's, CfNode's, etc. */ 
#include	"cfops.ext" 	/* Declare	classifier operators */

VOID	InSrtCf(), ReSrtCfs(), FindHighLowStrCf(), BMsgtoA(), BCndtoA(), BActtoA(),
		Int2Loci(), Mod_Cf(), Mod_Msg();


/*******************************

InSrtCf 	Insert-Sort classifier into classifier list.

	CfPtr 	Pointer to classifier to insert.

The classifier nodes are linked to form a two-way linked list sorted on strength.
The global pointer StrSrtCf (strength sorted classifiers) points to the lowest
strength classifier, and the NxtStrCf pointer in each node points to the higher
strength classifiers (or null).  The HiStrCf pointer points to the highest-strength
classifier, and the PrevStrCf pointers point to lower strength classifiers.

**** NOTE: It might be nice to add header-nodes to these lists, to save checking for NULLs.

*****/

VOID InSrtCf( CfPtr )
	struct	CfNode	*CfPtr;
{
	float			cfstr;
	struct CfNode	*prev;

	cfstr = CfPtr->Strength;

	if ( StrSrtCf == NULL ) {				 /* List is empty - so make new one first and only */
		StrSrtCf = HiStrCf = CfPtr;
		CfPtr->PrvStrCf = CfPtr->NxtStrCf = NULL;	
	}

	else if ( StrSrtCf->Strength >= cfstr ) { /* new one goes first */
		CfPtr->NxtStrCf = StrSrtCf;
		CfPtr->PrvStrCf = NULL;
		StrSrtCf->PrvStrCf = CfPtr;
		StrSrtCf = CfPtr;
	}	

	else {
			/* Move prev until it points to node that should be before new one */
		prev = StrSrtCf;
		while ( prev->NxtStrCf != NULL && cfstr > prev->NxtStrCf->Strength )
			prev=prev->NxtStrCf;

			/* link new node to node after it (if there is one) -- both ways */

		CfPtr->NxtStrCf = prev->NxtStrCf;		
		if ( prev->NxtStrCf == NULL )
			HiStrCf = CfPtr;						/* prev was last, so new one is now */
		else
			prev->NxtStrCf->PrvStrCf = CfPtr;		

			/* link new to prev -- both ways */

		CfPtr->PrvStrCf = prev;
		prev->NxtStrCf = CfPtr;
	}

} /* InSrtCf */


/*******************************

ReSrtCfs	Resort classifier list.

This just uses InSrtCf() to re-insert sort classifiers, since the list is typically almost
sorted anyway (i.e., most classifiers don't change relative strength).
For efficiency, start with the high-strength end, since InSrtCf() inserts at low end. 

******/

VOID ReSrtCfs ( )
{
	struct CfNode			*oldlist, *cfptr;
	extern unsigned int		CLSorted;

	oldlist	= HiStrCf; 						/* Thats the high end */
	StrSrtCf = HiStrCf = NULL;				/* Empty the list pointers for both ends */
	CLSorted = TRUE;						/* set ths global marker */

	while ( oldlist != NULL )	{
		cfptr	= oldlist;					/* pop a cf */
		oldlist = oldlist->PrvStrCf;
		InSrtCf( cfptr );					/* insert it into list */
	}

	HighestStrCf = HiStrCf;	 /* get these pointers in sync */
	LowestStrCf = StrSrtCf;

} /* end ReSrtCfs */


/*******************************

FindHighLowStrCf  Find Hi and Low strength classifiers.

Set HighestStrCf to point to highest strength one.
	LowestStrCf  to point to lowest strength one.
This is done WITHOUT sorting list, and so without changing StrSrtCf and HiStrCf.

However, both pairs ARE set by ReSrtCf(), so that
	StrSrtCf == LowestStrCf
	HiStrCf  == HighestStrCf
after sorting.

******/

VOID FindHighLowStrCf()
{
	register float		  high;   /* save this--it should change much since we work high to low */
	register struct CfNode	*cfptr;

    if ( NmCfs > 0 ) {
    	cfptr = HighestStrCf = LowestStrCf = HiStrCf;   /* first one is both high and low to start */
	    high = HighestStrCf->Strength;

		    /* check the rest, high to low */

    	for ( cfptr = cfptr->PrvStrCf; cfptr != NULL; cfptr = cfptr->PrvStrCf ) 
	    	if ( cfptr->Strength > high ) {
		    	HighestStrCf = cfptr;
			    high = cfptr->Strength;
    		}
	    	else if ( cfptr->Strength < LowestStrCf->Strength )
		    	LowestStrCf = cfptr;
    }

} /* end FindHighLowStrCf */


/***************************

IsMessage	See if character string is a legal Message.

	Return	TRUE if 'String' is a legal message string, else return FALSE.

	Legal messages must consist of STRNGSZ 0's and 1's followed by a \0.

******/

int IsMessage ( String )
	char String[];
{
	int		i;

	for ( i = 0; i < STRNGSZ; ++i ) 
		if ( String[i] != '0' && String[i] != '1' ) break;
	
	if ( i == STRNGSZ && String[i] == '\0' )
		return( TRUE );

	return( FALSE );

} /* IsMessage */


/******************************

IsCondAct	See if character string is a legal classifier condition or action string.

	Return	TRUE if 'String' is a legal classifier condition/action, else return FALSE.

	Legal condition/action's consist of STRNGSZ 0's, 1's, or #'s, followed by a '\0'.

******/

int IsCondAct ( String )
	char String[];
{
	int		i;

	for ( i = 0; i < STRNGSZ; ++i ) 
		if ( String[i] != '0' && String[i] != '1' && String[i] != '#' ) break;
	
	if ( i == STRNGSZ && String[i] == '\0' )
		return( TRUE );

	return( FALSE );

} /* IsCondAct */

/******************************

AMsgtoB 		Get binary integer form of ascii message.

	AsciiMsg	ascii form of full message.
	BinMsg		pointer to first element unsigned array into which AsciiMsg is to be placed.
				The array has INTPRSTR (integers per string) elements.
	Return		ERROR if not convertable, else return OK.

******/

int AMsgtoB ( AsciiMsg, BinMsg )
	char		AsciiMsg[];
	unsigned	BinMsg[];
{
	int		intcnt, aloci, iloci;
	
	aloci = 0;

	for (intcnt = 0; intcnt < INTPRSTR; ++intcnt) {
		BinMsg[intcnt] = 0;
		for ( iloci = 0; iloci < LOCIPRI; ++iloci) {
			BinMsg[intcnt] <<= 1;
			switch	( AsciiMsg[aloci++] )	{
				case '1':	BinMsg[intcnt] |= LSB;
							break;
				case '0':	break;
				default:	sprintf(GOutBuff,"\nERR (AMsgtoB): illegal char. in Msg: '%c'", AsciiMsg[aloci] );
							WriteStd( GOutBuff );
							return(ERROR);
			}
		}
	}

	return(OK);

} /* AMsgtoB */

/******************************

BMsgtoA 		Return ascii form of full binary message.

	BinMsg		Pointer to first element unsigned array which holds the message.
				The array has INTPRSTR (integers per string) elements.
	AsciiMsg	Character array into which the ascii form is to be placed.

******/

VOID BMsgtoA ( BinMsg, AsciiMsg )
	unsigned	BinMsg[];
	char		AsciiMsg[];
{
	int			intcnt, iloci, aloci;
	unsigned	tbinmsg;						/* temporary area for conversion */

	for ( intcnt = INTPRSTR-1, aloci = STRNGMX; intcnt >= 0; --intcnt ) {
		tbinmsg = BinMsg[intcnt];
		for ( iloci = LOCIPRI; iloci > 0; --iloci)	{
			AsciiMsg[aloci--] = ( tbinmsg & LSB ) ? '1' : '0';
			tbinmsg >>= 1;
		}
	}

} /* BMsgtoA */

/******************************

ACndtoB 	Convert Ascii form of classifier condition to binary.

******/

int ACndtoB ( AsciiCnd, CndBits, CndDCs )
	char			AsciiCnd[];
	unsigned int	CndBits[], CndDCs[];
{
	int intcnt, aloci, iloci;

	for ( intcnt = 0, aloci = 0; intcnt < INTPRSTR; ++intcnt) {
		CndBits[intcnt] = CndDCs[intcnt] = 0;
		for ( iloci = 0; iloci < LOCIPRI; ++iloci) {
			CndBits[intcnt] <<= 1;					/* zero into lsb */
			CndDCs[intcnt]	<<= 1;
			switch ( AsciiCnd[aloci++] ) {
				case '0':	CndDCs[intcnt] |= LSB;
							break;
				case '1':	CndBits[intcnt] |= LSB;
							CndDCs[intcnt]	|= LSB;
							break;
				case '#':	break;
				default:	sprintf(GOutBuff,"\nERR (ACndtoB): illegal char. in condition: '%c'", AsciiCnd[aloci-1] );
							WriteStd( GOutBuff );
							return(ERROR);
			};
		}
	}

	return( OK );

} /* ACndtoB */


/******************************

BCndtoA 	Return ascii	form of binary classifier condition.

******/

VOID BCndtoA ( CndBits, CndDCs, AsciiCnd )
	unsigned	CndBits[], CndDCs[];
	char		AsciiCnd[];
{
	int			intcnt, iloci, aloci;
	unsigned	tbits, tdcs;

	for ( intcnt = INTPRSTR-1, aloci = STRNGMX; intcnt >= 0; --intcnt)	{
		tbits	= CndBits[intcnt];
		tdcs	= CndDCs[intcnt];
		for ( iloci = LOCIPRI; iloci > 0; --iloci) {
			if		( tbits & LSB )
				AsciiCnd[aloci--] = '1';
			else if ( !(tbits & LSB) && (tdcs & LSB) )
				AsciiCnd[aloci--] = '0';
			else
				AsciiCnd[aloci--] = '#';
			tbits	>>= 1;
			tdcs	>>= 1;
		}
	}

} /* BCndtoA */

/******************************

AActtoB 	Convert ascii form of classifier action-part to binary form.

******/

int AActtoB ( AsciiAct, ActBits, ActDCs )
	char		AsciiAct[];
	unsigned	ActBits[], ActDCs[];
{
	int intcnt, iloci, aloci;

	for ( intcnt = 0, aloci = 0; intcnt < INTPRSTR; ++intcnt) {
		ActBits[intcnt] = ActDCs[intcnt] = 0;
		for ( iloci = 0; iloci < LOCIPRI; ++iloci) {
			ActBits[intcnt] <<= 1;
			ActDCs[intcnt]	<<= 1;
			switch ( AsciiAct[aloci++] ) {
				case '0':	break;
				case '1':	ActBits[intcnt] |= LSB;
							break;
				case '#':	ActDCs[intcnt]	|= LSB;
							break;
				default:	sprintf(GOutBuff,"\nERR (AActtoB): illegal char. in action: '%c'", AsciiAct[aloci-1] );
							WriteStd( GOutBuff );
							return(ERROR);
			 };
		}
	}

	return( OK );

} /* AActtoB */

/******************************

BActtoA 	Return ascii	form of binary classifier action-part.

******/

VOID BActtoA ( ActBits, ActDCs, AsciiAct )
	unsigned	ActBits[], ActDCs[];
	char		AsciiAct[];
{
	int			intcnt, iloci, aloci;
	unsigned	tbits, tdcs;

	for ( intcnt = INTPRSTR-1, aloci = STRNGMX; intcnt >= 0; --intcnt)	{
		tbits	= ActBits[intcnt];
		tdcs	= ActDCs[intcnt];
		for ( iloci = LOCIPRI; iloci > 0; --iloci) {
			if		( tbits & LSB )
				AsciiAct[aloci--] = '1';
			else if ( tdcs & LSB )
				AsciiAct[aloci--] = '#';
			else
				AsciiAct[aloci--] = '0';
			tbits	>>= 1;
			tdcs	>>= 1;
		}
	}

} /* BActtoA */


/******************************

Loci2Int	Convert message-string loci values (0,1) to integer.

	Loci	Char array form of message on {0,1}.
	Len		length of Loci to convert (must be less that bits/int of machine).
	Return	int form of ALoci, or ERROR if there is a problem.

******/

int Loci2Int ( Loci, Len )
	char	Loci[];
	int		Len;
{
	register int	i, bin;
	
	if ( Len > INTSZ )	{
		sprintf(GOutBuff,"\nWARNING (Loci2Int): Len of Loci (%d) > machine's INTSZ (%d).", Len, INTSZ );
		WriteStd( GOutBuff );
		sprintf(GOutBuff,"\nConverting first %d to int.", INTSZ );
		WriteStd( GOutBuff );
	}
	
	for ( i = 0, bin = 0; i < Len; ++i ) {
		bin <<= 1;
		switch	( Loci[i] ) {
			case '1':	bin |= LSB;
					 	break;
			case '0':	break;
			default:	sprintf(GOutBuff,"\nERR (Loci2Int): illegal char '%c' in string.", Loci[i] );
						WriteStd( GOutBuff );
						return(ERROR);
		}
	 }
	 return( bin );
	
} /* Loci2Int */


/******************************

Int2Loci	Convert integer to string loci values (0,1).

	Bin		integer (binary) form of string. Convert the least significant bits.	
	Loci	char array into with ascii form of message {0,1} is to be placed.
	Len		length of Loci to convert (must be less that bits/int of machine).
	Return	ERROR if there is a problem, else OK.

******/

VOID Int2Loci ( Bin, Loci, Len )
	int		Bin;
	char	Loci[];
	int		Len;
{
	register int	i;

	if ( Len > INTSZ )	{
		sprintf(GOutBuff,"\nWARNING in Loci2Int: Len of Loci (%d) greater than machine's INTSZ (%d).", Len, INTSZ );
		WriteStd( GOutBuff );
		sprintf(GOutBuff,"\nConverting first %d to int form.", INTSZ );
		WriteStd( GOutBuff );
	}

	for ( i = Len-1; i >= 0; --i )	{
		Loci[i] = ( Bin & LSB ) ?	'1' : '0';
		Bin >>= 1;
	}

} /* Int2Loci */


/******************************

CalcSpec	Calculate specificity for two conditions.

	Cnd1		|- Two conditions, each of size STRNGSZ, on {0,1,#} alphabet.
	Cnd2		|

	Cnd1Type	|- Condition 'type' of the associated conditions, i.e., either
	Cnd2Type	|  CMATCH or CNOTMATCH.

	For match condition, specificity = number of specific loci (o or 1) / string size
	For not-match condition, use the inverse, i.e., count don't-cares (#'s),
	since more #'s means the condition is more "specific"--i.e., a smaller
	set of messages will match it. (All #...# condition matches NO messages!)

	Return:		Specificity, i.e, 0.0 to 1.0 number.

******/

float CalcSpec ( Cnd1, Cnd2, Cnd1Type, Cnd2Type )
	char		Cnd1[STRNGSZ], Cnd2[STRNGSZ];
	unsigned int	Cnd1Type, Cnd2Type;
{
	register int	charcnt, specif;
	register float	fspec;

	specif = 0;

	if ( Cnd1Type == CMATCH ) {
		for ( charcnt = 0; charcnt <= STRNGMX; ++charcnt )
			if	( Cnd1[charcnt] != '#' )					/* Count 1's and 0's */
				++specif;
	}
	else {	 /* Cnd1Type must be CNOTMATCH */
		for ( charcnt = 0; charcnt <= STRNGMX; ++charcnt )
			if ( Cnd1[charcnt] == '#' ) 					/* Count the #'s */ 
				++specif;
	}

	if ( Cnd2Type == CMATCH ) {
		for ( charcnt = 0; charcnt <= STRNGMX; ++charcnt )
			if	( Cnd2[charcnt] != '#' )					/* Count 1's and 0's */
				++specif;
	}
	else {	 /* Cnd2Type must be CNOTMATCH */
		for ( charcnt = 0; charcnt <= STRNGMX; ++charcnt )
			if ( Cnd2[charcnt] == '#' ) 					/* Count the #'s */ 
				++specif;
	}

	fspec = specif * 1.0 / (STRNGSZ*2);

	return( fspec );
	
} /* CalcSpec */


/******************************

Mod_Cf	- modify a classifier.

******/

VOID Mod_Cf ( CmdPars )
	char CmdPars[];
{
	
	WriteStd( "\n\nCan't MODC (MODify Classifers) yet.\n\n" );

} /* Mod_Cf */

/******************************

Mod_Cf - modify a message.

******/ 
		
VOID Mod_Msg ( CmdPars )
	char	CmdPars[];
{

	WriteStd( "\nCan't MODM (MODify Messages) yet.\n\n" );

} /* Mod_Msg */
