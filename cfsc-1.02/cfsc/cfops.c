/*		CFOPS	for the CFS-C Classifier System

This file, CFOPS.C, is part of the CFS-C classifier system	(Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file implement the classifier operators (action-types), e.g.,
the standard "pass-through" operator,logical AND, OR, XOR, add, subtract, add 1, etc.

The subroutines:

	GetCOpCd	given code for operator, find its node. 
	GetCOpNa	given name of operator, find its node.

	COpPASS 	the standard pass-through operator.
	COpAND		logical AND two messages.
	COpOR		logical OR	two messages.
	COpXOR		logical exclusive-OR two messages.
	COpADD		add two messages.			(not implemented yet)
	COpADD1 	add 1 to a message.
	COpSUB		subtract two messages.		(not implemented yet)
	COpSUB1 	subtract 1 from a message.

**/

#include	"compiler.h"
#if ( LATTICEC || CBELLMTS || SUN3 || MPWC )
#include	<ctype.h>
#endif
#include	"utility.h"
#include	"core.ext"
#include	"cfops.def"

extern	char *GOutBuff;


/******************************

GetCOpCd	Get node for classifier operator, given an integer code number.	

	Code	Code for operator to be found.

	 RetVal	Set to ERROR if Code not found, else set to OK.

	Return:	Pointer to node for specified Code and RetVal set OK, if found, or
			Pointer to default node if Code not found, and set RetVal = ERROR.

******/

struct CfOpNode *GetCOpCd ( Code, RetVal )
	unsigned int	Code;
	int 			*RetVal;
{
	int		nodenum;
	struct CfOpNode *optr;
	
	for ( nodenum = 0, optr = CfOps; nodenum <= CFOPSMX; ++nodenum, ++optr )
		if ( optr->CfOpCode == Code )
			break;

	if ( nodenum > CFOPSMX ) {
		optr = COpDflt;
		*RetVal = ERROR;
	}
	else
		*RetVal = OK;

	return( optr );

} /* GetCOpCd */


/******************************

GetCOpNa	Get node for classifier operator, given its "name".

	Name	Name character array containing 'name' of operator to be found.	
			It must be a NULL terminated string.
			Note that Name must match stored CfOpName exactly (including case).

	 RetVal	Set to ERROR if Name not found, else set to OK.

	Return	Pointer to node found for Name, ans set RetVal to OK, or
			Pointer to default node if Name not found, and set RetVal to ERROR.

******/

struct CfOpNode *GetCOpNa ( Name, RetVal )
	char	Name[];
	int		*RetVal;
{
	register	int	 nodenum;
	char		*t, *s;					 /* t for test, s for stored */
	struct CfOpNode *optr;

	for ( nodenum=0, optr = CfOps; nodenum <= CFOPSMX; ++nodenum, ++optr ) {
		for ( t=Name, s=optr->CfOpName; *t == *s && *s != '\0'; ++t, ++s )
			;
		if ( *t == '\0' && ( *s == '\0' || *s == ' ' ) )
			break;
	}

	if ( nodenum > CFOPSMX ) {
		optr = COpDflt; 	
		*RetVal = ERROR;
	}
	else
		*RetVal = OK;

	return( optr );

} /* GetCOpNa */


/******************************

COpPASS - classifier operator for "pass-through".



******/ 

VOID COpPASS ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register int i;

	for ( i = 0; i < INTPRSTR; ++i)
		ResMsg->Message[i] = ( ( Msg1->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] ); 

} /* COpPASS */

/******************************

COpAND - classifier operator for logical add.
	
	AND the messages that match the classifiers conditions (Msg1 and Msg2),
	Then use that resultant string as input to PASS operator, using action part of classifier.

******/ 

VOID COpAND ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register int i;

	if ( Msg2 != NULL ) 
		for ( i = 0; i < INTPRSTR; ++i) {
			ResMsg->Message[i] = ( Msg1->Message[i] & Msg2->Message[i] );
			ResMsg->Message[i] = ( ( ResMsg->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] );
		}
	else
		for ( i = 0; i < INTPRSTR; ++i)
			ResMsg->Message[i] = Msg1->Message[i];

} /* COpAND */


/******************************

COpOR - classif+er operator for logical OR.
	
	OR the messages that match the classifiers conditions (Msg1 and Msg2),
	Then use that resultant string as input to PASS operator, using action part of classifier.

******/ 

VOID COpOR	( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register int i;

	if ( Msg2 != NULL )
		for ( i = 0; i < INTPRSTR; ++i) {
			ResMsg->Message[i] = ( Msg1->Message[i] | Msg2->Message[i] );
			ResMsg->Message[i] = ( ( ResMsg->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] );
		}
	else
		for ( i = 0; i < INTPRSTR; ++i)
			ResMsg->Message[i] = Msg1->Message[i];

} /* COpOR */


/******************************
COpXOR - classifier operator for logical "exclusive OR".
	
	XOR the messages that match the classifiers conditions (Msg1 and Msg2),
	Then use that resultant string as input to PASS operator, using action part of classifier.

******/ 

VOID COpXOR ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register int i;

	if ( Msg2 != NULL )
		for ( i = 0; i < INTPRSTR; ++i) {
			ResMsg->Message[i] = ( Msg1->Message[i] ^ Msg2->Message[i] );
			ResMsg->Message[i] = ( ( ResMsg->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] );
		}
	else
		for ( i = 0; i < INTPRSTR; ++i)
			ResMsg->Message[i] = Msg1->Message[i];

} /* COpXOR */


/******************************

COpADD - classifier operator for arithmetic add.

**** NOT IMPLEMENTED YET - just uses PASS ***

******/ 

VOID COpADD ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register	int	i;

	sprintf( GOutBuff, "\n\nCf %u has ADD operator: using PASS", Cf->Cf_Id );
	WriteStd( GOutBuff );

	for ( i = 0; i < INTPRSTR; ++i)
		ResMsg->Message[i] = ( ( Msg1->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] ); 

} /* COpADD */


/******************************

COpADD1 - classifier operator for arithmetic increment ("add 1").
	
	ADD1 to right-most	ADD1SZ loci of Msg1 message.
	ADD1SZ <= LOCIPRI and INTSZ for the given machine.

	Then use that resultant string as input to PASS operator, using action part of classifier.

******/ 

VOID COpADD1 ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register int i;
	char	msgbuf[STRNGSZ+1];
	
	BMsgtoA( Msg1->Message, msgbuf );

	for ( i = STRNGMX; i > STRNGMX-ADD1SZ; --i ) {
		if ( msgbuf[i] == '0' ) {
			msgbuf[i] = '1';
			break;
		}
		else
			msgbuf[i] = '0';
	}

	AMsgtoB( msgbuf, ResMsg->Message );

	for ( i = 0; i < INTPRSTR; ++i)
		ResMsg->Message[i] = ( ( ResMsg->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] );

} /* COpADD1 */


/******************************

COpSUB - classifier operator for arithmetic subtraction.

	**** NOT IMPLEMENTED YET - just uses PASS ***

******/ 

VOID COpSUB ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register	int	i;

	sprintf( GOutBuff, "\n\nCf %u has SUB operator: using PASS\n\n", Cf->Cf_Id );
	WriteStd( GOutBuff );

	for ( i = 0; i < INTPRSTR; ++i)
		ResMsg->Message[i] = ( ( Msg1->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] ); 

} /* COpSUB */


/******************************

COpSUB1 - classifier operator for arithmetic decrement ("subtract 1").


**** NOT IMPLEMENTED YET - just uses PASS ***

******/ 

VOID COpSUB1 ( Cf, Msg1, Msg2, ResMsg )
	struct	CfNode *Cf;
	struct MsgNode *Msg1, *Msg2, *ResMsg;
{
	register int i;
	char	msgbuf[STRNGSZ+1];
	
	BMsgtoA( Msg1->Message, msgbuf );

	for ( i = STRNGMX; i > STRNGMX-SUB1SZ; --i ) {
		if ( msgbuf[i] == '1' ) {
			msgbuf[i] = '0';
			break;
		}
		else
			msgbuf[i] = '1';
	}

	AMsgtoB( msgbuf, ResMsg->Message );

	for ( i = 0; i < INTPRSTR; ++i)
		ResMsg->Message[i] = ( ( ResMsg->Message[i] & Cf->ActDCs[i] ) ^ Cf->ActBits[i] );

} /* COpSUB1 */

