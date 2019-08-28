
/*		 GRANDCFS	for the CFS-C Classifier System

This file, GRANDCFS.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file can be called to to generate random classifiers
for the CFS-C classifier system.

The subroutines:

	GnRndCfs	generate random list of classifiers (process user's GRCFS command).
	GnRndCf 	generate 1 random cf.
	GnRndCA 	generate a random condition/action string and type (match or not/cf action-type).
	GtCOpRnd	generate a random cf operator (action type)

**/

#include	"config.h"

#include	"utility.h"
#include	"core.ext"
#include	"cfops.ext"

#include	"grandcfs.h"

extern unsigned int DemoLev;
extern char *GOutBuff;

#if MPWC
#define __SEG__ CFSCUTIL
#endif


/***************************************

GnRndCfs		Generate random classifiers and put them on the classifier list.

	ParString	Parameters to control generation; Null terminated string of the form:
				 [ HowMany [, MeanStr [, MeanBR [, ra [, i]]]]]
				where:
				 HowMany	the number to generate (default = 100)
				 MeanStr	mean strength for classifiers (default = CfStrDf)
				 MeanBR		mean BidRatio for classifiers (default = each gets its specifity)
				 ra			if literal string is present means generate action-types at random.
							(default = generate only PASS actions)
				 i			literal i => initialize list, else dont

******/

VOID 
GnRndCfs (ParString)
    char *ParString;
{

#if CRANDCFS
    int howmany, count, err, oldnmcfs;
    short passflag = TRUE;		/* Generate only PASS actions */
    float meanstr, meanBR;
    char *bufptr, *GetFloat (), *GetUInt ();
    extern float CfStrDf, SysTreas;
    extern unsigned int NmCfsMx;

    oldnmcfs = NmCfs;

    bufptr = GetUInt (ParString, &howmany, 100, ",", &err);
    if (err) {
	sprintf (GOutBuff, "\nWARNING (GnRndCfs): Illegal number to generate. Making %d.", howmany);
	WriteStd (GOutBuff);
    }
    bufptr = GetFloat (bufptr, &meanstr, CfStrDf, ",", &err);
    if (err)
	WriteStd ("\nWARNING (GnRndCfs): Illegal MeanStr value. Using default.");
    bufptr = GetFloat (bufptr, &meanBR, -1.0, ",", &err);	/* Use -1.0 to indicate none specified */
    if (err)
	WriteStd ("\nWARNING (GnRndCfs): Illegal MeanBR value. Using default.");

    while (*bufptr == ' ')
	++bufptr;			/* skip blanks */
    if (*bufptr == 'r') {
	passflag = FALSE;		/* random actions */
	while (*bufptr != ',' && *bufptr != '\0')	/* get to end or next par */
	    ++bufptr;
    }
    if (*bufptr == ',') {
	++bufptr;			/* past comma */
	while (*bufptr == ' ')
	    ++bufptr;			/* and blanks */
	if (*bufptr == 'i')
	    InitCfLs ();		/* Initialize the Cf list */
    }
    if (howmany > (CFLSTSZ - NmCfs)) {
	if (oldnmcfs > 0)
	    sprintf (GOutBuff, "\nWARNING: %d more wont' fit on list. Making %d.", howmany, CFLSTSZ - NmCfs);
	else
	    sprintf (GOutBuff, "\nWARNING (GnRndCfs): %d wont' fit on list. Making %d.", howmany, CFLSTSZ);
	WriteStd (GOutBuff);
	howmany = CFLSTSZ - NmCfs;
    }
    SysTreas = CfStrDf * howmany * 3;	/* Initialize the treasury */

    for (count = 0; count < howmany; ++count)	/* Fill it with random classifiers */
	GenRndCf (meanstr, meanBR, passflag);

    NmCfsMx = NmCfs;			/* maximum number is actual number */
    if (NmCfs != 0) {			/* recompute these averages */

#if SUN3				/* kludge for SUN compiler bug */
	AveCfStr = TotCfStr / (int) NmCfs;
	AveCfBR = TotCfBR / (int) NmCfs;
#else
	AveCfStr = TotCfStr / NmCfs;
	AveCfBR = TotCfBR / NmCfs;
#endif
    }
    if (oldnmcfs == 0)
	sprintf (GOutBuff, "\nDone. Generated %d random classifiers.\n", NmCfs);
    else
	sprintf (GOutBuff, "\nDone. Generated %d random classifiers. Total now %d.\n", count, NmCfs);
    WriteStd (GOutBuff);
#endif /* CRANDCFS	*/
}					/* GnRndCfs  */


#if CRANDCFS
/***************************************


GenRndCf		Generate a random classifier and add it to the classifier list.

	MeanStr		Set classifier strength = MeanStr + Noise

	MeanBR		If -1, set classifier BidRatio to its specificity.
				Else set it to MeanBR + Noise.

	PassFlag	TRUE	use PASS action type (operator).
				FALSE	select action type at random from those available.

******/

int 
GenRndCf (MeanStr, MeanBR, PassFlag)
    float MeanStr, MeanBR;
    int PassFlag;
{
    float strength, bidratio, UNoise (), CalcSpec ();
    unsigned int cnd1type, cnd2type, acttype;
    char cond1[STRNGSZ + 1], cond2[STRNGSZ + 1], action[STRNGSZ + 1];

    GnRndCA (cond1, &cnd1type, 'c');
    cnd1type = CMATCH;			/* Condition 1 can only be MATCH */
    GnRndCA (cond2, &cnd2type, 'c');
    if (PassFlag)
	GnRndCA (action, &acttype, 'p');/* Just get PASS ActTypes */
    else
	GnRndCA (action, &acttype, 'a');/* Get random ActType */

    strength = MeanStr + UNoise ((MeanStr / 20));
    if (MeanBR == -1.0)			/* Set BidRatio = specificity */
	bidratio = CalcSpec (cond1, cond2, cnd1type, cnd2type);
    else
	bidratio = MeanBR + UNoise ((MeanBR / 20));	/* Set bidratio to supplied value plus some noise */

   /* Finally, add a classifier with all these values to classifier list. */

    if (AddCf (cond1, cond2, action, cnd1type, cnd2type, acttype, 0, strength, bidratio,
	       0.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) == ERROR)
	return (ERROR);

    return (OK);

}					/* GenRndCf */


/***************************************

GnRndCA 	Generate a random condition or action string and 'type'.

	String	Buffer for the string (plus room for a \0 at the end)?>

	Type	Type of condition (MATCH or NOTMATCH), or for an action, its ActType (PASS, etc.)

	What	'c' = generate a condition-type
			'p' = generate an action with PASS ActType.
			'a' = generate an action with random ActType.

******/

VOID 
GnRndCA (String, Type, What)
    char String[];
    unsigned int *Type;
    char What;
{
    int loci, err;
    struct CfOpNode *atptr, *GetCOpNa (), *GtCOpRnd ();

    for (loci = 0; loci <= STRNGMX; ++loci) {
	if (URand01 () <= GRCFrNSL)	/* Test vs. fraction Non-Specific loci (#'s) */
	    String[loci] = '#';
	else {
	    if (URand01 () < 0.5)	/* Test vs. half of rest of probability */
		String[loci] = '1';
	    else
		String[loci] = '0';
	}
    }
    String[loci] = '\0';

    if (What == 'c') {			/* get the condition type */
	if (URand01 () <= GRCFrMC)
	    *Type = CMATCH;
	else
	    *Type = CNOTMATCH;
    } else {
	if (What == 'p')		/* Get pointer to PASS CfOpNode */
	    atptr = GetCOpNa ("PASS", &err);
	else
	    atptr = GtCOpRnd ();	/* Get one at random */

	*Type = atptr -> CfOpCode;
    }

}					/* GnRndCA */


/***************************************

GtCOpRnd		Get classifier operator (ActType) by random draw.

	Return:		Pointer to CfOpNode for operator to use.

The CfOpCPrb member of the CfNode's in the CfOps array defines the
cummulative probability distribution used to bias this draw of a
classifier operator.

******/

struct CfOpNode *
GtCOpRnd ()
{
    struct CfOpNode *optr;
    float randnum;
    extern struct CfOpNode CfOps[];

    randnum = URand01 ();		/* 0 <= randnum <= 1 */

    for (optr = CfOps; optr -> CfOpCode != OC_ENDL; ++optr)	/* Keep looking until... */
	if (randnum <= optr -> CfOpCPrb)/* cummulative prob <= randnum */
	    break;

    if (optr -> CfOpCode == OC_ENDL) {	/* just in case ... */
	WriteStd ("\nBUG (GtCOpRnd): invalid cummulative prob. over Cf Operators. Using PASS.\n");
	optr = CfOps;			/* PASS better be first! */
    }
    return (optr);

}					/* GtCOpRnd */

#endif /*	CRANDCFS */
