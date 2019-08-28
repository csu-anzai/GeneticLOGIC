
/*		DSCOPS	for the CFS-C Classifier System

This file, DSCOPS.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file implement various discovery learning "operators"
used by the CFS-C classifier system.
That is, these subroutines take as input one or more classifiers and produce
new classifiers that are modified versions of the input classifiers.
The subroutines:

	CDMsgs		cover detector messages
	PkPrntCD	pick parent for CDMsgs
	CfMtchS 	get 'match-score' for (MATCH only) classifier, message.
	CndMtchS	get 'match-score' for (MATCH only) condition, classifier
	GnrlzCf 	generalize (minimally) classifier so it matches message
	CDMGeo	  Modify classifier to cover detector message, George's algorithm.
				Change condition specific (0,1) bits to make conditions match message.

	DscBkgGA	dicovery background genetic algorithm
	CrFullCf	single cross in a full classifier.
	DCrFullCf	"Double" Crossover of classifier considered as one long string.
	CrCfStrg	Cross one string-part (condition or action) of two classifiers.
	RCrossSt	randomly cross to strings.
	ReprodCf	Reproduce (copy) a classifier.

	Mutate	  mutate a (new) classifier.
	MutatCA 	mutate one condition/action string.
**/

#include	"config.h"

#include	"utility.h"
#include	"core.ext"
#include	"cfops.ext"
#include	"dsclearn.ext"

#include	"dscops.h"

extern short int GenFlg;
extern unsigned int DemoLev;
extern char *GOutBuff;

#if MPWC
#define __SEG__ CFSCDSC
#endif

#if CDSC_ALL
/*****************************

DscCDM		Discovery--Cover Detector Messages.

Find first message from a detector (left loci 00) that was unmatched in the
current step and generate a new classifier that will 'cover' (match) it.

See CFS-C documentation for an overview of this.

******/

VOID 
DscCDM ()
{
    int mcnt, numcfgen, gencond, i;
    struct MsgNode *mptr;
    struct NCfNode *new;

#if CTEST
    if (DscDemo > 0)
	WriteStd ("\nChk for Det Msgs to cover.");
#endif

    for (mptr = CurMsgs, mcnt = numcfgen = 0; mcnt <= LastCMsg && NmNewCfs < MxNewCfs; ++mptr, ++mcnt) {

#if CTEST
	if (DscDemo >= 2) {
	    sprintf (GOutBuff, "\nCDM? Msg %d, FromDet? %d, IsMsg? %d  MsgMtch? %d",
		     mptr -> Msg_Id, mptr -> FromDet, mptr -> IsMsg, mptr -> MsgMtchd);
	    WriteStd (GOutBuff);
	}
#endif

	if (mptr -> FromDet && mptr -> IsMsg && !mptr -> MsgMtchd) {	/* lets cover this one */

#if CTEST
	    if (DscDemo > 0) {
		BMsgtoA (mptr -> Message, GCfBuff);	/* get the ascii form of the message */
		*(GCfBuff + STRNGSZ) = '\0';
		sprintf (GOutBuff, "\nCDMsg %d '%s'. Parent ", mptr -> Msg_Id, GCfBuff);
		WriteStd (GOutBuff);
	    }
#endif

	    if (CDMsgs == 2) {		/* George's algorithm: pick hi strength cf */

#if CTEST
		if (DscDemo > 0)
		    WriteStd ("(Geo hi strength):");
#endif

		gencond = PckPrnts (1);
		if (gencond < 1) {
		    sprintf (GOutBuff, "\nDscCDM: Geo PkPrnts didn't get one (step %d)\n",
			     CycleStp);
		    WriteStd (GOutBuff);
		    continue;
		}

#if GENLOG
		if (GenFlg) {
		    BMsgtoA (mptr -> Message, GMsgBuff);
		    *(GMsgBuff + STRNGSZ) = '\0';
		    sprintf (GOutBuff, "Step %d (CDM): '%s' P(G-HI-STR) %d ", CycleStp, GMsgBuff, Parents[0] -> Cf_Id);
		    WriteGen (GOutBuff);
		}
#endif
	    } else if (NmCfWon > 0) {	/* if some cfs produced msgs, just look at bidders */

#if CTEST
		if (DscDemo > 0)
		    WriteStd ("(select from bidders):");
#endif

		gencond = PkPrntCD (mptr, PKBDSPNT);	/* put pointer in Parents, ret closest condition */
		if (gencond < 0) {
		    sprintf (GOutBuff, "\nDscCDM: PkPrntCD(PKPDSPNT) didn't get parent (step %d)\n",
			     CycleStp);
		    WriteStd (GOutBuff);
		    continue;
		}

#if GENLOG
		if (GenFlg) {
		    BMsgtoA (mptr -> Message, GMsgBuff);
		    *(GMsgBuff + STRNGSZ) = '\0';
		    sprintf (GOutBuff, "Step %d (CDM): '%s' P(BID-STR) %d ", CycleStp, GMsgBuff, Parents[0] -> Cf_Id);
		    WriteGen (GOutBuff);
		}
#endif
	    } else {			/* no classifiers won, so search whole cflist for close match */

#if CTEST
		if (DscDemo > 0)
		    WriteStd ("(select close match):");
#endif

		gencond = PkPrntCD (mptr, PKCL1PNT);	/* put pointer in Parents, ret closest condition */
		if (gencond < 0) {
		    sprintf (GOutBuff, "\nDscCDM: PkPrntCD(PKCL1PNT) didn't get parent (step %d)\n",
			     CycleStp);
		    WriteStd (GOutBuff);
		    continue;
		}

#if GENLOG
		if (GenFlg) {
		    BMsgtoA (mptr -> Message, GMsgBuff);
		    *(GMsgBuff + STRNGSZ) = '\0';
		    sprintf (GOutBuff, "Step %d (CDM): '%s' P(ALL-CL1) %d ", CycleStp, GMsgBuff, Parents[0] -> Cf_Id);
		    WriteGen (GOutBuff);
		}
#endif
	    }

#if CTEST
	    if (DscDemo > 0) {
		PutCNd (Parents[0], GCfBuff, DEMOFMT1);
		sprintf (GOutBuff, "\n  %sOffspring:\n  ", GCfBuff);
		WriteStd (GOutBuff);
	    }
#endif

	    new = ReprodCf (Parents[0]);/* reproduce the parent, maybe mutate it */

	    if (CDMsgs == 1)		/* generalize condition(s) to cover amsg */
		GnrlzCf (mptr, new, gencond);	/* gencond indicates conds to generalize--set by PckPrnCD() */
	    else			/* Use George's algorithm to cover */
		CDMGeo (mptr, new);

	    if (CDMsgAct == 1) {	/* generate a random action */
		GnRndCA (GMsgBuff, &i, 'c');	/* generate random string */
		AActtoB (GMsgBuff, new -> NewActB, new -> NewActD);

#if CTEST
		if (DscDemo > 0)
		    WriteStd ("  [Generating a random action.]\n");
#endif
	    }

#if GENLOG
	    if (GenFlg) {
		WriteGen ("\n  ");
		WrtNCf (new, GOutBuff, 1);
		WriteGen (GOutBuff);
	    }
#endif

#if CTEST
	    if (DscDemo > 0) {
		WrtNCf (new, GOutBuff, 1);
		WriteStd (GOutBuff);
	    }
#endif

	    ++numcfgen;
	    break;			/* **** Just generate one / step ****** */
	}				/* it was an unused detector message */
    }

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "CDMsgs generated %d cfs\n", numcfgen);
	WriteStd (GOutBuff);
    }
#endif

    TotCDM += numcfgen;

}					/* DscCDM */


/*****************************

PkPrntCD		Pick parent classifier for Cover Detector discovery operator.

	Msg			Binary form of message to cover (match).

	How			How to pick parents:
				PKBDSPNT -- pick from cfs that bid this step, the highest strength.
				PKCL1PNT -- pick from cflist, the closest genotype.

	Return		Integer indicating which condition to generalize, namely
				1 (left), 2 (right), or 3 (both).
				Return -1 to indicate no parent picked for some reason.

				Put pointer to classifier in the Parents[0] array.

*******/

int 
PkPrntCD (Msg, How)
    struct MsgNode *Msg;
    int How;
{
    register cfcnt;
    int ret, tret, bstscore, tstscore;
    register struct CfNode *tstcf, *bstcf;

    if (!FitnessCalc)
	ReCalcFitness ();

    if (How == PKBDSPNT) {
	if (CandCfs == NULL) {
	    sprintf (GOutBuff, "\nPkPrntCD: no bidders (step %d)\n", CycleStp);
	    WriteStd (GOutBuff);
	    return (-1);
	}
	bstcf = CandCfs;		/* assume first one is best */
	bstscore = bstcf -> Strength;
	for (tstcf = bstcf -> NxtCand; tstcf != NULL; tstcf = tstcf -> NxtCand) {
	    tstscore = tstcf -> Strength;
	    if (tstscore > bstscore) {
		bstcf = tstcf;
		bstscore = tstscore;
	    }
	}

       /* Now figure out which condition to generalize--since these bid, we need only generalize one cond. */

	bstscore = CndMtchS (Msg -> Message, bstcf -> Cnd1Bits, bstcf -> Cnd1DCs, bstcf -> Cnd1Type);
	tstscore = CndMtchS (Msg -> Message, bstcf -> Cnd2Bits, bstcf -> Cnd2DCs, bstcf -> Cnd2Type);
	if (bstscore >= tstscore)
	    ret = 1;
	else
	    ret = 2;
    }
    /* end How is PKBDSPNT */ 
    else {				/* How is PKCL1PNT */
	if (NmCfs == 0) {
	    sprintf (GOutBuff, "\nPkPrntCD: NmCfs=0 (step %d)\n", CycleStp);
	    WriteStd (GOutBuff);
	    return (-1);
	}
	bstcf = CurCfs;			/* assume first is best */
	bstscore = CfMtchS (Msg, bstcf, &ret);	/* get score and condition(s) to generalize */

	for (tstcf = bstcf + 1, cfcnt = 2; cfcnt <= NmCfs; ++tstcf, ++cfcnt) {
	    tstscore = CfMtchS (Msg, tstcf, &tret);	/* get score and condition(s) to generalize */
	    if (tstscore > bstscore) {	/* if test is better, keep it as new best */
		bstcf = tstcf;
		bstscore = tstscore;
		ret = tret;
	    } else if (tstscore == bstscore) {	/* if its a tie, do a random draw */
		if (URand01 () <= 0.5) {/* just do a coin flip */
		    bstcf = tstcf;	/* NOTE: since we are moving from low strength cfs to */
		    bstscore = tstscore;/* to high, this give hi strength classifier better chance */
		    ret = tret;
		}
	    }
	}

    }					/* How is CDALLCL1 */

    Parents[0] = bstcf;			/* bstcf should be pointer to parent to use */
    return (ret);			/* this should indicate which condition to generalize */

}					/* PkPrntCD	*/


/*****************************

CfMtchS 		Get score that indicates how close classifier matches a message.

	Msg			Binary form of message to test.

	Cf			Pointer to classifier to test.

	Cond			Pointer to int indicating which conditions DON'T match perfectly:
				1 (left), 2 (right), 3 (neither matches), or 4 (both MATCH perfectly).

	Return		Integer score, where 0 is worst match,
				2*STRNGSZ is is a true match, both conditions.

Score is sum of number of matching specific loci for BOTH conditions.

******/

int 
CfMtchS (Msg, Cf, Cond)
    struct MsgNode *Msg;
    struct CfNode *Cf;
    int *Cond;
{
    int mscore1, mscore2, retscore;

    mscore1 = CndMtchS (Msg -> Message, Cf -> Cnd1Bits, Cf -> Cnd1DCs, Cf -> Cnd1Type);
    mscore2 = CndMtchS (Msg -> Message, Cf -> Cnd2Bits, Cf -> Cnd2DCs, Cf -> Cnd2Type);

    retscore = mscore1 + mscore2;

    if (mscore1 == STRNGSZ)
	if (mscore2 == STRNGSZ)
	    *Cond = 4;			/* matches both conditions */
	else
	    *Cond = 2;			/* matches only first--generalize 2nd */
    else if (mscore2 == STRNGSZ)
	*Cond = 1;			/* matches only 2nd--generalize first */
    else
	*Cond = 3;			/* not match--generalize both */

    return (retscore);

}					/* CfMtchS */


/*****************************

CndMtchS		Get score that indicates how close condition matches a message.

	Msg			Binary form of message to test.

	CndBits		|- binary form of condition to test.
	CndDCs		|
	CndType		Type of condition (match or NOTmatch).

	Return		Integer score--0 is no match, STRNGSZ is is a true match.
				
******/

int 
CndMtchS (Msg, CndBits, CndDCs, CndType)
    unsigned int Msg[], CndBits[], CndDCs[], CndType;
{
    int ret, i;
    char amsg[STRNGSZ + 1], acond[STRNGSZ + 1];

    ret = 0;

    if (CndType == CMATCH) {
	BMsgtoA (Msg, amsg);
	BCndtoA (CndBits, CndDCs, acond);
	amsg[STRNGSZ] = acond[STRNGSZ] = '\0';

	for (i = 0; i < STRNGSZ; ++i)
	    if (acond[i] == '#')
		continue;		/* no score for don't cares */
	    else if (acond[i] == amsg[i])
		++ret;			/* specific loci match--increase score */
    } else {				/* condition type is NOTmatch */
	ret = 0;			/* currently these are get the lowest score */
    }

    return (ret);

}					/* CndMtchS */


/*****************************

GnrlzCf 		Generalize one or both conditions of a classifier.

	Msg			Pointer to message that condition(s) should match after generalization.

	NewCf 		Pointer to NCfNode for classifier to be generalized.

	WhichCnd	Which condition to generalize: 1 (left), 2 (right), or 3 (both).

******/

VOID 
GnrlzCf (Msg, NewCf, WhichCnd)
    struct MsgNode *Msg;
    struct NCfNode *NewCf;
    int WhichCnd;
{
    register int i;
    short int numgen1, numgen2;

    numgen1 = numgen2 = 0;
    *(GMsgBuff + STRNGSZ) = *(GCfBuff + STRNGSZ) = '\0';
    BMsgtoA (Msg -> Message, GMsgBuff);

    if (WhichCnd == 1 || WhichCnd == 3) {
	BCndtoA (NewCf -> NewCnd1B, NewCf -> NewCnd1D, GCfBuff);
	for (i = 0; i <= STRNGMX; ++i)	/* generalize the best-match condition */
	    if (*(GCfBuff + i) != '#' && *(GCfBuff + i) != *(GMsgBuff + i)) {
		*(GCfBuff + i) = '#';
		++numgen1;
	    }
	ACndtoB (GCfBuff, NewCf -> NewCnd1B, NewCf -> NewCnd1D);

#if GENLOG
	if (GenFlg) {
	    sprintf (GOutBuff, "\n  Gnrlz C1 (%d loci).", numgen1);
	    WriteGen (GOutBuff);
	}
#endif

#if CTEST
	if (DscDemo > 1) {
	    sprintf (GOutBuff, "  |- Genrlz Cond 1 (%d loci).\n  ", numgen1);
	    WriteStd (GOutBuff);
	}
#endif
    }
    if (WhichCnd == 2 || WhichCnd == 3) {
	BCndtoA (NewCf -> NewCnd2B, NewCf -> NewCnd2D, GCfBuff);
	if (NewCf -> NewCnd2T == CMATCH) {
	    for (i = 0; i <= STRNGMX; ++i)
		if (*(GCfBuff + i) != '#' && *(GCfBuff + i) != *(GMsgBuff + i)) {
		    *(GCfBuff + i) = '#';
		    ++numgen2;
		}
	} else {			/* 2nd condition is NOT ! */
	    for (i = 0; i <= STRNGMX; ++i)
		if (*(GCfBuff + i) == '#') {
		    if (*(GMsgBuff + i) == '0')
			*(GCfBuff + i) = '1';
		    else
			*(GCfBuff + i) = '0';
		    ++numgen2;
		}
	}
	ACndtoB (GCfBuff, NewCf -> NewCnd2B, NewCf -> NewCnd2D);

#if GENLOG
	if (GenFlg) {
	    sprintf (GOutBuff, "\n  Gnrlz C2 (%d loci).", numgen2);
	    WriteGen (GOutBuff);
	}
#endif

#if CTEST
	if (DscDemo > 1) {
	    sprintf (GOutBuff, "  |- Genrlz Cond 2 (%d loci).\n  ", numgen2);
	    WriteStd (GOutBuff);
	}
#endif
    }
    i = numgen1 + numgen2;
    NewCf -> NewBR = NewCf -> NewBR - (i * MutdSpec);	/* recalc bidratio (specificity) */

    TotCDML += i;			/* total count of loci changed */
    if (numgen1 > 0)
	++TotCDMC;			/* total number of conditions generalized */
    if (numgen2 > 0)
	++TotCDMC;

}					/* GnrlzCf */


/*****************************

CDMGeo			Modify classifier to cover detector message, George's algorithm.
				Change condition specific (0,1) bits to make conditions match message.

	Msg			Pointer to message that condition(s) should match after change.

	NewCf 		Pointer to NCfNode for classifier to be changed.

******/

VOID 
CDMGeo (Msg, NewCf)
    struct MsgNode *Msg;
    struct NCfNode *NewCf;
{
    register int i;
    int numch1, numch2;

    numch1 = numch2 = 0;
    *(GMsgBuff + STRNGSZ) = *(GCfBuff + STRNGSZ) = '\0';
    BMsgtoA (Msg -> Message, GMsgBuff);

    BCndtoA (NewCf -> NewCnd1B, NewCf -> NewCnd1D, GCfBuff);
    for (i = 0; i <= STRNGMX; ++i)
	if (*(GCfBuff + i) != '#' && *(GCfBuff + i) != *(GMsgBuff + i)) {
	    *(GCfBuff + i) = *(GMsgBuff + i);	/* change cf to match message */
	    ++numch1;
	}
    ACndtoB (GCfBuff, NewCf -> NewCnd1B, NewCf -> NewCnd1D);

#if GENLOG
    if (GenFlg) {
	sprintf (GOutBuff, "\n  GeoCh C1 (%d loci).", numch1);
	WriteGen (GOutBuff);
    }
#endif

#if CTEST
    if (DscDemo > 1) {
	sprintf (GOutBuff, "  |- GeoCh Cond 1 (%d loci).\n  ", numch1);
	WriteStd (GOutBuff);
    }
#endif

    BCndtoA (NewCf -> NewCnd2B, NewCf -> NewCnd2D, GCfBuff);
    if (NewCf -> NewCnd2T == CMATCH) {
	for (i = 0; i <= STRNGMX; ++i)
	    if (*(GCfBuff + i) != '#' && *(GCfBuff + i) != *(GMsgBuff + i)) {
		*(GCfBuff + i) = *(GMsgBuff + i);
		++numch2;
	    }
    } else {				/* 2nd condition is NOT match */
	for (i = 0; i <= STRNGMX; ++i)
	    if (*(GCfBuff + i) != '#') {
		if (*(GMsgBuff + i) == '0')
		    *(GCfBuff + i) = '1';
		else
		    *(GCfBuff + i) = '0';
		++numch2;
		break;			/* just need to change 1! */
	    }
    }

    ACndtoB (GCfBuff, NewCf -> NewCnd2B, NewCf -> NewCnd2D);

#if GENLOG
    if (GenFlg) {
	sprintf (GOutBuff, "\n  GeoCh C2 to (%d loci).", numch2);
	WriteGen (GOutBuff);
    }
#endif

#if CTEST
    if (DscDemo > 1) {
	sprintf (GOutBuff, "  |- GeoCh Cond 2 (%d loci):\n  ", numch2);
	WriteStd (GOutBuff);
    }
#endif

    TotCDML += numch1 + numch2;		/* total count of loci changed */
    if (numch1 > 0)
	++TotCDMC;
    if (numch2 > 0)
	++TotCDMC;

}					/* CDMGeo */


/*****************************

DscBkgGA	Discovery 'background' genetic algorithm.

	This produces as many new classifiers as there is still room to produce
	during the current step. Since the other discovery operators may have
	produced the maximum number of new classifiers allowed per step, it could
	be that this will produce none. There must be room for two new ones for
	this to be applied.

	This basically gets a pool of parents to use for creating all new offspring.
	Depending on variable settings, this pool may be picked from the full list
	or just from bidding classifiers.

	For each pair of parents, they are copied and then crossover may be applied
	or not, depending on the XOVer rate. All offspring are also subject to mutation.

******/

VOID 
DscBkgGA ()
{
    float randnum;
    int nmparent;
    struct NCfNode *new1, *new2;	/* pointers to new nodes */

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "\nApply BkgGA to create %d new cfs", (MxNewCfs - NmNewCfs));
	WriteStd (GOutBuff);
    }
#endif

    if (MxNewCfs < 2) {
	WriteStd ("\nDscBkgGA: MxNewCfs < 2\n");
	MxNewCfs = 2;
    }
   /* First get a pool of parent classifiers. */

    if (URand01 () < BGABPPr) {
	nmparent = PkBPrnts (0, 'm');	/* Pick Hi bidders, then Hi Str, to FrNewCfs */
	++TotBGABP;
    } else
	nmparent = PckPrnts (0);	/* Pick high-strength parents */

   /* Now pick from the pool as need be, until max number of new cfs produced */

    while (NmNewCfs < MxNewCfs - 1) {	/* There must be at least 2 left! */
	nmparent = PckPrnts (2);	/* Get next parents from pool in Parents[0] and Parents[1] */
	if (nmparent != 2) {
	    sprintf (GOutBuff, "\n***DscBkgGA: nmparent < 2 (CycleStp %u)", CycleStp);
	    WriteStd (GOutBuff);
	    break;
	}
	++TotBkgGA;			/* increment the total counter */

#if CTEST
	if (DscDemo > 0) {
	    PutCNd (Parents[0], GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "\nThe Parents:\n  %s", GCfBuff);
	    WriteStd (GOutBuff);
	    PutCNd (Parents[1], GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "  %s", GCfBuff);
	    WriteStd (GOutBuff);
	}
#endif

#if GENLOG
	if (GenFlg) {
	    sprintf (GOutBuff, "Step %d (BkgGA): P %d, %d; ", CycleStp, Parents[0] -> Cf_Id, Parents[1] -> Cf_Id);
	    WriteGen (GOutBuff);
	}
#endif

	new1 = ReprodCf (Parents[0]);	/* make a copy in next NewCfs node, increment NmNewCfs */
	new2 = ReprodCf (Parents[1]);	/* and for this parent */
	if (NwCfStrV == NWSTRAP) {	/* new strength is average of parents */
	    new1 -> NewStr = (Parents[0] -> Strength + Parents[1] -> Strength) / 2;
	    new2 -> NewStr = new1 -> NewStr;
	}
	if ((randnum = URand01 ()) < BGASCPr) {
	    CrCfStrg (new1, new2);	/* Cross one string of the offspring */
	    ++TotBGASC;			/* Increment counter. */
	} else if (randnum < BGAFCPr) {	/* Full cross of classifier */

#if SINGLEXO
	    CrFullCf (new1, new2);	/* single crossover  */
#else
	    DCrFullCf (new1, new2);	/* double crossover */
#endif

	    ++TotBGAFC;			/* Increment counter. */
	} else {			/* No cross -- just use copies */
	    ++TotBGANC;			/* Increment counter. */

#if GENLOG
	    if (GenFlg)
		WriteGen ("DUP. ");
#endif

#if CTEST
	    if (DscDemo > 0)
		WriteStd ("No Xover-just copy\n");
#endif
	}

#if CTEST
	if (DscDemo > 0) {
	    WriteStd ("\nThe offspring:\n");
	    WrtNCf (new1, GOutBuff, 1);
	    WriteStd (GOutBuff);
	    WrtNCf (new2, GOutBuff, 1);
	    WriteStd (GOutBuff);
	}
#endif

#if GENLOG
	if (GenFlg) {
	    WriteGen ("The offspring:\n  ");
	    WrtNCf (new1, GOutBuff, 1);
	    WriteGen (GOutBuff);
	    WriteGen ("  ");
	    WrtNCf (new2, GOutBuff, 1);
	    WriteGen (GOutBuff);
	}
#endif

    }					/* more room to create pairs of new cfs */

}					/* DscBkgGA */


#if SINGLEXO				/* Set SINGLEXO to 1 (in UTILITY.H) to use this instead of DCrFullCf() */
/*****************************

CrFullCf		Cross full (NEW) classifier.

	CfPtr1		Pointer to first classifier.
	CfPtr2		Pointer to second classifier.

This function considers classifier as a string of size STRNGSZ*3+2, e.g.:
	1111000011110000m1111000011110000p1111000011110000
i.e., condtion1, condition2-type, condtion2, action-type, and action.
The cross occurs somewhere in this long string.

Note that this single crossover operator makes it almost certain
that alleles at the far ends of the classifier will be separated.
Compare this to the double crossover operator defined below,
which makes it likely that alleles at the far ends of a classifier
will remain together. Other versions are also possible.

******/

VOID 
CrFullCf (CfPtr1, CfPtr2)
    struct NCfNode *CfPtr1, *CfPtr2;
{
    char string1[STRNGSZ], string2[STRNGSZ], tchar;
    register int crosspt, i;
    unsigned int bits[INTPRSTR], type;

    crosspt = Round ((STRNGSZ * 3) * URand01 ()) + 1;	/* 1 <= crosspt <= STRNGSZ*3 + 1 */

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "FullCross start at %d. ", crosspt);
	WriteStd (GOutBuff);
    }
#endif

#if GENLOG
    if (GenFlg) {
	sprintf (GOutBuff, " FC %d ", crosspt);
	WriteGen (GOutBuff);
    }
#endif

    if (crosspt >= (STRNGSZ * 2) + 2) {	/* cross-point is in action-string (right of action-type) */
	BActtoA (CfPtr1 -> NewActB, CfPtr1 -> NewActD, string1);	/* Cross the actions */
	BActtoA (CfPtr2 -> NewActB, CfPtr2 -> NewActD, string2);
	for (i = crosspt - (STRNGSZ * 2) - 2; i <= STRNGMX; ++i) {	/* cross them */
	    tchar = string1[i];
	    string1[i] = string2[i];
	    string2[i] = tchar;
	}
	AActtoB (string1, CfPtr1 -> NewActB, CfPtr1 -> NewActD);	/* store the crosses */
	AActtoB (string2, CfPtr2 -> NewActB, CfPtr2 -> NewActD);
    } else if (crosspt >= STRNGSZ + 1) {/* cross-point is in condition 2 or at action-type */
	if (crosspt != (STRNGSZ * 2) + 1) {
	    BCndtoA (CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D, string1);	/* Cross the second conditions */
	    BCndtoA (CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D, string2);
	    for (i = crosspt - STRNGSZ - 1; i <= STRNGMX; ++i) {
		tchar = string1[i];
		string1[i] = string2[i];
		string2[i] = tchar;
	    }
	    ACndtoB (string1, CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D);	/* store the crosses */
	    ACndtoB (string2, CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D);

	    BCndtoA (CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D, string2);	/* Recalc Bidratios ***NOTE tricks here*** */
	    CfPtr1 -> NewBR = CalcSpec (string2, string1, CMATCH, CfPtr1 -> NewCnd2T);

	    BCndtoA (CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D, string1);
	    BCndtoA (CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D, string2);
	    CfPtr2 -> NewBR = CalcSpec (string1, string2, CMATCH, CfPtr2 -> NewCnd2T);
	}
	type = CfPtr1 -> NewActT;	/* exchange the action-part */
	CfPtr1 -> NewActT = CfPtr2 -> NewActT;
	CfPtr2 -> NewActT = type;

	for (i = 0; i < INTPRSTR; ++i) {
	    bits[i] = CfPtr1 -> NewActB[i];	/* exchange action string, too. */
	    CfPtr1 -> NewActB[i] = CfPtr2 -> NewActB[i];
	    CfPtr2 -> NewActB[i] = bits[i];
	    bits[i] = CfPtr1 -> NewActD[i];
	    CfPtr1 -> NewActD[i] = CfPtr2 -> NewActD[i];
	    CfPtr2 -> NewActD[i] = bits[i];
	}
    } else {				/* cross is in first condition or at condtion2 type */
	if (crosspt == STRNGSZ) {	/* its in condition2 type, so exchange */
	    type = CfPtr1 -> NewCnd2T;	/* cond2type and all of condition 1 */
	    CfPtr1 -> NewCnd2T = CfPtr2 -> NewCnd2T;
	    CfPtr2 -> NewCnd2T = type;
	}
	BCndtoA (CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D, string1);	/* Cross the first conditions */
	BCndtoA (CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D, string2);
	for (i = 0; i < crosspt; ++i) {
	    tchar = string1[i];
	    string1[i] = string2[i];
	    string2[i] = tchar;
	}
	ACndtoB (string1, CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D);	/* store the crosses */
	ACndtoB (string2, CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D);

	BCndtoA (CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D, string2);	/* Recalculate Bidratios */
	CfPtr1 -> NewBR = CalcSpec (string1, string2, CMATCH, CfPtr1 -> NewCnd2T);

	BCndtoA (CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D, string1);
	BCndtoA (CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D, string2);
	CfPtr2 -> NewBR = CalcSpec (string1, string2, CMATCH, CfPtr2 -> NewCnd2T);
    }

}					/* CrFullCf */


#else /* SINGLEXO is off -- use double xover */


/*****************************

DCrFullCf	Cross full (NEW) classifier--using "double crossover".

	CfPtr1	Pointer to first classifier.
	CfPtr2	Pointer to second classifier.

This function considers classifier as a string of size STRNGSZ*3+2, e.g.:
	1111000011110000m1111000011110000p1111000011110000
i.e., condtion1, condition2-type, condtion2, action-type, and action.
The crossed section occurs somewhere in this long string.

******/

VOID 
DCrFullCf (CfPtr1, CfPtr2)
    struct NCfNode *CfPtr1, *CfPtr2;
{
    char tchar;
    register int i;
    short int xstart, xend, type;

   /*	We want xstart and xend to point to loci
			that start/end segment to cross, i.e.:
				0 <= xstart <= STRNGSZ*3 + 1
				xstart <= xend <= STRNGSZ*3 + 1
		*/

    xstart = URandN (((STRNGSZ * 3) + 1));	/* get two points at random */
    xend = URandN (((STRNGSZ * 3) + 1));
    if (xstart > xend) {		/* make sure xstart is lower */
	type = xend;
	xend = xstart;
	xstart = type;
    }

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "DFullX xstart/xend %d/%d. ", xstart, xend);
	WriteStd (GOutBuff);
    }
#endif

#if GENLOG
    if (GenFlg) {
	sprintf (GOutBuff, " DFC %d/%d ", xstart, xend);
	WriteGen (GOutBuff);
    }
#endif

   /* NOTE: Using GCfBuff and GMsgBuff for two classifiers--don't use them for anything else here. */

    BCndtoA (CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D, GCfBuff);
    *(GCfBuff + STRNGSZ) = 'm';		/* place for condition 2 type */
    BCndtoA (CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D, &GCfBuff[STRNGSZ + 1]);
    *(GCfBuff + (STRNGSZ * 2) + 1) = 'p';	/* place for action type */
    BActtoA (CfPtr1 -> NewActB, CfPtr1 -> NewActD, &GCfBuff[(STRNGSZ * 2) + 2]);
    *(GCfBuff + (STRNGSZ * 3) + 2) = '\0';

    BCndtoA (CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D, GMsgBuff);
    *(GMsgBuff + STRNGSZ) = 'm';	/* place for condition 2 type */
    BCndtoA (CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D, &GMsgBuff[STRNGSZ + 1]);
    *(GMsgBuff + (STRNGSZ * 2) + 1) = 'p';	/* place for action type */
    BActtoA (CfPtr2 -> NewActB, CfPtr2 -> NewActD, &GMsgBuff[(STRNGSZ * 2) + 2]);
    *(GMsgBuff + (STRNGSZ * 3) + 2) = '\0';

#if CTEST
    if (DscDemo > 0) {
	WriteStd ("\nParents:\n  ");
	WriteStd (GCfBuff);
	WriteStd ("\n  ");
	WriteStd (GMsgBuff);
    }
#endif

   /* do the cross */

    for (i = xstart; i <= xend; ++i) {
	tchar = *(GCfBuff + i);
	*(GCfBuff + i) = *(GMsgBuff + i);
	*(GMsgBuff + i) = tchar;
    }

#if CTEST
    if (DscDemo > 0) {
	WriteStd ("\nOffspring:\n  ");
	WriteStd (GCfBuff);
	WriteStd ("\n  ");
	WriteStd (GMsgBuff);
    }
#endif

    ACndtoB (GCfBuff, CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D);
    ACndtoB (GCfBuff + STRNGSZ + 1, CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D);
    AActtoB (GCfBuff + (STRNGSZ * 2) + 2, CfPtr1 -> NewActB, CfPtr1 -> NewActD);

    ACndtoB (GMsgBuff, CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D);
    ACndtoB (GMsgBuff + STRNGSZ + 1, CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D);
    AActtoB (GMsgBuff + (STRNGSZ * 2) + 2, CfPtr2 -> NewActB, CfPtr2 -> NewActD);

    if (xstart <= STRNGSZ && xend >= STRNGSZ) {
	type = CfPtr1 -> NewCnd2T;	/* cross cond2 type */
	CfPtr1 -> NewCnd2T = CfPtr2 -> NewCnd2T;
	CfPtr2 -> NewCnd2T = type;
    }
    if (xstart <= (STRNGSZ * 2) + 1 && xend >= (STRNGSZ * 2) + 1) {
	type = CfPtr1 -> NewActT;	/* cross action types */
	CfPtr1 -> NewActT = CfPtr2 -> NewActT;
	CfPtr2 -> NewActT = type;
    }
   /* get the BidRatios--note pass condition strings separately */

    CfPtr1 -> NewBR = CalcSpec (GCfBuff, GCfBuff + STRNGSZ + 1, CMATCH, CfPtr1 -> NewCnd2T);
    CfPtr2 -> NewBR = CalcSpec (GMsgBuff, GMsgBuff + STRNGSZ + 1, CMATCH, CfPtr2 -> NewCnd2T);

}					/* DCrFullCf */

#endif /* else of SINGLEXO */


/*****************************

CrCfStrg	Cross one string-part of two NEW classifiers.

	CfPtr1	Pointer to first classifier.
	CfPtr2	Pointer to second classifier.

This function crosses the left-conditions, right-conditions, or actions of
two classifiers, with equiprobability.

******/

#define	XCND1PR	(float) 0.0		/* define cummul. prob. dist. to */
#define	XCND2PR	(float) 0.0		/* specify where cross will occur */

VOID 
CrCfStrg (CfPtr1, CfPtr2)
    struct NCfNode *CfPtr1, *CfPtr2;
{
    char string1[STRNGSZ + 1], string2[STRNGSZ + 1];
    float randnum;

    if ((randnum = URand01 ()) <= 0.033) {

#if CTEST
	if (DscDemo > 0)
	    WriteStd ("Cross first conds...");
#endif

#if GENLOG
	if (GenFlg)
	    WriteGen ("XC1 ");
#endif

	BCndtoA (CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D, string1);	/* Cross the first conditions */
	BCndtoA (CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D, string2);
	RCrossSt (string1, string2);	/* cross the strings */
	ACndtoB (string1, CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D);	/* store the crosses */
	ACndtoB (string2, CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D);
    } else if (randnum <= 0.667) {

#if CTEST
	if (DscDemo > 0)
	    WriteStd ("Cross second conds...");
#endif

#if GENLOG
	if (GenFlg)
	    WriteGen ("XC2 ");
#endif

	BCndtoA (CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D, string1);	/* Cross the second conditions */
	BCndtoA (CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D, string2);
	RCrossSt (string1, string2);	/* cross the strings */
	ACndtoB (string1, CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D);	/* store the crosses */
	ACndtoB (string2, CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D);
    } else {

#if CTEST
	if (DscDemo > 0)
	    WriteStd ("Cross actions...");
#endif

#if GENLOG
	if (GenFlg)
	    WriteGen ("XA  ");
#endif

	BActtoA (CfPtr1 -> NewActB, CfPtr1 -> NewActD, string1);	/* Cross the actions */
	BActtoA (CfPtr2 -> NewActB, CfPtr2 -> NewActD, string2);
	RCrossSt (string1, string2);	/* cross the strings */
	AActtoB (string1, CfPtr1 -> NewActB, CfPtr1 -> NewActD);	/* store the crosses */
	AActtoB (string2, CfPtr2 -> NewActB, CfPtr2 -> NewActD);

    }

    BCndtoA (CfPtr1 -> NewCnd1B, CfPtr1 -> NewCnd1D, string1);	/* Recalculate Bidratios */
    BCndtoA (CfPtr1 -> NewCnd2B, CfPtr1 -> NewCnd2D, string2);
    CfPtr1 -> NewBR = CalcSpec (string1, string2, CMATCH, CfPtr1 -> NewCnd2T);

    BCndtoA (CfPtr2 -> NewCnd1B, CfPtr2 -> NewCnd1D, string1);
    BCndtoA (CfPtr2 -> NewCnd2B, CfPtr2 -> NewCnd2D, string2);
    CfPtr2 -> NewBR = CalcSpec (string1, string2, CMATCH, CfPtr2 -> NewCnd2T);

}					/* CrCfStrg */



/*****************************

RCrossSt	Random Cross two Strings.

	String1	|- The ascii char arrays containing the strings.
	String2	|

Cross will exchange from 0 to end of string, i.e., at least 1 loci and
up to the whole thing!

******/

VOID 
RCrossSt (String1, String2)
    char String1[], String2[];
{
    register char tchar;
    register int i;

    i = Round ((STRNGMX - 1) * URand01 ()) + 1;	/* 1 <= i <= STRNGMX */

#if CTEST
    if (DscDemo > 1) {
	sprintf (GOutBuff, "  Cross starts at loci %d (from left)", i);
	WriteStd (GOutBuff);
    }
#endif

#if GENLOG
    if (GenFlg) {
	sprintf (GOutBuff, "%d. ", i);
	WriteGen (GOutBuff);
    }
#endif

    for (; i <= STRNGMX; ++i) {		/* cross them */
	tchar = String1[i];
	String1[i] = String2[i];
	String2[i] = tchar;
    }

}					/* RCrossSt */


/*****************************

ReprodCf	Reproduce a Classifier, possibly with a mutation.

	ParentCf	Pointer to source classifier node.

Copy condition 1 and 2, and the action, and the bidratio from the
ParentCf (CfNode) into the next available NEW classifier (NCfNode).
Increment NmNewCfs (which indicated where next is to go.)

Also increment TOTNmOfs and counters in ParentCf.

******/

struct NCfNode *
ReprodCf (ParentCf)
    struct CfNode *ParentCf;
{
    register int i, mu;
    register float f;
    struct NCfNode *ChildCf;

    ParentCf -> TotNmOfs += 1;		/* Increment Number of Offspring counter */
    ++TOTNmOfs;				/* increment total for all classifiers for run */

    if (NmNewCfs > MxNewCfs) {
	sprintf (GOutBuff, "\n***ReprodCf: NmNewCfs (%d) > MxNewCfs (%d), step %u\n",
		 NmNewCfs, MxNewCfs, CycleStp);
	WriteStd (GOutBuff);
	return (NULL);
    }
    ChildCf = &NewCfs[NmNewCfs++];	/* get pointer to next open new cf node, increment count */
    ChildCf -> NewCf_Id = NxtCfId++;	/* give it a new cf id number now */

    for (i = 0; i < INTPRSTR; ++i) {
	ChildCf -> NewCnd1B[i] = ParentCf -> Cnd1Bits[i];
	ChildCf -> NewCnd1D[i] = ParentCf -> Cnd1DCs[i];
	ChildCf -> NewCnd2B[i] = ParentCf -> Cnd2Bits[i];
	ChildCf -> NewCnd2D[i] = ParentCf -> Cnd2DCs[i];
	ChildCf -> NewActB[i] = ParentCf -> ActBits[i];
	ChildCf -> NewActD[i] = ParentCf -> ActDCs[i];
    }
    ChildCf -> NewCnd1T = ParentCf -> Cnd1Type;
    ChildCf -> NewCnd2T = ParentCf -> Cnd2Type;
    ChildCf -> NewActT = ParentCf -> ActType;

    if (NwCfStrV == NWSTRHPA)		/* new strength is 1/2 between parent and average */
	ChildCf -> NewStr = AveCfStr + (0.5 * abs ((ParentCf -> Strength - AveCfStr)));
    else
	ChildCf -> NewStr = AveCfStr * NwCfStrF;

    ChildCf -> NewBR = ParentCf -> BidRatio;

    f = URand01 ();			/* find number of mutations by looking */
    for (mu = 0; mu <= MUTABMX; ++mu)	/* through pre-calculated poisson distr. */
	if (f <= MuPrTab[mu])
	    break;

    for (i = 0; i < mu; ++i)
	Mutate (ChildCf);		/* mutate the child cf */

    return (ChildCf);

}					/* ReprodCf */


/*****************************

Mutate		Mutate an new (offspring) classifier.

	Cfptr 	Pointer to new classifier to be mutated, with probability given by MuPrTot.

******/

VOID 
Mutate (CfPtr)
    struct NCfNode *CfPtr;		/* note this is a NCfNode */
{
    int oldtype;
    register float randnum;
    struct CfOpNode *atptr, *GtCOpRnd ();	/* For mutating action type */

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "  |- Mutate cf %u:  ", CfPtr -> NewCf_Id);
	WriteStd (GOutBuff);
    }
#endif

    ++TotMu;				/* Mutate this one. Increment the mutation counter */

#if GENLOG
    if (GenFlg)
	WriteGen ("  Mu ");
#endif

    if ((randnum = URand01 ()) <= MuPrCnd1)
	MutatCA (CfPtr, 1);		/* The first condition string */

    else if (randnum <= MuPrCnd2)
	MutatCA (CfPtr, 2);		/* The second condition string */

    else if (randnum <= MuPrAct)
	MutatCA (CfPtr, 3);		/* The action string */

    else if (randnum <= MuPrCT2) {
	oldtype = CfPtr -> NewCnd2T;
	if (URand01 () <= MuFrMC) {	/* The second condition's type */
	    CfPtr -> NewCnd2T = CMATCH;	/* make it a match condition */

#if GENLOG
	    if (GenFlg)
		WriteGen ("C2T m. ");
#endif

#if CTEST
	    if (DscDemo > 0)
		WriteStd ("Cmd2Type to MATCH.\n  ");
#endif
	} else {
	    CfPtr -> NewCnd2T = CNOTMATCH;	/* make it a NOT-match condition */

#if GENLOG
	    if (GenFlg)
		WriteGen ("C2T ~. ");
#endif

#if CTEST
	    if (DscDemo > 0)
		WriteStd ("Cmd2Type to NOTMATCH.\n  ");
#endif
	}
	if (oldtype != CfPtr -> NewCnd2T) {	/* recalculate BidRatio after condition type change */
	    BCndtoA (CfPtr -> NewCnd1B, CfPtr -> NewCnd1D, GMsgBuff);	/* use two scratch buffers from CORE.DEF */
	    BCndtoA (CfPtr -> NewCnd2B, CfPtr -> NewCnd2D, GCfBuff);
	    CfPtr -> NewBR = CalcSpec (GMsgBuff, GCfBuff, CMATCH, CfPtr -> NewCnd2T);
	}
    } else {				/* Mutate the action type */
	atptr = GtCOpRnd ();		/* Get new one at random */
	CfPtr -> NewActT = atptr -> CfOpCode;	/* store it */

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "Action-Type from %4s.\n  ", atptr -> CfOpName);
	    WriteStd (GOutBuff);
	}
#endif

#if GENLOG
	if (GenFlg) {
	    sprintf (GOutBuff, "AT %4s. ", atptr -> CfOpName);
	    WriteGen (GOutBuff);
	}
#endif
    }

}					/* Mutate */


/*****************************

MutatCA 	Mutate a Condition or Action string.

	CfPtr	Pointer to NEW classifier to be mutated.

	What	What to mutate:
			1 - condition-1		string
			2 - condition-2		string
			3 - action			string

Just pick a loci at random and mutate it. The mutation is controlled by the
MuFrNSL (mutation fraction non-specific loci) variable: Probability new value is a # is just MuFrNSL.
The probability the new value is a 1 (0) is (1 - MuFrNSL)/2, i.e., equi-probable.

NOTE: The cf's BidRatio is changed when condition's specificity is changed.

******/

VOID 
MutatCA (CfPtr, What)
    struct NCfNode *CfPtr;
    unsigned int What;
{
    register float randnum;
    register unsigned int loci;
    char string[STRNGSZ + 1], oldval;

    if (What == 1) {
	BCndtoA (CfPtr -> NewCnd1B, CfPtr -> NewCnd1D, string);	/* Get the Ascii form */

#if GENLOG
	if (GenFlg)
	    WriteGen ("C1 ");
#endif
    } else if (What == 2) {
	BCndtoA (CfPtr -> NewCnd2B, CfPtr -> NewCnd2D, string);

#if GENLOG
	if (GenFlg)
	    WriteGen ("C2 ");
#endif
    } else {
	BActtoA (CfPtr -> NewActB, CfPtr -> NewActD, string);

#if GENLOG
	if (GenFlg)
	    WriteGen ("A ");
#endif
    }
    string[STRNGSZ] = '\0';

    loci = min ((URand01 () * STRNGSZ), STRNGMX);	/* make 0 <= loci <= STRNGMX, ie <= STRNGSZ-1 */
    oldval = string[loci];		/* save old value for demo or geneology */
    if ((randnum = URand01 ()) <= MuFrNSL)	/* decide what new value is to be */
	string[loci] = '#';
    else if (randnum <= MuFrNSL + ((1 - MuFrNSL) / 2))
	string[loci] = '0';
    else
	string[loci] = '1';

    if (What == 1)
	ACndtoB (string, CfPtr -> NewCnd1B, CfPtr -> NewCnd1D);	/* Store mutated string back in classifier */
    else if (What == 2)
	ACndtoB (string, CfPtr -> NewCnd2B, CfPtr -> NewCnd2D);
    else
	AActtoB (string, CfPtr -> NewActB, CfPtr -> NewActD);

#if CTEST
    if (DscDemo > 0) {
	WriteStd ("mutate ");
	if (What == 1)
	    WriteStd ("left condition");
	else if (What == 2)
	    WriteStd ("right condition");
	else
	    WriteStd ("action ");
	sprintf (GOutBuff, " at loci %u, from '%c' to '%c'.\n  ", loci, oldval, string[loci]);
	WriteStd (GOutBuff);
    }
#endif

#if GENLOG
    if (GenFlg) {
	sprintf (GOutBuff, "%u (%c->%c). ", loci, oldval, string[loci]);
	WriteGen (GOutBuff);
    }
#endif

    if (What == 1 || (What == 2 && CfPtr -> NewCnd2T == CMATCH)) {
	if (oldval == '#' && string[loci] != '#')	/* from # to non-#, so */
	    CfPtr -> NewBR += MutdSpec;	/* increment specificity.	*/
	else if (oldval != '#' && string[loci] == '#')	/* from non=# to #, so */
	    CfPtr -> NewBR -= MutdSpec;	/* decrement specificity */
    } else if (What == 2) {		/* 2nd condition, but it is NOT type, so... */
	if (oldval == '#' && string[loci] != '#')	/* from # to non-#, so */
	    CfPtr -> NewBR -= MutdSpec;	/* decrement specificity.	*/
	else if (oldval != '#' && string[loci] == '#')	/* from non=# to #, so */
	    CfPtr -> NewBR += MutdSpec;	/* increment specificity */
    }
}					/* MutatCA */

#endif /* CDSC_ALL */
