
/*		DSCOPS2 for the CFS-C Classifier System

This file, DSCOPS2.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file implement MORE discovery learning "operators"
used by the CFS-C classifier system.
That is, these subroutines take as input one or more classifiers and produce
new classifiers that are modified versions of the input classifiers.
The subroutines:

	DscCSS		couple stage setting classifiers
	PckCC		Pick candidates for coupling
	DscACPC 	asynchronous coupling of profitable classifiers
	DscTLB		trigger on Low average Bid operator
	DscCEff 	"cover effectors" operator.
**/

#include	"config.h"

#include	"utility.h"
#include	"core.ext"
#include	"cfops.ext"
#include	"dsclearn.ext"
#include	"dscops.h"
#include	"dscops2.h"

extern short int GenFlg;
extern unsigned int DemoLev;
extern char *GOutBuff;

struct CfNode *PckCCSS, *PckCCPrf;	/* used to pass pointers to classifiers to be coupled, chosen by PckCCSS */

#if MPWC
#define __SEG__ CFSCDSC
#endif

#if CDSC_ALL
/*****************************

DscCSS		Couple Stage Setting Classifiers Operator

We want to create 2 new classifiers that are linked, so that:
	- One, the "stage setter", has conditions from a classifier active 2 steps ago
		and an action that satisfies condition(s) of the other new classifier.
	- One, the "successor", that has one or two conditions that are satisfied
		by the other new classifier (if just one, then the other is the same
		as its parent) and an action from the most profitable classifier
		active 1 step ago.
The linking action/condition pair consists of the action string from
the classifier active 2 steps ago, with a random "tag" inserted in the
region from TagLMx to TagRMx.

The main differences between this and DscACPC operator are:

The parameters that control this:
	CSSRt	prob. check trigger during each step to see if operator should be applied.
	CSSFrM	fraction of coupled pairs with linkage message forced to a
			"source tag" (left-most 2 loci) of "11"
			(i.e., an "internal memory" type message is used).
			Otherwise the action "source tag" is just copied from
			"stage setter" parent's action.
	CSSFrRf	fraction of new times the "successor" has both conditions
			satisfied (equal to) the action of its "stage setter".
			Otherwise one condition is copied from the successor parent
			and one is equal to the action of the stage setter.

	Note: The following are the same as for the DscACPC operator--see
		description there for details.

	NwCCStrF	Control strength of the new classifiers:

	ACPCt		Threshold to control when profit is large enough to trigger the operator.

******/

VOID 
DscCSS ()
{
    int i;
    struct NCfNode *new1, *new2;

#if CTEST
    if (DscDemo > 0)
	WriteStd ("\nDscCSS:");
#endif

    if (PckCC () != 0) {		/* See if there are some to couple... */

	new1 = ReprodCf (PckCCSS);	/* new1 is the precursor (stage setter) cf */
	new2 = ReprodCf (PckCCPrf);	/* new2 is the successor (profitable) cf */

       /* Now put new linkage action/condition into GCfBuff;
				use GMsgBuff as scratch area for random string.
			*/

	BActtoA (new1 -> NewActB, new1 -> NewActD, GCfBuff);

	GnRndCA (GMsgBuff, &i, 'c');	/* generate random string */
	for (i = TagLMx; i <= TagRMx; ++i)	/* a kludge to get rid of #'s */
	    if (*(GMsgBuff + i) == '#') {
		if (URand01 () < 0.5)
		    *(GMsgBuff + i) = '0';
		else
		    *(GMsgBuff + i) = '1';
	    }
	for (i = TagLMx; i <= TagRMx; ++i)	/* insert the random string into action */
	    *(GCfBuff + i) = *(GMsgBuff + i);	/* we only need the one product */

	if (URand01 () < CSSFrM) {	/*	Force it to 11 source-tag */
	    *GCfBuff = '1';
	    *(GCfBuff + 1) = '1';
	}
	AActtoB (GCfBuff, new1 -> NewActB, new1 -> NewActD);	/* Store in stage-setter */

	ACndtoB (GCfBuff, new2 -> NewCnd2B, new2 -> NewCnd2D);	/* Store in cond2 of successor */
	new2 -> NewCnd2T = CMATCH;

	if (URand01 () < CSSFrRf)	/* Force it to be "reflex" classifier */
	    ACndtoB (GCfBuff, new2 -> NewCnd1B, new2 -> NewCnd1D);	/* Store in cond2 of successor */

       /* Now set the strength in the new classifiers */
       /* ***** A TEMP KLUDGE ***** */

	if (NwCCStrF == (float) -1.0) {	/* Use Ave Parent S */
	    new1 -> NewStr = (PckCCSS -> Strength + PckCCPrf -> Strength) / 2.0;
	    new2 -> NewStr = new1 -> NewStr;
	} else if (NwCCStrF != (float) 0.0) {	/* if its 0, use whatever Reprod gives us */
	    new1 -> NewStr = AveCfStr * NwCCStrF;	/* Else use Fraction * Ave str */
	    new2 -> NewStr = new1 -> NewStr;
	}
	++TotCSS;

       /* Recalculate specificity of new successor with new conditions */

	BCndtoA (new1 -> NewCnd1B, new1 -> NewCnd1D, GMsgBuff);
	new1 -> NewBR = CalcSpec (GCfBuff, GMsgBuff, CMATCH, CMATCH);

#if CTEST
	if (DscDemo > 0) {
	    WriteStd ("\nOffspring:\n  ");
	    WrtNCf (new1, GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "\n  %s", GCfBuff);
	    WriteStd (GOutBuff);
	    WrtNCf (new2, GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "  %s", GCfBuff);
	    WriteStd (GOutBuff);
	}
#endif

    }					/* there were some to couple */
}					/* DscCSS	*/

/*****************************

PckCC 	Pick candidates for coupling:
		Stage-Setter: active 2 steps ago
		Successor:	  active 1 step ago, and profit > threshold

	Return:	1 - there were candidates
			0 - there were not candidates (or an error occured)

	On Return 1, set
		PckCCSS		- the stage-setter classifier, active at t-2
		PckCCPrf	- the profitable successor, active t-1

******/

int 
PckCC ()
{
    register int i;
    short oldmsg, nmoldint;
    float hiprof;
    struct MsgNode *mp;

    if (NmNewCfs > MxNewCfs - 1) {	/* There must be room for 2! */

#if CTEST
	if (DscDemo > 0)
	    WriteStd ("\n   not room for 2 more new cfs.\n");
#endif

	return (0);
    } else if (MxNewCfs < 2) {
	WriteStd ("\n**PckCC: MxNewCfs < 2 (set to 2).\n");
	MxNewCfs = 2;
    }
   /* Count the old messages: that is where we will get a pointer
			to a stage-setter active 2 steps ago.
		*/

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "\nActCfPNm %d; OldMsgs (%d) are:\n", ActCfPNm, NmOMsgs);
	WriteStd (GOutBuff);
    }
#endif

    for (mp = OldMsgs, i = 0, nmoldint = 0; i <= LastOMsg; ++mp, ++i) {
	if (mp -> IsMsg) {
	    if (!mp -> FromDet)		/* count only those with producers */
		++nmoldint;

#if CTEST
	    if (DscDemo > 0) {
		PutMNd (mp, GMsgBuff, DEMOFMT1);
		WriteStd (GMsgBuff);
	    }
#endif
	}
    }

    if (ActCfPNm == 0 || nmoldint == 0) {

#if CTEST
	if (DscDemo > 0)
	    WriteStd ("\nActCfPNm or nmoldmsg are 0 => Can't couple.");
#endif

	return (0);
    }

#if CTEST
    if (DscDemo > 0)
	WriteStd ("\nActCfPrv are:");
#endif

    for (i = 0, hiprof = -99999, PckCCPrf = NULL; i < ActCfPNm; ++i) {

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\ncf %d, chngstr %6.2f", ActCfPrv[i].ActCf -> Cf_Id, ActCfPrv[i].ChngStr);
	    WriteStd (GOutBuff);
	}
#endif

	if (ActCfPrv[i].ChngStr > hiprof ||
		(ActCfPrv[i].ChngStr == hiprof && URand01 () <= 0.5)) {
	    hiprof = ActCfPrv[i].ChngStr;
	    PckCCPrf = ActCfPrv[i].ActCf;
	}
    }

#if CTEST
    if (DscDemo > 0) {
	sprintf (GOutBuff, "\nPckCCPrf %d, chngstr %6.2f", PckCCPrf -> Cf_Id, hiprof);
	WriteStd (GOutBuff);
    }
#endif

    if (PckCCPrf == NULL) {
	sprintf (GOutBuff, "\nPckCSS: PckCCPrf is NULL (NmActPNm %d, stp %d)!\n", ActCfPNm, CycleStp);
	WriteStd (GOutBuff);
	return (0);
    }
    if (hiprof < ACPCt * PckCCPrf -> Strength) {	/* it fails threshold test */

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\nhiprof < ACPCt (%.3f) * Cf Str (%.0f): no linking.\n",
		     ACPCt, PckCCPrf -> Strength);
	    WriteStd (GOutBuff);
	}
#endif

	return (0);
    }
   /* Now find classifiers active 2 steps ago, before the Profitable one,
			by looking at producers of messages on Old message list.
		*/

    if (nmoldint == 1) {		/* just one to link to, so just find it */
	for (mp = OldMsgs, i = 0; i <= LastOMsg; ++mp, ++i)
	    if (mp -> IsMsg && !mp -> FromDet) {
		PckCCSS = OldMsgs -> Producer;
		break;
	    }
    } else {				/* several to link to, so ... */
	oldmsg = URandN (nmoldint - 1);	/* pick one at random */

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\nrand oldmsg is %d. Look for it:", oldmsg);
	    WriteStd (GOutBuff);
	}
#endif

	for (mp = OldMsgs, i = 0, nmoldint = 0; i <= LastOMsg; ++mp, ++i) {
	    if (!mp -> IsMsg || mp -> FromDet)
		continue;		/* not a candidate */
	    else if (nmoldint != oldmsg)
		++nmoldint;		/* not the one, but a candidate */
	    else
		break;			/* this is the one! */
	}

	if (i > LastOMsg) {
	    sprintf (GOutBuff, "\n***PckCC: rand failed to find oldmsg (stp %d)!\n", CycleStp);
	    WriteStd (GOutBuff);
	    return (0);
	} else
	    PckCCSS = mp -> Producer;
    }

    if (PckCCSS == NULL) {
	sprintf (GOutBuff, "\n***PckCC: PckCCSS NULL (stp %d)!\n", CycleStp);
	WriteStd (GOutBuff);
	return (0);
    } else {				/* we have all the ducks in a row, at last */

#if CTEST
	if (DscDemo > 0) {
	    WriteStd ("\nParent StageSetter & Profitable cfs:");
	    PutCNd (PckCCSS, GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "\n  %s", GCfBuff);
	    WriteStd (GOutBuff);
	    PutCNd (PckCCPrf, GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "  %s", GCfBuff);
	    WriteStd (GOutBuff);
	}
#endif

	return (1);
    }

}					/* PckCC */


/*****************************

DscACPC		 Asynchronous coupling profitable classifier operator

We want to create 2 new classifiers that are linked, so that:
	- One, the "stage setter", has conditions from a classifier active 2 steps ago
		and an action that satisfies one condition of the other new classifier.
	- One, the "successor", has an its conditions one condition that is satisfied by
		the other new classifier, one condition the same, and an action
		from the most profitable classifier active 1 step ago.
The linking condition should be a cross of a random string and
the condition part of the classifier active 1 step ago.

The parameters that control this:

	ACPCRt		Prob. operator trigger is checked on a given step.

	Note:	The following are the same as for the DscCSS operator.

	NwCCStrF	Control strength of the new classifiers:
				-1.0	Use average of parents
				0.0		Use same strength assigned to new classifiers by BkgGA,
						CDMsg, etc., operators (controlled by NwCfStrV and NwCfStrF).
				f		use AveCfStr * f

	ACPCt		Threshold to control when profit is large enough to trigger
				the operator. It is trigger when "successor" classifier
				makes a "profit" (bid + tax + SysRew + payments to it for messages)
				that satifies:

					profit > Cf->Strength * ACPCt

				where ACPCt may be positive, zero, or negative!

******/

VOID 
DscACPC ()
{
    register int i, xstart, xend;
    struct NCfNode *new1, *new2;

    if (PckCC () != 0) {		/* See if there are some to couple... */

	new1 = ReprodCf (PckCCSS);	/* new1 is the precursor (stage setter) cf */
	new2 = ReprodCf (PckCCPrf);	/* new2 is the successor (profitable) cf */

#ifdef THIS
       /* This code fragment generates a linkage string
			that is random bits crossed with action of precursor,
			with 01 written over left bits.
			The problem with this is one gets a multitude of these
			linkage classifiers, all different, even though they are
			for the same situation.
		*/

       /* get new action/condition into GCfBuff; use GMsgBuff as scratch */

	BActtoA (new2 -> NewActB, new2 -> NewActD, GCfBuff);

	GnRndCA (GMsgBuff, &xstart, 'c');	/* generate random string (use xstart as scratch) */

       /*	We want xstart and xend to point to loci
				that start/end segment to cross, i.e.:
					0 <= xstart <= STRNGMX
				xstart <= xend <= STRNGMX
			*/

	xstart = URandN (STRNGMX);
	if (xstart != STRNGMX)
	    xend = URandN ((STRNGMX - xstart)) + xstart;
	else
	    xend = xstart;

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\nxstart/xend %d/%d. ", xstart, xend);
	    WriteStd (GOutBuff);
	}
#endif

	for (i = xstart; i <= xend; ++i)/* do the cross */
	    *(GMsgBuff + i) = *(GCfBuff + i);	/* we only need the one product */
#endif /* randomXaction linkage string */

       /* this code uses the condition part of the precursor
				as the tag for the successor!
				Here: cross the two conditions of the precursor.
			*/

	BCndtoA (new2 -> NewCnd1B, new2 -> NewCnd1D, GCfBuff);
	BCndtoA (new2 -> NewCnd2B, new2 -> NewCnd2D, GMsgBuff);

       /*	We want xstart and xend to point to loci
				that start/end segment to cross, i.e.:
					0 <= xstart <= STRNGMX
				xstart <= xend <= STRNGMX
			*/

	xstart = URandN (STRNGMX);
	if (xstart != STRNGMX)
	    xend = URandN ((STRNGMX - xstart)) + xstart;
	else
	    xend = xstart;

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\nxstart/xend %d/%d. ", xstart, xend);
	    WriteStd (GOutBuff);
	}
#endif

	for (i = xstart; i <= xend; ++i)/* do the cross */
	    *(GMsgBuff + i) = *(GCfBuff + i);	/* we only need the one product */

       /* Now put 01 to start linkage string (in GMsgBuf), and then
				put it as action of precursor and condition of successor.
			*/

	*GMsgBuff = '0';
	*(GMsgBuff + 1) = '1';
	AActtoB (GMsgBuff, new2 -> NewActB, new2 -> NewActD);
	ACndtoB (GMsgBuff, new1 -> NewCnd2B, new1 -> NewCnd2D);
	new1 -> NewCnd2T = CMATCH;

       /* ***** A TEMP KLUDGE ***** */
	if (NwCCStrF == (float) -1.0) {	/* Use Ave Parent S */
	    new1 -> NewStr = (PckCCSS -> Strength + PckCCPrf -> Strength) / 2.0;
	    new2 -> NewStr = new1 -> NewStr;
	} else if (NwCCStrF != (float) 0.0) {	/* if its 0, use whatever Reprod gives us */
	    new1 -> NewStr = AveCfStr * NwCCStrF;	/* Else use Fraction * Ave str */
	    new2 -> NewStr = new1 -> NewStr;
	}
	++TotACPC;

	BCndtoA (new1 -> NewCnd1B, new1 -> NewCnd1D, GCfBuff);
	new1 -> NewBR = CalcSpec (GCfBuff, GMsgBuff, CMATCH, CMATCH);

#if CTEST
	if (DscDemo > 0) {
	    WriteStd ("\nOffspring:\n  ");
	    WrtNCf (new1, GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "\n  %s", GCfBuff);
	    WriteStd (GOutBuff);
	    WrtNCf (new2, GCfBuff, DEMOFMT1);
	    sprintf (GOutBuff, "  %s", GCfBuff);
	    WriteStd (GOutBuff);
	}
#endif
    }					/* there was some to link */
}					/* DscACPC */



/*****************************

DscTLB		Discovery operator triggered on Low Bid.

	If average bid of current candidates is < average bid of all classifiers
	then the current candidates are not doing well, so we will generate
	a new classifier that matches the current situation but has
	a random action.

	The reason for this is to introduce some variance into the
	classifier sets that are bidding but doing poorly.

	The variables that control this:
	TBLRt	Prob this trigger is tested on each step.
	TLBt	Trigger is officially
				AveCurrentBid < TLBt * AverageBid
			where TLBt will typically be <= 1.0, and AverageBid is
			defined to be AveCfStr * AveCfBR * Bid_k.
	TLBFrE	Fraction of new classifiers forced to effector-message producers.
	TotTLB	Count of number of times this produced a new classifier.

===> CHANGES // KLUDGES ....
		1. Trigger use S instead of bid
			Current Ave S < TLBt * AveCfStr
		2. Check to prevent actions that start 00 .
		3. Just replace weakest Strength co-bidder, if there is one.
			Otherwise reproduce parent and use normal replacement algorithm.

******/

VOID 
DscTLB ()
{
    int i;
    float avelobid, avebid, lowstr;
    struct NCfNode *new1 = NULL;
    struct CfNode *cand, *lowcf;

#if CTEST
    if (DscDemo > 0)
	WriteStd ("\nTest TLB...");
#endif

    if (NmCandCf != 0) {

/****** ====> This uses bid...
		for ( cand = CandCfs, avelobid = 0; cand != NULL; cand = cand->NxtCand )
			avelobid += cand->CfBid;
		avelobid /= NmCandCf;
		avebid = AveCfStr * AveCfBR * Bid_k;
*******/

       /* === use strengths for comparison */
	lowstr = 9999999;
	lowcf = NULL;
	for (cand = CandCfs, avelobid = 0; cand != NULL; cand = cand -> NxtCand) {
	    avelobid += cand -> Strength;
	    if (cand -> Strength < lowstr) {
		lowcf = cand;
		lowstr = cand -> Strength;
	    }
	}
	avelobid /= NmCandCf;		/* get values for threshold check */
	avebid = AveCfStr;

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "Calc avebid %0.1f, Cur AveBid %0.1f, TLBt %0.1f)", avebid, avelobid, TLBt);
	    WriteStd (GOutBuff);
	    if (lowcf != NULL) {
		sprintf (GOutBuff, "low bid cf %d, str %0.1f.", lowcf -> Cf_Id, lowstr);
		WriteStd (GOutBuff);
	    }
	}
#endif

	if (avelobid <= avebid * TLBt) {
	    if (PkBPrnts (1, 'e') != 0) {	/* Pick 1 Hi bidder (else Hi Str) */

#if CTEST
		if (DscDemo > 0) {
		    WriteStd ("\nParent for TLB:");
		    PutCNd (Parents[0], GCfBuff, DEMOFMT1);
		    sprintf (GOutBuff, "\n  %s", GCfBuff);
		    WriteStd (GOutBuff);
		}
#endif

		++TotTLB;

		if (lowcf == NULL || lowcf == Parents[0])	/* if no other bidder is lower than parent, */
		    new1 = ReprodCf (Parents[0]);	/* reproduce parent. */

		GnRndCA (GMsgBuff, &i, 'c');	/* generate random string */

		if (URand01 () < TLBFrE) {	/* force to effector-message producer */
		    *GMsgBuff = '1';
		    *(GMsgBuff + 1) = '0';
		} else if (*GMsgBuff == '0' && *(GMsgBuff + 1) == '0') {
		    if (URand01 () < 0.5)	/* don't allow action to start 00 */
			*GMsgBuff = '1';
		    else
			*(GMsgBuff + 1) = '1';
		}
	       /* if no other lo S bidder, and there is a new classifier, modify that new one */
		if ((lowcf == NULL || lowcf == Parents[0]) && new1 != NULL)
		    AActtoB (GMsgBuff, new1 -> NewActB, new1 -> NewActD);
		else			/* else replace action of the low S co-bidder */
		    AActtoB (GMsgBuff, lowcf -> ActBits, lowcf -> ActDCs);

#if CTEST
		if (DscDemo > 0) {
		    WriteStd ("\nOffspring ");
		    if ((lowcf == NULL || lowcf == Parents[0]) && new1 != NULL) {
			WriteStd ("(use std replace alg):\n");
			WrtNCf (new1, GCfBuff, DEMOFMT1);
			sprintf (GOutBuff, "\n  %s", GCfBuff);
		    } else {
			WriteStd ("(replace low str co-bidder):\n");
			PutCNd (lowcf, GCfBuff, DEMOFMT1);
			sprintf (GOutBuff, "\n  %s", GCfBuff);
		    }
		    WriteStd (GOutBuff);
		}
#endif

	    }				/* got a parent */
	}				/* ave current bid below threshold */
    }					/* there were candidate bidders */
}					/* DscTLB */


/*****************************

DscCEff 	Discovery operator, 'Cover Effectors'.

This operator is invoked (with some prob.) when a "mistake" is made,
i.e., when the task domain sets MadeMstk (see call to this is DSCLEARN.C).
It is designed to help the system maintain a subset of effector-activating
classifiers, to keep the system "connected" to its effector interface.
Basically when applied the operator produces a new classifier by copying
a classifier (or two) that (a) activated the effector or (b) bid this step, and
then attaching a random action to the copy, and (if CEffSplz is on)
"specializing" a randomly selected # in the conditions (to 0 or 1 at random).

The parameters:
	CEffs	   number of cf to produce per application (0,1,2)
	CEffsRt	 Probability apply operator when mistake is made
	MadeMstk	set by task domain if mistake is made
	CEffSplz	control operator's effect on the condition part of new classifiers.
				0 = don't specialize, 1 = specialize one, 2 = specialize both

For each effector (with the CEffFlg set TRUE in the INIT.ENV file---as they
all usually do, execpt for default effectors), the algorithm works as follows:

	if the effector is matched by some messages (NmEMtch > 0) {
		if just 1 macthed, use it as parent for one (or two) offspring
		if 2 matched, use them
		if > 2 matched, pick one or two at random
	if the effector not matched, but there were bidders,
		pick 1 or 2 using probabilities proportional to bids
	if no bidders, don't do anything
	if we got some parents somehow, copy them, add random actions,
  	and if CEffSplz is > 0, specialize a condition in one or both.

******/

VOID 
DscCEff ()
{
    int nmparent, i, effnum;
    float prob, cummul, randnum;
    struct EffNode *eptr;
    struct MsgNode *mptr;
    struct NCfNode *newcfptr = NULL;

    for (effnum = 0; effnum < NmEffs; ++effnum) {
	eptr = &EffLst[effnum];
	if (!eptr -> CEffFlg)		/* dont cover ones with this flag set FALSE */
	    continue;
	Parents[0] = Parents[1] = NULL;	/* store pointer to parent cf here */
	nmparent = 0;

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\nCEffs (%s): mistake made: create %d new cf ", eptr -> EffName, CEffs);
	    WriteStd (GOutBuff);
	}
#endif

#if GENLOG
	if (GenFlg) {
	    sprintf (GOutBuff, "Step %d (CEffs) ", CycleStp);
	    WriteGen (GOutBuff);
	}
#endif

	if (eptr -> NmEMtch > 0) {	/* This effector matched this step */

#if CTEST
	    if (DscDemo > 0)
		WriteStd ("(parent from cfs tried):");
#endif

	    nmparent = CEffs;
	    TotCEfW += nmparent;

	    if (eptr -> NmEAct == 1) {
		Parents[0] = eptr -> EffMtch -> Producer;	/* Just take one we find--twice! */
		if (nmparent == 2) {
		    Parents[1] = Parents[0];

#if CTEST
		    if (DscDemo > 0)
			WriteStd ("(same parent twice):");
#endif
		}
	    } else if (eptr -> NmEAct == 2) {
		Parents[0] = eptr -> EffMtch -> Producer;	/* Just take the two we find. */
		if (nmparent == 2) {
		    Parents[1] = eptr -> EffMtch -> NxtEMsg -> Producer;

#if CTEST
		    if (DscDemo > 0)
			WriteStd ("(two parents):");
#endif
		}
	    } else if (eptr -> NmEAct > 2) {	/* there are more than two that made the incorrect prediction */

#if CTEST
		if (DscDemo > 0)
		    WriteStd ("(pick parent comp):");
#endif

		for (i = 0, cummul = 0.0; i < nmparent; ++i) {

#if SUN3				/* kludge for SUN compiler bug */
		    prob = 1.0 / (int) (eptr -> NmEAct - i);
#else
		    prob = (float) 1.0 / (eptr -> NmEAct - i);	/* prob = 1 / #cfs made mistake - # used already */
#endif

		    randnum = URand01 ();

#if CTEST
		    if (DscDemo > 1) {
			sprintf (GOutBuff, "\n   NmEAct %d parent %d, randnum %f, prob %f",
				 eptr -> NmEAct, i, randnum, prob);
			WriteStd (GOutBuff);
		    }
#endif

		    for (mptr = eptr -> EffMtch; mptr != NULL; mptr = mptr -> NxtEMsg) {
			cummul += prob;

#if CTEST
			if (DscDemo > 1) {
			    sprintf (GOutBuff, "\n	cummul %f", cummul);
			    WriteStd (GOutBuff);
			}
#endif

			if (randnum <= cummul) {
			    Parents[i] = mptr -> Producer;	/* Just take first one we find! */

#if CTEST
			    if (DscDemo > 0)
				WriteStd ("(got one):");
#endif

			    break;
			}
		    }
		}			/* endfor */
	    }

#if GENLOG
	    if (GenFlg) {
		sprintf (GOutBuff, "P(WRG-USED) %d", Parents[0] -> Cf_Id);
		WriteGen (GOutBuff);
		sprintf (GOutBuff, " & %d", Parents[1] -> Cf_Id);
		WriteGen (GOutBuff);
	    }
#endif
	}
	/* end parent from effector matches */ 
	else if (NmCfPost > 0) {	/* didn't make a guess, but cfs did post msgs */
	    nmparent = PkBPrnts (CEffs, 'e');	/* Pick high-bidder ('e' = just need one or two) */

	    if (nmparent == 1 && CEffs == 2) {	/* Use the one for both parents! */
		Parents[1] = Parents[2];
		nmparent = 2;
	    }
	    TotCEfB += nmparent;

#if CTEST
	    if (DscDemo > 0)
		WriteStd ("(parent from bidders without guesses):");
#endif

#if GENLOG
	    if (GenFlg) {
		sprintf (GOutBuff, "P(BIDDERS-NOGUESS) %d", Parents[0] -> Cf_Id);
		WriteGen (GOutBuff);
	    }
#endif
	}
	/* end parent from bidders */ 
	else {				/* didn't guess or bid */

#if CTEST
	    if (DscDemo > 0)
		WriteStd (": No bidders at all, so NONE generated.");
#endif

	    continue;			/* Do NOT use parents that didn't bid */

	}				/* end parent from full cflist */

       /* Now Parents[0] (and maybe 1) should contain pointer to classifier(s) to use as parent(s) */

	if (Parents[0] != NULL) {	/* we really do have a parent (or two)....	*/

#if CTEST
	    if (DscDemo > 0) {
		PutCNd (Parents[0], GCfBuff, DEMOFMT1);
		if (nmparent == 1) {
		    sprintf (GOutBuff, "\n  %sOffspring:\n  ", GCfBuff);
		    WriteStd (GOutBuff);
		} else {
		    sprintf (GOutBuff, "\n  %s\n  ", GCfBuff);
		    WriteStd (GOutBuff);
		    PutCNd (Parents[1], GCfBuff, DEMOFMT1);
		    sprintf (GOutBuff, "\n  %sOffspring:\n  ", GCfBuff);
		    WriteStd (GOutBuff);
		}
	    }
#endif

	    ++TotCEf;			/* increment the total counter */

	    newcfptr = ReprodCf (Parents[0]);	/* reproduce the parent, maybe mutate it */
	    MakeActS (GMsgBuff);	/* generate new action part at random */
	    AActtoB (GMsgBuff, newcfptr -> NewActB, newcfptr -> NewActD);
	    if (CEffSplz == 2 && newcfptr -> NewBR != (float) 1.0)
		SpclzNCf (newcfptr);	/* specialize its conditions a bit */

#if GENLOG
	    if (GenFlg) {
		WriteGen ("\n  ");
		WrtNCf (newcfptr, GOutBuff, 1);
		WriteGen (GOutBuff);
	    }
#endif

#if CTEST
	    if (DscDemo > 0) {
		WrtNCf (newcfptr, GOutBuff, 1);
		WriteStd (GOutBuff);
	    }
#endif

	    if (nmparent == 2 && Parents[1] != NULL) {
		++TotCEf;
		newcfptr = ReprodCf (Parents[1]);	/* reproduce the second parent, maybe mutate it */
		if (CEffSplz > 0 && newcfptr -> NewBR != (float) 1.0)
		    SpclzNCf (newcfptr);/* specialize its conditions a bit */
		else {
		    MakeActS (GMsgBuff);/* generate new action part at random */
		    AActtoB (GMsgBuff, newcfptr -> NewActB, newcfptr -> NewActD);
		}

#if GENLOG
		if (GenFlg) {
		    WriteGen ("\n  ");
		    WrtNCf (newcfptr, GOutBuff, 1);
		    WriteGen (GOutBuff);
		}
#endif

#if CTEST
		if (DscDemo > 0) {
		    WrtNCf (newcfptr, GOutBuff, 1);
		    WriteStd (GOutBuff);
		}
#endif
	    }

#if CTEST
	    else {
		sprintf (GOutBuff, "\nDscCEffs: Second parent not found, new cf not created (step %d)\n",
			 CycleStp);
		WriteStd (GOutBuff);
	    }
#endif

	}

#if CTEST
	else {
	    sprintf (GOutBuff, "\nDscCEffs: No parent found! No cf created (step %d)\n",
		     CycleStp);
	    WriteStd (GOutBuff);
	}
#endif

    }					/* all the effectors */

}					/* DscCEff */


/*****************************

SpclzNCf		Specialize the condition part of a new classifier.

	NewCf 		Pointer to new classifier (an NCfNode) to specialize.
				NOTE WELL: This cf must have BidRatio < 1!

	Return		1 if specialized, 0 if not.

Pick one loci to change so classifier is more specific.
Thus if in MATCH condition, change # to 0 or 1 (equiprobably).
If NOTMATCH condition, change 0 or 1 to #!

******/

int 
SpclzNCf (NewCf)
    struct NCfNode *NewCf;
{
    short int nmpound, plocs[STRNGSZ * 2];
    register unsigned int loci;
    char string1[STRNGSZ], string2[STRNGSZ], oldval;

    BCndtoA (NewCf -> NewCnd1B, NewCf -> NewCnd1D, string1);
    BCndtoA (NewCf -> NewCnd2B, NewCf -> NewCnd2D, string2);

    nmpound = 0;
    for (loci = 0; loci < STRNGSZ; ++loci)
	if (string1[loci] == '#')
	    plocs[nmpound++] = loci;	/* store locus */

    if (NewCf -> NewCnd2T == CMATCH) {
	for (loci = 0; loci < STRNGSZ; ++loci)
	    if (string2[loci] == '#')
		plocs[nmpound++] = loci + STRNGSZ;	/* locus in condition2 is locus + STRNGSZ */
    } else {				/* NOT condition, so count NOT #'s */
	for (loci = 0; loci < STRNGSZ; ++loci)
	    if (string2[loci] != '#')
		plocs[nmpound++] = loci + STRNGSZ;
    }

    if (nmpound == 0) {			/* no can do... */

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "\n**SpclzNCf: nothing to specialize in cf %d.\n", NewCf -> NewCf_Id);
	    WriteStd (GOutBuff);
	}
#endif

	return (0);
    }
    loci = URandN ((nmpound - 1));	/* pick one at random, 0..nmpound */
    loci = plocs[loci];			/* get the actual locus */

#if CTEST
    if (DscDemo > 0) {
	WriteStd ("	|- Spclz: ");
	if (DscDemo > 1) {
	    sprintf (GOutBuff, "(Nm# %d, loci %d) ", nmpound, loci);
	    WriteStd (GOutBuff);
	}
    }
#endif

    if (loci < STRNGSZ) {		/* its in first condition */
	oldval = string1[loci];		/* save old value for demo or geneology */
	if (oldval == '#') {		/* check just in case... */
	    if (URand01 () <= 0.5)
		string1[loci] = '1';
	    else
		string1[loci] = '0';

	    ACndtoB (string1, NewCf -> NewCnd1B, NewCf -> NewCnd1D);
	    NewCf -> NewBR += MutdSpec;	/* increment specificity. */
	}

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "1st Cnd @loc %u, '%c' to '%c'.\n  ", loci, oldval, string1[loci]);
	    WriteStd (GOutBuff);
	}
#endif
    } else {				/* its in 2nd condition */
	loci -= STRNGSZ;		/* remove STRNGSZ added earlier to get actual locus */
	oldval = string2[loci];		/* save old value for demo or geneology */
	if (NewCf -> NewCnd2T == CMATCH) {
	    if (oldval == '#') {	/* check just in case... */
		if (URand01 () <= 0.5)
		    string2[loci] = '1';
		else
		    string2[loci] = '0';
		NewCf -> NewBR += MutdSpec;
	    }
	} else {			/* specialize NOT by changing to # ! */
	    if (oldval != '#') {
		string2[loci] = '#';
		NewCf -> NewBR += MutdSpec;
	    }
	}

	ACndtoB (string2, NewCf -> NewCnd2B, NewCf -> NewCnd2D);

#if CTEST
	if (DscDemo > 0) {
	    sprintf (GOutBuff, "2nd Cnd @loc %u, '%c' to '%c'.\n  ", loci, oldval, string2[loci]);
	    WriteStd (GOutBuff);
	}
#endif
    }

    return (1);

}					/* SpclzNCf */

#endif /* CDSC_ALL */
