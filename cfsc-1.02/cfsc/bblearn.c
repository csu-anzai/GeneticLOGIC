/*		BBLEARN for the CFS-C Classifier System

This file, BBLEARN.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file carry out the "bucket-brigade" learning algorithm
of the CFS-C classifier system major-cycle.

Subroutines in this file include:

	UpdCfStr	Update classifier strength, using Bucket Brigade.
	ApplyBB 	Apply bucket brigade (called by UpdCfStr).
	ApplyBB1
	PaySuppl	pay suppliers of input messages used to produce a new message.
	DemoBB		demo-display bucket-brigade action.

**/ 

#include	"compiler.h"
#include	<math.h>
#if ( LATTICEC || CBELLMTS || SUN3 || MPWC )
#include	<ctype.h>
#endif
#include	"utility.h"
#include	"core.ext"
#include	"cfops.ext"
#include	"dsclearn.ext"
extern	unsigned int DemoLev;
extern	char			*GOutBuff;
float	URand01();
VOID	UpdCfStr(), ApplyBB(), ApplyBB1(), GetDMCnt(), DemoBB();
float	PaySuppl();

/*****************************

UpdCfStr		Update classifier strengths.

1. Subtract appropriate taxes.
2. Add payoff from environment to classifiers which produced messages.
3. Apply bucket-brigade (or other mechanism) to move strengths from classifier to classifier.

Note that the ShareRew system variable controls how the reward from the environment is distributed:
ShareRew Meaning
-------- --------------
	0		No sharing: each active classifier gets full reward.
	1		Share: each active classifier gets equal share of reward, and total is equal to reward from environment.
	2		No Sharing, as above, BUT reward each cf receives is multiplied by its BidRatio ** BRPow.
	3		Share, as above, BUT reward each cf receives is multiplied by its BidRatio ** BRPow.

BBType	Meaning
------	-------
0		Standard
1		Pay all matched messages
2		SysRew biaded by k
3		Goldberg's VSB
4		Goldberg's Necessity Auction


NOTE: These classifier state varaibles are also updated here:
	TotProd		total number of messages a classifier has produced.
	TotPost		total number of messages a classifier has posted (AFTER effector conflict resolution).
	TotPosRw	total number of times classifier active when reward > 0 received.
	TotNegRw	total number of times classifier active when reward < 0 received.
	TOTNmPRw	total number of classifiers that have had positive rewards.
	TOTNmNRw	total number of classifiers that have had negative rewards.

******/ 

float   WinnersTotBid = 0;


VOID UpdCfStr()
{
	register unsigned int	cfcnt, nmsomsg; 	/* nmsomsg = number source messages to a cf */
	unsigned int			nmefpost;			/* number cfs posted effector messages */
	float					newstr, changestr, tottax, ptax, bid, eshare, ishare;
	register float			ftemp;
	struct	CfNode			*cptr;
    float                   WinnersAveBid = 0.0;
#if CISYSREW
	float					ISysRew();
#endif

#if CTEST
	if ( DemoLev > 1 )
		WriteStd( "\nEntering UpdCfStr(): Updating Rule Strength..." );
#endif

        /* AllPayBid == 2 means get a total bid of all rules that POST
            messages (after conflict resolution), then get average of that.
            Pay that average rather than own bid, ala Holyoak etal.
        */

    if ( AllPayBd == 2 && NmCfPost > 0 ) {
    	for ( cptr = CandCfs; cptr != NULL; cptr = cptr->NxtCand ) {
            if ( cptr->NmPost > 0 ) {
                WinnersAveBid += cptr->CfBid;
            }
        }
        WinnersAveBid /= NmCfPost;
#if CTEST
	    if ( DemoLev > 0 ) {
			sprintf( GOutBuff, "\nFor %d winners, AveBid is %7.3f.", NmCfPost, WinnersAveBid );
			WriteStd( GOutBuff );
		}
#endif
    }        

		/* 24 Jan 91: BBType==2 means modulate SysRew by k, same k used to modulate bids. 
				NOTE: THis doesn't work for coupled rules, only for R from env */
	if ( BBType == 2 ) {
		SysRew *= Bid_k;
	}

	if ( ShareRew % 2 == 1 && NmCfPost > 0 ) {	/* if share reward amoung classifiers ... */
		if ( FrPayNEC == (float) 1.0 ) {
			eshare = SysRew / NmCfPost; 	/* pay all messages the same */
			ishare = eshare;
		}
		
		else {		/* Use FrPayNEC to pay non-effector msgs produces less of a reward share */
			for ( cptr = CandCfs, nmefpost = 0; cptr != NULL; cptr = cptr->NxtCand )
				if ( cptr->PstEfMsg > 0 )		/* get number of cfs that produced effector msgs */
					++nmefpost;
					
			if ( FrPayNEC == (float) 0.0 ) { 
				if ( nmefpost != 0 )
					eshare = SysRew / nmefpost; /* pay all to effector-message posters */
				else
					eshare = 0;
				ishare = 0; 					/* and none to others */
			}
			else {	/* 0 < FrPayNEC < 1.0 */
				if ( nmefpost == 0 ) {
					eshare = 0;
					ishare = FrPayNEC * SysRew / NmCfPost;
				}
				else {
					ftemp = ( (NmCfPost - nmefpost) * FrPayNEC ) + nmefpost;
#if CTEST
					if ( DemoLev > 0 ) {
						sprintf( GOutBuff, "\nnmNECpost %d, nmefpost %d, FrPayDet %.2f, ftemp %.0f",
							(NmCfPost - nmefpost), nmefpost, FrPayNEC, ftemp );
						WriteStd( GOutBuff );
					}
#endif
					eshare = SysRew / ftemp;		/* effector activators get this (perhaps bigger) share */
					ishare = eshare * FrPayNEC; 	/* others get this (perhaps smaller) share */
#if CTEST
					if ( DemoLev > 0 ) {
						sprintf( GOutBuff, " eshare %.0f, ishare %.0f", eshare, ishare );
						WriteStd( GOutBuff );
					}
#endif
				}
			}
		}
	}

	else {							/* not share SysRew, so... */
		eshare = SysRew;				/* effector activators get full reward */
		ishare = SysRew * FrPayNEC; 	/* others may get less if FrPayNEC < 1 */
	}

	for ( cfcnt = 1, cptr = CurCfs; cfcnt <= NmCfs; ++cfcnt, ++cptr ) {

		if ( AHeadTax == (float) 0.0 )	{		/* if Additive (abs amount) Head tax is off */
			tottax		= HeadTax;					/* ALL pay this tax (rate). */
			changestr	= 0.0;						/* init changestr */
		}
		else {									/* using the Additive tax */
			changestr = AHeadTax;					/* init changestr to reflect the tax */
			tottax		= 0.0;
		}

		if ( cptr->NmMtch > 0 ) {					/* Then this classifier was a bidder */
			tottax		+= BidTax;						/* Pay the bid tax */

			if ( cptr->NmProd == 0 )					/* didn't win bidding competition... */
				tottax += FBidTax;						/* so tax it with the bid/fail-win tax */
			
			else {		/* Won bid and produced msgs (before effector confl res) */
				cptr->StpLPrd = CycleStp;				/* mark it as producer this step. */
				cptr->TotProd += cptr->NmProd;			/* Increment this accumulator. */
#if SUN3 /* kludge for bug in SUN3 compiler */
				ptax = (float) cptr->NmProd / (int) NmIMsgMx;	/* producer tax proport to fraction of MsgList filled */
#else
				ptax = (float) cptr->NmProd / NmIMsgMx; /* producer tax proportional to fraction of MsgList filled */
#endif
				ptax = pow( ptax, PrdTaxPw );			/* raise to power to pressure generalists even more */
				tottax += ptax * PrdTaxMx;				/* Add Producer tax, scaled from 0 to the maximum rate. */

				if ( cptr->NmPost > 0 ) {			/* It posted messages (AFTER effector conflict resolution). */
					cptr->StpLPst = CycleStp;			/* mark it as posting messages this step */
					cptr->TotPost += cptr->NmPost;		/* Increment this accumulator. */

					if ( BBType == 3 ) {
						ftemp = eshare - cptr->CfBid; 
						ftemp *= ftemp;
						if ( DemoLev > 0 ) {
							sprintf(GOutBuff,"\nCf %u: s %f, bid %.2f, bb %.2f, eshare %f, df2 %f, oV %f.",
								cptr->Cf_Id, cptr->Strength, cptr->CfBid, cptr->CfBBid, eshare, ftemp, cptr->Var );
							WriteStd( GOutBuff );
						}
						cptr->Var = ((1.0 - Varb) * cptr->Var) + (Varb * ftemp);
					}

#if CISYSREW	/* compile code for individual rewards */
					changestr = ISysRew( cptr );		/* Each cf that posts msgs gets the its own reward */

					if ( changestr > 0 ) {
						cptr->TotPosRw += 1;			/* Increment total count of rewards. */
						++TOTNmPRw; 					/* increment total for all classifiers */
					}
					else if ( changestr < 0 ) {
						cptr->TotNegRw += 1;
						++TOTNmNRw;
					}

#else	/* compile standard code--all active classifiers get same reward from the environment */

					if ( ShareRew <= 1 ) {		/* don't modify share by BidRatio */
						if ( cptr->PstEfMsg )
							changestr = eshare; 	/* effector activator share */
						else
							changestr = ishare; 	/* internal message producer share */
					}

					else {						/* bias reward by bidratio raised to a power */
						if ( BRPow == 0 )
							changestr = 1;
						else if ( BRPow == 1 )
							changestr = cptr->BidRatio;
						else if ( BRPow == 2 )
							changestr = cptr->BidRatio * cptr->BidRatio;
						else
							changestr = pow( cptr->BidRatio, BRPow );

						if ( cptr->PstEfMsg )
							changestr *= eshare;	/* effector activator share */
						else
							changestr *= ishare;	/* internal message producer share */
					}

					if ( SysRew > 0 ) {
						cptr->TotPosRw += 1;		/* Increment total count of rewards. */
						++TOTNmPRw; 				/* increment total for all classifiers */
					}

					else if ( SysRew < 0 )	{
						cptr->TotNegRw += 1;
						++TOTNmNRw;
					}
#endif
				}

			}	/*classifier was a producer */

		}	/* classifier was a bidder */

		changestr -= (tottax * cptr->Strength); 	/* Apply the taxes */

		newstr = cptr->Strength + changestr;		/* get new strength value */

		if ( newstr > CfStrMax ) {				/* If strength is above maximum... */
			changestr = CfStrMax - cptr->Strength;	/* set change to difference and ... */
			newstr = CfStrMax;						/* set strength to the maximum. */
		}
		else if ( newstr < CfStrMin ) {			/* If strength is below minimum... */
			changestr = cptr->Strength - CfStrMin;	/* set to difference, and ... */
			newstr = CfStrMin;						/* set strength to the minimum. */
		}

		cptr->Strength = newstr;					/* In any case, store the new strength. */
		cptr->ChngStr = changestr;					/* store change in strength, too */
		SysTreas -= changestr;						/* update the system treasury. */

			/*	Now calCulate bid share to be paid for each input message used.
				Note that if the classifier has 2 match conditions, it requires
				two messages for each posted, whereas if the second condition is
				a not-match condition, it requires only 1 input message for each posted.

				NOTE:	If BBType is 1, pay for ALL matching messages,
						not just used to post messages.
			*/

		if ( cptr->NmProd > 0 ) {	/* If produced msgs, figure a BidShare based on "after tax" */
            if ( AllPayBd == 2 ) {	/* holyoak etal average bid payment */
                bid = WinnersAveBid;
#if CTEST
				if ( DemoLev > 0 ) {
					sprintf( GOutBuff, "\ncf %d: bid was %8.3f, now use ave %8.3f SysRew %6.1f.", 
						cptr->Cf_Id, cptr->CfBid, bid, SysRew );
					WriteStd( GOutBuff );
				}
#endif
			}
#ifdef THIS		/* its not obvious how to do this...with many cobidders... */
            else if ( AllPayBd == 3 ) {	/* goldberg's necessity auction */
                bid = WinnersAveBid;
#if CTEST
				if ( DemoLev > 0 ) {
					sprintf( GOutBuff, "\ncf %d: bid was %8.3f, now use ave %8.3f SysRew %6.1f.", 
						cptr->Cf_Id, cptr->CfBid, bid, SysRew );
					WriteStd( GOutBuff );
				}
#endif
			}
#endif
            else {
    			bid = cptr->CfBid;	/* resources and the amount bid and number of msgs used */
			}
			if ( BBType != 1 )		/* calc nmsomsg (num source messages) as described above */
				nmsomsg = (cptr->Cnd2Type == CMATCH) ? (cptr->NmProd * 2) : cptr->NmProd;
			else {
				nmsomsg = cptr->NmMMC1 + cptr->NmMMC1;	/* will pay all these matches */
				GetDMCnt( cptr );		/* must count number of ALL matches that are from detectors */
			}

				/*	DMDscnt (detector message discount) is set when DMShare is set.
					It is < 0 and reflects a bias toward paying source classifiers
					larger shares of the bid instead of paying for detector messages.
					DMDscnt = (1 - DMShare), where 0 <= DMShare <= 1.
					DMShare = 1 means pay same to detectors as to source classifiers.
					DMShare = 0.1 means pay 9x more to source classifier than to detector.

					Note the redundant code to keep the execution time down while
					trying to make all payments for detector messages used here.
				*/

			if ( cptr->NmDetMM == nmsomsg ) {	/* used only detector messages, so	*/
				bid *= FrPayDet;
				if ( bid <= newstr - CfStrMin ) 	/* if cf has enough to pay for its bid and still be S > min */
					cptr->BidShare = bid / nmsomsg;
				else
					cptr->BidShare = (newstr - CfStrMin) / nmsomsg;

				cptr->Strength	-= bid;				/* make payment for detector messages now */
				cptr->ChngStr	-= bid;				/* since only msgs from detectors, pay it all */
				SysTreas		+= bid;
			}

			else {			/* used some or all non-detector messages */
				ftemp = (cptr->NmDetMM * DMDscnt) + nmsomsg;	/* for biasing payments to classifiers */

				if ( bid <= newstr - CfStrMin ) 	/* if cf has enough to pay for its bid and still be S > min */
					cptr->BidShare = bid / ftemp;
				else					/* cf Str < bid, so to keep above minimum, set share so that pay as much as can */
					cptr->BidShare = (newstr - CfStrMin) / ftemp;

				if ( cptr->NmDetMM != 0 ) {		/* if some detector messages used */
					ftemp = (cptr->BidShare * DMShare) * FrPayDet;
					ftemp *= cptr->NmDetMM;
					cptr->Strength	-= ftemp;		/* make payment for them all now */
					cptr->ChngStr	-= ftemp;
					SysTreas		+= ftemp;
				}
			}

		}	/* was a producer */

	} /* each classifer */

	if ( NmNMsgs > 0 ) { 			/* if there were new messages produced... */
		if ( BBType == 0 || BBType == 2 ) /* Apply Bucket Brigade to pay for producing them. */
			ApplyBB();					/* this is "standard" version */
		else
			ApplyBB1(); 				/* Special Version */
	}

} /* UpdCfStr - update classifer strengths */

/*****************************

ApplyBB 	Apply the bucket-bridage algorithm to producers of all messages on the NewMsgs message list.


For each new message M, the producer of M must pay a share of its bid to each
producer of the 'input' messages used to produce M.
The 'input' messages are pointed to by MtchMsg1 and MtchMsg2 pointers in the
MsgNode for M. The amonut to pay is the BidShare value stored in the CfNode of producer of M.

NOTES:	1.	If the AllPayBd flag is off (0), ONLY pay for messages that have the
			IsMsg flag set TRUE--these are messages that have NOT been removed by conflict resolution.
			If AllPayBd is on (1), then pay for all messages produced.

            30 NOV 90: AllPayBid==2 => Pay equal share of total bid of all who
                posted messages (after conflict resolution), a la Holyoak etal.
                See UpdCfStr() above.
            
		2.	Payment can only increment strength to the maximum allowed, CfStrMax.
			The actual payment is just enough to push the receiver to the limit.
		3.	Payment for messages from detectors has already been done, so
			DO NOT make payments for those here.
		4.	For messages produced by a classi}er with a NOT condition, there is nothing
			to pay for that condition, so no payment is made.
		5.	The MsgUsed flag is set for messages used to produce the new messages,
			i.e., it is set for the messages that are being paid for here.

******/

VOID ApplyBB ( )
{
	register unsigned int	mcnt;
	register float			paid, share;
	struct MsgNode	*Input1, *Input2,		/* The messages used to produce this new message. */
					*mptr;					/* The produced message */

	for ( mcnt = 0, mptr = NewMsgs; mcnt <= LastNMsg; ++mcnt, ++mptr ) {
		if ( mptr->IsMsg || AllPayBd == 1 )	{
			Input1 = mptr->MtchMsg1;					/* first msg used */
			Input2 = mptr->MtchMsg2;					/* second message used (may be NULL if not-match condition) */
			paid = 0;									/* none paid yet */
			share = mptr->Producer->BidShare;			/* get the share */

			Input1->MsgUsed = TRUE; 					/* Mark the 'input' message as used to create new message */
			if ( Input1->Producer != NULL ) 			/* if not from detector */
				paid = PaySuppl( Input1->Producer, share ); /* Pay supplier, and store amt paid */

			if ( Input2 != NULL ) {					/* if there is a second input message... */
				Input2->MsgUsed = TRUE; 				/* Mark this one used, too */

				if ( Input2->Producer != NULL )
					paid += PaySuppl( Input2->Producer, share );

			}	/* endif there is a second input message */

			mptr->Producer->Strength -= paid;			/* Pay up! */
			mptr->Producer->ChngStr -= paid;

		}	/* endif message mptr is really a message */
	}

#if CTEST
	if ( DemoLev > 0 )
		DemoBB();
#endif

} /* ApplyBB */


/*****************************

ApplyBB1	Apply the bucket-bridage algorithm to producers of all messages on the NewMsgs message list.

	*** This version pays to producers of all messages that match
		a classifier that POSTS one or more messages, whether or
		not those matches resulted in any of the posted messages.

		Thus is differs from ApplyBB() when OneMPerC is ON, and when
		a classifier doesn't end up POSTing all the messages it
		could PRODUCE.

NOTES:	1. Payment can only increment strength to the maximum allowed, CfStrMax.
			The actual payment is just enough to push the receiver to the limit.
		2.	Payment for messages from detectors has been done, so DO NOT
			make payments for those here.
		3.	For messages produced by a classifier with a NOT condition, there is nothing
			to pay for that condition, so no payment is made.
		4.	The MsgUsed flag is set for messages used to produce the new messages,
			i.e., it is set for the messages that are being paid for here.

******/

VOID ApplyBB1 ( )
{
	unsigned int	 m1ui, m1bit, matchcnt;
	register float	 paid, share;
	struct CfNode	*payer;
	struct MsgNode	*msgptr;

	for ( payer = CandCfs; payer != NULL; payer = payer->NxtCand ) {
		if ( payer->NmPost == 0 )			/* Skip those that didn't POST messages */
			continue;

		paid = 0;

#if CTEST
		if ( DemoLev >= 2 ) {
			sprintf( GOutBuff, "\nCf %d making BB payments...", payer->Cf_Id );
			WriteStd( GOutBuff );
		}
#endif
			/* pay for each (non-detector) message matching condition 1 */

		for ( m1ui = matchcnt = 0; matchcnt < payer->NmMMC1 && m1ui < INTPRML; ++m1ui ) {
			for ( m1bit = 0; matchcnt < payer->NmMMC1 && m1bit < INTSZ; ++m1bit ) {
				if ( payer->MMBitsC1[m1ui] & ORMBits[m1bit] ) {	/* Msg matched by condition 1 */
					msgptr = (CurMsgs + (m1ui * INTSZ) + m1bit);	/* get pointer to it */
					msgptr->MsgUsed = TRUE; 			/* mark it used and... */
					if ( msgptr->Producer != NULL ) { /* if its NOT a detector message... */
						paid += PaySuppl( msgptr->Producer, payer->BidShare );	/* Pay up */
#if CTEST
						if ( DemoLev >= 3 ) {
							sprintf( GOutBuff, " (Pay %.1f to cf %d)", payer->BidShare, msgptr->Producer->Cf_Id );
							WriteStd( GOutBuff );
						}
#endif
					}
				}
			}
		}

			/* And pay for each (non-detector) message matching condition 2 */

		if ( payer->Cnd2Type == CMATCH ) {
			for ( m1ui = matchcnt = 0; matchcnt < payer->NmMMC2 && m1ui < INTPRML; ++m1ui ) {
				for ( m1bit = 0; matchcnt < payer->NmMMC2 && m1bit < INTSZ; ++m1bit ) {
					if ( payer->MMBitsC2[m1ui] & ORMBits[m1bit] ) {	/* Msg matched by condition 1 */
						msgptr = (CurMsgs + (m1ui * INTSZ) + m1bit);	/* get pointer to it */
						msgptr->MsgUsed = TRUE; 			 /* mark it used and... */
						if ( msgptr->Producer != NULL ) { /* if its NOT a detector message... */
							paid += PaySuppl( msgptr->Producer, payer->BidShare );	/* Pay up */
#if CTEST
							if ( DemoLev >= 3 ) {
								sprintf( GOutBuff, " (Pay %.1f to cf %d)", payer->BidShare, msgptr->Producer->Cf_Id );
								WriteStd( GOutBuff );
							}
#endif
						}
					}
				}
			}
		}	/* cond 2 is MATCH type */

		payer->Strength -= paid;		/* and take it from payer's account */
		payer->ChngStr	-= paid;

	}	/* processing candidate classifiers */

#if CTEST
	if ( DemoLev > 0 )
		DemoBB();
#endif

} /* ApplyBB1 */

/*****************************

GetDMCnt	Get count of detector messages that match classifier.
			Store result in CfPtr->NmDetMM.

			Used when BBType = 1, so payment can go to ALL matching
			messages, whether those are used to post messages or not.

******/

VOID GetDMCnt ( CfPtr )
	struct CfNode *CfPtr;
{
	unsigned int m1ui, m1bit, matchcnt;

	CfPtr->NmDetMM = 0;
	for ( m1ui = matchcnt = 0; matchcnt < CfPtr->NmMMC1 && m1ui < INTPRML; ++m1ui ) {
		for ( m1bit = 0; matchcnt < CfPtr->NmMMC1 && m1bit < INTSZ; ++m1bit ) {
			if ( ( CfPtr->MMBitsC1[m1ui] & ORMBits[m1bit] ) &&	/* Msg matched by condition 1 */
				(CurMsgs + (m1ui * INTSZ) + m1bit)->Producer == NULL ) /* its a detector message... */
				CfPtr->NmDetMM += 1;							/* so count it. */
		}
	}

	for ( m1ui = matchcnt = 0; matchcnt < CfPtr->NmMMC1 && m1ui < INTPRML; ++m1ui ) {
		for ( m1bit = 0; matchcnt < CfPtr->NmMMC2 && m1bit < INTSZ; ++m1bit ) {
			if ( ( CfPtr->MMBitsC2[m1ui] & ORMBits[m1bit] ) &&	/* Msg matched by condition 2 */
				(CurMsgs + (m1ui * INTSZ) + m1bit)->Producer == NULL ) /* its a detector message... */
				CfPtr->NmDetMM += 1;							/* so count it. */
		}
	}

} /* GetDMCnt */


/*****************************

PaySuppl		Pay supplier of 'input' message used to produce a new message.

	Supplier	Pointer to classifier to be paid (should not be NULL).
	Payment		maximum amount to be paid. Actual payment may be less if
				payment puts supplier over CfStrMax ('maximum classifer strength').

	Return		amount actually paid. 

	NOTE:	The ActCfPrv list (of classifiers active last step) is updated
			to reflect payments, if any, to those classifiers.
******/

float PaySuppl( Supplier, Payment )
	struct CfNode	*Supplier;
	float			Payment;
{
	register int	 i;
	register float	 chngestr;

	if ( Supplier->Strength + Payment <= CfStrMax )	/* Payment won't put supplier strength over limit... */
		chngestr = Payment; 						/* So pay it all */
	else											/* else it will put strength over limit, so... */
		chngestr = CfStrMax - Supplier->Strength;	/* Pay only enough to get to limit. */

	Supplier->Strength	+= chngestr; 				/* Update strength of supplier */
	Supplier->ChngStr	+= chngestr; 				/* and update change-strength counter */

	for ( i = 0; i < NmActCfTminus1; ++i )			/* find Supplier in previously active list */
		if ( ActCfPrv[i].ActCf == Supplier )
			break;

	if ( ActCfPrv[i].ActCf == Supplier )		/* just in case... */
		ActCfPrv[i].ChngStr += chngestr;
	else {
		WriteStd( "\n**PaySuppl: ActCfPrv[i] != Supplier (BUG)!" );
		sprintf( GOutBuff, "\nNmActCfTminus1 %d, supplier [%ld] id %d, ActCfPrv Ids:",
					 NmActCfTminus1, Supplier, Supplier->Cf_Id );
		WriteStd( GOutBuff );
		for ( i = 0; i < NmActCfTminus1; ++i ) {
			sprintf( GOutBuff, "\nActCfPrv[%d] Id %d", i, ActCfPrv[i].ActCf->Cf_Id );
			WriteStd( GOutBuff );
		}
		WriteStd( "\n" );
	}

	return( chngestr ); 								 /* return amount actually paid */
		
} /* PaySuppl */


/*****************************

DemoBB		'demonstration' display of Bucket Brigade.

******/

VOID DemoBB ( )
{
	register unsigned int	mcnt;
	float			dshare;
	int 			nmsomsg;
	struct CfNode	*prod;				/* producer message */
	struct MsgNode	*Input1, *Input2,	/* The messages used to produce this new message. */
					*mptr;				/* The produced message */

	WriteStd("\nApplying bucket brigade...");
	sprintf( GOutBuff,"\nMsg_Id  Producer to Input1-Prod (share) Input2-Prod (share)");
	WriteStd( GOutBuff );
	for ( mcnt = 0, mptr = NewMsgs; mcnt <= LastNMsg; ++mcnt, ++mptr ) {
		if ( mptr->IsMsg )	{
			prod = mptr->Producer;
			Input1 = mptr->MtchMsg1;
			Input2 = mptr->MtchMsg2;
			nmsomsg = (prod->Cnd2Type == CMATCH) ? (prod->NmProd * 2) : prod->NmProd;
			if ( prod->NmDetMM == nmsomsg )
				dshare = 1.0;
			else
				dshare = DMShare;
			if ( Input1->Producer != NULL ) {
				sprintf(GOutBuff,"\n %3u	  %3u		   %3u (%5.1f)",
					mptr->Msg_Id, prod->Cf_Id, Input1->Producer->Cf_Id, prod->BidShare );
			}
			else {
				sprintf(GOutBuff,"\n %3u	  %3u		   Det (%5.1f)",
					mptr->Msg_Id, prod->Cf_Id, (FrPayDet * dshare * prod->BidShare) );
			}
			WriteStd( GOutBuff );

			if ( Input2 == NULL )
				WriteStd("		  (no input2)");

			else {
				if ( Input2->Producer != NULL )
					sprintf(GOutBuff,"		  %3u (%5.1f)", Input2->Producer->Cf_Id, prod->BidShare );
				else
					sprintf(GOutBuff,"		  Det (%5.1f)", (FrPayDet * dshare * prod->BidShare) );
				WriteStd( GOutBuff );
			}
		}
	}

	WriteStd("\n"); 

} /* DemoBB	*/

