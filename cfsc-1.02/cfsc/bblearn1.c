/*  BBLEARN1.C

   26 July 1989:  Version that includes changes to UpdCfStr to allow for 
   accumulating changes before applying them, under control
   of user or environemnt.

/*
		BBLEARN for the CFS-C Classifier System

This file, BBLEARN.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file carry out the "bucket-brigade" learning algorithm
of the CFS-C classifier system major-cycle.

Subroutines in this file include:

	UpdCfStr	Update classifier strength, using Bucket Brigade.
	ApplyBB 	Apply bucket brigade (called by UpdCfStr).
	DemoBB		demo-display bucket-brigade action.

**/ 

#include	"compiler.h"
#include	<math.h>
#if ( LATTICEC || CBELLMTS || SUN3 || MPWC )
#include	<ctype.h>
#endif
int         sprintf();

#include	"utility.h"
#include	"core.ext"
#include	"cfops.ext"
#include	"dsclearn.ext"
extern	unsigned int DemoLev;
extern	char			*GOutBuff;
float	URand01();

VOID    WriteStd();
VOID	UpdCfStr(), ApplyBB(), DemoPaySuppliers();
VOID 	PaySuppliers(), TARPRulePaySuppliers(), RDTARulePaySuppliers();



/*****************************

UpdCfStr		Update classifier strengths.

1. Subtract appropriate taxes.
2. Add payoff from environment to classifiers which produced messages.
3. Apply bucket-brigade (or other mechanism) to move strengths from classifier to classifier.
4. The MsgUsed flag is set for messages used to produce the new messages,
   i.e., it is set for the messages that are being paid for here.

NOTE:
This version is designed as kludge for use with CARD, so that
if the card defined variable ControlBucketBrigade is TRUE, then
ChngStr is accumulated until the environment sets a switch indicating
ChngStr should be apply to real strength.  
In particular, apply ChngStr to Strength when BucketBrigadeReleased is TRUE.

So that ChngStr can be displayed, the step after BucketBrigade is set
TRUE, all ChngStr's are reset in GetDMsgs() in environment.

Note that the ShareRew system variable controls how the reward from the environment is distributed:
ShareRew Meaning
-------- --------------
	0		No sharing: each active classifier gets full reward.
	1		Share: each active classifier gets equal share of reward, and total is equal to reward from environment.

NOTE: These classifier state varaibles are also updated here:
	TotProd		total number of messages a classifier has produced.
	TotPost		total number of messages a classifier has posted (AFTER effector conflict resolution).
	TotPosRw	total number of times classifier active when reward > 0 received.
	TotNegRw	total number of times classifier active when reward < 0 received.
	TOTNmPRw	total number of classifiers that have had positive rewards.
	TOTNmNRw	total number of classifiers that have had negative rewards.

******/ 

VOID UpdCfStr()
{
	register unsigned int	cfcnt, mcnt;
	register struct	CfNode	*cptr;
	register struct MsgNode	*mptr;
	unsigned int			nmefpost;			/* number cfs posted effector messages */
    float                   effrewshare, newstr, reward;
	float					tottax, ptax;
    extern int              CallISysRew, BucketBrigadeReleased;
    extern float            ISysRew();

#if CTEST
	if ( DemoLev > 1 )
		WriteStd( "\nEntering UpdCfStr(): Updating Rule Strength..." );
#endif

        /* first pay suppliers and decrement by bids... */

	if ( NmNMsgs > 0 ) {	/* if there were new messages produced... */
        ApplyBB();
    }

        /* next get reward to pay to any effector activators... */

    if ( NmCfPost > 0 ) { 
        if ( ShareRew == 0 ) {
            effrewshare = SysRew;
        }

    	else {  /* Share reward amoung effector activators that agree with decision */
			for ( cptr = CandCfs, nmefpost = 0; cptr != NULL; cptr = cptr->NxtCand ) {
				if ( cptr->PstEfMsg > 0 )		/* get number of cfs that produced effector msgs */
					++nmefpost;
            }
					
			if ( nmefpost != 0 )
				effrewshare = SysRew / nmefpost; /* pay all to effector-message posters */
			else
				effrewshare = 0;
		} /* shared reward */
	}   /* some messages posted */

#if CTEST
    if ( DemoLev > 0 ) {
        sprintf( GOutBuff, "\n  Share of reward is %f. Rules that posted msgs =>", effrewshare );
        WriteStd( GOutBuff );
    }
#endif

        /* Now do adjustments, rule by rule... */

	for ( cfcnt = 1, cptr = CurCfs; cfcnt <= NmCfs; ++cfcnt, ++cptr ) {
		tottax		= HeadTax;					/* ALL pay this tax (rate). */

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

                if ( cptr->NmPost > 0 ) {
#if CTEST
                    if ( DemoLev > 0 ) {
                        sprintf( GOutBuff, " %d", cptr->Cf_Id );
                        WriteStd( GOutBuff );
                    }
#endif
				      	/* It posted messages (AFTER effector conflict resolution). */

					cptr->StpLPst = CycleStp;			/* mark it as posting messages this step */
					cptr->TotPost += cptr->NmPost;		/* Increment this accumulator. */

                        /* Get rewards if not using individual rewards */

                    if ( !UseISysRew ) {
                        if ( cptr->PstEfMsg ) {
                            cptr->ChngStr += effrewshare; 
                        }
                    }

				} /* cf posted messages (after effector resol.) */
			}	/*classifier was a producer */
		}	/* classifier was a bidder */


        if ( UseISysRew && CallISysRew ) {  /* if using "individual" rewards...and environment signals "call me" */
			reward  = ISysRew( cptr );	/* Each cf that posts msgs gets the its own reward */
            cptr->ChngStr += reward;
			if ( reward > 0 ) {
   				cptr->TotPosRw += 1;		/* Increment total count of rewards. */
    			++TOTNmPRw; 				/* increment total for all classifiers */
	    	}
		    else if ( reward < 0 ) {
			    cptr->TotNegRw += 1;
    			++TOTNmNRw;
			}
        }

            /* now update Chng (Bids already transfered and Rewards added, if any) */

		cptr->ChngStr -= (tottax * cptr->Strength);

            /* Calculate new strength, and check to keep in bounds */

        if ( BucketBrigadeReleased ) {
            newstr = cptr->Strength + cptr->ChngStr;
        	if ( newstr > CfStrMax ) {				    /* If strength is above maximum... */
    	    	cptr->ChngStr = CfStrMax - cptr->Strength;	/* set change to difference and ... */
	    		cptr->Strength = CfStrMax;			/* set strength to the maximum. */
    		}
	    	else if ( newstr < CfStrMin ) {		    	/* If strength is below minimum... */
    			cptr->ChngStr = cptr->Strength - CfStrMin;	/* set to difference, and ... */
	    		cptr->Strength = CfStrMin;  		/* set strength to the minimum. */
    		}
            else {
                cptr->Strength = newstr;
            }
        }

	} /* endfor each classifer */

        /* some final recalculations */

    if ( NmCfs > 0 ) {
#if SUN3 /* kludge for SUN 3 compiler bug */
	    AveCfStr = TotCfStr / (int) NmCfs;  /* Recalculate this averages */
#else
	    AveCfStr = TotCfStr / NmCfs;
#endif
    }

} /* UpdCfStr - update classifer strengths */

/*****************************

ApplyBB    Move bid(share) of consumers to strength of producers.

Note that this assumes each rule can produce at most one msg/step.

For each new message M, the producer of M must pay a share of its bid to each
producer of the 'input' messages used to produce M.
The 'input' messages are pointed to by MtchMsg1 and MtchMsg2 pointers in the
MsgNode for M. 

The MsgUsed flag is set for messages used to produce the new messages,
i.e., it is set for the messages that are being paid for here.

******/

VOID ApplyBB ( )
{
	register unsigned int	mcnt;
	struct MsgNode	        *mptr;				/* The produced message */
    struct CfNode           *producer;

#if CTEST
    if ( DemoLev > 0 ) {
        WriteStd( "\nApply Buicket Brigade:" );
    }
#endif

	for ( mcnt = 0, mptr = NewMsgs; mcnt <= LastNMsg; ++mcnt, ++mptr ) {
		if ( mptr->IsMsg )	{
            producer = mptr->Producer;
#if CTEST
            if ( DemoLev > 0 ) {
                sprintf( GOutBuff, "\nFor Msg %d, Cf %d pays =>", mptr->Msg_Id, producer->Cf_Id );
                WriteStd( GOutBuff );
            }
#endif
            producer->ChngStr -= producer->CfBid; /* Decrement strengths by bids */
			PaySuppliers( producer, mptr->MtchMsg1, mptr->MtchMsg2 ); /* transfer to prior producers */
        }
	} /* endfor each message */

} /* ApplyBB */



/*****************************

PaySuppliers

    Update ChngStr of classifier(s) that produced messages
    (on the previous step) used by Consumer classifier(s) on this step.

    Consumer's ChngStrs have already been decremented by its bids.
    Now just add the appropriate amount to the suppliers, if any.
    Do NOT pay any to detectors, of course.

*******/

VOID PaySuppliers ( Consumer, Msg1, Msg2 )
    struct CfNode   *Consumer;
    struct MsgNode  *Msg1, *Msg2;
{
    register int    i, mcnt;
    register float  bidshare;
    register struct CfNode   *supplier1, *supplier2;
    register struct MsgNode  *mptr;

            /* get pointers to suppliers, if any */

    if ( Msg1 != NULL ) {       /* get supplier of first input message */
        supplier1 = Msg1->Producer;
        Msg1->MsgUsed = TRUE;   /* msg was used this step */
    }
    else {
        supplier1 = NULL;
    }

    if ( Msg2 != NULL ) {       /* and for second input message */
        supplier2 = Msg2->Producer;
        Msg2->MsgUsed = TRUE;
    }
    else {
        supplier2 = NULL;
    }

       	/*	Calculate bid share to be paid for each input message used.
	        Note that if the classifier has 2 match conditions, it requires
   			two messages for each posted, whereas if the second condition is
    		a not-match condition, it requires only 1 input message for each posted.

            Also, pay bid only to classifiers. If one message is from
            a detector, and the other from a classifier, pay it all to the classifier.

            So it boils down to 4 cases, one of which involves no payments...
    	*/

    if ( supplier1 != NULL ) {
        if ( supplier2 == NULL ) {
                /* no second message, first from supplier---it gets full bid */
            supplier1->ChngStr += Consumer->CfBid;    /* update change */
        }

        else {  /* there is a first and second supplier... */
            bidshare = Consumer->CfBid / 2.0;     /* each gets 1/2 */
            supplier1->ChngStr += bidshare;     /* update change */
            supplier2->ChngStr += bidshare;
        } /* endelse there is first and second suppliers */
    }

        /* else supplier1 is NULL... */

    else if ( supplier2 != NULL ) {     /* but there is a supplier2 to pay. */
        supplier2->ChngStr += Consumer->CfBid;     /* update change */
    }

    DemoPaySuppliers( Consumer, supplier1, supplier2 );

} /* end PaySuppliers */




VOID DemoPaySuppliers ( Consumer, Supplier1, Supplier2 )
    struct CfNode *Consumer, *Supplier1, *Supplier2;
{

#if CTEST
    if ( DemoLev > 0 ) {
        if ( Supplier1 == NULL ) {
            WriteStd( " [Sup1 is Det; " );
            if ( Supplier2 == NULL )
                WriteStd( "Sup2 is Det or NOT]" );
            else {
                sprintf( GOutBuff, "Sup2 Cf %d: %3.1f]", Supplier2->Cf_Id, 
                    Consumer->CfBid );
                WriteStd( GOutBuff );
            }
        }
        else { /* Sup1 is not null, so... */
            if ( Supplier2 == NULL ) {
                sprintf( GOutBuff, " [Sup1 Cf %d: %3.1f,Sup2 NULL.]",
                    Supplier1->Cf_Id, Consumer->CfBid );
                WriteStd( GOutBuff );
            }
            else {
                sprintf( GOutBuff, " [Sup1/2 Cfs %d/%d: %3.1f]",
                    Supplier1->Cf_Id, Supplier2->Cf_Id, (Consumer->CfBid/2.0) );
                WriteStd( GOutBuff );
            }
        }
    }
#endif

} /* end DemoPaySuppliers */
