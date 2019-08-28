/*			CORE for the CFS-C Classifier System

This file, CORE.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file carry out all the steps of the
CFS-C classifier system "major-cycle" with the exception of
(a) the bucket-brigade	(see the file BBLEARN.C), and
(b) the discovery algorithm (see the file DSCLEARN.C).

This files contains these subroutines:
	RdDetect	append messages from detectors to current message-list. 
	GenCands	the 'inner-loop': generates MtchNode's and Candidate classifier list.
	GCndsDem	demo-display of candidate classifiers and match nodes.
	GenNMsgs	calculate bids, pick winners, generate new messages, link to effectors.
				(Linking is done by ChkEftrs, a function in ENVIRON.C ).
	PostCfMs	post classifiers's new messages (after it wins bidding competition).
	MakeNMsg	make a new message and store in NewMsgs list.
	CalcBid 	calculate bid for a classifier.
	GenBehav	Activate 'matched' effectors and change the environment accordingly.
	ChkEftrs	Link one message to all effectors it matches.
**/

#include	"compiler.h"
#include	<math.h>
#if ( LATTICEC || CBELLMTS || SUN3 || MPWC )
#include	<ctype.h>
#endif
#include	"utility.h"
#include	"core.def"
#include	"cfops.ext"

extern unsigned int DemoLev;
extern short int	ClampFlg;
extern char 		*GInBuff, *GOutBuff;

float	URand01();

VOID	RdDetect(), GCndsDem(), GenCand(), GenNMsgs(), CalcBid(), GenBehav(), ChkEftrs();


/********************************

RdDetect	Get messages from detectors and append to message list.

	This calls the Domain dependent subroutine, GetDMsgs .

	Notes:
	1. This is called on every cycle step, even though it actually gets messages only every
		DetectRt steps. I did this to allow for a system in which there are environment-detectors and
		internal-detectors, and in which the rate of reading messages from those detectors might be different.

	2. Note that NmDMsgMx (number of message-nodes on CurMsgs reserved for detector-messages)
		may be less than the compiled maximum number of detector messages, DMSGSSZ,
		since the program/user can modify the ratio of internal to detector messages allowed on the list.

******/

VOID RdDetect ( )
{
	unsigned int	i, NmDMRead;
	char	MStrings[(STRNGSZ*DMSGSSZ)+DMSGSSZ],	/* Storage for messages */
			*mp;									/* pointer to next string */
	float	MIntens[DMSGSSZ];						/* Storage for intensities */
	extern	unsigned int NmDetMsg;					/* Will be number of detector messages added to msglist */
	extern	unsigned int DetectRt;					/* Read in messages every DetectRt steps. */
	extern	unsigned int ADDMInt, ADDMFmt;			/* Autodisplay interval and format control */

	NmDetMsg = 0;								 	/* none yet */

	if ( CycleStp % DetectRt == 0 ) {
		for ( i = 0; i < DMSGSSZ; ++i ) 		 	/* set all entries to default */ 
			MIntens[i] = MsgIntDf;

		NmDMRead = GetDMsgs( MStrings, MIntens );	/* Get the messages and intensities */	

		if ( NmDMRead > NmDMsgMx ) {
			sprintf(GOutBuff,"\nWARNING (RdDetect): GetDMsgs returned too many (%d) msgs.", NmDMRead );
			WriteStd( GOutBuff );
			sprintf(GOutBuff,"\nLoading only %d.", NmDMsgMx );
			WriteStd( GOutBuff );
			NmDMRead = NmDMsgMx;
		}
		TOTDtMsg += NmDMRead;						/* Increment total for run. */
						
		for ( i = 0, mp = MStrings; i < NmDMRead; ++i, mp = &MStrings[(STRNGSZ*i)+i] ) {
			CopyChar( GInBuff, mp, STRNGSZ );		/* Just get the next message string */
			sprintf( &GInBuff[STRNGSZ], ",,%f", MIntens[i] );
			StoreMsg( GInBuff, 'd' );				/* Add to CurMsgs as FromDet, increment NmCMsgs, NmDetMsg */
		}

		if ( CycleStp % ADDMInt == 0 )				/* If auto-display this step... */
			DsplMsgs( "", ADDMFmt, 'd' );			/* display detector messages */

	}

} /* RdDetect */


/********************************

GenCands	Generate list of "candidate classifiers" to be used in bidding procedure. 

Also generate a set of "candidate matches", i.e., a tuples (Classifier, Msg1, Msg2)
such that Msg1 satisifies first conditon of the Classifier & Msg2 satisfies the second condition.
These are represented, for each Cf, by bits set in the ( MMBitsC1 X MMBitsC2 ) arrays in each CfNode.

Note: For NON-MATCH conditions, some Msg2's, etc., would be NULL
		with the implication that no message exists that satifies the condition.)

Entry:	CurCfs		the list of classifiers (just an array of nodes).
		CurMsgs		pointer to first message on the current message list (from last step and from Environment).

Exit:	CandCfs		list of candidate classifiers. This just runs through nodes in the CurCfs list.
		Each candidate classifier node has:
		NmMtch = number of candidate matches the classifier has.
		MMBitsC1 = string bits (int array), one bit per possible message, set to 1 when message matches cond 1.
		NmMMC1 = number of messages that match condition 1.
		MMBitsC1 = string bits (int array), one bit per possible message, set to 1 when message matches cond 1.
		NmMMC1 = number of messages that match condition 1.
		NmProd = NmPost = 0 - it hasn't produced any new messages yet. 
		Support = sum of intensities from all the messages that satisified
					the classifier, i.e., of all the messages pointed to by the 
					candidate matches hanging on this classifier.
					NOTE WELL -- In the current version:
						Each message contributes its support ONCE ONLY to a classifier, even
						when that message participates in a number of 'matches' (cf,msg1,msg2 triples).

Also accumulate some totals:
	 TotCCSup	total support for all candidate classifiers
	 NmCandCf	total number of candidate classifiers.
	 NmMtchs	total number of candidate matches (candidate new messages).		

 NOTE:	A starting place in the classifier list is chosen at random,
		and classifiers are processed from the start to the end of the list,
		and then from the beginning of the list (array) to the chosen start classifier.
		This eliminates (over time) any bias introduced by generating a list
		of candidate (matched) classifiers in a particular order.

******/

GenCands ( )
{
	struct	CfNode	*cp;				/* Point to classifier being processed. */
	struct	MsgNode	*mp1, *mp2;			/* Point to messages being processed. */
	int 	IsMatch2, cfcnt, mcnt1, mcnt2,
			m1bui, m1bit, m2bui, m2bit; 	/* Flags and loop counters. */
	unsigned int	randcnt, randstrt, randend;		 /* for random start in cflist */
	register int	i;

#if CTEST
	if ( DemoLev > 0 )
		WriteStd("\nGenerating matches...");
#endif

		/* get random place to start in cflist */

	randstrt = URandN( NmCfs - 1 ); 	/* From 0..NmCfs-1 */
	
	for ( randcnt = 0; randcnt <= 1; ++randcnt ) {	/* process two parts of list */
		if ( randcnt == 0 ) {		/* first... */
			cp = CurCfs + randstrt; 	/* start in middle, somewhere in 1st to NmCfs-th spot */
			cfcnt = randstrt + 1;		/* as if we did randstart, so +1 is next */
			randend = NmCfs;			/* go to end */
		}
		else {						/* second... */ 
			cp = CurCfs;				/* start at beginning */
			cfcnt = 1;
			randend = randstrt; 		/* do the randstrt we skipped in first time through */
		}
	
		for ( ; cfcnt <= randend; ++cfcnt, ++cp ) {
			cp->NmMMC1 = cp->NmMMC2 = 0;				/* Init count of matchs for conditions */
			cp->Support = 0;							/* Init support */
			for ( i = 0; i < INTPRML; ++i ) 			/* Init array entries that have bits set */
				cp->MMBitsC1[i] = cp->MMBitsC2[i] = 0;	/* for messages matched by the conditions */

#if CTEST
			if ( DemoLev == DEBUGFMT )	{
				sprintf( GOutBuff, "\nCheck cf %u:", cp->Cf_Id );
				WriteStd( GOutBuff );
			}
#endif
			for ( mp1=CurMsgs, mcnt1=0; mcnt1 <= LastCMsg; ++mcnt1, ++mp1 ) {
				if	( !mp1->IsMsg ) continue;			/* skip if msg removed by effector conflict resolution */

				for ( i = 0; i < INTPRSTR; ++i) 		/* check mp1 against condition 1 */
					if ( ( (mp1->Message[i] & cp->Cnd1DCs[i]) ^ cp->Cnd1Bits[i]) )	break;

				if ( i == INTPRSTR ) {				/* Must have matched the first condition. */
					m1bui = mcnt1 / INTSZ; 			/* get appropriate entry in MMBitsC arrays */
					m1bit = mcnt1 % INTSZ; 			/* get appropriate bit in the entry */
					cp->MMBitsC1[m1bui] = cp->MMBitsC1[m1bui] | ORMBits[m1bit];	/* set the bit now */
					cp->NmMMC1++;						/* increment count of matches for cond 1 */
					if	( !( cp->MMBitsC2[m1bui] & ORMBits[m1bit] ) )	 /* if not matched already */
						cp->Support += mp1->Intensit;	/* Accumulate support from message that matched condition 1 */
#if CTEST
					if ( DemoLev == DEBUGFMT )	{
						sprintf(GOutBuff,"\n   Msg %u matched cond1 (sup %f).", mp1->Msg_Id, cp->Support );
						WriteStd( GOutBuff );
					}	
#endif
					if	( cp->Cnd2Type == CMATCH )	 {
#if CTEST
						if ( DemoLev == DEBUGFMT )
							WriteStd( " Check MATCH cond 2:" );
#endif
						for ( mp2=CurMsgs, mcnt2=0; mcnt2 <= LastCMsg; ++mcnt2, ++mp2 ) {
							if	( !mp2->IsMsg ) continue;			/* skip if msg removed by effector conflict resolution */

							for ( i = 0; i < INTPRSTR; ++i) 		/* check mp2 against condition 2 */
								if ( ( (mp2->Message[i] & cp->Cnd2DCs[i]) ^ cp->Cnd2Bits[i]) )	 break;

							if ( i == INTPRSTR ) {				/* Must have matched the 2nd condition. */
								GenCand( cp );						/* Make cp a candidate classifier */
								mp1->MsgMtchd = TRUE;				/* Messages were matched by at least one bidding cf */
								mp2->MsgMtchd = TRUE;
								m2bui = mcnt2 / INTSZ; 			/* get appropriate entry in the MMBitsC arrays */
								m2bit = mcnt2 % INTSZ; 			/* get appropriate bit in that entry */
								if ( !( cp->MMBitsC2[m2bui] & ORMBits[m2bit] ) ) {
									cp->MMBitsC2[m2bui] = cp->MMBitsC2[m2bui] | ORMBits[m2bit]; /* set the bit */
									cp->NmMMC2++;						/* incr. counter of matches for cond 2 */
									if ( !( cp->MMBitsC1[m2bui] & ORMBits[m2bit] ) ) /* didn't match first cond, either */
										cp->Support += mp2->Intensit;	/* Accumulate support */
								}
#if CTEST
								if ( DemoLev == DEBUGFMT ) {
									sprintf(GOutBuff,"\n	GenMatch with Msg %u (sup %f).", mp2->Msg_Id, cp->Support );
									WriteStd( GOutBuff );
								}
#endif
							}
						}	 /* each message compared to each condition 2 */
					}

					else { 				/* Cond. 2 is NOTMATCH type */
#if CTEST
						if ( DemoLev == DEBUGFMT )	
							WriteStd("   Checking NOTMATCH cond2:");
#endif
						for ( IsMatch2=FALSE, mp2=CurMsgs, mcnt2=0; mcnt2 <= LastCMsg; ++mcnt2, ++mp2 ) {
							if	( !mp2->IsMsg ) continue;			/* skip if msg removed by effector conflict resolution */
#if CTEST
							if	( DemoLev == DEBUGFMT ) {
								sprintf(GOutBuff,"  Check msg %u", mp2->Msg_Id );
								WriteStd( GOutBuff );
							}
#endif
							for ( i = 0; i < INTPRSTR; ++i) 		/* check mp2 against cond 2 */
								if ( ( (mp2->Message[i] & cp->Cnd2DCs[i]) ^ cp->Cnd2Bits[i]) )		break;

							if ( i == INTPRSTR ) {				/* Must have matched the first condition. */
#if CTEST
								if ( DemoLev == DEBUGFMT )
									WriteStd("--matched! NOT satisfied.");
#endif
								IsMatch2 = TRUE;
								break;
							}
						}	/* each messaage compared to condition 2 */

						if ( !IsMatch2 ) {						/* Nothing matches the NOTMATCH condition, so */
							GenCand( cp );							/* Make cp a candidate classifier */
							mp1->MsgMtchd = TRUE;					/* Messages were matched by at least one bidding cf */
							cp->Support += CfSuppDf;				/* add a preset amount of support */
#if CTEST
							if ( DemoLev == DEBUGFMT ) {
								sprintf(GOutBuff,"\n	  GenMatch for Msgs %u & NOT.", mp1->Msg_Id );
								WriteStd( GOutBuff );
							}
#endif
						}

					} /* checking non-match condition 2 */
				} /* condition1 matched */

			} /* Each message compared to each condition 1 */

				/* Clean up for this classifier. */

			if ( cp->NmMtch > 0 ) { 					/* If classifier is candidate, add its support to total. */
				++NmCandCf; 							/* incr total number of candidate classifiers. */
				TotCCSup += cp->Support;				/* incr total support for candidates */
				NmMtchs  += cp->NmMtch; 				/* incr total number of matches this step */
			}

		} /* each classifier */

	} /* each part of classifier list (randstarts) */

	if ( NmCandCf != 0 )
#if SUN3 /* kludge for SUN compiler bug */
		AveCCSup = TotCCSup / (int) NmCandCf;			 /* Calculate average support for candidates. */
#else
		AveCCSup = TotCCSup / NmCandCf; 				 /* Calculate average support for candidates. */
#endif
	else
		AveCCSup = 0;

#if CTEST
	if ( DemoLev > 0 )	GCndsDem(); 					/* Display some stuff */
#endif

	return( OK );

} /* GenCands -- Generate_Candidate_Matches */


/********************************

GenCand 	Generate a candidate classifier, i.e., add it to list.

	Also increment some counters.

******/

VOID GenCand ( cp )
	struct CfNode	*cp;
{

	cp->TotMtch += 1;				/* Increment this classifier's match count */
	cp->NmMtch	+= 1;

	if ( cp->NmMtch == 1 ) { 		/* First match for this classifier, so add it to candidate list */
#if CTEST
		if ( DemoLev == DEMDEBUG ) {
			sprintf(GOutBuff,"\n	 Cf %u added to CandCfs (now %u of them).", cp->Cf_Id, NmCandCf+1 );
			WriteStd( GOutBuff );
		}
#endif
		if ( CandCfs == NULL )			/* Its first one, and so last */
			LstCndCf = CandCfs = cp;
		else {					/* Append to end of other candidates */ 
			LstCndCf->NxtCand = cp;
			LstCndCf = cp;	
		}
		cp->NxtCand = NULL; 			/* In either case, none after this classifier. */
	}

} /* GenCand */


/********************************

GCndsDem	Demo display candidates

******/

VOID GCndsDem()
{

	sprintf(GOutBuff,"\nDone generating matches. Total generated: %3.3d", NmMtchs);
	WriteStd( GOutBuff );
	sprintf(GOutBuff,"\nTotal number of candidate classifiers:	%3.3d", NmCandCf );
	WriteStd( GOutBuff );
	sprintf(GOutBuff,"\nAverage support for candidates:		   %6.2f\n", AveCCSup );
	WriteStd( GOutBuff );

} /* GCndsDem */


/********************************

GenNMsgs	Generate new messages for candidates that win the bidding competition.
	
Generate new messages for candidate matches of (classifiers) that win the bidding.
1. Calculate a bid for the candidate classifier, and accumulate total.
	Also calculate and store biased bid for each candidate, and accumulate total biased bids.
2. For each of candidiate (bidding) classifier, see if its is a winner, i.e., gets to produce a new messages.
	See below for details on this 'competition' step.
3. For each classifier that is to generate a new messages:
	Generate new messages for every 'candidate match' on its Matches list. 
	That is, for each candidate Match:
	Generate the new message string and mark the msg node as being a as message ( MtchIsMs ).
	Set the messages intensity = bid of the classifier that is producing it.
	Link the message to any effectors it matches, and increment counter in message node
	to indicate the number of effectors it matches.
4. Update some local (to classifiers) and global (NmCfWon, NmNMsgs, etc.) totals.

First, if the number of candidate messages is less than maximum number of
messages allowed for internal (non-detector) messages, just put make them all winners --
in this case there is no competition, really.

However, if there are more matches (candidate messages) than can fit in new message list,
run a competition to pick classifiers to post messages.
The basic idea is to calculate a probability of winning for each classifier (and all its matches).
This will be a function of the bids (CURRENT: bid**2 over sum of bid**2 for all candidates)
Then make draws under this probability distribution, each draw designated a winner,
until the message list is full.

******/ 
	
VOID GenNMsgs ( )
{
	float			randnum, cumprob, totbbid, fractot;
	struct CfNode	*cfptr;
	struct MsgNode	*mptr;
	unsigned int	i, candtry; 				/* candtry is kludge for DelHal case */

#if CTEST
	if ( DemoLev > 0 )	{
		sprintf(GOutBuff,"\nGenerate msgs (max %d) from %d candidate-matches...", NmIMsgMx, NmMtchs );
		WriteStd( GOutBuff );
	}
#endif

	NmIEMsgs = NmIIMsgs = 0;					/* Haven't posted any effector or internal messages yet */

	if ( NmMtchs <= NmIMsgMx || (OneMPerC && NmCandCf <= NmIMsgMx) ) { /* There's room for all the matches. */
#if CTEST
		if ( DemoLev > 0 )
			WriteStd( "no competition:\n" );
#endif
		for ( cfptr = CandCfs; cfptr != NULL; cfptr = cfptr->NxtCand )	{
			CalcBid( cfptr );							/* calculate its bid */
			PostCfMs( cfptr );							/* Post all its candidate matches into messages */			
		}
	}
	else {											/* do some competition */
		candtry = 0;									/* No candidates have tried to produce msgs yet */
#if CTEST
		if ( DemoLev > 0 )
			WriteStd( "competition:\n" );
#endif
		for ( cfptr = CandCfs; cfptr != NULL; cfptr = cfptr->NxtCand )
			CalcBid( cfptr );							/* calculate its bid */
		totbbid = TotBBid;								/* Total 'biased' bids for candidates not yet winners */
		fractot = totbbid * 0.01;						/* keep record of 1/100 of total, to see when to re-sum. */

		while ( NmNMsgs < NmIMsgMx ) { 				/* while there is room for more new msgs. */
			randnum = URandF( totbbid );				/* randnum is uniform in 0...TotBBid: use it for the draw */
#if CTEST
			if ( DemoLev == DEMDEBUG ) {
				sprintf( GOutBuff, "\nPick winner (rand is %.4f, tot-bias-bids %7.4f):", randnum, totbbid );
				WriteStd( GOutBuff );
			}
#endif
			for ( cfptr=CandCfs, cumprob=0; cfptr != NULL; cfptr=cfptr->NxtCand ) {
				if ( cfptr->NmProd > 0 )
					continue;							/* It already won--ignore it */
				else {
					cumprob += cfptr->CfBBid;			/* cummuilative prob = sum of biased bids */
#if CTEST
					if ( DemoLev == DEMDEBUG ) {
						sprintf( GOutBuff, "\n   Cand Cf %u: cumprob %.4f ", cfptr->Cf_Id, cumprob );
						WriteStd( GOutBuff );
					}
#endif
					if	( randnum <= cumprob )			/* cf wins! */
						break;							/* so stop the 'draw' */
				}
			}

			if ( cfptr == NULL ) {					/* Oops! maybe a problem,--probably a rounding err */
				WriteStd("\n\nWARNING (GenNMsgs): Rand draw of winner didn't find 1 after >= 1 tries!");
				sprintf( GOutBuff, "\nCycleStp %d, NmNMsgs %d, totbbid %f, randnum %f, cumprob %f.",
						CycleStp, NmNMsgs, totbbid, randnum, cumprob );
				WriteStd( GOutBuff );
				WriteStd("\nTry again (assume round error).\n");
			}
			else {									/* got a winner, so... */
				PostCfMs( cfptr );						/* post its messages */
				++candtry;								/* this candidate tried to produce msgs */
				if ( candtry >= NmCandCf )				/* all candidates have tried to produce msgs... */
					break;								/* so stop */
				if ( NmNMsgs < NmIMsgMx )				/* If more room on messages list, */
					totbbid -= cfptr->CfBBid;			/* adjust total biased-bids down--one less candidate */
			}

			if ( totbbid < fractot ) {				/* its time to recalculate the total biased bid */
#if CTEST
				if ( DemoLev >= DEMDEBUG ) {
					WriteStd( "\n\nNOTE (GenNMsgs): Recalc. tot-bias-bid for rand draw." );
					sprintf( GOutBuff, "\nAt CycleStep %d:  NmNMsgs %d, totbbid %f, fractot %f.",
								CycleStp, NmNMsgs, totbbid, fractot );
					WriteStd( GOutBuff );
				}
#endif
				for ( totbbid=0.0, cfptr = CandCfs; cfptr != NULL; cfptr = cfptr->NxtCand )
					if ( cfptr->NmProd == 0 )			/* for those cfs that haven't won yet... */
						totbbid += cfptr->CfBBid;		/* add biased bid to the total biased bid */
				fractot = totbbid * 0.01;				/* recalculate the fraction total for future checks */
#if CTEST
				if ( DemoLev >= DEMDEBUG ) {
					sprintf( GOutBuff, "  (New totbbid %f, fractot %f).\n\n", totbbid, fractot );
					WriteStd( GOutBuff );
				}
#endif
			}
		}	/* new msg list not full */
	} /* competition was necessary */

	NmCfPost = NmCfWon; 								/* All winners produced messages */
	NmNMsgIn = NmNMsgs; 								/* All messages produced 'internally' */
	LastNMsg = NmNMsgs - 1; 							/* Set array index to last new message */

#if CTEST
	if ( DemoLev > 0 ) {
		WriteStd("\n\nDone generating new messages.");
		sprintf(GOutBuff,"\n%u Classifier(s) generated %u message(s).", NmCfWon, NmNMsgs ); 
		WriteStd( GOutBuff );
		if ( DemoLev >= 2 ) {
			WriteStd("\nThe message(s):");
			WriteStd("\n  Msg_Id		Message			Producer   Intensity (Bid)\n");		
			for ( i = 1, mptr = NewMsgs; i <= NmNMsgs; ++i, ++mptr ) {
				PutMNd( mptr, GMsgBuff, 1 );
				WriteStd( GMsgBuff );
			}
		}
	}
#endif

} /* GenNMsgs */


/********************************

PostCfMs	Post classifier's messages, i.e., make new messages for candidate matches of a classifier.

	CfPtr 	Pointer to classifier that has won and that is to generate and post messages.

Generate messages from the candidate-matches of the specified classifier.
if ( !OneMPerC ) {
	 Generate as many as will fit on NewMsgs list ( until NmNMsgs = NmIMsgMx ).
else
	 Generate 1 msg / cf, picked from candidate matches with prob = incoming intensity.

For now: Just generate in order until none left to generate or until list is full.

NOTES:	If NmIEMMx > 0 then use it to limit "effector messages" produced, i.e.,
		those that begin "10", in MakeNMsg(), in which case it returns 0 to this SR.

		Initialize NmDetMM (number of detector messages used as "input" messages to cf)
		here, and increment it in MakeNMsg each time a new message is made.
		This will be used (in BBLEARN.C) to calculate a bid share.
		(Note: It is decremented in GenBehav() if message is deleted (not POSTed)
		by the effector conflict resolution mechanism.)

******/

int PostCfMs ( CfPtr )
	struct	CfNode	*CfPtr;
{
	unsigned int	tempcnt, postcnt, prodcnt, mcnt1, mcnt2, m1ui, m1bit, m2ui, m2bit;
	unsigned int	matchcnt1, matchcnt2;
	float			randnum, cumint, totint;
	struct MsgNode	*msgptrs[MSGLSTSZ], *match1, *match2;
	
#if CTEST
	if ( DemoLev >= 1 ) {
		sprintf( GOutBuff, "\n   Cf %u posting msgs (from %u matches).", CfPtr->Cf_Id, CfPtr->NmMtch ); 
		WriteStd( GOutBuff );
		sprintf( GOutBuff, "\n   (%u match cond1, %u match cond2).", CfPtr->NmMMC1, CfPtr->NmMMC2 );
		WriteStd( GOutBuff );
	}
#endif

	prodcnt = postcnt = 0;	/* count number produced and posted so far */	
	CfPtr->NmDetMM = 0; 	/* count number of detector messages used to produce messages */

	if ( OneMPerC ) {
			/*	This only gets to produce one message.	Select the match for each condition
				at random, prob proportional to matching messages intensity.
				First work way through arrays of bits indicating messages matching each condition
				to get total intensity of messages matching it.
			*/

		for ( m1ui = 0, totint = 0, matchcnt1 = 0; matchcnt1 < CfPtr->NmMMC1 && m1ui < INTPRML; ++m1ui ) {
			for ( m1bit = 0; matchcnt1 < CfPtr->NmMMC1 && m1bit < INTSZ; ++m1bit ) {
				if ( CfPtr->MMBitsC1[m1ui] & ORMBits[m1bit] ) {	/* Msg matched by condition 1 */
					mcnt1 = ( m1ui * INTSZ ) + m1bit;				/* get entry in msglist array for msg assoc. with m1bit */
					msgptrs[ matchcnt1 ] = (CurMsgs + mcnt1);		/* get pointer to that message */
					totint += msgptrs[matchcnt1]->Intensit; 		/* add total intensity */
					++matchcnt1;									/* increment count of matches examined so far */
				}
			}	/* over bits in one word */
		} /* over words representing the condition */

			/* now pick one of the matchcnt1 messages matching the first condition */

		randnum = URandF( totint ); 	/* get max random number < total intensity */
		match1 = NULL;
#if CTEST
		if ( DemoLev >= 2 ) {
			sprintf( GOutBuff, "\n		OneMPerC is ON: for cond1, rand %.1f, totint %.1f", randnum, totint );
			WriteStd( GOutBuff );
		}
#endif
		for ( tempcnt = cumint = 0; tempcnt < matchcnt1; ++tempcnt ) {
			cumint += msgptrs[tempcnt]->Intensit;
#if CTEST
			if ( DemoLev >= 3 ) {
				sprintf( GOutBuff, "\n		(msg %d, cum %.0f) ", msgptrs[tempcnt]->Msg_Id, cumint );
				WriteStd( GOutBuff );
			}
#endif
			if ( randnum <= cumint	) {
				match1 = msgptrs[tempcnt];			/* this is it */
				break;
			}
		}	
		if ( match1 == NULL ) {
			sprintf( GOutBuff, "\n**PostCfMs: OneMPerC=1, match1 is NULL (stp %u)!\n", CycleStp );
			WriteStd( GOutBuff );
			return( 0 );
		}
#if CTEST
		if ( DemoLev >= 2 ) {
			sprintf( GOutBuff, " => match1 msg %d", match1->Msg_Id );
			WriteStd( GOutBuff );
		}
#endif

		if ( CfPtr->Cnd2Type == CNOTMATCH ) {	/* That it--no second message */
#ifdef M_I86LM		/* kludge for problem with Microsoft 5.0 compiler L model */
			if ( (tempcnt = MakeNMsg( CfPtr, match1, 0L )) == 1 )
#else
			if ( (tempcnt = MakeNMsg( CfPtr, match1, NULL )) == 1 )
#endif
				++prodcnt;				/* return 1 means produce, no post (DelHall was on) */
			else if ( tempcnt == 2 ) {
				++prodcnt;
				++postcnt;				/* return 2 means produce AND post */
			}
#if CTEST
			if ( DemoLev >= DEMDEBUG )
				WriteStd( "  & Cond2 is NOT. " );
#endif
		}	/* endif cond2 is NOTMATCH */

		else { /* Now find a match message for match2 */
			for ( m2ui = 0, totint = 0, matchcnt2 = 0; matchcnt2 < CfPtr->NmMMC2 && m2ui < INTPRML; ++m2ui ) {
				for ( m2bit = 0; matchcnt2 < CfPtr->NmMMC2 && m2bit < INTSZ; ++m2bit ) {
					if ( CfPtr->MMBitsC2[m2ui] & ORMBits[m2bit] ) {	/* Msg matched by condition 2 */
						mcnt2 = ( m2ui * INTSZ ) + m2bit;				/* get entry in msglist array for msg assoc. with m2bit */
						msgptrs[ matchcnt2 ] = (CurMsgs + mcnt2);		/* get pointer to that message */
						totint += msgptrs[matchcnt2]->Intensit; 		/* add total intensity */
						++matchcnt2;
					}
				}	/* over bits in one word */
			} /* over words representing the condition */

				/* now pick one of the matchcnt2 messages matching the second condition */

			randnum = URandF( totint ); 	/* get max random number < total intensity */
			match2 = NULL;
#if CTEST
			if ( DemoLev >= 3 ) {
				sprintf( GOutBuff, "\n		OneMPerC is ON: for cond2, rand %.1f, totint %.1f", randnum, totint );
				WriteStd( GOutBuff );
			}
#endif
			for ( tempcnt = cumint = 0; tempcnt < matchcnt2; ++tempcnt ) {
				cumint += msgptrs[tempcnt]->Intensit;
#if CTEST
				if ( DemoLev >= 3 ) {
					sprintf( GOutBuff, "\n		(msg %d, cum %.0f) ", msgptrs[tempcnt]->Msg_Id, cumint );
					WriteStd( GOutBuff );
				}
#endif
				if ( randnum <= cumint	) {
					match2 = msgptrs[tempcnt];			/* this is it */
					break;
				}
			}	
			if ( match2 == NULL ) {
				sprintf( GOutBuff, "\n**PostCfMs: OneMPerC=1, match2 is NULL (stp %u)!\n", CycleStp );
				WriteStd( GOutBuff );
				return( 0 );
			}
#if CTEST
			if ( DemoLev >= 2 ) {
				sprintf( GOutBuff, " => match2 msg %d. ", match2->Msg_Id );
				WriteStd( GOutBuff );
			}
#endif
				/* Now we have matches for both conditions--so try to make a message */

			if ( ( tempcnt = MakeNMsg( CfPtr, match1, match2 ) ) == 1 )
				++prodcnt;
			else if ( tempcnt == 2 ) {
				++prodcnt;
				++postcnt;
			}

		} /* second condition is MATCH */

	} /* OneMPerC */

	else {	/* Not One Msg Per Classifier.	*/
		/* Work way through arrays of bits indicating messages matching each condition
			making new messages (or trying to) as we go, until list is full.
		*/

		for ( m1ui = matchcnt1 = 0; matchcnt1 < CfPtr->NmMMC1 && NmNMsgs < NmIMsgMx && m1ui < INTPRML; ++m1ui ) {
			for ( m1bit = 0; matchcnt1 < CfPtr->NmMMC1 && NmNMsgs < NmIMsgMx && m1bit < INTSZ; ++m1bit ) {

				if ( CfPtr->MMBitsC1[m1ui] & ORMBits[m1bit] ) {	/* Msg matched by condition 1 */
					++matchcnt1;
					mcnt1 = ( m1ui * INTSZ ) + m1bit;			/* get entry in msglist array for msg assoc. with m1bit */
#if CTEST
					if ( DemoLev >= DEMDEBUG ) {
						sprintf( GOutBuff, "\n	Msg %u matches cond1;", (CurMsgs + mcnt1)->Msg_Id );
						WriteStd( GOutBuff );
					}
#endif

					if ( CfPtr->Cnd2Type == CNOTMATCH ) {
#ifdef M_I86LM				/* kludge for problem with Microsoft 5.0 compiler L model */
						if ( (tempcnt = MakeNMsg( CfPtr, (CurMsgs + mcnt1), 0L )) == 1 )
#else
						if ( (tempcnt = MakeNMsg( CfPtr, (CurMsgs + mcnt1), NULL )) == 1 )
#endif
							++prodcnt;				/* return 1 means produce, no post (DelHall was on) */
						else if ( tempcnt == 2 ) {
							++prodcnt;
							++postcnt;				/* return 2 means produce AND post */
						}
#if CTEST
						if ( DemoLev >= DEMDEBUG )
							WriteStd( "  Cond2 is NOT. " );
#endif
					}

					else {	/* Cnd2Type is CMATCH condition */
						matchcnt2 = 0;
						for ( m2ui=0; matchcnt2 < CfPtr->NmMMC2 && NmNMsgs < NmIMsgMx && m2ui < INTPRML; ++m2ui) {
							for ( m2bit=0; matchcnt2<CfPtr->NmMMC2 && NmNMsgs<NmIMsgMx && m2bit < INTSZ; ++m2bit) {
								if ( CfPtr->MMBitsC2[m2ui] & ORMBits[m2bit] ) {	/* Msg matched by condition 2 */
									++matchcnt2;
									mcnt2 = ( m2ui * INTSZ ) + m2bit;
#if CTEST
									if ( DemoLev >= DEMDEBUG ) {
										sprintf(GOutBuff,"\n	   Msg %u matches cond2; ", (CurMsgs + mcnt2)->Msg_Id );
										WriteStd( GOutBuff );
									}
#endif
									if ( (tempcnt = MakeNMsg( CfPtr, (CurMsgs + mcnt1), (CurMsgs + mcnt2) )) == 1 )
										++prodcnt;
									else if ( tempcnt == 2 ) {
										++prodcnt;
										++postcnt;
									}
								}
							} /* checking bits in one entry of MMBitsC2 */
						} /* checking entries in MMBitsC2 */
					} /* Cnd2Type is CMATCH */
				} /* msg1 matched condition 1 */
			} /* checking bits in one entry of MMBitsC1 */
		}	/* checking entries in MMBitsC1 */
	} /* Not OneMPerC */
	
	CfPtr->PstEfMsg = 0;				/* assume none are effector messages for now */
	CfPtr->NmProd = prodcnt;			/* it posted this many messages */
	CfPtr->NmPost = postcnt;			/* It produced this many messages */
	if ( prodcnt > 0 )					/* may be 0 if tried to produce too many effector messages */
		++NmCfWon;						/* the number of winners this cycle */
	
#if CTEST
	if ( DemoLev >= 1 ) {
		sprintf( GOutBuff, "\n   => Posted %u new msgs.", postcnt );
		WriteStd( GOutBuff );
	}
#endif

} /*	PostCfMs */


/********************************

MakeNMsg	Use "match" ( Cf, Msg1, Msg2 ) to generate a new message.
			Return 1 if message produced, 2 if message produced and posted.
			0 otherwise.

Store in next available node on NewMsgs list (NextNMsg), and update that pointer
and associated counter. Also update the node of the classifier that produced it.

NOTES: Caller must be sure there is room for a new message.

	The 1 for produce and 2 for posted returns is so that caller
	can set postcnt and prodcnt, which may be different if
	DelHall (delete hallucinations) is on.

	It may also be different if MxDupMsg is being used and
	the message being made is a duplicate (over the limit)
	of some message already on the list, in which case
	the message is just not produced (and so the producer may not pay a
	bid if that is the only message it is trying to produce).

	If NmIEMMx > 0 then use it to limit "effector messages" produced,
	i.e., those that begin "10", in case return 0 to caller.

	Increment classifier's NmDetMM for each input message
	from a detector that is actually used to produce a message.
	(Note: It is decremented in GenBehav() if message is deleted (not POSTed)
	by the effector conflict resolution mechanism.)

******/

int MakeNMsg( Cf, Msg1, Msg2 )
	struct CfNode	*Cf;
	struct MsgNode	*Msg1, *Msg2;
{
	unsigned int			err;
	register unsigned int	i, dupcnt, tmsgcnt;
	struct	CfOpNode		*coptr, *GetCOpCd();
	struct	MsgNode			*tmsgptr;

	coptr = GetCOpCd( Cf->ActType, &err );
	if ( err == ERROR ) {
		sprintf(GOutBuff,"\nMkMsgWin: can't find CfOpNode for ActType %u in Cf %u.",Cf->ActType,Cf->Cf_Id );
		WriteStd( GOutBuff );
		return( 0 );
	}
	(*coptr->CfOpfn)( Cf, Msg1, Msg2, NextNMsg );	/* Make the new message */

	if ( DelHall ) { 					/* Test for "hallucination"--left bits 00 */
		i = NextNMsg->Message[0];
#if ( STRNGSZ < INTSZ )
		i >>= STRNGSZ - 2;					/* left 2 bits in right end now */
#else
		i >>= INTSZ - 2;
#endif
		if ( !i )							/* if its all zeros, */
			return( 1 );					/* dont produce it, but tell cf you did (so it gets charged) */
	}

	if ( MxDupMsg > 0 ) {				/* Check new messages for duplicates: only produce if max not reached. */
		dupcnt = tmsgcnt = 0;
		for ( tmsgptr = NewMsgs; tmsgcnt < NmNMsgs && dupcnt < MxDupMsg; ++tmsgcnt, ++tmsgptr ) {
			for ( i = 0; i < INTPRSTR; ++i )	/* compare candiate (NextNMsg) and existing (tmsgptr) msgs */
				if ( NextNMsg->Message[i] != tmsgptr->Message[i] )
					break;					/* if any part is different, quit */
			if ( i == INTPRSTR )			/* no part differed, so new message matched message tmsgptr */
				++dupcnt;					/* so increment dup counter */
		}
		if ( dupcnt >= MxDupMsg ) {		/* DO NOT produce it--too many duplicates */
#if CTEST
			if ( DemoLev >= 1 )
				WriteStd( "  [Msg is excess dup]" );
#endif
			return( 0 );
		}
	}

	if ( NmIEMMx > 0 ) {				/* Test for limit on effector messages */
		i = NextNMsg->Message[0];
#if ( STRNGSZ < INTSZ )
		i >>= STRNGSZ - 2;					/* left 2 bits in right end now */
#else
		i >>= INTSZ - 2;
#endif
		if ( i == 2 ) {					/* if msg is 10...0 (effector message) */
			if ( NmIEMsgs == NmIEMMx ) {	/* if got all that will fit */
#if CTEST
				if ( DemoLev >= 1 )
					WriteStd( "  [Msg is excess eff]" );
#endif
				return( 0 );				/* dont produce it */
			}
			else
				++NmIEMsgs; 				/* Increment count */
		}
		else {							/* Must NOT be effector message */
			if ( NmIIMsgs == NmIIMMx ) {	/* if got all that will fit */
#if CTEST
				if ( DemoLev >= 1 )
					WriteStd( "  [Msg is excess int]" );
#endif
				return( 0 );				/* dont produce it */
			}
			else
				++NmIIMsgs; 				/* Increment count */
		}
	}

	NextNMsg->Msg_Id	= ++NmNMsgs; 		/* Increment New Messages counter and use that as Msg_Id */
	NextNMsg->IsMsg		= TRUE;				/* assume it will be a message */
	NextNMsg->FromDet	= FALSE; 			/* Its not from a detector. */
	NextNMsg->Intensit	= Cf->CfBid; 		/* Its intensity is its producer's bid. */
	NextNMsg->Producer	= Cf;				/* Cf is its producer. */
	NextNMsg->MtchMsg1	= Msg1;				/* Store pointers to 'input' messages */
	NextNMsg->MtchMsg2	= Msg2;
	if ( Msg1->FromDet )					/* its from a detector */
		Cf->NmDetMM += 1;
	if ( Msg2 != NULL && Msg2->FromDet )	/* check for NULL (not-match condition) */
		Cf->NmDetMM += 1;

#if CTEST
	if ( DemoLev > 1 )	{
		sprintf(GOutBuff,"-> produced Msg %d.", NextNMsg->Msg_Id );
		WriteStd( GOutBuff );
	}
#endif
	++NextNMsg; 							 /* Update pointer to next new message to use */

	return( 2 );

} /* MakeNMsg */


/********************************

CalcBid 	Calculate Bid of classifier.

On entry, Cfptr points to candidate classifier, which has:
	 Strength	its strength
	 BidRatio	its bid-ratio ('specificity')
	 Support	support for classifier, accumulated from messages in 'matches'
AveCCSup contains average support value for all candidate classifiers. Bid is calucalted as:

	Bid = max	| Bid_k * Strength * (BidRatio**BRPow) * (Support/avesupp)
				| CfBidMin

(If SuppBid flag is FALSE, then the Support term is not included in the bid.)
However, the bid is NOT allowed to be > the classifier's strength (which occurs
when a classifier's support is much bigger than the average support, namely 1/bid_k larger).

This bid is stored in CfBid variable in the classifier node. ALSO, a 'biased' bid value is stored
in the CfBBid variable in each node. This value is used to calculate the classifier's probability of winning.

	Biased_Bid = (Bid * BidRatio**EBRPow) ** BidPow

The EBRPow (effective bidratio power) can be used to bias the competition against
'generalist' classifiers, i.e., those with low bidratio (specificity).
That is. the Biased_Bid is the value used to calculate the probability that
the classifier will win the competition and produce messages.

BBType 3: Use Goldberg's VBS.
		  Calc usual bid, but CfBBid is now different.

******/

VOID CalcBid ( Cfptr )
	struct CfNode	*Cfptr;
{
	register	unsigned int	bits;
	register	float			tbid, f1, f2;
	float		NRand1();

	Cfptr->TotNmBid += 1;	  	/* Increment this classifier's bid count */ 
    Cfptr->StpLBid = CycleStp;  /* remember it bid this step */
	
	if ( SuppBid ) {
		if ( BRPow == 1 )
			tbid = Bid_k * Cfptr->BidRatio * (Cfptr->Support / AveCCSup) * Cfptr->Strength;
		else if ( BRPow == 2 )
			tbid = Bid_k * Cfptr->BidRatio * Cfptr->BidRatio * (Cfptr->Support / AveCCSup) * Cfptr->Strength;
		else
			tbid = Bid_k * pow( Cfptr->BidRatio, BRPow ) * (Cfptr->Support / AveCCSup) * Cfptr->Strength;
	}
	else {
		if ( BRPow == 1 )
			tbid = Bid_k * Cfptr->BidRatio * Cfptr->Strength;
		else if ( BRPow == 2 )
			tbid = Bid_k * Cfptr->BidRatio * Cfptr->BidRatio * Cfptr->Strength;
		else
			tbid = Bid_k * pow( Cfptr->BidRatio, BRPow ) * Cfptr->Strength;
	}

	tbid = max( tbid, CfBidMin );
	if ( tbid > Cfptr->Strength && tbid > CfBidMin ) {
		++TOTBGStr;
		tbid = Cfptr->Strength;
	}

	Cfptr->CfBid = tbid;
	TotCCBid += tbid;
	HiCCBid	= max( tbid, HiCCBid );
	LowCCBid = min( tbid, LowCCBid );

	if ( EBRPow == 0 ) {
		if ( BidPow == 1 )
			Cfptr->CfBBid = tbid;
		else if ( BidPow == 2 )
			Cfptr->CfBBid = tbid * tbid;
		else
			Cfptr->CfBBid = pow( tbid, BidPow );
	}
	else {
		tbid = tbid * pow( Cfptr->BidRatio, EBRPow );
		Cfptr->CfBBid = pow( tbid, BidPow );
	}

		/*	The following kludge is a test of biasing the competition toward
			classifiers that are linked to classifiers active on the prior step.
			BtCSB - Bias to Coupled Stage-Setter Bids (second condition starts 10)
			BtCMB - Bias to Coupled Memory Bids (second condition starts 01 or 11)
		*/

	if ( BtCSB != (float) 0.0 ) {
		bits = Cfptr->Cnd2Bits[0];		/* get first part of second condition */
#if ( STRNGSZ < INTSZ )
		bits >>= STRNGSZ - 2;			/* shift left 2 bits to right end */
#else
		bits >>= INTSZ - 2;
#endif
		if ( bits == 2 )				/* it must have started 10... */
			Cfptr->CfBBid *= BtCSB; 	/* so increase biased bid */
	}
	else if ( BtCMB != (float) 0.0 ) {
		bits = Cfptr->Cnd2Bits[0];		/* get first part of second condition */
#if ( STRNGSZ < INTSZ )
		bits >>= STRNGSZ - 2;			/* shift left 2 bits to right end */
#else
		bits >>= INTSZ - 2;
#endif
#if CTEST
		if ( DemoLev > 1 ) {
			sprintf( GOutBuff, "\n BtCMB > 0, cf %d (bits %d), oldbbid %0.1f", Cfptr->Cf_Id, bits, Cfptr->CfBBid );
			WriteStd( GOutBuff );
		}
#endif
		if ( bits == 1 || bits == 3 )	 /* it must have started *1... */
			Cfptr->CfBBid *= BtCMB; 	 /* so increase biased bid */
#if CTEST
		if ( DemoLev > 1 ) {
			sprintf( GOutBuff, "=> bbid %0.1f", Cfptr->CfBBid );
			WriteStd( GOutBuff );
		}
#endif
	}	/* for BtCMB != 0 */

	if ( BBType == 3 ) {
		f1 = Varbeta * sqrt( Cfptr->Var );
		f2 = NRand1( (float) 0.0, f1 ); 
		if ( DemoLev > 0 ) {
			sprintf( GOutBuff, "\ncf %u: Var %.2f, rt %.2f, noise %.2f + BBid %.1f = %.1f.", 
				Cfptr->Cf_Id, Cfptr->Var, f1, f2, Cfptr->CfBBid, Cfptr->CfBBid+f2 );
			WriteStd( GOutBuff );	
		}
		Cfptr->CfBBid += f2;
	}

	TotBBid += Cfptr->CfBBid;
	
#if CTEST
	if ( DemoLev > 1 )	{
		if ( BBType == 3 ) {
			sprintf(GOutBuff,"\nCf %d (sup. %7.2f) bid %7.2f (bb %8.1f) to produce %d msgs (VBS: Var %0.3f, sigma %0.3f):",
				Cfptr->Cf_Id, Cfptr->Support, Cfptr->CfBid, Cfptr->CfBBid, Cfptr->NmMtch, Cfptr->Var, tbid );
			WriteStd( GOutBuff );
		}
		else {
			sprintf(GOutBuff,"\nCf %d (sup. %7.2f) bid %7.2f (bb %8.1f) to produce %d msgs:",
				Cfptr->Cf_Id, Cfptr->Support, Cfptr->CfBid, Cfptr->CfBBid, Cfptr->NmMtch );
			WriteStd( GOutBuff );
		}
	}
#endif

} /* CalcBid -- Calculate Bid */


/********************************

GenBehav	Link new messages to effectors they match,
			then generate system behavior and apply effector conflict resolution
			mechanism to un-POST and produced messages that don't agree with 
			behavior actually chosen (for each effector).

On Entry:	NewMsgs	list of new messages to compare to effector condition-parts.
			CandCfs	list of classifiers that produced messages on NewMsgs list.
			EffLst	array of effector-nodes (EffNode's).

NOTE:	TOTMsPrd (total number of new messages) is incrmented at the beginning of
		this subroutine, to get total BEFORE effector conflict-resolution deletes messages.

		Set MadeMstk FALSE before generating behavior.

******/

VOID GenBehav ( )
{
	register int	i;
	unsigned int	nummtch  = 0;		 /* number of effectors matched this step */
	unsigned int	numact	 = 0;		 /* number of effectors activated this step */
	unsigned int	behavstp = FALSE;	 /* Set true if this is a step at which behavior is to be generated */
	struct EffNode	*eptr;
	struct MsgNode	*mptr;
	char			msg[STRNGSZ+1];

	msg[STRNGSZ] = '\0';

	TOTMsPrd += NmNMsgs;				 /* increment total for whole run */
	ChkEftrs(); 						 /* link messages to effectors they match */
	if ( CycleStp % EffectRt == 0 ) 	 /* Generate behavior every EffectRt steps */
		behavstp = TRUE;
	MadeMstk = FALSE;					 /* Didn't make a mistake (yet!) */

#if CTEST
	if	( DemoLev >= 2 ) { 
		WriteStd("\nActivate any matched effectors...");
    }
#endif

	for ( i = 0, eptr = EffLst; i < NmEffs; ++i, ++eptr ) {
		if ( (eptr->ECndType==CMATCH && eptr->NmEMtch > 0) || (eptr->ECndType==CNOTMATCH && eptr->NmEMtch==0) ) {
			++nummtch;									/* increment local count of effectors matched */
			eptr->ETotMtch += eptr->NmEMtch;			/* increment match count for this effector for the run */
			TOTEMtch += eptr->NmEMtch;					/* increment match count for all effectors for run */

#if CTEST
        	if	( DemoLev >= 2 ) { 
		        sprintf(GOutBuff,"\nEffector %d matched, collect action support...", i ); 
        		WriteStd( GOutBuff );
            }
#endif

			for ( mptr = eptr->EffMtch; mptr != NULL; mptr = mptr->NxtEMsg )
				(*eptr->Eff_Set)( mptr );				/* set effector switches */

#if CTEST
        	if	( DemoLev >= 2 ) {
        		WriteStd( "pick an action and do it..." ); 
            }
#endif

			(*eptr->Eff_Fn)( behavstp );				/* Use settings to select an action, and do it if behavstp true */

#if CTEST
        	if	( DemoLev >= 2 ) {
                sprintf( GOutBuff, "do conflict resolution (NmNMsgIn %d)...", NmNMsgIn );
        		WriteStd( GOutBuff );
            }
#endif

			for ( mptr = eptr->EffMtch; mptr != NULL; mptr = mptr->NxtEMsg ) {
				BMsgtoA( mptr->Message, msg );			/* get string version of message */

				if ( (*eptr->EQEfAct)( msg ) ) { 	/* same action, so keep it and... */
					mptr->Producer->PstEfMsg += 1;		/* mark as effector activator this step */
					mptr->Producer->TotEMtch += 1;		/* Producer generated a msg. that matched an effector. */
					if ( behavstp ) 					/* If this is a step to generate behavior, */
						mptr->Producer->TotEAct += 1;	/* the producer also activated an effector. */
					eptr->NmEAct += 1;					/* increment count of number that activated this effector */
#if CTEST
                	if	( DemoLev >= 3 ) { 
                        msg[STRNGSZ] = '\0'; 
                		sprintf(GOutBuff,"\n  Msg %d '%s' agrees.", mptr->Msg_Id, msg );
                		WriteStd( GOutBuff );
                    }
#endif
				}

				else {								/* mptr not consistant with action, so... */
					mptr->IsMsg = FALSE;				/* Remove it */
					mptr->Producer->NmPost -= 1;		/* decrement producer's number 'posted' */
					if ( mptr->Producer->NmPost == 0 )
						--NmCfPost;
					mptr->Producer->TotEMtch += 1;		/* Producer still generated msg that matched an effector */
					--NmNMsgIn; 						/* Decrement count of new internal messages */
					--TOTEAMsg; 						/* Decrement total count of all effector-activating msgs */

					if ( mptr->MtchMsg1->FromDet )		/* Decrement producers count of used detector messages */
						mptr->Producer->NmDetMM -= 1;	/* (This count incremented in MakeNMsg() and used in */
					if ( mptr->MtchMsg2 != NULL && mptr->MtchMsg2->FromDet ) /* UpdCfStr() in BBLEARN.C .) */
						mptr->Producer->NmDetMM -= 1;
#if CTEST
                	if	( DemoLev >= 3 ) { 
                        msg[STRNGSZ] = '\0'; 
                		sprintf(GOutBuff,"\n  Msg %d '%s' disagrees (NmNMsgIn %d).", mptr->Msg_Id, msg, NmNMsgIn );
                		WriteStd( GOutBuff );
                    }
#endif
				}
			}

			if ( behavstp ) {			 /* if activated this step, ... */
				++numact;					 /* increment the local counter */
				eptr->ETotAct += 1; 		 /* increment counter in effector */
			}
		}
	}

	TOTEAct += numact;						/* increment total for all effectors for run */

	DoBkgBeh(); 							/* Do any behaviors defined as 'background', i.e., done every step. */

	if ( !ClampFlg ) {					/* if not in "clamped" region... */
		GtSysRew(); 						/* Get reward for system, if any to be got. */

		if ( SysRew > 0 )
			++TOTSyRwP; 						/* increment counts for run */
		else if ( SysRew < 0 )
			++TOTSyRwN;
	}

#if CTEST
	if	( DemoLev >= 2 ) { 
		WriteStd("\nDone generating behavior: ");
		sprintf(GOutBuff,"\n  Effectors matched:   %u.", nummtch ); 
		WriteStd( GOutBuff );
		sprintf(GOutBuff,"\n  Effectors activated: %u.", numact );
		WriteStd( GOutBuff );
		sprintf(GOutBuff,"\n  Reward to System:	%5.2f.\n", SysRew );
		WriteStd( GOutBuff );
	}
#endif

} /* GenBehav -- generate behavior */


/********************************

ChkEftrs	Check all new messages against all effectors in the EffLst array,
			and link each message to all effectors it matches (satisifies).

Entry:	NewMsgs contains messages to check, EffLst is array of effector nodes to check.

Exit:	For each matched EffNode:
		- decrement number of times that effector has been matched this cycle step (NumEMtch).
		- increment total number of times effector matched ( ETotMtch ).
		- link appropriate messages to EffMtch, a list of matching MsgNode's, linked to each
			other by the NxtMtch pointer in the MsgNodes.

NOTE:	For effectors with 'non-match' conditions: just increment NmEMtch counter in the
		effector node (thus if counter remains 0, no message matched, so it IS satisfied and should be activated.

******/

VOID ChkEftrs ( )
{
	register int		i;			/* counter for binary parts of messages */
	unsigned int		mcnt, ecnt;	/* counters for looping thru msg and effector lists */
	struct EffNode		*ep;		/* pointer to effector to check */
	struct MsgNode		*Mptr,		/* message to check against all effectors. */
						*lm;		/* pointer to last message on effectors list of matching messages */

#if CTEST
	if	( DemoLev >= 2 ) { 
		WriteStd("\nLink effector-messages to effectors..." );
    }
#endif

	for ( ecnt = 0, ep = EffLst; ecnt < NmEffs; ++ecnt, ++ep )	{
		ep->NmEMtch = ep->NmEAct = 0;				/* none matched or activated it this step yet */
		ep->EffMtch = lm = NULL;

		for ( mcnt = 1, Mptr = NewMsgs; mcnt <= NmNMsgs; ++mcnt, ++Mptr ) {

			for ( i = 0; i < INTPRSTR; ++i )
				if ( ( (Mptr->Message[i] & ep->ECndDCs[i]) ^ ep->ECndBit[i]) )	break;

			if ( i == INTPRSTR ) {				/* matched! */
				ep->NmEMtch += 1;					/* increment count of matches for this effector */

				if ( ep->ECndType == CMATCH ) {
					ep->ETotMtch += 1;				/* increment total matches for whole run. */
					if ( lm == NULL )			
						ep->EffMtch = lm = Mptr;	/* Its first and last match */
					else {
						lm->NxtEMsg = Mptr; 		/* Append it to list */
						lm = Mptr;					/* Its last now */
					}
				} /* effector has 'match' condition */

			} /* message matched effector */

		} /* each new message */

		if ( lm != NULL )
			lm->NxtEMsg = NULL; 				/* Make last match, if one, end of list */

	} /* each effector */
	
} /* ChkEftrs */
