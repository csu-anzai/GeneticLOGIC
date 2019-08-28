/*****************************

14 JAN 1989: Version prior to changes made today, to alter
   trigger operation, and to take into account already coupled rules.

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
	CSSRt	prob. use operator if trigger is satisfied (This is called only if CSSRt != 0).
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

VOID DscCSS ( )
{
	int 			i;
	struct NCfNode *new1, *new2;

#if CTEST
	if ( DscDemo > 0 )
		WriteStd( "\nDscCSS:" );
#endif

	if ( PckCC () != 0 ) {				/* See if there are some to couple... */

		new1 = ReprodCf( PckCCSS ); 		/* new1 is the precursor (stage setter) cf */
		new2 = ReprodCf( PckCCPrf );		/* new2 is the successor (profitable) cf */

			/* Now put new linkage action/condition into GCfBuff;
				use GMsgBuff as scratch area for random string.
			*/

		BActtoA( new1->NewActB, new1->NewActD, GCfBuff );

		GnRndCA( GMsgBuff, &i, 'c' );			/* generate random string */
		for ( i = TagLMx; i <= TagRMx; ++i )	/* a kludge to get rid of #'s */
			if ( *(GMsgBuff+i) == '#' ) {
				if ( URand01() < 0.5 )
					*(GMsgBuff+i) = '0';
				else
					*(GMsgBuff+i) = '1';
			}

		for ( i = TagLMx; i <= TagRMx; ++i )	/* insert the random string into action */
			*(GCfBuff+i) = *(GMsgBuff+i);		/* we only need the one product */

		if ( URand01() < CSSFrM ) {			/*	Force it to 11 source-tag */
			*GCfBuff = '1';
			*(GCfBuff+1) = '1';
		}

		AActtoB( GCfBuff, new1->NewActB, new1->NewActD );	/* Store in stage-setter */

		ACndtoB( GCfBuff, new2->NewCnd2B, new2->NewCnd2D ); /* Store in cond2 of successor */
		new2->NewCnd2T = CMATCH;

		if ( URand01() < CSSFrRf )				/* Force it to be "reflex" classifier */
			ACndtoB( GCfBuff, new2->NewCnd1B, new2->NewCnd1D ); /* Store in cond2 of successor */

			/* Now set the strength in the new classifiers */
				/* ***** A TEMP KLUDGE ***** */

		if ( NwCCStrF == (float) -1.0 ) {	/* Use Ave Parent S */
			new1->NewStr = ( PckCCSS->Strength + PckCCPrf->Strength ) / 2.0;
			new2->NewStr = new1->NewStr;
		}
		else if ( NwCCStrF != (float) 0.0 ) {	/* if its 0, use whatever Reprod gives us */	
			new1->NewStr = AveCfStr * NwCCStrF;		/* Else use Fraction * Ave str */
			new2->NewStr = new1->NewStr;
		}

		++TotCSS;

			/* Recalculate specificity of new successor with new conditions */

		BCndtoA( new1->NewCnd1B, new1->NewCnd1D, GMsgBuff );
		new1->NewBR = CalcSpec( GCfBuff, GMsgBuff, CMATCH, CMATCH );

#if CTEST
		if ( DscDemo > 0 ) {
			WriteStd( "\nOffspring:\n  " );
			WrtNCf( new1, GCfBuff, DEMOFMT1 );
			sprintf( GOutBuff, "\n  %s", GCfBuff );
			WriteStd( GOutBuff );
			WrtNCf( new2, GCfBuff, DEMOFMT1 );
			sprintf( GOutBuff, "  %s", GCfBuff );
			WriteStd( GOutBuff );
		}
#endif

	}	/* there were some to couple */

} /* DscCSS	*/

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

int PckCC ( )
{
	register int	i;
	short			oldmsg, nmoldint;
	float			hiprof;
	struct MsgNode *mp;

	if	( NmNewCfs > MxNewCfs - 1 ) {	/* There must be room for 2! */
#if CTEST
		if ( DscDemo > 0 )
			WriteStd( "\n   not room for 2 more new cfs.\n" );
#endif
		return( 0 );
	}
	else if ( MxNewCfs < 2 ) {
		WriteStd( "\n**PckCC: MxNewCfs < 2 (set to 2).\n" );
		MxNewCfs = 2;
	}

		/* Count the old messages: that is where we will get a pointer
			to a stage-setter active 2 steps ago.
		*/

#if CTEST
	if ( DscDemo > 0 ) {
		sprintf( GOutBuff, "\nActCfPNm %d; OldMsgs (%d) are:\n", ActCfPNm, NmOMsgs );
		WriteStd( GOutBuff );
	}
#endif

	for ( mp = OldMsgs, i = 0, nmoldint = 0; i <= LastOMsg; ++mp, ++i ) {
		if ( mp->IsMsg ) {
			if ( !mp->FromDet ) 		/* count only those with producers */
				++nmoldint;
#if CTEST
			if ( DscDemo > 0 ) {
				PutMNd( mp, GMsgBuff, DEMOFMT1 );
				WriteStd( GMsgBuff );
			}
#endif
		}
	}

	if ( ActCfPNm == 0 || nmoldint == 0 ) {
#if CTEST
		if ( DscDemo > 0 )
			WriteStd( "\nActCfPNm or nmoldmsg are 0 => Can't couple." );
#endif
		return ( 0 );
	}

#if CTEST
	if ( DscDemo > 0 )
		WriteStd( "\nActCfPrv are:" );
#endif

	for ( i = 0, hiprof = -99999, PckCCPrf = NULL; i < ActCfPNm; ++i ) {
#if CTEST
		if ( DscDemo > 0 ) {
			sprintf( GOutBuff, "\ncf %d, chngstr %6.2f", ActCfPrv[i].ActCf->Cf_Id, ActCfPrv[i].ChngStr );
			WriteStd( GOutBuff );
		}
#endif
		if ( ActCfPrv[i].ChngStr > hiprof ||
			 ( ActCfPrv[i].ChngStr == hiprof && URand01() <= 0.5 ) ) {
			hiprof = ActCfPrv[i].ChngStr;
			PckCCPrf = ActCfPrv[i].ActCf;
		}
	}

#if CTEST
	if ( DscDemo > 0 ) {
		sprintf( GOutBuff, "\nPckCCPrf %d, chngstr %6.2f", PckCCPrf->Cf_Id, hiprof );
		WriteStd( GOutBuff );
	}
#endif

	if ( PckCCPrf == NULL ) {
		sprintf( GOutBuff, "\nPckCSS: PckCCPrf is NULL (NmActPNm %d, stp %d)!\n", ActCfPNm, CycleStp );
		WriteStd( GOutBuff );
		return( 0 );
	}

	if ( hiprof < ACPCt * PckCCPrf->Strength ) {	/* it fails threshold test */
#if CTEST
		if ( DscDemo > 0 ) {
			sprintf( GOutBuff, "\nhiprof < ACPCt (%.3f) * Cf Str (%.0f): no linking.\n",
				ACPCt, PckCCPrf->Strength );
			WriteStd( GOutBuff );
		}
#endif
		return( 0 );
	}

		/* Now find classifiers active 2 steps ago, before the Profitable one,
			by looking at producers of messages on Old message list. 
		*/

	if ( nmoldint == 1 ) {		/* just one to link to, so just find it */
		for ( mp = OldMsgs, i = 0; i <= LastOMsg; ++mp, ++i )
			if ( mp->IsMsg && !mp->FromDet ) {
				PckCCSS = OldMsgs->Producer;
				break;
			}
	}

	else {			/* several to link to, so ... */
		oldmsg = URandN( nmoldint - 1 );	/* pick one at random */
#if CTEST
		if ( DscDemo > 0 ) {
			sprintf( GOutBuff, "\nrand oldmsg is %d. Look for it:", oldmsg );
			WriteStd( GOutBuff );
		}
#endif
		for ( mp = OldMsgs, i = 0, nmoldint = 0; i <= LastOMsg; ++mp, ++i ) {
			if ( !mp->IsMsg || mp->FromDet )
				continue;					/* not a candidate */
			else if ( nmoldint != oldmsg )
				++nmoldint; 				/* not the one, but a candidate */
			else	
				break;						/* this is the one! */
		}

		if ( i > LastOMsg ) {
			sprintf( GOutBuff, "\n***PckCC: rand failed to find oldmsg (stp %d)!\n", CycleStp );
			WriteStd( GOutBuff );
			return( 0 );
		}
		else
			PckCCSS = mp->Producer;
	}

	if ( PckCCSS == NULL ) {
		sprintf( GOutBuff, "\n***PckCC: PckCCSS NULL (stp %d)!\n", CycleStp );
		WriteStd( GOutBuff );
		return( 0 );
	}

	else {			/* we have all the ducks in a row, at last */
#if CTEST
		if ( DscDemo > 0 ) {
			WriteStd( "\nParent StageSetter & Profitable cfs:" );
			PutCNd( PckCCSS, GCfBuff, DEMOFMT1 );
			sprintf( GOutBuff, "\n  %s", GCfBuff );
			WriteStd( GOutBuff );
			PutCNd( PckCCPrf, GCfBuff, DEMOFMT1 );
			sprintf( GOutBuff, "  %s", GCfBuff );
			WriteStd( GOutBuff );
		}
#endif
		return( 1 );
	}

} /* PckCC */

