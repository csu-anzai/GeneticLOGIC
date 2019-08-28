/*			FSW1.C	

Subroutines that interact and define the FSW1, Finite State World V1, environment
for the CFS-C classifier system.

This file and the file FSW1A.C contains the following subroutines:

A.	Subroutines required for all environments linked to the CFS-C system:
	LoadEnv		|- Load environment and effectors from a file.
	InitEnv		Initialize the environent variables.
	ReadEnv		|
    ReadFSW     |
    CheckTransitions |
    ReadStates  |
    ReadVarLoc  |
    LoadGref3   |- load special environment 

	DsplEnv		|- display environment variables, status, etc.
	WrtEnv		|
	SaveEnv		|
	GetDMsgs	get new messages to be added to the message list.
	DoEnvCmd	execute an environment command.
	GtSysRew	Get reward for system, after activation of effectors.
	StoreSup	
	PkTrans	 
	DftTrans	 
	EQTrans		see if message is consistent with action taken.
	DoBkgBeh	re-initialize attribute settings.
	MakeActS	Make an action string.
	DisCfIE	 Display classifiers, Interpreted wrt the Environment implemented.

B.	Subroutines specific to the FSW environment:
	ReadFSW		| - load
	RdStates	|
	RdVarLoc	|
	 SetEnv		set environment variable (via ECMD command).
	HelpEnv		help specific to environment (via ECMD command).
*/

#include	"compiler.h"
#include	"stdio.h"
#if (LATTICEC || CBELLMTS || MPWC )
#include	"ctype.h"
#endif
#include	"utility.h"
#include	"cfsio.ext"
#include	"cfops.ext"
#include	"core.ext"
#include	"fsw1.def"

extern unsigned int DemoLev;
char				*GetUInt(), *GetFloat(), *GetInt(), *StpTok2();
float				URand01();

VOID	ReadEnv(), InitEnv(), ReadFSW(), CheckTransitions(), 
        RdStates(), LoadGref3(), WrtEnv(), SaveEnv(), MakeActS(), DisCfIE(),
		SetEnv(), HelpEnv(), DsplDetM(), DsCfSTag(), DsCfSta(),
        DsCfEffS(), DsCfAct(), FSWGetCf(), FSWGetSB();
char			*RdVarLoc();
struct TrNode	*TrAlloc();
struct PaNode	*PaAlloc();
struct VLociNd	*VLAlloc();

#if MPWC
#define __SEG__ CFSCINIT
#endif

/********************************

LoadEnv 		Load environment for FSW1 from specified file.

	FDName		String name of file from which initial state of environment is to be read.

	LoadEnv()	just opens the file, calls ReadEnv()( to do the work, and closes the file.

******/

int LoadEnv ( FDName )
	char	*FDName;
{
	FILE		*fileptr;
	extern char	*EnvInFN;

	while ( *FDName == ' ' )	++FDName;

	if ( (fileptr = GetFILE( FDName, EnvInFN, "r")) == NULL ) {
		WriteStd( "---No environment loaded.\n");
		return( ERROR );
	}

	ReadEnv ( fileptr );

	if ( CloseFILE( FDName, EnvInFN, fileptr ) == ERROR )
		return( ERROR );

}	/* LoadEnv	*/


/*******************************

ReadEnv 	Load FSW environment specifications.

	FilePtr		Open FILE pointer.
				Format of information in file is:
				; The number of states followed by the initial state,
				; followed by TotNmDft and NmInvldE values (for save/restart).
					4, 0, 0, 0[, g3]
                ; The optional g3 indicates load rest of stuff to ENDTRANS
                ; using the LoadGref3() subroutine.
				; Next comes rows of state number, followed by VisitCnt for it, followed
				; by optional STRNGSZ string of 0's and 1's specifying the state's 
				; attributes as seen by the detectors. If not supplied,
				; the NoAttr short int is set TRUE (no Detector Message for it).
				; After attributes can be optional list of variable loci,
				; one loci, prob1 pair for each. (Count loci from 0 left).
				; For example, 4,0.9 means 90% of locus 4 will be 1, rest 0.
				 	0, 1, 0000 0000 0001 0000
					1, 0,
					2, 0, 0000 0000 0111 0000, 4,0.1, 5,0.9
					3, 0, 0000 0000 1111 0000
				ENDSTATES
				; Now the payoff function, consisting of  s,u/ pairs separated by slashes
				; for all states s with non-zero m	payoffs, followed by the
				; ENDPAYOFFS constant.
					1,10/ 2,5
				ENDPAYOFFS
				; Now the transition function tuples (s,e,s1,p1,s2,p2,...).
				; The e's are effector values from the set (0,1,...EFFVALSZ).
				; The first value entered is the default "do nothing" setting.
				; It is used when no effector message are generated, as well
				; as when the messages vote to do nothing (i.e. for setting 0).
				; For each s,e combination:
				; specify a list of destination states and probabilities for
				; each being the path taken. If the probabilities don't add
				; up to 1.0, you will get a warning on loading.
				; NOTE the "2,r, 3" line: That means for any EffSet,
				; transition is from state 0 to 3 (MaxRInit) at random.
					0,0, 1,.333, 2,.333, 3,.333
					0,1, 1,1.0
					0,2, 3,1.0
					1,0, 2,1.0
					1,1, 1,1.0
					2,r, 3
				ENDTRANS
				; The effectors. For Effector1, the right most 4 pairs are used to pick a
				; transition path (see StoreSup for details).
				 10## #### ## ## ## ##, Effector1, 0, 0 
				~10## #### ## ## ## ##, Default, 0, 0
				END-EFFS
				; An optional list of effector message loci to use
				; as loci for control effectors. Include up to 8 on a line,
				; separated by comma's, each in range 2..STRNGSZ.
				8,9, 10,11, 12,13, 14,15
				END-ELOCI

******/

VOID ReadEnv ( FilePtr )
	FILE	*FilePtr;
{
	int		retlen, err, l, lcnt;
	float	f;
	char	*cptr;
	int		StoreSup(), PkTrans(), EQTrans(), DftTrans();	/* Make addresses resolvable */

	InitEnv( ); 							/* Get memory, initialize */

	ReadFSW( FilePtr ); 					/* Read in rewards and letter sequence itself */

	EffLst[0].Eff_Set = StoreSup;			/* Addresses of functions */
	EffLst[0].Eff_Fn  = PkTrans;
	EffLst[0].EQEfAct = EQTrans;
	EffLst[0].CEffFlg = TRUE;				/* Apply DscCEff() to this effector to generate new actions */

	EffLst[1].Eff_Set = StoreSup;
	EffLst[1].Eff_Fn  = DftTrans;
	EffLst[1].EQEfAct = EQTrans;
	EffLst[1].CEffFlg = FALSE;

	NmEffs = ReadEffs( FilePtr );

	sprintf( GOutBuff, "\nLoaded %d effectors.\n", NmEffs );
	WriteStd( GOutBuff );

		/* Now read effector loci to be used (if present) */

	for ( lcnt = 0, l = STRNGSZ - StrAttSz; lcnt < StrAttSz && lcnt < STRATTSZ; ++lcnt, ++l )
		EffLoci [ lcnt ] = l;
	lcnt = 0;
	while ( ReadS( GInBuff, GIBUFFMX, FilePtr, &retlen) != EOF ) {
		if ( *GInBuff == COMCHAR )
			continue;
		else if ( retlen == 0 || strcmp( GInBuff, "END-ELOCI" ) == 0 )
			 break;
		else {
			for ( lcnt = 0, cptr = GInBuff; lcnt < STRATTSZ; ++lcnt ) {
				cptr = GetInt( cptr, &l, -1, ",", &err);
				if ( l == -1 )
					break;
				else if ( l < 2 || l > STRNGMX || err ) {
					WriteStd( "\n**ReadEnv: eff-loci out of range.\n" );
					exit ( );
				 }
				else
					EffLoci [ lcnt ] = l;
			}
		}
	}

	if ( lcnt != 0 ) {
		if ( lcnt % 2 != 0 )
			WriteStd( "\n**ReadEnv: Eff-Loci cnt not even!\n" );
		StrAttSz = lcnt;
		StrAttMx = StrAttSz - 1;
		EffAttSz = lcnt / 2;
		EffAttMx = EffAttSz - 1;
			/* f = EffAttSz;
				EffValMx = pow( 2.0, f ) - 1;
			*/
		EffValMx = (2 << EffAttMx) - 1;
	}

	sprintf( GOutBuff, "\nStrAttSz %d, EffAttSz %d, EffValMx %d, EffLoci:\n",
				StrAttSz, EffAttSz, EffValMx );
	WriteStd( GOutBuff );
	for ( lcnt = 0; lcnt < StrAttSz; ++lcnt ) {
		sprintf( GOutBuff, " %d", EffLoci [ lcnt ] );
		WriteStd( GOutBuff );
	}
	WriteStd( "\n" );

} /* ReadEnv */



/********************************

InitEnv 	Initialize the environment. 

Just zero out the payoff function and transition function.

******/

VOID InitEnv( )
{
	int				i, effset, firstcall;
	struct TrNode	*tnode, *txnode;
	struct PaNode	*pnode, *pxnode;
	struct VLociNd	*vlnode;
	char			*calloc();
	struct	StateNd	*nxtstate;
	
	WriteStd("\nInitializing FSW...\n");

		/* get space for state nodes, { initialize them. */

	if ( States == NULL ) {
		firstcall = TRUE;
		if ( (States = (struct StateNd *) calloc( STATESZ, sizeof(struct StateNd))) == NULL ) {
			WriteStd("\n**InitEnv: NULL from calloc()\n");
			exit( ERROR );
		}
	}
	else
		firstcall = FALSE;
		
	for ( i = 0, nxtstate = States; i < STATESZ; ++i, ++nxtstate )	{
		nxtstate->NoAttr = FALSE;
		nxtstate->Payoff = 0.0;
		nxtstate->VisitCnt = 0;
		nxtstate->BestNxt = -1;
		nxtstate->NmBest = 0;
		nxtstate->NmNeg = 0;
						
		if ( firstcall ) {
			nxtstate->Transits = NULL;
			nxtstate->VarLoci = NULL;
		}
		
		else {
			for ( tnode = nxtstate->Transits, nxtstate->Transits = NULL; tnode != NULL; ) {
				effset = tnode->EffSet;
				for ( pnode = tnode->Paths; pnode != NULL; ) {
					pxnode = pnode;
					pnode = pnode->NxtPath;
					if ( PaFree( pxnode ) == ERROR ) {	
						sprintf( GOutBuff, "\n**InitEnv: State %d, EffSet %d", i, effset );
						WriteStd( GOutBuff );
					}
				}
				txnode = tnode;
				tnode = tnode->NxtTrans;
				if ( TrFree( txnode ) == ERROR ) {
					sprintf( GOutBuff, "\n**InitEnv: State %d, EffSet %d", i, effset );
					WriteStd( GOutBuff );
				}
			}
			
			for ( vlnode = nxtstate->VarLoci; nxtstate->VarLoci != NULL; vlnode = nxtstate->VarLoci ) {
				nxtstate->VarLoci = vlnode->NxtVL;
				if ( VLFree( vlnode ) == ERROR ) {
					sprintf( GOutBuff, "\n**InitEnv: State %d, varloc %d", i, vlnode->VarLocus );
					WriteStd( GOutBuff );
				}
			}
			
		} /* end not firstcall */
		
	}

	OldState = CurState = NULL;
	EfValBin = 0;

	TotSup = 0.0;

	for ( i = 0; i <= EffValMx; ++i )
		EfValSup[i] = 0;

} /* InitEnv */


/********************************

ReadFSW 		Read in number of states, initial state, payoff function,
				and the transition function specifications.

	FilePtr 	FILE pointer to opened file that has required information.
				See ReadEnv() above for layout.

******/

VOID ReadFSW ( FilePtr )
	FILE	*FilePtr;
{
	int 			RetLen, err;
	unsigned int	state1, state2, effset, startstate;
	float			payoff, prob, totprob, f;
	char			*bufptr;
	struct TrNode	*tnode, *ltnode;
	struct PaNode	*pnode, *lpnode;

	DetMsgMn = 1;			/* Minimum space for detector messages. */

		/* Read in line with number of states and initial state */

	while ( ReadS( GInBuff, GIBUFFMX, FilePtr, &RetLen) != EOF ) {
		if ( *GInBuff == COMCHAR || RetLen == 0 )
			continue;
		else {
			bufptr = GetUInt( GInBuff, &NmStates, 9999, ",", &err);
			bufptr = GetUInt( bufptr, &startstate, 9999, ",", &err);
			bufptr = GetUInt( bufptr, &TotNmDft, 0, ",", &err);
			bufptr = GetUInt( bufptr, &NmInvldE, 0, ", ", &err);
			if ( NmStates > STATEMX ) {
				WriteStd( "\nReadFSW: NmStates too large.\n" );
				exit( );
			}
			if ( startstate >= NmStates || startstate == 9999 ) {
				sprintf( GOutBuff, "\nReadFSW: illegal startstate %d--using 0.", startstate );
				WriteStd( GOutBuff );
				startstate = 0;
			}
			break;
		}
	}

    if ( *bufptr != '\0' ) {     /* check for g3 option */
        for ( ; *bufptr == ' '; ++bufptr ) ;    /* skip blanks */
        if ( *bufptr != '\0' && strncmp( bufptr, "g3", 2 ) == 0 ) {
            LoadGref3( );
        	while ( ReadS( GInBuff, GIBUFFMX, FilePtr, &RetLen) != EOF ) {
                printf( "\nGInBuff is '%50.50s'.", GInBuff );
		        if ( strncmp( GInBuff, "ENDTRANS", 7 ) == 0 )
			        break;      /* get to place where effectors begin */
            }
            CheckTransitions();
        	CurState = OldState = States + startstate;		/* get pointed at start state */
	        if ( strcmp( GInBuff, "ENDTRANS" ) != 0 ) {
                sprintf( GOutBuff, "\nNot on ENDTRANS after loading gref3 on '50.50%s'.\n", GInBuff );
                WriteStd( GOutBuff );
                exit;
            }
            return;
        }
        else {
            WriteStd( "\nWarning: extra chars at end of line with state count, etc.\n" );
        }
    }

		/* Read in lines with State Number, VisitCnt and Attribute string for each state */

	RdStates( FilePtr );

		/* Read in payoff function pairs s,u/ until we get ENDSTATES on a line */
		
	while ( ReadS( GInBuff, GIBUFFMX, FilePtr, &RetLen) != EOF ) {
		if ( *GInBuff == COMCHAR || RetLen == 0 )
			continue;
		else if ( strcmp( GInBuff, "ENDPAYOFFS" ) == 0 )
			break;
		else {
			bufptr = GInBuff;
			while ( *bufptr != '\0' ) {
				bufptr = GetUInt( bufptr, &state1, 9999, ",", &err);
				bufptr = GetFloat( bufptr, &payoff, 0.0, "/", &err);
				if ( state1 >= NmStates || state1 == 9999 ) {
					sprintf( GOutBuff, "\nReadFSW (rewards): state %d not valid.", state1 );
					WriteStd( GOutBuff );
				}
				else {
					CurState = States + state1;
					CurState->Payoff = payoff;
				}
			}
		}
	}

		/* Read in transition function lines (s,r,s1,p1,...) until we get ENDTRANS on a line */

	while ( ReadS( GInBuff, GIBUFFMX, FilePtr, &RetLen) != EOF ) {
		if ( *GInBuff == COMCHAR || RetLen == 0 )
			continue;
		else if ( strcmp( GInBuff, "ENDTRANS" ) == 0 )
			break;
		else {
			bufptr = GInBuff;
			bufptr = GetUInt( bufptr, &state1, 9999, ",", &err);
			if ( state1 >= NmStates || state1 == 9999 || err ) {
				sprintf( GOutBuff, "\nReadFSW (trans): source state (%d) not valid.", state1 );
				WriteStd( GOutBuff );
				continue;
			}
			
			if ( *bufptr == 'r' ) {		/* random transition from here */
				++bufptr; ++bufptr; 		/* move past r and comma */
				bufptr = GetUInt( bufptr, &effset, 9999, ",", &err);
				if ( effset > NmStates - 1 || effset < 0 ) {
					WriteStd( "\nReadFSW (MaxRInit): illegal value.\n" );
					exit ( );
				}
				else {
					CurState = States + state1;
					CurState->MaxRInit = effset;
					continue;				/* go on to next line */
				}
			}

				/* set CurState to proper node, then get effector setting */
				
			CurState = States + state1;
			bufptr = GetUInt( bufptr, &effset, 9999, ",", &err);

			if ( effset > EffValMx ) {
				sprintf( GOutBuff, "\nReadFSW (trans): effset %d (state1 %d) not valid.", effset, state1 );
				WriteStd( GOutBuff );
				continue;
			}

				/* see if effset is in list already from a previous line */

			for ( tnode = CurState->Transits; tnode != NULL; tnode = tnode->NxtTrans )
				if ( tnode->EffSet == effset )
					break;

			if ( tnode == NULL ) {			/* Not there, add a new TrNode */
				tnode = TrAlloc();				/* get a TrNode */
				tnode->EffSet = effset; 		/* initialize */
				tnode->NxtTrans = NULL;
				tnode->Paths = NULL;
				CurState->NmEffTr += 1;
				if ( CurState->Transits == NULL )	/* append to list for this state */
					CurState->Transits = tnode;
				else
					ltnode->NxtTrans = tnode;	/* tlnode points to last on list for this state */
				ltnode = tnode; 				/* tnode is last now, in any case */
			}
			
			else {							/* its an existing TrNode... */ 
				lpnode = tnode->Paths;			/* find last PathNd attached to it */
				if ( lpnode == NULL ) {
					sprintf( GOutBuff, "\nReadFSW (trans): no paths on tnode (st %d, eff %d)!",
								state1, effset );
					WriteStd( GOutBuff );
				}
				else {			/* just move down list till the end is in sight */
					for ( ; lpnode->NxtPath != NULL; lpnode = lpnode->NxtPath )
						;
				}
			}

				/* Here tnode points to TrNode, and lpnode to last PathNd (or NULL) */
				/* So get the transition (state,prob) values for this effset value */

			while ( *bufptr != '\0' ) {
				bufptr = GetUInt( bufptr, &state2, 9999, ",", &err);
				if ( state2 >= NmStates || state2 == 9999 || err ) {
					sprintf( GOutBuff, "\nReadFSW (trans): destin %d not valid for %d,%d.",
							state2, state1, effset );
					WriteStd( GOutBuff );
					continue;
				}

				bufptr = GetFloat( bufptr, &prob, -1.0, ",", &err);

				if ( prob < 0.0 || prob > 1.0 || err ) {
					sprintf( GOutBuff, "\nReadFSW (trans): prob (%f) not valid for %d,%d.",
								prob, state1, effset );
					 WriteStd( GOutBuff );
					 continue;
				}
							
				OldState = States + state2; 		/* pointer to destination */
				pnode = PaAlloc();					/* get a node */
				pnode->NxtPath = NULL;				/* Initialize */
				pnode->PathProb = prob;
				pnode->PathDest = OldState;
				if ( tnode->Paths == NULL ) 		/* add to end of list */
					tnode->Paths = pnode;			/* first one! */
				else
					lpnode->NxtPath = pnode;		/* append to last one */
				lpnode = pnode; 					/* its last now */

			} /* more transitions on line */
		} /* got a real line */
	} /* got more transitions */

		/* lets check the transitions... */

    CheckTransitions();
	
	CurState = OldState = States + startstate;		/* get pointed at start state */

} /* ReadFSW */

/*******************************

******/

VOID CheckTransitions ()
{
	unsigned int	state1, effset;
	float			totprob;
	struct TrNode	*tnode;
	struct PaNode	*pnode;

    printf( "\nCheck transitions..." );

	for ( CurState = States, state1 = 0; state1 < NmStates; ++state1, ++CurState ) {
        sprintf( GOutBuff, "(s %d...) ", state1 );
        WriteStd( GOutBuff );
		if ( CurState->NmEffTr == 0 && CurState->MaxRInit == 0 ) {
			sprintf( GOutBuff, "\nFSWRead: NmEffTr and MaxRInit both 0 for s%d!", state1 );
			WriteStd( GOutBuff );
		}
		for ( tnode = CurState->Transits; tnode != NULL; tnode = tnode->NxtTrans ) {
			totprob = (float) 0.0;
			for ( pnode = tnode->Paths; pnode != NULL; pnode = pnode->NxtPath )
				totprob += pnode->PathProb;
			if ( totprob < (float) 1.0 ) {
				sprintf( GOutBuff, "\nFSWRead: totprob %f for state %d, effset %d < 1.0.",
						totprob, state1, tnode->EffSet );
				WriteStd( GOutBuff );
			}
		}	/* transitions */
	} /* states */

    printf( "ok.\n" );

} /* end CheckTransitios */

/*******************************

******/

VOID RdStates ( FilePtr )
	FILE *FilePtr;
{
	int 			RetLen, err;
	unsigned int	state1, vc;
	struct StateNd	*stateptr;
	char			*bufptr, *cptr, *cxptr, attbuf[STRNGSZ+20];

	while ( ReadS( GInBuff, GIBUFFMX, FilePtr, &RetLen) != EOF ) {
		if ( *GInBuff == COMCHAR || RetLen == 0 )
			continue;
		else if ( strcmp( GInBuff, "ENDSTATES" ) == 0 )
			break;
		else {
			bufptr = GetUInt( GInBuff, &state1, 9999, ",", &err);
			if ( state1 >= NmStates || state1 == 9999 ) {
				sprintf( GOutBuff, "\nReadFSW (rewards): state %d not valid.", state1 );
				 WriteStd( GOutBuff );
			}
			else {
				stateptr = States + state1;
				stateptr->Id = state1;
				bufptr = GetUInt( bufptr, &vc, 0, ",", &err );
				if ( err ) {
					sprintf( GOutBuff, "\nReadFSW (visits): not valid for state %d.", state1 );
					WriteStd( GOutBuff );
					vc = 0;
				}
				stateptr->VisitCnt = vc;
				stateptr->Transits = NULL;
				stateptr->NmEffTr = 0;

				bufptr = StpTok2( bufptr, attbuf, sizeof(attbuf), "," );
				for ( cxptr = cptr = attbuf; *cxptr != '\0'; ++cxptr )
					if ( *cxptr != ' ' )			/* get rid of blanks */
						*cptr++ = *cxptr;
				*cptr = '\0';
				if ( attbuf[0] == '\0' )			/* No attribute string */
					stateptr->NoAttr = TRUE;
				else {
					attbuf[0] = attbuf[1] = '0';	/* must start 00 */
					if ( !IsMessage( attbuf ) ) {
						sprintf( GOutBuff, "\nIllegal state-desc. string on '%s'.\nNot loaded.", GInBuff ); 
						WriteStd( GOutBuff );
					}
					else
						AMsgtoB( attbuf, stateptr->Attrib );
							/* Read in any variable loci */
					while ( *bufptr != '\0' )
						bufptr = RdVarLoc ( bufptr, stateptr );
				}
			}
		}
	}

} /* RdStates */


/*******************************

******/

char *RdVarLoc ( BufPtr, State )
	char *BufPtr;
	struct StateNd *State;
{
	int 			err;
	unsigned int	varlocus;
	float			prob;
	char			*bufptr;
	struct VLociNd	*vlnode;

	bufptr = GetUInt( BufPtr, &varlocus, 9999, ",", &err);
	if ( varlocus < 2 || varlocus > STRNGMX ) {
		sprintf( GOutBuff, "\nIllegal locus, string in '%s'.\nNot used.", GInBuff );
		WriteStd( GOutBuff );
	}
	else {
		bufptr = GetFloat( bufptr, &prob, 0.0, ",", &err);
		if ( prob <= (float) 0.0 || prob >= (float) 1.0 ) {
			sprintf( GOutBuff, "\nBad locus prob, string in '%s'.\nNot used.", GInBuff );
			WriteStd( GOutBuff );
		}
		else {
			vlnode = VLAlloc();
			vlnode->VarLocus = varlocus;
			vlnode->Prob1	 = prob;
			if ( State->VarLoci == NULL ) {	/* its first */
				State->VarLoci = vlnode;
				vlnode->NxtVL = NULL;
			}
			else {	 /* insert at beginning */
				vlnode->NxtVL = State->VarLoci;
				State->VarLoci = vlnode;
			}
		}
	}

	return( bufptr );

} /* RdVarLoc */


/*******************************

LoadGref3   Load world used by grefenstette in Machine learning vol 2 issue.

******/

VOID  LoadGref3 ( )
{
    int             row, col;
    char            attribute[STRNGSZ];
    struct StateNd  *stateptr;
    struct TrNode   *tnode;

    WriteStd( "\nLoading gref3..." );

    strcpy( attribute, "00000000" );    /* all start with 8 zeros */

    for ( row = 0, NmStates = 0; row <= 7; ++row ) {

        sprintf( GOutBuff, "\nrow % (first state %d)...", row, NmStates );
        WriteStd( GOutBuff );

        Int2Loci( row, &attribute[8], 3 );  /* get row into 3 bit ascii */

        for ( col = 0; col <= 31; ++col, ++NmStates ) {

            stateptr = States + NmStates;   /* pointer to state */
            stateptr->Id = NmStates;        /* id is just its number */

            Int2Loci( col, &attribute[11], 5 ); /* get the col into 5 bit ascii */
            attribute[STRNGSZ] = '\0';
            
			if ( !IsMessage( attribute ) ) {
				sprintf( GOutBuff, "\nIllegal state-desc >%s<!", attribute ); 
				WriteStd( GOutBuff );
			}
			else
				AMsgtoB( attribute, stateptr->Attrib );

            stateptr->NoAttr = FALSE;
            stateptr->VarLoci = NULL;
            stateptr->Payoff = 0;
            stateptr->VisitCnt = 0;
            stateptr->MaxRInit = 0;
            stateptr->BestNxt = 0;
            stateptr->NmBest = 0;
            stateptr->NmNeg = 0;

                /* add the straight transition */

			if ( (tnode = TrAlloc()) == NULL ) {	    /* get a TrNode */
                sprintf( GOutBuff, "\nTrAlloc NULL for row %d, col %d (straight).\n", row, col );
                WriteStd( GOutBuff );
                exit;
            }
			tnode->EffSet = 0;      /* 0 is straigth */
			if ( (tnode->Paths = PaAlloc()) == NULL ) {   /* attach the one path for this setting */
                sprintf( GOutBuff, "\nPaAlloc NULL for row %d, col %d (straight).\n", row, col );
                WriteStd( GOutBuff );
                exit;
            }
			tnode->Paths->NxtPath = NULL;
			tnode->Paths->PathProb = 1.0;
			tnode->Paths->PathDest = States + (NmStates + 32);

            stateptr->Transits = tnode; /* link first tnode to state */
            stateptr->NmEffTr = 3;      /* will be 3 transitions when done */

                /* now lets do the right (1) transition; link it to first tnode */

			if ( (tnode->NxtTrans = TrAlloc()) == NULL ) { /* get a TrNode */
                sprintf( GOutBuff, "\nTrAlloc NULL for row %d, col %d (right).\n", row, col );
                WriteStd( GOutBuff );
                exit;
            }
            tnode = tnode->NxtTrans;        /* link it on */
			tnode->EffSet = 1;
			if ( (tnode->Paths = PaAlloc()) == NULL ) {   /* attach the one path for this setting */
                sprintf( GOutBuff, "\nPaAlloc NULL for row %d, col %d (right).\n", row, col );
                WriteStd( GOutBuff );
                exit;
            }
			tnode->Paths->NxtPath = NULL;
			tnode->Paths->PathProb = 1.0;
            if ( col == 31 ) 
    			tnode->Paths->PathDest = States + (NmStates + 1);
            else
    			tnode->Paths->PathDest = States + (NmStates + 33);

                /* now lets do the left (2) transition, and link to to previous one */

			if ( (tnode->NxtTrans = TrAlloc()) == NULL ) { /* get a TrNode */
                sprintf( GOutBuff, "\nTrAlloc NULL for row %d, col %d (left).\n", row, col );
                WriteStd( GOutBuff );
                exit;
            }
            tnode = tnode->NxtTrans;        /* link it on */
			tnode->EffSet = 2;
			if ( (tnode->Paths = PaAlloc()) == NULL ) {   /* attach the one path for this setting */
                sprintf( GOutBuff, "\nPaAlloc NULL for row %d, col %d (left).\n", row, col );
                WriteStd( GOutBuff );
                exit;
            }
			tnode->Paths->NxtPath = NULL;
			tnode->Paths->PathProb = 1.0;
            if ( col == 0 ) 
    			tnode->Paths->PathDest = States + (NmStates + 63);
            else
    			tnode->Paths->PathDest = States + (NmStates + 31);

            tnode->NxtTrans = NULL;     /* last effector setting */

        } /* endfor cols */

    } /* endfor rows */

    if ( NmStates != 256 ) {
        sprintf( GOutBuff, "\nNmStates not 256 after load (%d).\n", NmStates );
        WriteStd( GOutBuff );
        exit;
    }
    else {
        sprintf( GOutBuff, "\nLoaded %d states...load end states...", NmStates );
        WriteStd( GOutBuff );
    }

        /* set up the end states */

    for ( ; NmStates <= 287 ; ++NmStates ) {
        stateptr = States + NmStates;   /* pointer to state */
        stateptr->Id = NmStates;        /* id is just its number */
        stateptr->NoAttr = TRUE;
        stateptr->Payoff = -0.01;  /* will override this below */
        stateptr->VisitCnt = 0;
        stateptr->MaxRInit = 31;
        stateptr->BestNxt = 0;
        stateptr->NmBest = 0;
        stateptr->NmNeg = 0;
        stateptr->Transits = NULL;
        stateptr->NmEffTr = 0;
    } /* endfor end states */

    WriteStd( "set payoffs..." );

    (States + 258)->Payoff = 50;
    (States + 259)->Payoff = 75;
    (States + 260)->Payoff = 125;
    (States + 261)->Payoff = 250;
    (States + 262)->Payoff = 500;
    (States + 263)->Payoff = 1000;
    (States + 264)->Payoff = 1000;
    (States + 265)->Payoff = 500;
    (States + 266)->Payoff = 250;
    (States + 267)->Payoff = 125;
    (States + 268)->Payoff = 75;
    (States + 269)->Payoff = 50;

    (States + 274)->Payoff = 50;
    (States + 275)->Payoff = 75;
    (States + 276)->Payoff = 125;
    (States + 277)->Payoff = 250;
    (States + 278)->Payoff = 500;
    (States + 279)->Payoff = 1000;
    (States + 280)->Payoff = 1000;
    (States + 281)->Payoff = 500;
    (States + 282)->Payoff = 250;
    (States + 283)->Payoff = 125;
    (States + 284)->Payoff = 75;
    (States + 285)->Payoff = 50;

    sprintf( GOutBuff, "\nDone loading gref3 (%d states).\n", NmStates );
    WriteStd( GOutBuff );


}  /* end LoadGref3 */



/********************************

TrAlloc 	Call system routine alloc to get space for an TrNode.
TrFree		Give space for TrNode back to system.
PaAlloc 	|- ditto for PaNode.
PaFree		|
VLAlloc 	|- ditto for VLociNd.
VLFree		|

******/

struct	TrNode *TrAlloc ()
{
	char	*malloc();
	struct	TrNode *tnode;

	if ( (tnode = (struct TrNode *) malloc(sizeof(struct TrNode))) == NULL ) {
		WriteStd("\n**TrAlloc: NULL from malloc()\n");
		exit( ERROR );
	}

	return( tnode );
		
} /* TrAlloc */


int TrFree ( tnode )
	struct	TrNode *tnode;
{

#if ( CBELLMTS || LATTICEC || DECSTATION )
	free( (char *) tnode );
#else
#if APOLLOC 				
	if ( free( (char *) tnode ) == 0 ) {	/* Apollo C free() return 0 means error (I think) */
		WriteStd("\n**TrFree: 0 from free()\n");
		return( ERROR );
	}
#else
	if ( free( (char *) tnode ) != 0 ) { 
		WriteStd("\n**TrFree: non-0 from free()\n");
		return( ERROR );
	}
#endif
#endif

	return( OK );
	
} /* TrFree */

/**********/

struct	PaNode *PaAlloc ()
{
	char	*malloc();
	struct	PaNode *pnode;

	if ( (pnode = (struct PaNode *) malloc(sizeof(struct PaNode))) == NULL ) {
		WriteStd("\n**PaAlloc: NULL from malloc()\n");
		exit( ERROR );
	}

	return( pnode );
		
} /* PaAlloc */
	
int PaFree ( pnode )
	struct	PaNode *pnode;
{

#if ( CBELLMTS || LATTICEC || DECSTATION )
	free( (char *) pnode );
#else
#if APOLLOC 				
	if ( free( (char *) pnode ) == 0 ) {	/* Apollo C free() return 0 means error (I think) */
		WriteStd("\n**PaFree: 0 from free()\n");
		return( ERROR );
	}
#else
	if ( free( (char *) pnode ) != 0 ) { 
		WriteStd("\n**PaFree: non-0 from free()\n");
		return( ERROR );
	}
#endif
#endif

	return( OK );
	
} /* PaFree */

/**********/

struct	VLociNd *VLAlloc ()
{
	char	*malloc();
	struct	VLociNd *tnode;

	if ( (tnode = (struct VLociNd *) malloc(sizeof(struct VLociNd))) == NULL ) {
		WriteStd("\n**VLAlloc: NULL from malloc()\n");
		exit( ERROR );
	}

	return( tnode );
		
} /* VLAlloc */
	
int VLFree ( tnode )
	struct	VLociNd *tnode;
{

#if ( CBELLMTS || LATTICEC || DECSTATION )
	free( (char *) tnode );
#else
#if APOLLOC 				
	if ( free( (char *) tnode ) == 0 ) {	/* Apollo C free() return 0 means error (I think) */
		WriteStd("\n**VLFree: 0 from free()\n");
		exit( ERROR );
	}
#else
	if ( free( (char *) tnode ) != 0 ) { 
		WriteStd("\n**VLFree: non-0 from free()\n");
		exit( ERROR );
	}
#endif
#endif

	return( OK );
	
} /* VLFree */

#if MPWC
#define __SEG__ CFSCWRITE
#endif


/*****************************

DsplEnv 	Display environment variable values, status, etc.

	FDName	Name of destination file/device.

	Format	Control format of display (or turn it off).

******/

int DsplEnv ( FDName, Format )
	char	*FDName;
	int		Format;
{
	FILE		*destin, *GetFILE();
	extern char	*DisOutFN, *StdOutFN;
	extern FILE	*DisOFILE, *StdOFILE;

	if ( strcmp( FDName, DisOutFN ) == 0 || *FDName == '\0' )
		destin = DisOFILE;						/* DisOutFN is already open */
	else if ( strcmp( FDName, StdOutFN ) == 0 )
		destin = StdOFILE;						/* StdOutFN is already open */
	else if ( (destin = GetFILE( FDName, "", WRITEMODE )) == NULL ) 
		return( ERROR );

	WrtEnv( destin, Format );

	if ( destin != DisOFILE && destin != StdOFILE )	/* Dont close these streams! */
		CloseFILE( FDName, "", destin );

	return( OK );

} /* DsplEnv	*/


/*****************************

WrtEnv			Write enviornment state to open file/device.

	Destin		Opened file/device.

	Format		Control format of display.

******/

VOID WrtEnv ( FilePtr, Format )
	FILE	*FilePtr;
	int		Format;
{
	int 			state, effset, vc, totvc;
	float			f;
	struct TrNode	*tnode;
	struct PaNode	*pnode;
	struct StateNd	*snode;

	if ( Format == SAVEFMT )
		SaveEnv( FilePtr );

	else if ( Format == 1 ) {
		sprintf( GOutBuff, "\nState %d  (r %.1f, tr %.1f) [Dft %u, Invld %u, r %u] CycleStp %u.",
			CurState->Id, CurState->Payoff, FSWTSR, TotNmDft, NmInvldE, EfValBin, CycleStp );
		WriteS( GOutBuff, FilePtr );
		WriteS( "\nVisit Count: ", FilePtr );
		for ( state = 0, snode = States; state < NmStates; ++state, ++snode ) {
			sprintf( GOutBuff, " %3u", snode->VisitCnt );
			WriteS( GOutBuff, FilePtr );
		}
		WriteS( "\n", FilePtr );
		
			 /* NOTE: using effset to count NmBest across all states */

		for ( state = effset = totvc = 0, snode = States; state < NmStates; ++state, ++snode ) {
			if ( snode == CurState )				/* correct for state not left yet */
				vc = snode->VisitCnt - 1;
			else
				 vc = snode->VisitCnt;

			if ( snode->BestNxt != -1 ) {
				if ( state % 8 == 0 && state == 0 )
					WriteS( "Best: ", FilePtr );
				else if ( state % 8 == 0 )
					WriteS( "\nBest: ", FilePtr );
				if ( vc != 0 )
					f = 1.0 * snode->NmBest / vc;
				else
					f = -1;
				totvc += vc;
				effset += snode->NmBest;
				sprintf( GOutBuff, " %d,%u,%u,%.3f, ", state, snode->NmBest, snode->VisitCnt, f );
				WriteS( GOutBuff, FilePtr );
			}
		}

		if ( effset != 0 ) {
			if ( totvc != 0 )
				f = 1.0 * effset / totvc;
			else
				f = -1;
			sprintf( GOutBuff, ": (%d,%d) %.3f\n", effset, totvc, f );
			 WriteS( GOutBuff, FilePtr );
		}
	}

	else if ( Format == 2 ) {
		WriteS( "\n	 States:", FilePtr );
		WriteS( "\nCur State   Attributes	   PayOff  VisitCnt  NmBest(Sta) NmNeg", FilePtr );

		for ( state = 0, snode = States; state < NmStates; ++state, ++snode ) {
			if ( snode->NoAttr )
				sprintf( GInBuff, "	No Attr	" );
			else {
				BMsgtoA( snode->Attrib, GInBuff );
				*(GInBuff + STRNGSZ) = '\0';
			}
			if ( snode->BestNxt != -1 )
				sprintf( GOutBuff, "\n%3d	  %s  %5.0f   %4u   %5u (%2u)  %5u", state,
					 GInBuff, snode->Payoff, snode->VisitCnt,
					 snode->NmBest, snode->BestNxt, snode->NmNeg );
			else
				sprintf( GOutBuff, "\n%3d	  %s  %5.0f   %4u   %5u (--)  %5u", state,
					 GInBuff, snode->Payoff, snode->VisitCnt,
					 snode->NmBest, snode->NmNeg );
			WriteS( GOutBuff, FilePtr );
		}
		WriteS( "\n", FilePtr );
	}

	else if ( Format == 3 ) {
		sprintf( GOutBuff, "\nCurState %d  (rew %.1f, totrew %.1f) [Dft %u, Invld %u, r %u] CycleStp %d.",
			CurState->Id, CurState->Payoff, FSWTSR, TotNmDft, NmInvldE, EfValBin, CycleStp );
		WriteS( GOutBuff, FilePtr );
		WriteS( "\nVisit Rate: ", FilePtr );
		for ( state = 0, snode = States; state < NmStates; ++state, ++snode ) {
			f = 1000.0 * snode->VisitCnt / CycleStp;
			sprintf( GOutBuff, " %4.0f", f );
			WriteS( GOutBuff, FilePtr );
		}
		WriteS( "\nEnd Rates\n", FilePtr );
	}

	else if ( Format == 4 ) {
		WriteS( "\n	 Transitions:", FilePtr );
		WriteS( "\nCur State EffSet   Prob   NewState", FilePtr );

		for ( state = 0, snode = States; state < NmStates; ++state, ++snode ) {
			sprintf( GOutBuff, "\n  %3d  ", state );
			WriteS( GOutBuff, FilePtr );
			if ( snode->NmEffTr == 0 ) {
				sprintf( GOutBuff, "	MaxRInit %u", snode->MaxRInit );
				WriteS( GOutBuff, FilePtr );
				continue;
			}
			for ( tnode = snode->Transits; tnode != NULL; tnode = tnode->NxtTrans ) {
				if ( tnode == snode->Transits ) {
					sprintf( GOutBuff, "  %3d  ", tnode->EffSet );
					WriteS( GOutBuff, FilePtr );
				}
				else {
					sprintf( GOutBuff, "\n		 %3d  ", tnode->EffSet );
					WriteS( GOutBuff, FilePtr );
				}
				for ( pnode = tnode->Paths; pnode != NULL; pnode = pnode->NxtPath ) {
					if ( pnode == tnode->Paths ) {
						sprintf( GOutBuff, "  %6.4f	%3d",
							pnode->PathProb, pnode->PathDest->Id );
						WriteS( GOutBuff, FilePtr );
					}
					else {
						sprintf( GOutBuff, "\n				%6.4f	%3d",
							pnode->PathProb, pnode->PathDest->Id );
						WriteS( GOutBuff, FilePtr );
					}
				}
			}
		}
		WriteS( "\n", FilePtr );
	}

} /* WrtEnv */


/*****************************

SaveEnv 	Write environment state in form ready for reload by LoadEnv().

	FilePtr	FILE pointer to already open file to write to.

******/

VOID SaveEnv ( FilePtr )
	FILE	*FilePtr;
{
	int				state,	effset;
	struct StateNd	*snode;
	struct TrNode	*tnode;
	struct PaNode	*pnode;
	struct VLociNd	*vlnode;

	fprintf( FilePtr, "; FSW Environment state:\n" );
	fprintf( FilePtr, "%d, %u, %u, %u\n", NmStates, CurState->Id, TotNmDft, NmInvldE );
	for ( state = 0, snode = States; state < NmStates; ++state, ++snode ) {
		if ( snode->NoAttr )
			fprintf( FilePtr, "%u, %u", state, snode->VisitCnt );
		else {
			BMsgtoA( snode->Attrib, GOutBuff );
			GOutBuff[STRNGSZ] = '\0';
			fprintf( FilePtr, "%u, %u, %s", state, snode->VisitCnt, GOutBuff );
			for ( vlnode = snode->VarLoci; vlnode != NULL; vlnode = vlnode->NxtVL )
				fprintf( FilePtr, ", %d,%f", vlnode->VarLocus, vlnode->Prob1 );
		}
		fprintf( FilePtr, "\n" );
	}
	fprintf( FilePtr, "ENDSTATES\n" );
	for ( state = 0, snode = States; state < NmStates; ++state, ++snode )
		fprintf( FilePtr, "%d, %.4f/\n", state, snode->Payoff );
	fprintf( FilePtr, "ENDPAYOFFS\n" );

	for ( state = 0, snode = States; state < NmStates; ++state, ++snode ) {
		if ( snode->NmEffTr == 0 ) {
			sprintf( GOutBuff, "%d,r, %d\n", state, snode->MaxRInit );
			fprintf( FilePtr, GOutBuff );
			continue;
		}
		for ( tnode = snode->Transits; tnode != NULL; tnode = tnode->NxtTrans ) {
			sprintf( GOutBuff, "%d,%d", state, tnode->EffSet );
			fprintf( FilePtr, GOutBuff );
			for ( pnode = tnode->Paths; pnode != NULL; pnode = pnode->NxtPath ) {
				sprintf( GOutBuff, ", %d,%8.6f", pnode->PathDest->Id, pnode->PathProb );
				fprintf( FilePtr, GOutBuff );
			}
			fprintf( FilePtr, "\n" );
		}
	}
	fprintf( FilePtr, "ENDTRANS\n" );

	WrtEffs( FilePtr ); 				/* This writes the effectors and END-EFFS terminator */

	for ( effset = 0; effset < StrAttSz; ++effset )
		fprintf( FilePtr, "%d,", EffLoci [ effset ] );
	fprintf( FilePtr, "\nEND-ELOCI\n" );

} /* SaveEnv */

#if MPWC
#define __SEG__ Main
#endif

/*****************************

GetDMsgs		Get messages from detectors to be added to the message-list.

	MStrings	Char array to hold ALL messages, end to end.
				Each message in the array is terminated by a '\0'.
				There is room for DMSGSMX messages -- see CORE.H for DMSGSMX definition.

	MIntens		Point to first of array of floats, each one the
				'Intensity' to store with the corresponding message strings pointed to
				by the MStrings array. (Room for DMSGSMX of these, too.)

	Return:		Number of messages from detectors.

Just write States value for CurState.

******/

int GetDMsgs( MStrings, MIntens )
	char		MStrings[];
	float		MIntens[];
{
	struct VLociNd *vlnode;

	if ( CurState->NoAttr )
		return( 0 );

	else {
		BMsgtoA( CurState->Attrib, MStrings );
		MStrings[STRNGSZ] = '\0';
		MIntens[0] = MsgIntDf;
		for ( vlnode = CurState->VarLoci; vlnode != NULL; vlnode = vlnode->NxtVL )
			if ( URand01() < (float) vlnode->Prob1 )
				MStrings[vlnode->VarLocus] = '1';
			else
				MStrings[vlnode->VarLocus] = '0';
	}

	return( 1 );

} 	/* GetDMsgs */


/*****************************

StoreSup		Subroutines to store support in set specified by the effector message.

	MsgPtr		The message that activated this effector. 

Add Support to each effector value specified by Msg (&increment total support).
"Support" is the "biased bid" of the producer of Msg.

There are StrAttSz loci in the effector message specify what
settings are supported. These are located at the loci indicated by
the values in EffLoci array (default is 8-15).
Each pair of bits specifies a setting for an "attribute",
i.e., a bit in a EffAttSz long binary number:
	pair	setting
	value	 value
	 00 		0
	 01 	 	1
	 1# 	don't care--support both 0 and 1 equally.
where the rightmost pair specifies the rightmost bit, etc.
The attributes taken as a binary number represent a transition path.
Thus a given message may support several paths or just one.

******/

int StoreSup ( MsgPtr )
	struct MsgNode *MsgPtr;
{
	register unsigned int	 s, i;
	char					 EMsg[STRNGSZ+1], msg[EFFATTSZ+1];
	
	BMsgtoA( MsgPtr->Message, EMsg );		/* Get ascii version of msg */
	EMsg[STRNGSZ] = '\0';

	for ( s = 0; s < StrAttSz; ++s )		/* Extract values from EMsg Loci */
		EfSetAsc[s] = EMsg[ EffLoci [s] ];

#if CTEST
	if ( DemoLev > 0 ) { 
		EfSetAsc[s] = '\0';
		sprintf( GOutBuff, "\nStoreSup: EMsg '%s', EfSetAsc '%s' Support %6.1f;",
			EMsg, EfSetAsc, MsgPtr->Producer->CfBBid );
		WriteStd( GOutBuff );
	}
#endif

	for ( s = 0, i = 0; s < StrAttSz; s += 2, ++i )
		if	 ( EfSetAsc[s] == '1' )
			msg[i] = '#';
		else if ( EfSetAsc[s+1] == '1' )
			msg[i] = '1';
		else
			msg[i] = '0';

	msg[EffAttSz] = '\0';

#if CTEST
	if ( DemoLev > 0 ) { 
		sprintf( GOutBuff, " msg set '%s'.", msg ); 
		WriteStd( GOutBuff );
	}
#endif

	for ( s = 0; s <= EffValMx; ++s )			/* Just march down all effector values */
		if ( IsMemSym( s, msg ) ) {
#if CTEST
			if ( DemoLev > 1 ) {
				sprintf( GOutBuff, " Val %d Supported!", s );
				WriteStd( GOutBuff );
			}
#endif
			EfValSup[s] += MsgPtr->Producer->CfBBid;
			TotSup		+= MsgPtr->Producer->CfBBid;
		}

	return( OK );

} /* StoreSup */



/****************************

IsMemSym		See if symbol is member of set of symbols consistent with message.

	Symbol		Binary form of symbol to check.
	Set			EffAttSz long ascii string in {0,1,#} that specifies set of symbols.

******/

int IsMemSym ( Symbol, Set )
	unsigned int	Symbol;
	char			Set[];
{
	unsigned int	aloci, SetBits, SetDCs;

	SetBits = SetDCs = 0;

	for ( aloci = 0; aloci < EffAttSz; ++aloci) {
		SetBits <<= 1;				/* zero into lsb */
		SetDCs	<<= 1;
		switch ( Set[aloci] ) {
			case '0':	SetDCs |= LSB;
						break;
			case '1':	SetBits |= LSB;
						SetDCs	|= LSB;
						break;
			case '#':	break;
			default:	sprintf(GOutBuff,"\nERR (IsMemSym %d): bad char. at %d >%c< in condition: '%4.4s'", Symbol, aloci, Set[aloci], Set );
						WriteStd( GOutBuff );
						return(ERROR);
		};
	}

#if CTEST
	if ( DemoLev > 2 ) {
		sprintf( GOutBuff, "\nIsSymMem? Sym is %d, Set is '%s' (bits %d, dsc %d)", Symbol, Set, SetBits, SetDCs );
		WriteStd( GOutBuff );
	}
#endif

	if ( !((Symbol & SetDCs) ^ SetBits) )
		return( TRUE );

	return( FALSE );

} /* IsMemSym */



/*	***************************

PkTrans 		Pick Transition path based on support stored for various values.

	BehavStp	If TRUE, make OldState = CurState and CurState = T(s,e).

	ConflictResolution 0: pick proportional to support, 1 means pick max.

	First use support stored to chose an effector setting.
	If setting chosen is invalid for the current state, then
	use HiTrRsl to determine how to select effector setting:
		0	Just use first setting stored
		1	Pick effector setting at random from allowed settings
	At any rate, once a setting is chosen, then pick a path
	(i.e., a next state) based on the probabilities assoicated
	with the (CurState,EffSet) pair.

	NOTE: If invalid setting is chosen, set MadeMstk TRUE.
			(It may trigger discovery algorithms to generate a new classsifier;
			see GetSysRw() for more information.)

	NOTE: If CurState NmEffTr is 0, then use MaxRInit value to
			determine what next state should be, namely,
			a state chosen randomly from 0..MaxRInit.

******/

int PkTrans ( BehavStp )
{
	register float	ran, cum, maxf;
	register int	dft, i, maxe;
	struct TrNode	*tnode;
	struct PaNode *pnode;

	if ( CurState->NmEffTr == 0 ) {
		if ( BehavStp ) {
			OldState = CurState;
			CurState = States + URandN( CurState->MaxRInit );
#if CTEST
			if ( DemoLev > 0 )
				WriteStd( "\nUse MaxRInit to pick next state.\n" );
#endif
		}
		return( OK );
	}
	
	ran = TotSup * URand01();
	if ( ran == (float) 0.0 )			/* a KLUDGE */
		ran = TotSup * URand01();

#if CTEST
	if ( DemoLev > 0 ) {
		if ( DemoLev > 1 ) {
			WriteStd( "\nEffSet  Support" );
			for ( EfValBin = 0, cum = 0.0; EfValBin <= EffValMx; ++EfValBin ) {
				sprintf(GOutBuff, "\n%4d	%6.1f", EfValBin, EfValSup[EfValBin] );
				WriteStd( GOutBuff );
			}
		}
		sprintf( GOutBuff, "\nPkTrans: ran is %f, TotSup is %f", ran, TotSup );
		WriteStd( GOutBuff );
	}
#endif

	if ( ConflictResolution == 0 ) {
		for ( EfValBin = 0, cum = 0.0; EfValBin <= EffValMx; ++EfValBin ) {
			cum += EfValSup[EfValBin];
			if ( ran <= cum )
				break;
		}
#if CTEST
		if ( DemoLev > 0 ) {
			sprintf( GOutBuff, "  EffSet is %d (cum %f).", EfValBin, cum );
			WriteStd( GOutBuff );
		}
#endif
	}
	else {
		for ( EfValBin = 0, maxe = 0, maxf = -1; EfValBin <= EffValMx; ++EfValBin ) {
			if ( EfValSup[EfValBin] > maxf ) {
				maxf = EfValSup[EfValBin];
				maxe = EfValBin;
			}
		}
		EfValBin = maxe;
#if CTEST
		if ( DemoLev > 0 ) {
			sprintf( GOutBuff, "  EffSet is %d (max %f).", EfValBin, maxf );
			WriteStd( GOutBuff );
		}
#endif
	}


	for ( tnode = CurState->Transits; tnode != NULL; tnode = tnode->NxtTrans )
		if ( tnode->EffSet == EfValBin )
			break;

	if ( tnode == NULL ) {
		++NmInvldE;
		MadeMstk = TRUE;
		if ( HiTrRsl == 0 ) {			/* Use first transitions as default */
			tnode = CurState->Transits;
#if CTEST
			if ( DemoLev > 0 ) {
				WriteStd( "\nUsing default EffSet" );
				if ( tnode != NULL ) {
					sprintf( GOutBuff, " %d.", tnode->EffSet );
					WriteStd( GOutBuff );
				}
			}
#endif
		}
		else {									/* pick at random */
#if CTEST
			if ( DemoLev > 0 ) {
				sprintf( GOutBuff, "\nPk EffSet at random, from state %d's %d settings",
					 CurState->Id, CurState->NmEffTr );
				WriteStd( GOutBuff );
			}
#endif
			if ( CurState->NmEffTr == 0 )	 /* There aren't any! */
				tnode = NULL;
			else if ( CurState->NmEffTr == 1 )	 /* only one! */
				tnode = CurState->Transits;
			else {								/* pick from > 1 */
				dft = URandN( CurState->NmEffTr - 1 );	/* return 0..NmEfTr-1 */
				for ( i=0, tnode=CurState->Transits; i < dft && tnode != NULL; ++i )
					tnode = tnode->NxtTrans;		/* move down list */
#if CTEST
				if ( DemoLev > 0 ) {
					sprintf( GOutBuff, " => #%d, effset %d.", i, tnode->EffSet );
					WriteStd( GOutBuff );
				}
#endif
			}
		}

		if ( tnode == NULL ) {
			sprintf( GOutBuff, "\n**PkTrans: no exit from %d. Going to random state.\n", CurState->Id );
			WriteStd( GOutBuff );
			if ( BehavStp ) {
				OldState = CurState;
				CurState = States + URandN( NmStates-1 );
			}
			return( OK );
		}
	}

	ran = URand01();			/* Get ready to pick path for CurState,EfValBin pair */
	for ( pnode = tnode->Paths, cum = 0.0; pnode != NULL; pnode = pnode->NxtPath )
		if ( (cum += pnode->PathProb) >= ran )
			break;

	if ( pnode == NULL ) {
		sprintf( GOutBuff, "\n**PkTrans: path null! cur %d, eff %d, cum %f, ran %f. Going to Random State.",
				CurState->Id, tnode->EffSet, cum, ran );
		WriteStd( GOutBuff );
		if ( BehavStp ) {
			OldState = CurState;
			CurState = States + URandN( NmStates-1 );
		}
		return( OK );
	}

	if ( BehavStp ) {
#if CTEST
		if ( DemoLev > 0 ) {
			sprintf( GOutBuff, "  Destin %d (ran %f, cum %f)\n", pnode->PathDest->Id, ran, cum );
			WriteStd( GOutBuff );
		}
#endif
		OldState = CurState;
		CurState = pnode->PathDest;
	}

	return( OK );

} /* PkTrans */


/*****************************

DftTrans		Make default transition, ie., do this when do nothing else.

	If BehavStp is TRUE, then move down randomly selected path
	using first effector setting as the (default) setting.

	If DftBehFlg is FALSE, do nothing.

  NOTES:If DftRand is 1, then pick effector setting at random
		from all valid settings for the current state, and
		then select path given that transition at random.
		DftRand is 0 by default, i.e., use first listed effector setting.

		If default action taken and DftMstk is 1, then set MadeMstk TRUE.
		This trigger may be used by discovery algorithms to generate
		a classifier to emit an effector message in the situation
		that caused it to do nothing.
		(MadeMstk is also set TRUE if SysRew < 0; see GetSysRw() in this file.)

	NOTE:If CurState NmEffTr is 0, then use MaxRInit value to
		determine what next state should be, namely,
		a state chosen randomly from 0..MaxRInit.
		And DO NOT set MadeMstk TRUE.

******/

int DftTrans ( BehavStp )
	int	BehavStp;
{
	int 			dft, i;
	float			cum, ran;
	struct TrNode	*tnode;
	struct PaNode	*pnode;

	++TotNmDft;
	
	if ( BehavStp ) {
		if ( DftMstk )
			MadeMstk = TRUE;
			
		OldState = CurState;

		if ( CurState->NmEffTr == 0 ) {
			CurState = States + URandN( CurState->MaxRInit );
#if CTEST
			if ( DemoLev > 0 )
				WriteStd( "\nDftTrans: Use MaxRInit to pick next state.\n" );
#endif
			return( OK );
		}

			/* if DftRand is off, or if only 1 effector setting, use first one. */

		if ( !DftRand || CurState->NmEffTr == 1 ) {
			tnode = CurState->Transits; 	/* get first effector setting transitions */
			if ( tnode == NULL ) {
				sprintf( GOutBuff, "\n**DftTrans: no exit from %d. Going to random state.\n",
							CurState->Id );
				WriteStd( GOutBuff );
				CurState = States + URandN( NmStates-1 );
				return( OK );
			}
		}

		else {	/* DftRand is on and there are > 1 setting: select effector setting at random */
#if CTEST
			if ( DemoLev > 0 ) {
				sprintf( GOutBuff, "\nPk EffSet at random, from state %d's %d settings",
					 CurState->Id, CurState->NmEffTr );
				WriteStd( GOutBuff );
			}
#endif
			dft = URandN( CurState->NmEffTr - 1 );	/* return 0..NmEfTr-1 */
			for ( i=0, tnode = CurState->Transits; i < dft && tnode != NULL; ++i )
				tnode = tnode->NxtTrans;		/* move down list */

			if ( tnode == NULL ) {
				sprintf( GOutBuff, "\n**PkTrans: DftRand => tnode=NULL from %d? Go to rand state.\n",
					CurState->Id );
				WriteStd( GOutBuff );
				OldState = CurState;
				CurState = States + URandN( NmStates-1 );
				return( OK );
			}
#if CTEST
			if ( DemoLev > 0 ) {
				sprintf( GOutBuff, " => #%d, effset %d.", i, tnode->EffSet );
				WriteStd( GOutBuff );
			}
#endif
		}

			/* We've got a tnode, one way or another. Now get a path */

#if CTEST
		if ( DemoLev > 0 ) {
			sprintf( GOutBuff, "\nDftTrans effset %d.", tnode->EffSet );
			WriteStd( GOutBuff );
		}
#endif
		ran = URand01();			/* Get ready to pick path for CurState,EfValBin pair */
		for ( pnode = tnode->Paths, cum = 0.0; pnode != NULL; pnode = pnode->NxtPath )
			if ( (cum += pnode->PathProb) >= ran )
				break;
		if ( pnode == NULL ) {
			sprintf( GOutBuff, "\n**DftTrans: path null! cur %d, eff %d, cum %f, ran %f. Going to Random State.",
				CurState->Id, tnode->EffSet, cum, ran );
			WriteStd( GOutBuff );
			CurState = States + URandN( NmStates-1 );
		}
		else {
			CurState = pnode->PathDest;
#if CTEST
			if ( DemoLev > 0 ) {
				sprintf( GOutBuff, "  Dest %d (ran %f, cum %f)\n", pnode->PathDest->Id, ran, cum );
				WriteStd( GOutBuff );
			}
#endif
		}
	} /* BehavStp was true */

	return( OK );

} /* DftTrans */



/*****************************

EQTrans 		See if messages is consistent with path actually made.

	EMsg		Null terminated ascii form of message to compare.

	RETURN		TRUE if the message is consistant with actual path.
				FALSE otherwise.

******/

int EQTrans ( EMsg )
	char	EMsg[];
{
	register int	s, i;
	char			msg[EFFATTSZ+1];

	/*	sprintf( GOutBuff, "\ninto EQTrans. StrAttSz %d, EffAttSz %d ", StrAttSz, EffAttSz );
		WriteStd( GOutBuff );
	*/

	for ( s = 0; s < StrAttSz; ++s )	/* Extract values from EMsg Loci */
		EfSetAsc [s] = EMsg[ EffLoci[s] ];

#if CTEST
	if ( DemoLev > 0 ) { 
		EfSetAsc[s] = '\0';
		sprintf( GOutBuff, "\nEQTrans: EMsg '%s', EfSetAsc '%s'", EMsg, EfSetAsc );
		WriteStd( GOutBuff );
	}
#endif

	for ( s = i = 0; s < StrAttSz; s += 2, ++i )
		if	 ( EfSetAsc[s] == '1' )
			msg[i] = '#';
		else if ( EfSetAsc[s+1] == '1' )
			msg[i] = '1';
		else
			msg[i] = '0';

	msg[EffAttSz] = '\0';

	if ( IsMemSym( EfValBin, msg ) )
		return( TRUE );

	return( FALSE );

} /* EQTrans */



/*****************************

DoBkgBeh	This just does initialization for next step.

It also does some counting of what happened when we left OldState:
 OldState->NmBest is incremented if CurState is BestNxt from old state.
 OldState->NmNeg is incremented if CurState has a negative reward.

******/

VOID DoBkgBeh ( )
{
	register int i;

	CurState->VisitCnt += 1;

	if ( OldState->BestNxt == CurState->Id )
		OldState->NmBest += 1;
	else if ( SysRew < (float) 0.0 )
		OldState->NmNeg += 1;

	TotSup = 0.0;

	for ( i = 0; i <= EffValMx; ++i )
		EfValSup[i] = 0;

	if ( FSWNmCfs > 0 ) {
		for ( i = 0; i < FSWNmCfs; ++i )
			if ( FSWCfs[i]->Strength >= FSWCSBds[i] && !FSWCBdWr[i] ) {
				sprintf( GOutBuff, "\n==(%d)=> Cf %d strength %6.1f >= bound %6.1f.\n",
					CycleStp, FSWCfs[i]->Cf_Id, FSWCfs[i]->Strength, FSWCSBds[i] );
				WriteStd( GOutBuff );
				FSWCBdWr[i] = TRUE;
			}
	}

} /* DoBkgBeh */



/*****************************

GtSysRew	Get reward from environment, if any to be got.
			Set global variable SysRew to that value ( to 0 if none ).

	Just look in the Payoffs table.

NOTE:	This also sets the MadeMstk variable (CORE.DEF) to TRUE if reward
		is less than zero.	It may be used as trigger to generate a new
		classi}er to cover situation that led to negative reward.
		MadeMstk is also set TRUE when default action is taken---see
		DftTrans() in this file.

		MadeMstk is set FALSE in GenBehav(), in CORE.C of CFS-C.

******/
	
VOID GtSysRew ( )
{
	extern unsigned int CycleStp;

	if ( CycleStp % EffectRt != 0 ) {	 /* Get reward only when Generate behavior, every EffectRt steps */
		SysRew = 0.0;
		return;
	}

	SysRew = CurState->Payoff;
	
	FSWTSR += SysRew;
	
	if ( SysRew < (float) 0.0 )
		MadeMstk = TRUE;

} /* GtSysRew */



/****************************

MakeActS		Make new action string for the LETSEQ domain.

	NewAct		Char buffer (STRNGSZ+1) into which action string is to be placed.

Currently: Just generate a random string as GENRANDCFS does, and {
overlay the leftmost 2 loci with '01' to make sure its an action. 

******/

VOID MakeActS ( NewAct )
	char	NewAct[];
{
	unsigned int 		ri;
	extern unsigned int DscDemo;

	GnRndCA( NewAct, &ri, 'c' );		/* generate random string */
	NewAct[0] = '1';					/* make it into an action */
	NewAct[1] = '0';

	NewAct[STRNGSZ] = '\0';

#if CTEST
	if ( DscDemo > 1 ) {
		sprintf( GOutBuff, "  |- MakeActS: ctype %d, action '%s'\n  ", ri, NewAct );
		WriteStd( GOutBuff );
	}
#endif

} /* end MakeActS */
