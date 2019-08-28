/*   "Concept" counter for CFS-C log files for LetseqC environment.

Start of run looks like:

  di  env,2  cl,22
  < x818-tst.cm1
  set adenvint=100  adenvfmt=2  adcfint=1000  adcffmt=22  dscflst=a

where tst.cm1 file looks like:

  ; TEST
  set dscflst=0
  di cl,2 cl,22
  set clamp=1 adcfint=1 dscflst=b adcffmt=22
  di env,1 env,2
  step
  di env,1 env,2
  step
  di env,1 env,2
  ... and so on for as much of sequence as is of interest
  ; ENDTEST

Run should continue, displaying full classifier list periodically,
and then going into the TEST...ENDTEST region.

The classifier list to be examined looks like:

Current Classifiers (cycle-step 1):

 180:  G:   [2] 1101###,  G: p [02] 0#0#001   //  Y:   [2] 0100010 {4198,0.81,0.0}
 153: XY:   [1] 001001#,  D:   [3] 11110#0	// XY:   [3] 0110011 {4196,0.88,0.0}
 105:  Y: e,i [02] 1#00001, GY: w  [1] 101#111 //  G:   [2] 0100100 {4194,0.53,0.0}

*/

#include  <stdio.h>
#include  <math.h>
#include <ctype.h>
#if  LATTICEC
#include  <DOS.H>
#endif

#include  "compiler.h"
#include  "utility.h"
#include  "cfsio.ext"
unsigned int   URndSd;   /* we need this to include UTILITY.C at link time. */
short int EchoFlg;

#ifdef THIS			  /* inlcude if linked to cfsutil  */
#include "core.ext"
unsigned int   NmCfs; 
struct CfNode *HiStrCf, *StrSrtCf;
float		  TotCfStr, AveCfStr, TPBiasStr, TRBiasStr;   /* Define to resolve externs in */
#endif

#define	DISSZ	   100	   /* number of times things displayed in a run */

#define	CONSZ		256	  /* max len of classifier */
#if ( CBELLMTS )
#define	CONLSTSZ	1024	  /* maximum number counted  */
#else
#define	CONLSTSZ	 400
#endif
#define	STRNGSZ	  16	   /* cf string size */

#define   STATESZ	   10	   /* number of states that can be examined in TEST */
#define   TOTSZ		 50	   /* number of test displays that can be collected */

struct  ConNode
{   char			 *Concept;
	int			  Cnt;
	float			Str;
	float			BidRatio;
	float			HiStr;
	float			LowStr;
	struct ConNode  *Nxt;
}
	Concepts [ CONLSTSZ ],
   *ConSort;

int		 ConCnt;   /* concept count */
int		 IntCnt;   /* instance count */
float	   ConStr;
float	   ConBR;

short int   EndOnly = FALSE;
short int   CntSort = FALSE;
short int   StrSort = FALSE;
short int   SumOnly = FALSE;	/* print counts only--not all concepts */
short int   CalcC   = FALSE;	/* calc george's C value */
short int   CalcE   = FALSE;	/* calc entropy */
float	   CutFrac = 1.0;	  /* stop display if get to this fraction of total cnt or str */

int   LociZero [STRNGSZ*3];	  /* store count of number of cf's with 0 at each loci */
int   LociOne  [STRNGSZ*3];
int   LociHash [STRNGSZ*3];

unsigned int  CycleStp;
short	int  TestReg = 0;
float		 TotCfStr = 0.0;
unsigned int  TotNmCf = 0;

char   inline[200];
FILE  *InFptr;
int	numfiles = 0;	/* number files supposed to read */
int	NumFilC  = 0;	/* number of files actually read */

unsigned int TSteps[DISSZ];
unsigned int TCons[DISSZ];
unsigned int TIndiv[DISSZ];
float		TBR[DISSZ];
float		TAveS[DISSZ];
float		TCSTS[DISSZ];
float		TC[DISSZ];
float		TE[DISSZ];
int		  DisCount = 0;

int		  CurLet;

unsigned int NmStates = 0;  /* use to count within a TEST region */
unsigned int States[STATESZ];
unsigned int StatesNC[STATESZ];  /* Number of concepts active for state */
unsigned int StatesNI[STATESZ];  /* Number of individuals active for state */
float		StatesStr[STATESZ]; /* total strength of classifiers active for state */
unsigned int FConCnt = 0;	   /* store from non-TEST region */
float		FEntropy = 0;	  /* store from non-TEST region */
int		  PrntSH = FALSE;
FILE		   *SumFPtr = NULL;	 
int		  TStepLst[TOTSZ];			   /* list of steps at which tots collected */
int		  TStepNxt = 0;				  /* next place for it */
unsigned int TConcepts[TOTSZ];
float		TEntropy[TOTSZ];
unsigned int TStateNC[STATESZ][TOTSZ];
unsigned int TStateNI[STATESZ][TOTSZ];
float		TStatesStr[STATESZ][TOTSZ];
float		TTotCfStr[STATESZ][TOTSZ];

VOID   Converge(), Getstrng(), ReadCon(), PrntCon(), Help(),
	   PrntTSta(), PrntTSum();
char  *GetInt(), *GetFloat(), *StpTok2();
int	strcmpn();

main ( argc, argv )
	int	argc;
	char  *argv[];
{
	int			 ret, len, err, i, filenum, idflg;
	unsigned int	id, sptr;
	char		   *idptr, *cptr, path[64], basefnam[64], fname[64], buff[64];
	char			sumfname[64], *malloc();

	idflg = FALSE;

	for ( i = 0; i < CONLSTSZ; ++i )
	{   if ( (Concepts[i].Concept = malloc ( CONSZ )) == NULL )
		{   printf( "\nERR, NULL from malloc for space for Concepts[%d].Concept!.", i );
			exit( ERROR );
		}
	}

	for ( i = 0; i < DISSZ; ++i )
	{	TCons[i] = TIndiv[i] = TSteps[i] = 0;
		TBR[i] = TAveS[i] = TCSTS[i] = TC[i] = TE[i] = (float) 0.0;
	}

	for ( i = 0; i < TOTSZ; ++i ) {
		TStepLst[i] = -1;
		TConcepts[i] = 0;
		TEntropy[i] = 0;
		for ( sptr = 0; sptr < STATESZ; ++sptr ) {
			TStateNC[sptr][i] = 0;
			TStateNI[sptr][i] = 0;
			TStatesStr[sptr][i] = 0;
			TTotCfStr[sptr][i] = 0;
		}
	}

	if ( argc == 1 ) {
		Help();
		exit();
	}

	for ( i = 2; i < argc; ++i ) {
		if ( strcmpn( argv[i], "s", 1 ) == 0 ) {
			StrSort = TRUE;
			CntSort = FALSE;
			cptr = argv[i];
			++cptr;
			GetInt( cptr, &ret, 100, " ", &err );
			if ( ret < 0 || ret > 100 )
				printf( "\nFraction must be 1..100%; using 100.\n" );
			else
				CutFrac = 1.0 * ret / 100.0;
		}

		else if ( strcmpn( argv[i], "n", 1 ) == 0 ) {
			CntSort = TRUE;
			StrSort = FALSE;
			cptr = argv[i];
			++cptr;
			GetInt( cptr, &ret, 100, " ", &err );
			if ( ret < 0 || ret > 100 )
				printf( "\nFraction must be 1..100%; using 100.\n" );
			else
				CutFrac = 1.0 * ret / 100.0;
		}

		else if ( strcmpn( argv[i], "c", 1 ) == 0 )
			CalcC = TRUE;

		else if ( strcmpn( argv[i], "e", 1 ) == 0 )
			CalcE = TRUE;

		else if ( strcmpn( argv[i], "os", 2 ) == 0 )
			SumOnly = TRUE;

		else if ( strcmpn( argv[i], "oe", 1 ) == 0 )
			EndOnly = TRUE;

#ifdef THIS
		else if ( *argv[i] == 'i' ) {
			idptr = argv[i];
			++idptr;
			idflg = TRUE;
		}
#endif

		else {
			printf("\nIllegal parameter '%s', ignored.", argv[i] );
			printf("\nFor help, enter  CCLS1\n");
		}
	}
		
		/* lets get the basic filename and perhaps a number of files */
		/* could be  filename  or  filename,3  or some other digit */

	cptr = StpTok2( argv[1], buff, sizeof(buff), " ," );
	if ( *cptr == '\0' ) {
		strcpy( basefnam, buff );
		numfiles = 1;
	} 
	else {
		strcpy( basefnam, buff );
		if ( sscanf( cptr, "%d", &numfiles ) != 1 ) {
			printf("\n\nIllegal first parameter (%s)--enter ccls1 for help.\n\n", argv[1] );
			exit();
		}
		else if ( numfiles < 1 || numfiles > 10 ) {
			printf("\n\nIllegal number of files on first parameter (%s)--enter ccls1 for help.\n\n",
					 argv[1] );
			exit();
		}
	}

		/* get states to look at in TEST region */

	if ( idflg ) {
		for ( i = 0; i < STATESZ; ++i ) {
			idptr = GetInt( idptr, &id, -1, ",", &err );
			if ( err ) {
				printf("\nIllegal State Id in run parameter.\n");
				exit();
			}
			else if ( id == -1 )
				break;
			else {
				States [i] = id;
				++NmStates;
			}
		}

		for ( cptr = basefnam, i = 0; *cptr != '\0' && *cptr != '.' && i < 64; ++cptr, ++i )
			sumfname[i] = *cptr;
		sumfname[i] = '\0';
		strcat( sumfname, ".sm" );
		printf( "\nsumfname '%s'", sumfname );
		if ( (SumFPtr=fopen( sumfname, APPENDMODE )) == NULL )
			printf("\n\nERROR--can't open sum file '%s'.\n\n", sumfname );
	}

		/* From each file, read lines until done */

	for ( filenum = 0; filenum < numfiles; ++filenum ) {
		strcpy( fname, basefnam );				  /* get base name */

		if ( numfiles > 1 ) {					/* more than one file to process */		
			sprintf( buff, "%d", filenum );
			strcat( fname, buff );
		}

		if ( (InFptr=fopen( fname, READMODE )) == NULL ) {
			printf("\n\nERROR--can't open log file '%s': no data collected from it.\n\n", fname );
			if ( numfiles == 1 )  {
				printf("For help, enter ccls1.\n" );
				exit();
			}
			continue;
		}

		if ( filenum > 1 ) 
			printf( "\n" );
		printf( "\nExtracting CFS-C data from from the file " );
#if LATTICEC
		getcwd( path, sizeof(path) );
		printf( "%s\\", path );
#endif
		printf( "%s ...\n", fname );

		PrntSH = FALSE;

		if ( SumOnly ) {
			printf("\nStep	   Cnc  Ind   BR	AveS	CS/TS	 C	   E");
			DisCount = 0;
		}

		if ( EndOnly ) {   /* get only data after ENDRUN comment in log */
			while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF )
			  if ( strcmpn( inline, "; ENDRUN", 8 ) == 0 ||
				   strcmpn( inline, "C-? ; ENDRUN", 12 ) == 0 )
					break;
		}

		while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF ) {
			if ( strcmpn( inline, "; TEST", 6 ) == 0 ||
				 strcmpn( inline, "C-? ; TEST", 10 ) == 0 ) {
				TestReg = 1;
				if ( NmStates != 0 )
					for ( sptr = 0; sptr < STATESZ; ++sptr ) {
						StatesNC[sptr] = 0;
						StatesNI[sptr] = 0;
						StatesStr[sptr] = 0;
					}
				}

			else if ( strcmpn( inline, "; ENDTEST", 9 ) == 0 ||
				 strcmpn( inline, "C-? ; ENDTEST", 13 ) == 0 ) {
				TestReg = 0;
				if ( !PrntSH && NmStates != 0 ) {
					fprintf( SumFPtr, "Summary counts for TEST states in %s.\n", fname );
					fprintf( SumFPtr, "\n			  " );
					for ( sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr )
						fprintf( SumFPtr, " |	 %2u	  ", States[sptr] );
					PrntSH = TRUE;
				}
				if ( NmStates != 0 && SumFPtr != NULL ) {
					fprintf( SumFPtr, "\n%4d %3d %5.3f", CycleStp, FConCnt, FEntropy );
					for ( sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr ) {
						fprintf( SumFPtr, " | %3u %3u", StatesNI[sptr], StatesNC[sptr] );
						if ( TotCfStr != 0 ) 
							fprintf(SumFPtr, " %4.2f", StatesStr[sptr]/TotCfStr );
						else
							fprintf( SumFPtr, "	 " );
					}
						/* store full-list FConCnt and FEntropy */
					id = IntInLst( CycleStp, TStepLst, TStepNxt);
					if ( id < 0 || id >= TStepNxt || id > TOTSZ ) 
						printf("\nid out of range at FConCnt...\n" );
					else {
						TConcepts[id] += FConCnt;
						TEntropy[id]  += FEntropy;
					}
				}
			}

				/* in TEST region, keep CurLet up to date... */

			else if ( TestReg && strcmpn( inline, "Guess", 5 ) == 0 ) {
				sscanf( inline, "Guess is '%c', letter is '%c'.", &CurLet, &CurLet );
			}

			else if ( strcmpn( inline, "Current Cl", 10 ) == 0 ) {
				sscanf( inline, "Current Classifiers (cycle-step %d) %s", &CycleStp, buff );
				ReadS( inline, sizeof(inline), InFptr, &len);  /* blank line */
				ReadS( inline, sizeof(inline), InFptr, &len);  /* first classifier or table title */  

				if ( strcmpn( inline, "Id", 2 ) == 0 )
					continue;

				else  {
					ReadCon( );

					PrntCon( );
				}
			}
		}  /* more lines in a file */

		if ( fclose( InFptr ) != 0 ) 
			printf("\n\nERR: couldn't close log file '%s'!!\n", fname );

		if ( SumFPtr != NULL ) 
			fprintf( SumFPtr, "\n\n" );

		++NumFilC;

	}  /* The individual files */

	if ( NumFilC > 1 ) {
		if ( idflg )
			PrntTSta();
		if ( SumOnly ) {
			for ( i = 0; i < DisCount; ++i ) {
				TCons[i] /= NumFilC;
				TIndiv[i] /= NumFilC;
				TBR[i] /= NumFilC;
				TAveS[i] /= NumFilC;
				TCSTS[i] /= NumFilC;
				TC[i] /= NumFilC;
				TE[i] /= NumFilC;
			}
			PrntTSum();
		}
	}
 
} /* main */

VOID  ReadCon ( )	   
{
	int			 ret, len, err, i, overflow;
	float		   str, br;
	char		   *cp,  con[CONSZ+1], *CfString;

	TotCfStr = TotNmCf = 0;

	if ( CalcC || CalcE )
		for ( i = 0; i < STRNGSZ*3; ++i ) {
			LociZero[i] = 0;
			LociOne[i]  = 0;
			LociHash[i] = 0;
		}

	for ( i = 0; i < CONLSTSZ; ++i ) {
		Concepts [ i ] . Cnt		 = 0;
		Concepts [ i ] . Str		 = 0;
		Concepts [ i ] . BidRatio	= 0;
		Concepts [ i ] . Concept [0] = '\0';
		Concepts [ i ] . Nxt		 = NULL;
	}
	ConCnt  = IntCnt = 0;
	ConStr  = ConBR  = 0;
	ConSort = NULL;
	overflow = FALSE;

		/* NB--> inline has }rst cf in it on entry. Format is:

 180:  G:   [2] 1101###,  G: p [02] 0#0#001   //  Y:   [2] 0100010 {4198,0.81,0.0}

		*/
	do {
		for ( cp = inline; *cp == ' ' && *cp != '\0'; ++cp )  ; /* check for blank line */
		if ( *cp == '\0' )
			break;											  /* quit if it was all blanks */

		for ( cp = inline; *cp != '>' && *cp != '\0'; ++cp )  ; /* skip id */
		++cp;
		for ( ; *cp == ' ' && *cp != '\0'; ++cp )  ; /* skip blanks */
		for ( i = 0; *cp != '{' && *cp != '\0' && i < CONSZ; ++cp, ++i )  
			con[i] = *cp;
		con[i] = '\0';
		++cp;
		cp = GetFloat ( cp, &str, (float) -1.0, ",", &err );
		cp = GetFloat ( cp, &br, (float) -1.0, ",}", &err );

		TotCfStr += str;
		++TotNmCf;

		for ( ; *cp != 'm' && *cp != '1' && *cp != '#' && *cp != '\0'; ++cp )  
			 ;	/* get to uninterpreted classifier, if there */
		CfString = cp;	  /* leave pointer for Converge() */
#ifdef THIS
		for (  ; *cp != '\0' && i < CONSZ; ++cp, ++i )
			con[i] = *cp;   /* append to concept */
		con[i] = '\0';
#endif
		
		for ( i = 0; i < ConCnt; ++i ) 
			if ( strcmp( Concepts [ i ] . Concept, con ) == 0 )
				break;
			  
		if ( i == ConCnt ) {  /* its new--init and increment ConCnt */
			if ( ConCnt == CONLSTSZ ) {
				if ( !overflow )  {
					printf( "\n**WARNING: ConCnt == CONLSTSZ: some concepts not counted.\n ");
					overflow = TRUE;
				}
			}
			else {
				strcpy ( Concepts [ ConCnt ] . Concept, con );
				Concepts [ ConCnt ] . Cnt	   = 1;
				Concepts [ ConCnt ] . Str	   = str;
				Concepts [ ConCnt ] . BidRatio  = br;
				Concepts [ ConCnt ] . HiStr	 = str;
				Concepts [ ConCnt ] . LowStr	 = str;
				++ConCnt;
			}
		}
		else {	/* its old--increment concept totals  */
			Concepts [ i ] . Cnt	   += 1;
			Concepts [ i ] . Str	   += str;
			Concepts [ i ] . BidRatio  += br;
			if ( str > Concepts [ i ] . HiStr )
				Concepts [ i ] . HiStr = str;
			if ( str < Concepts [ i ] . LowStr )
				Concepts [ i ] . LowStr = str;
		}

		ConStr += str;	  /* in any case increase global totals */
		ConBR  += br;
		IntCnt += 1;

		if ( CalcC || CalcE )
			Converge ( CfString );

	} while ( (ret = ReadS( inline, sizeof(inline), InFptr, &len)) != EOF 
				&& len != 0 );

		/* In TEST region, get full list strength at end of list */

	if ( TestReg ) {
		ret = ReadS( inline, sizeof(inline), InFptr, &len);
		sscanf( inline, "Number of Classifiers: %d. Ave. strength %f (total %f).",
			&TotNmCf, &str, &TotCfStr );
	}

}  /* ReadCon  */

/*  
Converge: on entry Cf points to classifier
  (uninterpreted---with only trits from conditions and action).
*/

VOID  Converge ( Cf )
	char *Cf;
{
	int loc;

	for ( loc = 0; *Cf != '\0' && loc < 3 * STRNGSZ; ++loc, ++Cf )
		if ( *Cf == '0' )
			LociZero[loc] += 1;
		else if ( *Cf == '1' )
			LociOne[loc] += 1;
		else
			LociHash[loc] += 1;

}  /* Converge  */


VOID  PrntCon ( )
{
	int	 i, itot, loc, cntres, ires, sptr, tptr;
	float   strtot, brtot, C, ci, f, f1, strres, brres, hires, lores;
	struct  ConNode *cnp, *tcnp;

	C = 0;

		/*  Sort, starting at bottom of array
			(most likely lowest S and count since cflist display hi S to low).
			Remember the concepts are stored in Concepts[0..CntCnt-1].
		*/

	if ( !SumOnly ) {
		printf( "\n-------------  Step %5d  ------------------", CycleStp );
		if ( TestReg )
			printf("  CurLet %c", CurLet );
	}
	else {
		if ( !TestReg )
			printf( "\n%5d	", CycleStp );
		else 
			printf("\n%5d (%c)", CycleStp, CurLet );
	}

	if ( ConCnt == 1 )
		ConSort = &Concepts [ ConCnt - 1 ];  /* get the only one! */

	else if ( ConCnt > 1 ) {
		ConSort = &Concepts [ ConCnt - 1 ];  /* get the first one from bottom */
		ConSort->Nxt = NULL;
		i = ConCnt - 2;					  /* get ready for next up */

		if ( StrSort ) {
			for ( cnp = &Concepts [ i ]; i >= 0; --cnp, --i ) {
				if ( (cnp->Str/cnp->Cnt) >= (ConSort->Str/cnp->Cnt) ) {  
					cnp->Nxt = ConSort;		 /* its first now */
					ConSort  = cnp;
				}
				else {
						/* move tcnp until cnp goes right after it */
					for ( tcnp = ConSort; tcnp->Nxt != NULL; tcnp = tcnp->Nxt )
						if ( (cnp->Str/cnp->Cnt) >= (tcnp->Nxt->Str/cnp->Cnt) )
							break;
					cnp->Nxt  = tcnp->Nxt;	  /* insert after tcnp */
					tcnp->Nxt = cnp;
				}
			}
		}
		else  {
			for ( cnp = &Concepts [ i ]; i >= 0; --cnp, --i ) {
				if ( cnp->Cnt >= ConSort->Cnt ) {  
					cnp->Nxt = ConSort;		 /* its first now */
					ConSort  = cnp;
				}
				else {
						/* move tcnp until cnp goes right after it */
					for ( tcnp = ConSort; tcnp->Nxt != NULL; tcnp = tcnp->Nxt )
						if ( cnp->Cnt >= tcnp->Nxt->Cnt )
							break;
					cnp->Nxt  = tcnp->Nxt;	  /* insert after tcnp */
					tcnp->Nxt = cnp;
				}
			}
		}
	}
	
	if ( ConStr > (float) 0.0 && ConCnt > 0 && (StrSort || CntSort) && !SumOnly )
		printf( "\n Cnt   Str/Tot  Br  Concept\n" );  

	if ( ConStr == (float) 0.0 && !SumOnly )
		 printf( "\nTotal Concept Str 0 at step %u!\n", CycleStp );

	else if ( ConCnt == 0 && !SumOnly )
		printf( "\nNo Concepts at step %u!\n", CycleStp );

	else {
		strtot = brtot = strres = brres = 0;
		itot = cntres = ires = 0;
		for ( cnp = ConSort, i = 0; cnp != NULL && i < ConCnt; ++i, cnp = cnp->Nxt ) {
			if ( ( i <= 10  && (StrSort || CntSort) )	||
				 ( CntSort && itot <= CutFrac * IntCnt ) ||
				 ( StrSort && strtot <= CutFrac * ConStr ) ) {
				if ( !SumOnly ) 
					printf( "%4d  %7.3f  %4.2f  %s (%4.0f,%4.0f,%4.0f)\n", cnp->Cnt,
						(cnp->Str/ConStr), (cnp->BidRatio/cnp->Cnt), cnp->Concept,
						cnp->HiStr, (cnp->Str/cnp->Cnt), cnp->LowStr );
			}
			else {
				strres += cnp->Str;		/* residuals not above cutoff */
				brres  += cnp->BidRatio;
				ires   += cnp->Cnt;
				cntres += 1;
				if ( hires > cnp->HiStr )	hires = cnp->HiStr; 
				if ( lores < cnp->LowStr )	lores = cnp->LowStr; 
			}
			strtot += cnp->Str;
			brtot  += cnp->BidRatio;
			itot += cnp->Cnt;
		}
		if ( cntres != 0 && ( StrSort || CntSort ) && !SumOnly ) 
			printf( "%4d  %7.3f  %4.2f  (in %d residuals) (%4.0f,%4.0f,%4.0f)\n", ires, 
					(strres/ConStr), (brres/ires), cntres,
					 hires, (strres/ires), lores );

		if ( TotCfStr != (float) 0.0 && TotNmCf == 0 ) {
			if ( !SumOnly ) {
				printf( "\n%4d %4.2f  %4.2f   (totals/averages for %d concepts)",
					 IntCnt, strtot/TotCfStr, (brtot/IntCnt), ConCnt ); 
				printf( "\n				  (ConStr %.0f (%.0f), ListStr %.0f (**)\n\n",
					strtot, strtot/IntCnt, TotCfStr );
			}
			else {
				if ( IntCnt != 0 ) {
					printf( "  %3d  %3d  %4.2f  %5.0f  %5.3f", ConCnt, IntCnt,
						 (brtot/IntCnt), strtot/IntCnt, strtot/TotCfStr );
					if ( !TestReg ) {
						TBR[DisCount] += (brtot/IntCnt);
						TAveS[DisCount] += strtot/IntCnt;
						TCSTS[DisCount] += strtot/TotCfStr;
					}
				}
				else 
					printf( "  %3d  %3d   ****   ***  %5.3f", ConCnt, IntCnt,  strtot/TotCfStr );
				if ( !TestReg ) {
					TCons[DisCount] += ConCnt;
					TIndiv[DisCount] += IntCnt;
				}
			}
		}
		else if ( TotCfStr != 0 && TotNmCf != 0 ) {
			if ( !SumOnly ) {
				printf( "\n%4d %4.2f  %4.2f   (totals/averages for %d concepts)",
					IntCnt, strtot/TotCfStr, (brtot/IntCnt), ConCnt );
				printf( "\n				  (ConStr %.0f (%.0f), ListStr %.0f (%.0f)\n\n",
					strtot, strtot/IntCnt, TotCfStr, TotCfStr/TotNmCf );
			}
			else {
				if ( IntCnt != 0 ) {
					printf( "  %3d  %3d  %4.2f  %5.0f	%5.3f", ConCnt, IntCnt,
						 (brtot/IntCnt), strtot/IntCnt, strtot/TotCfStr );
					if ( !TestReg ) {
						TBR[DisCount] += (brtot/IntCnt);
						TAveS[DisCount] += strtot/IntCnt;
						TCSTS[DisCount] += strtot/TotCfStr;
					}
				}
				else 
					printf( "  %3d  %3d  ****	***	 %5.3f", ConCnt, IntCnt,  strtot/TotCfStr );
				if ( !TestReg ) {
					TCons[DisCount] += ConCnt;
					TIndiv[DisCount] += IntCnt;
				}
			}
		}
		else {
			if ( !SumOnly ) {
				printf( "\n%4d  *S*   %4.2f   (totals/averages for %d concepts)",
					 IntCnt, (brtot/IntCnt), ConCnt );
				printf( "\n				  (ConStr %.0f (**), ListStr %.0f (**)\n\n",
					 strtot, TotCfStr );
			}
			else {
				printf( "  %3d  %3d							 ", ConCnt, IntCnt );
				if ( !TestReg ) {
					TCons[DisCount] += ConCnt;
					TIndiv[DisCount] += IntCnt;
				}
			}
		}

		if ( NmStates != 0 && TestReg && (sptr = IntInLst( CurLet, States, NmStates )) >= 0 ) {
			if ( sptr < STATESZ ) {
				StatesNC[sptr] += ConCnt;
				StatesNI[sptr] += IntCnt;
				StatesStr[sptr] += strtot;
					  /* save in totals array */
				tptr = IntInLst( CycleStp, TStepLst, TStepNxt);
				if ( tptr >= TOTSZ )
					printf( "\ntptr > TOTSZ?\n" );
				else {
					if ( tptr < 0 ) {	/* its a new cyclestep */
						if ( TStepNxt >= TOTSZ ) {
							printf( "\nToo many CycleSteps for Totals.\n" );
							tptr = TOTSZ-1;
						}
						else {
							tptr = TStepNxt++;		  /* so use next spot */
							TStepLst[tptr] = CycleStp;  /* store step there */
						}
					}
					TStateNC[sptr][tptr] += ConCnt;	/* add on the totals */
					TStateNI[sptr][tptr] += IntCnt;
					TStatesStr[sptr][tptr] += strtot;
					TTotCfStr[sptr][tptr] += TotCfStr;
				}
			}
		} 
	}

	if ( CalcC && IntCnt != 0 )  {
		if ( !SumOnly )
			printf( "\nConvergence C  (%d indivuals): ", IntCnt );

		for ( C = 0, loc = 0; loc < STRNGSZ*3; ++loc ) {
			i = (2 * LociZero[loc]) - IntCnt;
			if ( i < 0 )			/* *** Why does abs fail here in microsoft C? */
				i = 0 - i;
			ci = 1.0 * i / IntCnt; 
			C += ci;
		}

		if ( !SumOnly )
			printf( " %5.3f (step %u).\n", C / (3 * STRNGSZ), CycleStp );
		else {
			printf( "   %5.3f", C / (3 * STRNGSZ) );
			if ( !TestReg )
				TC[DisCount] += C / (3 * STRNGSZ);
		}

	}

	if ( CalcE && IntCnt != 0 )  {
		if ( !SumOnly )
			printf( "'Entropy' E	(%d indivuals): ", IntCnt );

		for ( C = 0, loc = 0; loc < STRNGSZ*3; ++loc ) {
			ci = 0;
			if ( LociZero[loc] != 0 )  {
				f = LociZero[loc] * 1.0 / IntCnt;
				ci =  f * log ( f ); 
			}
			if ( LociOne[loc] != 0 )  {
				f = LociOne[loc] * 1.0 / IntCnt;
				ci +=  f * log ( f ); 
			}
			if ( LociHash[loc] != 0 )  {
				f = LociHash[loc] * 1.0 / IntCnt;
				ci +=  f * log ( f ); 
			}

			f = -1.0 * ci / log ( 3.0 ); 
			C += f;
		}

		if ( !SumOnly )
			printf( " %5.3f (step %u).\n", C / (3 * STRNGSZ), CycleStp );
		else {
			printf( "   %5.3f", C / (3 * STRNGSZ) );
			if ( !TestReg )
				TE[DisCount] += C / (3 * STRNGSZ);
		}
	}

	if ( NmStates != 0 && !TestReg ) {  /* save for possible use in sum table */
		FConCnt = ConCnt;
		FEntropy = C / (3 * STRNGSZ);
	}

	if ( !TestReg ) {
		if ( NumFilC == 0 ) 
			TSteps[DisCount] = CycleStp;
		++DisCount;
	}

} /* PrntCon  */

VOID  PrntTSta ( )
{
	unsigned int sptr, tptr;

	fprintf( SumFPtr, "\nTotals for %d files", NumFilC  );
	fprintf( SumFPtr, "\n			  " );
	for ( sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr )
		fprintf( SumFPtr, " |	 %2u	  ", States[sptr] );

	if ( NmStates != 0 && SumFPtr != NULL && NumFilC > 0 ) {
		for ( tptr = 0; tptr < TStepNxt && tptr < TOTSZ; ++tptr ) {
			TConcepts[tptr] /= NumFilC;
			TEntropy[tptr] /= NumFilC;
			fprintf( SumFPtr, "\n%4d %3d %5.3f", TStepLst[tptr], TConcepts[tptr], TEntropy[tptr] );
			for ( sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr ) {
				TStateNI[sptr][tptr] /= NumFilC;
				TStateNC[sptr][tptr] /= NumFilC;
				TStatesStr[sptr][tptr] /= NumFilC;
				TTotCfStr[sptr][tptr] /= NumFilC;
				fprintf( SumFPtr, " | %3u %3u", TStateNI[sptr][tptr], TStateNC[sptr][tptr] );
				if ( TTotCfStr[sptr][tptr] != 0 ) 
					fprintf(SumFPtr, " %4.2f", TStatesStr[sptr][tptr]/TTotCfStr[sptr][tptr] );
				else
					fprintf( SumFPtr, "	 " );
			}
		}
	}

} /* PrntTSta */



VOID PrntTSum()
{
	int i;

	printf( "\n\nTotals for %d runs:", NumFilC );
	printf("\nStep	   Cnc  Ind   BR	AveS	CS/TS	 C	   E");
	for ( i = 0; i < DisCount; ++i ) {
		printf( "\n%5d	   %3d  %3d  %4.2f  %5.0f	%5.3f  %5.3f  %5.3f",
			TSteps[i], TCons[i], TIndiv[i], TBR[i], TAveS[i], TCSTS[i],
			TC[i], TE[i] );
	}

}  /* PrntTSum */


VOID  Help ( )
{


printf("\nCCLS1 reads log files produced by the CFS-C/Letseq1 system, in which");
printf("\nclassifiers have been displayed one or more times, and produces");
printf("\nas output a compressed form of each display of the classifier list.");
printf("\n");
printf("\nThe display is similar to the 'Macro-State' description of classifiers");
printf("\nas discussed by Stewart Wilson in his article 'Classifier Systems and");
printf("\nthe Animat Problem', Machine Learning 2, p199-228 (1987).");
printf("\n");
printf("\nUsage:");
printf("\n");
printf("\nccls1 x001.lg0  s90  c  e");
printf("\n  Reads from the file x001.lg0");
printf("\n  Counts as separate 'concepts' each classifier 'type' (i.e.,");
printf("\n  genotypically alike) for those that have the top 90% (the 's90') of the");
printf("\n  total strength). For each concept, displays number of classifiers,");
printf("\n  total strength, strength as percent of list, etc.");
printf("\n  'c' means for each display, calculated a 'convergence' measure");
printf("\n  (0 = random, 1 = converged).");
printf("\n  'e' means for each display, calculate an 'entropy' measure (1=random,0=converged).");
printf("\n  NOTE: To use c or e options, display classifiers in format 23." );

printf("\n[press any key to continue...]");
getch();
printf("\nccls1  x001.lg,3  s100 e");
printf("\n  As above, but collects information from files x001.lg0, x001.lg1, and x001.lg2.");
printf("\n  Also, doesn't calculate convergences, and collects concepts up to 100% of");
printf("\n  total stregth (i.e., all of them).");
printf("\n");
printf("\nAs an example, a portion of a Letseq1 run could look like this:");
printf("\n  di env,1 env,2  cl,22");
printf("\n  ; TEST");
printf("\n  set dscflst=b adcfint=1 adcffmt=22");
printf("\n  di env,1");
printf("\n  step");
printf("\n  di env,1");
printf("\n  step");
printf("\n	... and so on for a appropriate number of steps...");
printf("\n  set  dscflst=0 adcfint=1000");
printf("\n  ; ENDTEST");
printf("\nRun should continue, displaying full classifier list periodically,");
printf("\nand then going into the next TEST...ENDTEST region.");
printf("\n");
printf("\nNote that TEST commands can be put in another file and the CFS-C '<' command");
printf("\nused to read them in periodically.");
printf("\n");

printf("\n[press any key to continue...]");
getch();
printf("\nSome limits (which could be compiled into larger values:");
printf("\nBasic string-size of messages/classifiers: %d", STRNGSZ );
printf("\nSize of (displayed) classifier: %d", CONSZ );
printf("\nNumber of concepts in CC file: %d", CONLSTSZ );
printf("\nNumber of display points per run: %d", TOTSZ );

} /* Help */
/**/
/*
strcmpn	 Compare leftmost N characters of two strings.

	Str1	Strings to compare (possibly NULL terminated).
	Str2
	N	   Number of characters to compare.

	Return: If leftmost N characters are the same, return 0
			else  return != 0 .
*/

int  strcmpn ( Str1, Str2, N )
	char  Str1[], Str2[];
	int   N;
{
	int  ret, i;
	
	for ( ret = 1, i = 0; Str1[i] == Str2[i] && i < N; ++i )
		;

	if ( i == N ) 
		ret = 0;

	return( ret );

} /* strcmpn */
/**/
/*
IntInLst	Check for int in list (array) of ints.

	Test	Int to look for.

	List	Array of int's.

	Max	 Last entry to check.

	Return  N   if Test is N-th entry in List[0...Max].
			-1  otherwise
*/

int  IntInLst ( Test, List, Max )
	int  Test, List[], Max;
{
	int  ret, i;

	for ( i = 0; i <= Max; ++i ) 
		if ( Test == List[i] )
			break;

	if ( i <= Max )
		ret = i;
	else
		ret = -1;

	return( ret );

}  /* IntInLst */

