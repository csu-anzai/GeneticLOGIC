/*   "Concept" counter for CFS-C log files for FSW1 system

See Help() for a description.

Limits to check (depend on machine):
  STRNGSZ   basic cfs-c message length
  CONSZ	 maximum total "concept" length
  CONLSTSZ  maximum number of concepts
  TOTSZ	 maximum number of displays per run

Link to the cfsc files cfsio.obj and utilty.obj for some utilities.

Note: There are some other parameters and "features" which I no longer
 use and will remove as soon as I get some time.
*/

#include "config.h"
#include "utility.h"
#include "cfsio.ext"
#include "concnt-1.h"

unsigned int URndSd;			/* we need this to include UTILITY.C at link time. */
short int EchoFlg;

#define	CONSZ		300		/* max len of classifier */

#if ( CBELLMTS )
#define	CONLSTSZ	1024		/* maximum number counted  */
#else
#define	CONLSTSZ	 400
#endif

#define	STRNGSZ	  16			/* cf string size */

#define   STATESZ	   10		/* number of states that can be examined in TEST */
#define   TOTSZ		 50		/* number of test displays that can be collected */

struct ConNode {
    char *Concept;
    int Cnt;
    float Str;
    float BidRatio;
    float HiStr, LowStr;
    struct ConNode *Nxt;
}

Concepts[CONLSTSZ],
   *ConSort;

int ConCnt;				/* concept count */
int IntCnt;				/* instance count */
float ConStr;
float ConBR;

short int EndRun = FALSE;
short int CntSort = FALSE;
short int StrSort = FALSE;
short int CalcC = FALSE;		/* calc george's C value */
short int CalcE = FALSE;		/* calc entropy */
float CutFrac = 1.0;			/* stop display if get to this fraction of total cnt or str */

int LociZero[STRNGSZ * 3];		/* store count of number of cf's with 0 at each loci */
int LociOne[STRNGSZ * 3];
int LociHash[STRNGSZ * 3];

float CycleStp;
short int TestReg = 0;
float TotCfStr = 0.0;
unsigned int TotNmCf = 0;

char inbuf[512];
FILE *InFptr;
int numfiles = 0;			/* number files supposed to read */
int NumFilC = 0;			/* number of files actually read */

unsigned int CurState;
unsigned int NmStates = 0;		/* use to count within a TEST region */
unsigned int States[STATESZ];
unsigned int StatesNC[STATESZ];		/* Number of concepts active for state */
unsigned int StatesNI[STATESZ];		/* Number of individuals active for state */
float StatesStr[STATESZ];		/* total strength of classifiers active for state */
unsigned int FConCnt = 0;		/* store from non-TEST region */
float FEntropy = 0;			/* store from non-TEST region */
int PrntSH = FALSE;
FILE *SumFPtr = NULL;
float TStepLst[TOTSZ];			/* list of steps at which tots collected */
int TStepNxt = 0;			/* next place for it */
unsigned int TConcepts[TOTSZ];
float TEntropy[TOTSZ];
unsigned int TStateNC[STATESZ][TOTSZ];
unsigned int TStateNI[STATESZ][TOTSZ];
float TStatesStr[STATESZ][TOTSZ];
float TTotCfStr[STATESZ][TOTSZ];

short int BooleFlg = FALSE;		/* set true with b parameter, for Boole-N domains */

VOID Converge (), Getstrng (), ReadCon (), ReadConBoole (), PrntCon (), Help (), PrntTSta ();
char *GetInt (), *GetFloat (), *StpTok2 ();
int strcmpn (), IntInLst (), fInLst ();


/************************

*********/

main (argc, argv)
    int argc;
    char *argv[];
{
    int ret, len, err, i, filenum, idflg;
    unsigned int id, sptr, tstep;
    char *idptr, *cptr, path[64], basefnam[64], fname[64], buff[64];
    char sumfname[64], *malloc ();

    idflg = FALSE;

    for (i = 0; i < CONLSTSZ; ++i) {
	if ((Concepts[i].Concept = malloc (CONSZ)) == NULL) {
	    printf ("\nERR, NULL from malloc for space for Concepts[%d].Concept!.", i);
	    exit (ERROR);
	}
    }

    for (i = 0; i < TOTSZ; ++i) {
	TStepLst[i] = -1;
	TConcepts[i] = 0;
	TEntropy[i] = 0;
	for (sptr = 0; sptr < STATESZ; ++sptr) {
	    TStateNC[sptr][i] = 0;
	    TStateNI[sptr][i] = 0;
	    TStatesStr[sptr][i] = 0;
	    TTotCfStr[sptr][i] = 0;
	}
    }

    if (argc == 1) {
	Help ();
	exit (1);
    }
    for (i = 2; i < argc; ++i) {
	if (strcmpn (argv[i], "er", 2) == 0)
	    EndRun = TRUE;

	else if (strcmpn (argv[i], "s", 1) == 0) {
	    StrSort = TRUE;
	    CntSort = FALSE;
	    cptr = argv[i];
	    ++cptr;
	    GetInt (cptr, &ret, 100, " ", &err);
	    if (ret < 0 || ret > 100)
		printf ("\nFraction must be 1..100%%; using 100.\n");
	    else
		CutFrac = 1.0 * ret / 100.0;
	} else if (strcmpn (argv[i], "n", 1) == 0) {
	    CntSort = TRUE;
	    StrSort = FALSE;
	    cptr = argv[i];
	    ++cptr;
	    GetInt (cptr, &ret, 100, " ", &err);
	    if (ret < 0 || ret > 100)
		printf ("\nFraction must be 1..100%%; using 100.\n");
	    else
		CutFrac = 1.0 * ret / 100.0;
	} else if (strcmpn (argv[i], "c", 1) == 0)
	    CalcC = TRUE;

	else if (strcmpn (argv[i], "e", 1) == 0)
	    CalcE = TRUE;

	else if (strcmpn (argv[i], "b", 1) == 0)
	    BooleFlg = TRUE;

	else if (*argv[i] == 'i') {
	    idptr = argv[i];
	    ++idptr;
	    idflg = TRUE;
	} else {
	    printf ("\nIllegal parameter '%s', ignored.", argv[i]);
	    printf ("\nFor help, enter concnt-1 .\n");
	}
    }

   /* lets get the basic filename and perhaps a number of files */
   /* could be  filename  or  filename,3  or some other digit */

    cptr = StpTok2 (argv[1], buff, sizeof (buff), " ,");
    if (*cptr == '\0') {
	strcpy (basefnam, buff);
	numfiles = 1;
    } else {
	strcpy (basefnam, buff);
	if (sscanf (cptr, "%d", &numfiles) != 1) {
	    printf ("\n\nIllegal first parameter (%s)--enter CONCNT for help.\n\n", argv[1]);
	    exit (1);
	} else if (numfiles < 1 || numfiles > 10) {
	    printf ("\n\nIllegal number of files on first parameter (%s)--enter CONCNT for help.\n\n",
		    argv[1]);
	    exit (1);
	}
    }

   /* get states to look at in TEST region */

    if (idflg) {
	for (i = 0; i < STATESZ; ++i) {
	    idptr = GetUInt (idptr, &id, -1, ",", &err); /* was GetInt  -joke */
	    if (err) {
		printf ("\nIllegal State Id in run parameter.\n");
		exit (1);
	    } else if (id == -1)
		break;
	    else {
		States[i] = id;
		++NmStates;
	    }
	}

	for (cptr = basefnam, i = 0; *cptr != '\0' && *cptr != '.' && i < 64; ++cptr, ++i)
	    sumfname[i] = *cptr;
	sumfname[i] = '\0';
	strcat (sumfname, ".sm");
	printf ("\nsumfname '%s'", sumfname);
	if ((SumFPtr = fopen (sumfname, APPENDMODE)) == NULL)
	    printf ("\n\nERROR--can't open sum file '%s'.\n\n", sumfname);
    }
   /* From each file, read lines until done */

    for (filenum = 0; filenum < numfiles; ++filenum) {
	strcpy (fname, basefnam);	/* get base name */

	if (numfiles > 1) {		/* more than one file to process */
	    sprintf (buff, "%d", filenum);
	    strcat (fname, buff);
	}
	if ((InFptr = fopen (fname, READMODE)) == NULL) {
	    printf ("\n\nERROR--can't open log file '%s': no data collected from it.\n\n", fname);
	    if (numfiles == 1) {
		printf ("For help, enter concnt-1 .\n");
		exit (1);
	    }
	    continue;
	}
	printf ("\nExtracting CFS-C data from from the file ");

#if LATTICEC
	getcwd (path, sizeof (path));
	printf ("%s\\", path);
#endif

	printf ("%s ...\n", fname);

	PrntSH = FALSE;

	if (EndRun) {
	    while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF) {
		if (strcmpn (inbuf, "C-? ; ENDRUN", 12) == 0 ||
			strcmpn (inbuf, "; ENDRUN", 8) == 0)
		    break;
	    }
	}
	while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF) {
	    if (strcmpn (inbuf, "; TEST", 6) == 0 ||
		    strcmpn (inbuf, "C-? ; TEST", 10) == 0) {
		TestReg = 1;
		if (NmStates != 0)
		    for (sptr = 0; sptr < STATESZ; ++sptr) {
			StatesNC[sptr] = 0;
			StatesNI[sptr] = 0;
			StatesStr[sptr] = 0;
		    }
	    } else if (strcmpn (inbuf, "; ENDTEST", 9) == 0 ||
		       strcmpn (inbuf, "C-? ; ENDTEST", 13) == 0) {
		TestReg = 0;
		if (!PrntSH && NmStates != 0) {
		    fprintf (SumFPtr, "Summary counts for TEST states in %s.\n", fname);
		    fprintf (SumFPtr, "\n			  ");
		    for (sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr)
			fprintf (SumFPtr, " |	 %2u	  ", States[sptr]);
		    PrntSH = TRUE;
		}
		if (NmStates != 0 && SumFPtr != NULL) {
		    fprintf (SumFPtr, "\n%5.0f %3d %5.3f", CycleStp, FConCnt, FEntropy);
		    for (sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr) {
			fprintf (SumFPtr, " | %3u %3u", StatesNI[sptr], StatesNC[sptr]);
			if (TotCfStr != (float) 0)
			    fprintf (SumFPtr, " %4.2f", StatesStr[sptr] / TotCfStr);
			else
			    fprintf (SumFPtr, "	 ");
		    }
		   /* store full-list FConCnt and FEntropy */
		    id = fInLst (CycleStp, TStepLst, TStepNxt);
/* id is unsigned: how can id be < 0, asked GCC 2.x?
		if (id < 0 || id >= TStepNxt || id > TOTSZ) */
		if (id >= TStepNxt || id > TOTSZ)
			printf ("\nid out of range at FConCnt...\n");
		    else {
			TConcepts[id] += FConCnt;
			TEntropy[id] += FEntropy;
		    }
		}
	    }
	   /* in TEST region, keep CurState up to date... */

	    else if (TestReg && strcmpn (inbuf, "State", 5) == 0) {
		sscanf (inbuf, "State %d", &CurState);
	    } else if (strcmpn (inbuf, "Current Cl", 10) == 0) {
		sscanf (inbuf, "Current Classifiers (cycle-step %f) %s", &CycleStp, buff);
		ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* blank line */
		ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* first classifier or table title */

		if (strcmpn (inbuf, "Id", 2) == 0)
		    continue;

		else {
		    if (BooleFlg)
			ReadConBoole ();
		    else
			ReadCon ();

		    PrntCon ();
		}
	    }
	}				/* more lines in a file */

	if (fclose (InFptr) != 0)
	    printf ("\n\nERR: couldn't close log file '%s'!!\n", fname);

	if (SumFPtr != NULL)
	    fprintf (SumFPtr, "\n\n");

	++NumFilC;

    }					/* The individual files */

    if (NumFilC > 1 && idflg)
	PrntTSta ();

}					/* main */

/************************

*********/

VOID 
ReadCon ()
{
    int ret, len, err, i, overflow, teststate, startstate, endstate;
    float str, br;
    char *cp, con[CONSZ + 1], tbuff[8], statebuff[128];

    TotCfStr = 0;
    TotNmCf = 0;

    if (CalcC || CalcE)
	for (i = 0; i < STRNGSZ * 3; ++i) {
	    LociZero[i] = 0;
	    LociOne[i] = 0;
	    LociHash[i] = 0;
	}
    for (i = 0; i < CONLSTSZ; ++i) {
	Concepts[i].Cnt = 0;
	Concepts[i].Str = 0;
	Concepts[i].BidRatio = 0;
	Concepts[i].Concept[0] = '\0';
	Concepts[i].Nxt = NULL;
    }
    ConCnt = IntCnt = 0;
    ConStr = ConBR = 0;
    ConSort = NULL;
    overflow = FALSE;

   /* NB--> inbuf has first cf in it on entry  */

    do {
	for (cp = inbuf; *cp == ' ' && *cp != '\0'; ++cp);	/* check for blank line */
	if (*cp == '\0')
	    break;			/* quit if it was all blanks */

	for (cp = inbuf; *cp != '>' && *cp != '\0'; ++cp);	/* skip id */
	++cp;
	for (; *cp == ' ' && *cp != '\0'; ++cp);	/* skip blanks */

	for (i = 0; *cp != '[' && *cp != '\0' && i < CONSZ; ++cp, ++i)	/* get first condition */
	    con[i] = *cp;
	if (*cp != '\0' && i < CONSZ) {
	    con[i++] = *cp;		/* must be the [ */
	    ++cp;			/* get past the [ */
	}
	con[i] = '\0';

       /* move the first condition */

	for (; *cp != ']' && *cp != '\0' && i < CONSZ; ++cp, ++i)
	    con[i] = *cp;
	if (*cp != '\0' && i < CONSZ) {	/* if not these, must be ] */
	    con[i++] = *cp;
	    ++cp;			/* get past the ] */
	}
	con[i] = '\0';

#ifdef X				/* this is to compress states--done in FSW? now */
       /* now read of each state number, and compress continuous ranges */

	statebuff[0] = '[';		/* store list of states here */
	statebuff[1] = '\0';
	startstate = endstate = -2;
	while (TRUE) {
	    cp = GetInt (cp, &teststate, -2, ",]", &err);
	    if (teststate == endstate + 1)
		endstate = teststate;	/* its just next is series */
	    else {			/* its not next in series */
		if (endstate != -2) {	/* not first, so print stored state(s) */
		    if (strlen (statebuff) > 1)	/* not first printed, so insert comma */
			strcat (statebuff, ",");
		    if (endstate == startstate)	/* just the one */
			sprintf (tbuff, "%d", startstate);
		    else
			sprintf (tbuff, "%d-%d", startstate, endstate);
		    strcat (statebuff, tbuff);
		}
		startstate = endstate = teststate;	/* start anew */
	    }
	    if (*cp == ';') {		/* got past the ] */
		if (strlen (statebuff) > 1)	/* not first printed, so insert comma */
		    strcat (statebuff, ",");
		if (endstate != -2) {
		    if (endstate == startstate)	/* just the one */
			sprintf (tbuff, "%d", startstate);
		    else
			sprintf (tbuff, "%d-%d", startstate, endstate);
		    strcat (statebuff, tbuff);
		}
		break;			/* and quit */
	    }
	}
	strcat (statebuff, "]");
	strcpy (&con[i], statebuff);

	for (; con[i] != '\0' && i < CONSZ; ++i)	/* get i to end of con */
	    ;
	if (i == CONSZ) {
	    printf ("\nERROR: i in con[i] too large after c1.");
	    break;
	}
#endif

       /* past end ] for first condition now */

	con[i++] = ';';
	if (i > 79) {
	    con[i++] = '\n';
	    con[i++] = ' ';
	    con[i++] = ' ';
	    con[i++] = ' ';
	    con[i++] = ' ';
	    con[i++] = ' ';
	} else
	    con[i++] = ' ';
	con[i] = '\0';			/* end of first condition */
	++cp;				/* should be past the ], so move past the ; */

       /* done with first condition, so go on to next condition */

	for (; *cp == ' ' && *cp != '\0'; ++cp);	/* skip blanks */

	for (; *cp != '[' && *cp != '\0' && i < CONSZ; ++cp, ++i)	/* get second condition */
	    con[i] = *cp;
	if (*cp != '\0' && i < CONSZ) {
	    con[i++] = *cp;		/* must be the [ */
	    ++cp;			/* get past the [ */
	}
	con[i] = '\0';

       /* move the second condition */

	for (; *cp != ']' && *cp != '\0' && i < CONSZ; ++cp, ++i)
	    con[i] = *cp;
	if (*cp != '\0' && i < CONSZ) {	/* if not these, must be ] */
	    con[i++] = *cp;
	    ++cp;			/* get past the ] */
	}
	con[i] = '\0';

#ifdef X				/* This compression is done by fsw1 now */
       /* now read of each state number, and compress continuous ranges */
	statebuff[0] = '[';		/* store list of states here */
	statebuff[1] = '\0';
	startstate = endstate = -2;
	while (TRUE) {
	    cp = GetInt (cp, &teststate, -2, ",]", &err);
	    if (teststate == endstate + 1)
		endstate = teststate;	/* its just next is series */
	    else {			/* its not next in series */
		if (endstate != -2) {	/* not first, so print stored state(s) */
		    if (strlen (statebuff) > 1)	/* not first printed, so insert comma */
			strcat (statebuff, ",");
		    if (endstate == startstate)	/* just the one */
			sprintf (tbuff, "%d", startstate);
		    else
			sprintf (tbuff, "%d-%d", startstate, endstate);
		    strcat (statebuff, tbuff);
		}
		startstate = endstate = teststate;	/* start anew */
	    }
	    if (*cp == ' ') {		/* got past the ] */
		if (strlen (statebuff) > 1)	/* not first printed, so insert comma */
		    strcat (statebuff, ",");
		if (endstate != -2) {
		    if (endstate == startstate)	/* just the one */
			sprintf (tbuff, "%d", startstate);
		    else
			sprintf (tbuff, "%d-%d", startstate, endstate);
		    strcat (statebuff, tbuff);
		}
		break;			/* and quit */
	    }
	}
	strcat (statebuff, "]");
	strcpy (&con[i], statebuff);

	for (; con[i] != '\0' && i < CONSZ; ++i)	/* get i to end of con */
	    ;
#endif

	if (i > 79) {
	    con[i++] = '\n';
	    con[i++] = ' ';
	    con[i++] = ' ';
	    con[i++] = ' ';
	    con[i++] = ' ';
	    con[i++] = ' ';
	}
       /* past ] at end of condition 2 now; now work on action part */

	for (; *cp != '[' && *cp != '\0' && i < CONSZ; ++cp, ++i)	/* get first condition */
	    con[i] = *cp;
	if (*cp != '\0' && i < CONSZ) {
	    con[i++] = *cp;		/* must be the [ */
	    ++cp;			/* get past the [ */
	}
	con[i] = '\0';

       /* move the action */

	for (; *cp != ']' && *cp != '\0' && i < CONSZ; ++cp, ++i)
	    con[i] = *cp;
	if (*cp != '\0' && i < CONSZ) {	/* if not these, must be ] */
	    con[i++] = *cp;
	    ++cp;			/* get past the ] */
	}
	con[i] = '\0';

#ifdef  X				/* this is to compress the action */
       /* now read of each state number, and compress continuous ranges */
	statebuff[0] = '[';		/* store list of states here */
	statebuff[1] = '\0';
	startstate = endstate = -2;
	while (TRUE) {
	    cp = GetInt (cp, &teststate, -2, ",]", &err);
	    if (teststate == endstate + 1)
		endstate = teststate;	/* its just next is series */
	    else {			/* its not next in series */
		if (endstate != -2) {	/* not first, so print stored state(s) */
		    if (strlen (statebuff) > 1)	/* not first printed, so insert comma */
			strcat (statebuff, ",");
		    if (endstate == startstate)	/* just the one */
			sprintf (tbuff, "%d", startstate);
		    else
			sprintf (tbuff, "%d-%d", startstate, endstate);
		    strcat (statebuff, tbuff);
		}
		startstate = endstate = teststate;	/* start anew */
	    }
	    if (*cp == ' ') {		/* got past the ] */
		if (strlen (statebuff) > 1)	/* not first printed, so insert comma */
		    strcat (statebuff, ",");
		if (endstate != -2) {
		    if (endstate == startstate)	/* just the one */
			sprintf (tbuff, "%d", startstate);
		    else
			sprintf (tbuff, "%d-%d", startstate, endstate);
		    strcat (statebuff, tbuff);
		}
		break;			/* and quit */
	    }
	}
	strcat (statebuff, "]");
	strcpy (&con[i], statebuff);
#endif

       /* now past the ] after the action, one way or another */

	for (; *cp != '{' && *cp != '\0'; ++cp);	/* get past the { */
	if (*cp == '{')
	    ++cp;
	cp = GetFloat (cp, &str, (float) -1.0, ",", &err);
	cp = GetFloat (cp, &br, (float) -1.0, ",}", &err);
	if (str == -1.0 || br == -1.0) {
	    printf ("\ni %d, cp->%s<inbuf:", i, cp);
	    for (err = 0, cp = inbuf; *cp != '\0'; ++cp, ++err) {
		printf ("%c", *cp);
		if (err % 80 == 0)
		    printf ("\n");
	    }
	    printf ("*len=%d", err);
	}
	TotCfStr += str;
	++TotNmCf;

	for (i = 0; i < ConCnt; ++i)
	    if (strcmp (Concepts[i].Concept, con) == 0)
		break;

	if (i == ConCnt) {		/* its new--init and increment ConCnt */
	    if (ConCnt == CONLSTSZ) {
		if (!overflow) {
		    printf ("\n**WARNING: ConCnt == CONLSTSZ: some concepts not counted.\n ");
		    overflow = TRUE;
		}
	    } else {
		strcpy (Concepts[ConCnt].Concept, con);
		Concepts[ConCnt].Cnt = 1;
		Concepts[ConCnt].Str = str;
		Concepts[ConCnt].BidRatio = br;
		Concepts[ConCnt].HiStr = str;
		Concepts[ConCnt].LowStr = str;
		++ConCnt;
	    }
	} else {			/* its old--increment concept totals  */
	    Concepts[i].Cnt += 1;
	    Concepts[i].Str += str;
	    Concepts[i].BidRatio += br;
	    if (str > Concepts[i].HiStr)
		Concepts[i].HiStr = str;
	    if (str < Concepts[i].LowStr)
		Concepts[i].LowStr = str;
	}

	ConStr += str;			/* in any case increase global totals */
	ConBR += br;
	IntCnt += 1;

	if (CalcC || CalcE)
	    Converge ();

    } while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF
	     && len != 0);

   /* In TEST region, get full list strength at end of list */

    if (TestReg) {
	ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
	sscanf (inbuf, "Number of Classifiers: %d. Ave. strength %f (total %f).",
		&TotNmCf, &str, &TotCfStr);
    }
}					/* ReadCon  */


/************************

*********/

VOID 
ReadConBoole ()
{
    int ret, len, err, i, overflow;
    float str, br;
    char *cp, con[CONSZ + 1], tbuff[8];

    TotCfStr = 0;
    TotNmCf = 0;

    if (CalcC || CalcE)
	for (i = 0; i < STRNGSZ * 3; ++i) {
	    LociZero[i] = 0;
	    LociOne[i] = 0;
	    LociHash[i] = 0;
	}
    for (i = 0; i < CONLSTSZ; ++i) {
	Concepts[i].Cnt = 0;
	Concepts[i].Str = 0;
	Concepts[i].BidRatio = 0;
	Concepts[i].Concept[0] = '\0';
	Concepts[i].Nxt = NULL;
    }
    ConCnt = IntCnt = 0;
    ConStr = ConBR = 0;
    ConSort = NULL;
    overflow = FALSE;

   /* NB--> inbuf has first cf in it on entry  */

    do {
	for (cp = inbuf; *cp == ' ' && *cp != '\0'; ++cp);	/* check for blank line */
	if (*cp == '\0')
	    break;			/* quit if it was all blanks */

	for (cp = inbuf; *cp != '>' && *cp != '\0'; ++cp);	/* skip id */
	++cp;
	for (; *cp == ' ' && *cp != '\0'; ++cp);	/* skip blanks */

	for (i = 0; *cp != '{' && *cp != '\0' && i < CONSZ; ++cp, ++i)	/* get whole thing to { */
	    con[i] = *cp;
	con[i] = '\0';
	if (*cp == '{')
	    ++cp;

	cp = GetFloat (cp, &str, (float) -1.0, ",", &err);
	cp = GetFloat (cp, &br, (float) -1.0, ",}", &err);
	if (str == -1.0 || br == -1.0) {
	    printf ("\ni %d, cp->%s<inbuf:", i, cp);
	    for (err = 0, cp = inbuf; *cp != '\0'; ++cp, ++err) {
		printf ("%c", *cp);
		if (err % 80 == 0)
		    printf ("\n");
	    }
	    printf ("*len=%d", err);
	}
	TotCfStr += str;
	++TotNmCf;

	for (i = 0; i < ConCnt; ++i)
	    if (strcmp (Concepts[i].Concept, con) == 0)
		break;

	if (i == ConCnt) {		/* its new--init and increment ConCnt */
	    if (ConCnt == CONLSTSZ) {
		if (!overflow) {
		    printf ("\n**WARNING: ConCnt == CONLSTSZ: some concepts not counted.\n ");
		    overflow = TRUE;
		}
	    } else {
		strcpy (Concepts[ConCnt].Concept, con);
		Concepts[ConCnt].Cnt = 1;
		Concepts[ConCnt].Str = str;
		Concepts[ConCnt].BidRatio = br;
		Concepts[ConCnt].HiStr = str;
		Concepts[ConCnt].LowStr = str;
		++ConCnt;
	    }
	} else {			/* its old--increment concept totals  */
	    Concepts[i].Cnt += 1;
	    Concepts[i].Str += str;
	    Concepts[i].BidRatio += br;
	    if (str > Concepts[i].HiStr)
		Concepts[i].HiStr = str;
	    if (str < Concepts[i].LowStr)
		Concepts[i].LowStr = str;
	}

	ConStr += str;			/* in any case increase global totals */
	ConBR += br;
	IntCnt += 1;

	if ((CalcC || CalcE) && !BooleFlg)
	    Converge ();

    } while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF
	     && len != 0);

   /* In TEST region, get full list strength at end of list */

    if (TestReg) {
	ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
	sscanf (inbuf, "Number of Classifiers: %d. Ave. strength %f (total %f).",
		&TotNmCf, &str, &TotCfStr);
    }
}					/* ReadConBoole  */


/*********************

Converge: on entry inbuf has classifier line to read.
	May be in two formats (1 and 22)!
	In either case, the full condition/action strings must be in the log.
	In fmt 22, the string looks like:
		id> XYGD: string [interpreted]; ditto // XYGD: string [interpreted]
	Note that 'string' in fmt 22 is all the bits except the tag.

*******/

VOID 
Converge ()
{
    int fmt22, loc, i;
    char *cp, tbuff[STRNGSZ + 1];

    for (cp = inbuf; *cp != '>' && *cp != ',' && *cp != '\0'; ++cp);
    if (*cp == '>')
	fmt22 = TRUE;
    else
	fmt22 = FALSE;

    if (fmt22) {
       /* first work on first condition */
	for (cp = inbuf; *cp != '>' && *cp != '\0'; ++cp);
	++cp;
	++cp;
	Getstrng (cp, tbuff);

       /* printf("\n%s", tbuff ); */

	for (loc = 0; loc < STRNGSZ; ++loc)
	    if (tbuff[loc] == '0')
		LociZero[loc] += 1;
	    else if (tbuff[loc] == '1')
		LociOne[loc] += 1;
	    else
		LociHash[loc] += 1;

       /* now work on second condition */

	for (cp = inbuf; *cp != ';' && *cp != '\0'; ++cp);
	++cp;
	++cp;
	++cp;
	Getstrng (cp, tbuff);

       /* printf(" ; %s ", tbuff ); */

	for (; loc < STRNGSZ * 2; ++loc)
	    if (tbuff[loc - STRNGSZ] == '0')
		LociZero[loc] += 1;
	    else if (tbuff[loc] == '1')
		LociOne[loc] += 1;
	    else
		LociHash[loc] += 1;

       /* and the action part... */

	for (cp = inbuf; *cp != '=' && *cp != '\0'; ++cp);
	++cp;
	++cp;
	++cp;
	++cp;
	Getstrng (cp, tbuff);

       /* printf("==> %s", tbuff ); */

	for (; loc < STRNGSZ * 3; ++loc)
	    if (tbuff[loc - (STRNGSZ * 2)] == '0')
		LociZero[loc] += 1;
	    else if (tbuff[loc] == '1')
		LociOne[loc] += 1;
	    else
		LociHash[loc] += 1;
    }
}					/* Converge  */


/************************

*********/

VOID 
Getstrng (cp, tbuff)
    char *cp, tbuff[];
{
    int i;

   /* first get to interpreted tag region. */

    if (strcmpn (cp, "   D", 4) == 0)
	strcpy (tbuff, "00");
    else if (strcmpn (cp, "X   ", 4) == 0)
	strcpy (tbuff, "01");
    else if (strcmpn (cp, "X  D", 4) == 0)
	strcpy (tbuff, "0#");
    else if (strcmpn (cp, "  G ", 4) == 0)
	strcpy (tbuff, "10");
    else if (strcmpn (cp, " Y  ", 4) == 0)
	strcpy (tbuff, "11");
    else if (strcmpn (cp, " YG ", 4) == 0)
	strcpy (tbuff, "1#");
    else if (strcmpn (cp, "  GD", 4) == 0)
	strcpy (tbuff, "#0");
    else if (strcmpn (cp, "XY  ", 4) == 0)
	strcpy (tbuff, "#1");
    else
	strcpy (tbuff, "##");

   /* now get to start of rest of the string, and copy into tbuff */
    for (; *cp != ':' && *cp != '\0'; ++cp);
    ++cp;
    ++cp;
    for (i = 2; i < STRNGSZ; ++i, ++cp)
	tbuff[i] = *cp;
    tbuff[i] = '\0';

}					/* Getstrng */


/************************

*********/

VOID 
PrntCon ()
{
    int i, itot, loc, cntres, ires, sptr, tptr;
    unsigned int tstep;
    float strtot, brtot, C, ci, f, f1, strres, brres, hires, lores;
    struct ConNode *cnp, *tcnp;

    C = 0;

   /*  Sort, starting at bottom of array
			(most likely lowest S and count since cflist display hi S to low).
			Remember the concepts are stored in Concepts[0..CntCnt-1].
		*/

    printf ("\n-------------  Step %5.0f  ------------------", CycleStp);
    if (TestReg)
	printf ("  State %d", CurState);

    if (ConCnt == 1)
	ConSort = &Concepts[ConCnt - 1];/* get the only one! */

    else if (ConCnt > 1) {
	ConSort = &Concepts[ConCnt - 1];/* get the first one from bottom */
	ConSort -> Nxt = NULL;
	i = ConCnt - 2;			/* get ready for next up */

	if (StrSort) {
	    for (cnp = &Concepts[i]; i >= 0; --cnp, --i) {
		if ((cnp -> Str / cnp -> Cnt) >= (ConSort -> Str / cnp -> Cnt)) {
		    cnp -> Nxt = ConSort;	/* its first now */
		    ConSort = cnp;
		} else {
		   /* move tcnp until cnp goes right after it */
		    for (tcnp = ConSort; tcnp -> Nxt != NULL; tcnp = tcnp -> Nxt)
			if ((cnp -> Str / cnp -> Cnt) >= (tcnp -> Nxt -> Str / cnp -> Cnt))
			    break;
		    cnp -> Nxt = tcnp -> Nxt;	/* insert after tcnp */
		    tcnp -> Nxt = cnp;
		}
	    }
	} else {
	    for (cnp = &Concepts[i]; i >= 0; --cnp, --i) {
		if (cnp -> Cnt >= ConSort -> Cnt) {
		    cnp -> Nxt = ConSort;	/* its first now */
		    ConSort = cnp;
		} else {
		   /* move tcnp until cnp goes right after it */
		    for (tcnp = ConSort; tcnp -> Nxt != NULL; tcnp = tcnp -> Nxt)
			if (cnp -> Cnt >= tcnp -> Nxt -> Cnt)
			    break;
		    cnp -> Nxt = tcnp -> Nxt;	/* insert after tcnp */
		    tcnp -> Nxt = cnp;
		}
	    }
	}
    }
    if (ConStr > (float) 0.0 && ConCnt > 0 && (StrSort || CntSort))
	printf ("\nCnt Str/Tot Br Concept\n");

    if (ConStr == (float) 0.0)
	printf ("\nTotal Concept Str 0 at step %5.0f!\n", CycleStp);

    else if (ConCnt == 0)
	printf ("\nNo Concepts at step %5.0f!\n", CycleStp);

    else {
	strtot = brtot = strres = brres = 0;
	itot = cntres = ires = hires = 0;
	lores = 999999;
	for (cnp = ConSort, i = 0; cnp != NULL && i < ConCnt; ++i, cnp = cnp -> Nxt) {
	    if ((i <= 10 && (StrSort || CntSort)) ||
		    (CntSort && itot <= CutFrac * IntCnt) ||
		    (StrSort && strtot <= CutFrac * ConStr)) {
		printf ("%3d %5.3f %2.2f %s (%4.0f,%4.0f,%4.0f)\n", cnp -> Cnt,
			(cnp -> Str / ConStr), (cnp -> BidRatio / cnp -> Cnt), cnp -> Concept,
			cnp -> HiStr, (cnp -> Str / cnp -> Cnt), cnp -> LowStr);
	    } else {
		strres += cnp -> Str;	/* residuals not above cutoff */
		brres += cnp -> BidRatio;
		ires += cnp -> Cnt;
		cntres += 1;
		if (hires > cnp -> HiStr)
		    hires = cnp -> HiStr;
		if (lores < cnp -> LowStr)
		    lores = cnp -> LowStr;
	    }
	    strtot += cnp -> Str;
	    brtot += cnp -> BidRatio;
	    itot += cnp -> Cnt;
	}
	if (cntres != 0 && (StrSort || CntSort))
	    printf ("%3d %5.3f %2.2f (in %d residuals) (%4.0f,%4.0f,%4.0f)\n",
		    ires, (strres / ConStr), (brres / ires), cntres,
		    hires, (strres / ires), lores);

	if (TotCfStr != (float) 0 && TotNmCf == 0) {
	    printf ("\n%3d %3.2f %3.2f  (totals/averages for %d concepts)",
		    IntCnt, strtot / TotCfStr, (brtot / IntCnt), ConCnt);
	    printf ("\n			   (ConStr %.0f (%.0f), ListStr %.0f (**)\n\n",
		    strtot, strtot / IntCnt, TotCfStr);
	} else if (TotCfStr != (float) 0 && TotNmCf != 0) {
	    printf ("\n%3d %3.2f %3.2f  (totals/averages for %d concepts)",
		    IntCnt, strtot / TotCfStr, (brtot / IntCnt), ConCnt);
	    printf ("\n			   (ConStr %.0f (%.0f), ListStr %.0f (%.0f)\n\n",
		    strtot, strtot / IntCnt, TotCfStr, TotCfStr / TotNmCf);
	} else {
	    printf ("\n%3d *S*  %3.2f  (totals/averages for %d concepts)",
		    IntCnt, (brtot / IntCnt), ConCnt);
	    printf ("\n			   (ConStr %.0f (**), ListStr %.0f (**)\n\n",
		    strtot, TotCfStr);
	}

	if (NmStates != 0 && TestReg && (sptr = IntInLst (CurState, States, NmStates)) >= 0) {
	    if (sptr < STATESZ) {
		StatesNC[sptr] += ConCnt;
		StatesNI[sptr] += IntCnt;
		StatesStr[sptr] += strtot;
	       /* save in totals array */
		tptr = IntInLst (CycleStp, TStepLst, TStepNxt);
		if (tptr >= TOTSZ)
		    printf ("\ntptr > TOTSZ?\n");
		else {
		    if (tptr < 0) {	/* its a new cyclestep */
			if (TStepNxt >= TOTSZ) {
			    printf ("\nToo many CycleSteps for Totals.\n");
			    tptr = TOTSZ - 1;
			} else {
			    tptr = TStepNxt++;	/* so use next spot */
			    TStepLst[tptr] = CycleStp;	/* store step there */
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

    if (CalcC && IntCnt != 0) {
	printf ("\nConvergence C  (%d indivuals): ", IntCnt);

	for (C = 0, loc = 0; loc < STRNGSZ * 3; ++loc) {
	    i = (2 * LociZero[loc]) - IntCnt;
	    if (i < 0)			/* *** Why does abs fail here in microsoft C? */
		i = 0 - i;
	    ci = 1.0 * i / IntCnt;
	    C += ci;
	}

	printf (" %5.3f (step %5.0f).\n", C / (3 * STRNGSZ), CycleStp);
    }
    if (CalcE && IntCnt != 0) {
	printf ("'Entropy' E	(%d indivuals): ", IntCnt);

	for (C = 0, loc = 0; loc < STRNGSZ * 3; ++loc) {
	    ci = 0;
	    if (LociZero[loc] != 0) {
		f = LociZero[loc] * 1.0 / IntCnt;
		ci = f * log (f);
	    }
	    if (LociOne[loc] != 0) {
		f = LociOne[loc] * 1.0 / IntCnt;
		ci += f * log (f);
	    }
	    if (LociHash[loc] != 0) {
		f = LociHash[loc] * 1.0 / IntCnt;
		ci += f * log (f);
	    }
	    f = -1.0 * ci / log (3.0);
	    C += f;
	}

	printf (" %5.3f (step %5.0f).\n", C / (3 * STRNGSZ), CycleStp);
    }
    if (NmStates != 0 && !TestReg) {	/* save for possible use in sum table */
	FConCnt = ConCnt;
	FEntropy = C / (3 * STRNGSZ);
    }
}					/* PrntCon  */


/************************

*********/

VOID 
PrntTSta ()
{
    unsigned int sptr, tptr;

    fprintf (SumFPtr, "\nTotals for %d files", NumFilC);
    fprintf (SumFPtr, "\n			  ");
    for (sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr)
	fprintf (SumFPtr, " |	 %2u	  ", States[sptr]);

    if (NmStates != 0 && SumFPtr != NULL && NumFilC > 0) {
	for (tptr = 0; tptr < TStepNxt && tptr < TOTSZ; ++tptr) {
	    TConcepts[tptr] /= NumFilC;
	    TEntropy[tptr] /= NumFilC;
	    fprintf (SumFPtr, "\n%4.0f %3d %5.3f", TStepLst[tptr], TConcepts[tptr], TEntropy[tptr]);
	    for (sptr = 0; sptr < NmStates && sptr < STATESZ; ++sptr) {
		TStateNI[sptr][tptr] /= NumFilC;
		TStateNC[sptr][tptr] /= NumFilC;
		TStatesStr[sptr][tptr] /= NumFilC;
		TTotCfStr[sptr][tptr] /= NumFilC;
		fprintf (SumFPtr, " | %3u %3u", TStateNI[sptr][tptr], TStateNC[sptr][tptr]);
		if (TTotCfStr[sptr][tptr] != 0)
		    fprintf (SumFPtr, " %4.2f", TStatesStr[sptr][tptr] / TTotCfStr[sptr][tptr]);
		else
		    fprintf (SumFPtr, "	 ");
	    }
	}
    }
}					/* PrntTSta */



/*
strcmpn	 Compare leftmost N characters of two strings.

	Str1	Strings to compare (possibly NULL terminated).
	Str2
	N	   Number of characters to compare.

	Return: If leftmost N characters are the same, return 0
			else  return != 0 .
*/

int 
strcmpn (Str1, Str2, N)
    char Str1[], Str2[];
    int N;
{
    int ret, i;

    for (ret = 1, i = 0; Str1[i] == Str2[i] && i < N; ++i);

    if (i == N)
	ret = 0;

    return (ret);

}					/* strcmpn */

/*
IntInLst	Check for int in list (array) of ints.

	Test	Int to look for.

	List	Array of int's.

	Max	 Last entry to check.

	Return  N   if Test is N-th entry in List[0...Max].
			-1  otherwise
*/

int 
IntInLst (Test, List, Max)
    int Test, List[], Max;
{
    int ret, i;

    for (i = 0; i <= Max; ++i)
	if (Test == List[i])
	    break;

    if (i <= Max)
	ret = i;
    else
	ret = -1;

    return (ret);

}					/* IntInLst */

/*****************

fInLst	Check for float in list (array) of floats.

	Test	float to look for.

	List	Array of float's.

	Max	 Last entry to check.

	Return  N   if Test is N-th entry in List[0...Max].
			-1  otherwise

*****/

int 
fInLst (Test, List, Max)
    float Test, List[];
    int Max;
{
    int ret, i;

    for (i = 0; i <= Max; ++i)
	if (Test == List[i])
	    break;

    if (i <= Max)
	ret = i;
    else
	ret = -1;

    return (ret);

}					/* fInLst */


/************************

*********/

VOID 
Help ()
{

    printf ("\nConCnt-1 reads log files produced by the CFS-C/FSW1 system, in which");
    printf ("\nclassifiers have been displayed one or more times, and produces");
    printf ("\nas output a compressed form of each display of the classifier list.");
    printf ("\n");
    printf ("\nThe display is similar to the 'Macro-State' description of classifiers");
    printf ("\nas discussed by Stewart Wilson in his article 'Classifier Systems and");
    printf ("\nthe Animat Problem', Machine Learning 2, p199-228 (1987).");
    printf ("\n");
    printf ("\nUsage:");
    printf ("\nconcnt-1 x001.lg0  s90  c  e");
    printf ("\n  Reads from the file x001.lg0");
    printf ("\n  Counts as separate 'concepts' each classifier 'type' (i.e.,");
    printf ("\n  genotypically alike) for those that have the top 90%% (the 's90') of the");
    printf ("\n  total strength). For each concept, displays number of classifiers,");
    printf ("\n  total strength, strength as percent of list, etc.");
    printf ("\n  'c' means for each display, calculated a 'convergence' measure");
    printf ("\n  (0 = random, 1 = converged).");
    printf ("\n  'e' means for each display, calculate an 'entropy' measure (1=random,0=converged).");
    printf ("\n  NOTE: To use c or e options, display classifiers in formats 1 or 22.");
    printf ("\n[press any key to continue...]");

    getch ();
    printf ("\nconcnt-1  x001.lg,3  s100 e");
    printf ("\n  As above, but collects information from files x001.lg0, x001.lg1, and x001.lg2.");
    printf ("\n  Also, doesn't calculate convergences, and collects concepts up to 100%% of");
    printf ("\n  total stregth (i.e., all of them).");
    printf ("\n");
    printf ("\nAs an example, a portion of a FSW1 run could look like this:");
    printf ("\n  di env,1 env,2  cl,22");
    printf ("\n  ; TEST");
    printf ("\n  set dscflst=b adcfint=1 adcffmt=22");
    printf ("\n  ecmd cs 0");
    printf ("\n  di env,1");
    printf ("\n  step");
    printf ("\n  ecmd cs 1");
    printf ("\n  di env,1");
    printf ("\n  step");
    printf ("\n	... and so on for states of interest");
    printf ("\n  set  dscflst=0 adcfint=1000");
    printf ("\n  ; ENDTEST");
    printf ("\nRun should continue, displaying full classifier list periodically,");
    printf ("\nand then going into the next TEST...ENDTEST region.");
    printf ("\n");
    printf ("\nNote that TEST commands can be put in another file and the CFS-C '<' command");
    printf ("\nused to read them in periodically.");
    printf ("\n");

    printf ("\n[press any key to continue...]");
    getch ();

    printf ("\nTo use with the Boole-N domains, include the parameter 'b'.");
    printf ("\nNOTE ==> 'c' and 'e' measures not currently supported for 'b'.\n");

    printf ("\nSome limits (which could be compiled into larger values:");
    printf ("\nBasic string-size of messages/classifiers: %d", STRNGSZ);
    printf ("\nSize of (displayed) classifier: %d", CONSZ);
    printf ("\nNumber of concepts in CC file: %d", CONLSTSZ);
    printf ("\nNumber of display points per run: %d", TOTSZ);

}					/* Help */
