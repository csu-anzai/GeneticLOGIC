/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990-1992                                     */
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI                        	*/
/*  University of Dortmund                                    	*/
/*  Baroper Str. 301						*/
/*  D-4600 Dortmund 50						*/
/*                                                           	*/
/*  e-mail: baeck@ls11.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/
 
/*
 *
 *	$Id: input.c,v 1.5 1992/06/19 17:00:09 baeck Exp $
 *	$Log: input.c,v $
 * Revision 1.5  1992/06/19  17:00:09  baeck
 * C_bit removed
 *
 * Revision 1.4  1992/06/19  16:45:07  baeck
 * Adaptive recombination removed
 *
 * Revision 1.3  1992/06/16  13:11:51  baeck
 * Adaptive crossover point number deleted
 *
 * Revision 1.2  1992/06/16  12:14:21  baeck
 * Copyright note added
 *
 * Revision 1.1  1992/06/12  11:35:03  baeck
 * Initial revision
 *
 *
 *
 *      file:   input.c
 *
 *    author:   Thomas Baeck
 *
 *   created:   1981
 *
 *   purpose:   Set up filenames and read the input file, and
 *        	initialize variables for this run.
 *
 *        	See init.c for the initialization of variables for each
 *        	experiment.
 *
 *  modified:   26 jun 86
 *
 *		Thomas Baeck, 19 jul 90   
 *			respect fopen() errors.
 *
 *		Thomas Baeck, 07 aug 90   
 *			File 'pop' added for population dumps.
 *
 *		Thomas Baeck, 27 nov 90
 *			Removed the packing of bits to characters in order
 *			to simplify the implementation of genetic operators
 *			as well as to avoid the unnecessary amount of time
 *			for Packing and Unpacking. Our machine has enough 
 *			memory to work with chararacter-bitstrings.
 *
 *		Thomas Baeck, 05 dec 90
 *			Get all arguments via the command line instead
 *			by reading from a file.
 *		
 */
 
#include "extern.h"

#define	fopen(a,b)	fOpen((a),(b))

#define	OPTSTR		"A:B:C:EG:I:L:M:N:P:R:S:T:U:V:W:X:Y:b:d:e:f:g:hi:n:o:qr:s:t:v:x:"

#define USAGE 		"GENEsYs 1.0\t\t\t(c) 1991 Thomas Baeck\n\n\
GA-specific options:\n\
\t-A murts\t\t# number of mutation rates\n\
\t-B mbits\t\t# number of bits encoding the mutation rate\n\
\t-C xprob\t\t# crossover probability\n\
\t-E      \t\t# elitist selection strategy\n\
\t-G gapsz\t\t# generation gap size\n\
\t-I chlgt\t\t# cooling generations for boltzmann selection\n\
\t-L lngth\t\t# length of object var. genotype (continuous)\n\
\t        \t\t# or length of full genotype     (binary    )\n\
\t-M S,A,X\t\t# mutation scheme\n\
\t   S    \t\t# simple\n\
\t   A    \t\t# adaptive, including mutation rates\n\
\t   X    \t\t# adaptive, excluding mutation rates\n\
\t   O    \t\t# mutation rate calculation, ONLY FOR f12\n\
\t-N Etamx\t\t# maximum expected value for linear ranking\n\
\t-P popsz\t\t# population size\n\
\t-R mprob\t\t# mutation probability\n\
\t-S P,B,R,I,W,M,C\t# reproduction scheme\n\
\t   P    \t\t# proportional\n\
\t   B    \t\t# Boltzmann\n\
\t   R    \t\t# linear ranking\n\
\t   I    \t\t# inverse linear ranking\n\
\t   W    \t\t# Whitley's index calculation\n\
\t   M    \t\t# random\n\
\t   C    \t\t# random with best group copying\n\
\t-T tmpct\t\t# temperature control for boltzmann selection\n\
\t-U muval\t\t# value of Mu\n\
\t-V roval\t\t# value of Rho\n\
\t-W wdwsz\t\t# window size for standard scaling\n\
\t-X xpnts\t\t# number of crossover points\n\
\t-Y xy   \t\t# recombination scheme (alleles/mutation)\n\
\t   x,y={S,U,D,I,R,_}\t# possible settings:\n\
\t   S    \t\t# simple n-point crossover\n\
\t   U    \t\t# uniform crossover\n\
\t   D    \t\t# discrete recombination\n\
\t   I    \t\t# intermediate recombination\n\
\t   R    \t\t# random intermediate recombination\n\
\t   _    \t\t# no recombination\n\n\
General options:\n\
\t-b bfreq\t\t# create bitmap dump of best individual\n\
\t-d dfreq\t\t# dump interval in generations\n\
\t-e noexp\t\t# total number of experiments\n\
\t-f nofct\t\t# index of the objective function\n\
\t-g maxsp\t\t# number of generation wo. evaluations\n\
\t-h      \t\t# this help information\n\
\t-i infil\t\t# filename of an inputfile\n\
\t-n dimns\t\t# dim. of objective function (if changeable)\n\
\t-o a,i,k,l,m\t\t# additional options to the GA\n\
\t   n,p,s,t,v\n\
\t   z\n\
\t-q      \t\t# create directory name and exit\n\
\t-r seed \t\t# seed for random number generator\n\
\t-s savsz\t\t# best structures to save\n\
\t-t tottr\t\t# total number of trials\n\
\t-v intvl\t\t# number of trials between data collections\n\
\t-x suffx\t\t# file suffix\n\n\
Additional options (following the general option -o):\n\
\t a      \t\t# evaluate all structures\n\
\t i      \t\t# read init data from file init.<suf>\n\
\t k      \t\t# collect best proportion data\n\
\t l      \t\t# dump last generation\n\
\t m      \t\t# collect mutation rate data (much data !)\n\
\t n      \t\t# simplified termination\n\
\t p      \t\t# dump phenotype population\n\
\t s      \t\t# trace schema history\n\
\t t      \t\t# trace GA run\n\
\t v      \t\t# collect object variable data (much data !)\n\
\t z      \t\t# collect selection probabilities\n\
\n"



extern 	FILE 	       *fOpen();
extern 	FUNCTION	f_tab[];

 
void
Input(argc,argv)
int 	argc;
char 	**argv;

{
	extern 	void	InzVal(),
			InzGenFil(),
			SetFlg();

	extern	char   *optarg,
		       *CrtFilNam();

	extern	int	optind,
			opterr,
			mpCfgFct(),
			GetDta();

    	FILE 	       *fp;
 
    	register int 	i,		/* loop control			*/
			InfFlg = 0;	/* no input file available	*/

	int		strlen(),
			get_fdx(),	/* functions for the f_tab[]	*/
			CrtPgm(),	/* bitmap file creation 	*/
			DspFctHlp();	/* display function help info	*/

	int		s,		/* a single option		*/
			DimFlg = 0,	/* function dimension changed 	*/
			QryFlg = 0;	/* directory name query flag */

    	char 		Msg[NSZ],	/* messages, filenames,...	*/
			Opt[NSZ];	/* option string		*/

	char	       *strcat(),
		       *strcpy(),
		       *get_fnm();

    	void 		AllStg(),	/* storage allocation */
			ChkCst();	/* consistency check */

	InzVal();			/* init variables and filenames */

	strcpy(Opt, "");
	strcpy(Msg, "");

	opterr = 0;			/* no error message from getopt	*/

    	while ((s = getopt(argc, argv, OPTSTR)) != -1) { /* process options */

		switch (s) { 	  /* Options for the genetic algorithm */

			case 'A': /* number of mutation rates for each ind. */

				sscanf(optarg, "%d", &NbrMttRts);
				break;

			case 'B': /* bits for adaptive mutation */

				sscanf(optarg, "%d", &M_bits);
				break;

			case 'C': /* crossover rate */

				sscanf(optarg, "%lf", &C_rate);
				break;

			case 'E': /* elitist selection scheme */

				Eliteflag = 1;
				break;

			case 'G': /* Generation gap */

				sscanf(optarg, "%lf", &Gapsize);
				break;

			case 'I': /* cooling duration in generations */

				scanf(optarg, "%d", &ChnLgt);
				break;

			case 'L': /* length of individuals */

				sscanf(optarg, "%d", &ChrLen);
				break;

			case 'M': /* mutation scheme */

				sscanf(optarg, "%c", &MttScm);
				break;

			case 'N': /* Eta max (Baker), a (Whitley), ranking */

				sscanf(optarg, "%lf", &Eta_max);
				break;

			case 'P': /* population size */

				sscanf(optarg, "%d", &Popsize);
				break;

			case 'R': /* mutation rate */

				sscanf(optarg, "%lf", &M_rate);
				break;

			case 'S': /* selection scheme */

				sscanf(optarg, "%c", &SltScm);
				break;

			case 'T': /* temperature control parameter */

				sscanf(optarg, "%lf", &CtlPar);
				break;

			case 'U': /* Mu, degree of right extinctiveness */

				sscanf(optarg, "%d", &Mu);
				break;

			case 'V': /* rho, degree of left extinctiveness */

				sscanf(optarg, "%d", &Rho);
				break;

			case 'W': /* size of scaling window */

				sscanf(optarg, "%d", &Windowsize);
				break;

			case 'X': /* #crossover-points */

				sscanf(optarg, "%d", &C_points);
				break;

			case 'Y': /* recombination scheme choice */

				sscanf(optarg, "%s", RecScm);
				break;


			/* Options for data monitoring, general control */

			case 'b': /* bitmap dump frequency */

				sscanf(optarg, "%d", &PgmFrq);
				break;

			case 'd': /* dump frequency */

				sscanf(optarg, "%d", &Dump_freq);
				break;

			case 'e': /* #experiments */

				sscanf(optarg, "%d", &Totalexperiments);
				break;

			case 'f': /* actual objective function to use */

				sscanf(optarg, "%d", &FctNbr);
				F_nbr = FctNbr - 1;
				if (get_fnm(F_nbr) == (char *) 0 
				|| mpCfgFct(get_fnm(F_nbr), argc, argv) != 0) {
					sprintf(Msg, "Input: -%c %d", 
						s, FctNbr);
					Error(Msg);
				}
				if (!DimFlg) {
					FctDim = f_tab[F_nbr].dim;
				}
				break;

			case 'g': /* maximum #gens without evaluation */

				sscanf(optarg, "%d", &Maxspin);
				break;

			case 'h': /* help information */

				printf("%s", USAGE);
				DspFctHlp();
				exit(1);
				break;
				
			case 'i': /* infile given */
	
				sscanf(optarg, "%s", Infile);
    				if ((fp = fopen(Infile, "r")) != NULL) {
					GetDta(fp);
    					fclose(fp);
				  	InfFlg = 1;
    				}
				else {
        				sprintf(Msg, "Input: can't open %s", 
						Infile);
        				Error(Msg);
				}

				sscanf(Infile, "in.%s", Sfx);
				InzGenFil(Sfx);
				break;

			case 'n': /* dimension of objective function */

				sscanf(optarg, "%d", &FctDim);
				DimFlg = 1;
				break;

			case 'o': /* string of additional options */

				sscanf(optarg, "%s", Options);
				break;

			case 'q': /* directory name query */
	
				QryFlg = 1;
				break;

			case 'r': /* seed for random number generator */

				sscanf(optarg, "%d", &OrigSeed);
				break;

			case 's': /* number of structures saved	*/

				sscanf(optarg, "%d", &Savesize);
				break;

			case 't': /* total #function evaluations */

				sscanf(optarg, "%d", &Totaltrials);
				break;

			case 'v': /* report interval */

				sscanf(optarg, "%d", &Interval);
				break;

			case 'x': /* the suffix for filenames */

				sscanf(optarg, "%s", Sfx);
				break;

			default:  

				sprintf(Msg, "Input: illegal option %c\n", s);
				Error(Msg);
				break;

		} /* end switch */

	} /* end while */

  	for (i = 0; Options[i] != '\0'; i++) {	/* set options */
		SetFlg(Options[i]);
	}
	
	ChkCst();			/* consistency check */

	if (QryFlg) {			/* a directory query; exit */
		strcpy(Sfx, CrtFilNam());
		printf("%s\n", Sfx);
		exit(0);
	}

	if (!strlen(Sfx)) {		/* no suffix given; create one */
		strcpy(Sfx, CrtFilNam());
	}
	InzGenFil(Sfx);

	AllStg();			/* storage allocation */ 
	CrtPgm();			/* bitmap file creation */
    	Seed = OrigSeed;

	if (Traceflag) {		/* echo input parameters */
		WrtDta(stdout);
	}

	if (!InfFlg) {			/* create infile, if not existing */

		if ((fp = fopen(Infile, "w")) != NULL) { 
			strcat(Options, Opt);
			WrtDta(fp);
			fclose(fp);
		}
		else {
            		sprintf(Msg, "Input: can't open %s", Infile);
            		Error(Msg);
		}
	}

	if ((fp = fopen(Outfile, "w")) == NULL) {
           	sprintf(Msg, "Input: can't open %s", Outfile);
            	Error(Msg);
        }
        fclose(fp);
 
} /* end Input() */




void
ChkCst()

	/* check the dependencies of input parameters for consistency;	*/
	/* in they are not consistent, the ga is aborted.		*/
{
	FILE   	       *fp;

	char 		Msg[NSZ];

	double		fabs();

	register int	i;

	int		NewLgt,
			DimChk();

	if (ChrLen < 1) {
		sprintf(Msg, "Input: Chromosome length (%d) too small", ChrLen);
		Error(Msg);
	}

	if ((FctDim > 0) && (ChrLen > INTSIZE)) {
		sprintf(Msg, "Input: Chromosome length (%d) too large", ChrLen);
		Error(Msg);
	}

	if (FctDim == 0) {		/* length calculation */
		Length = ChrLen;
	}
	else {
		Length = ChrLen * FctDim;
	}

	if (Mu > Popsize) {		/* Relations Mu, Rho, Lambda */
		sprintf(Msg, "Input: Warning - Mu correction (%d -> %d)",
				Mu, Popsize);
		if ((fp = fopen(Logfile, "a")) != NULL) {
			fprintf(fp, "%s\n", Msg);
			fclose(fp);
		}
	}

	if (Mu < 1) {
		sprintf(Msg, "Input: Mu (%d) invalid", Mu);
		Error(Msg);
	}

	if (Rho < 1) {
		sprintf(Msg, "Input: Rho (%d) invalid", Rho);
		Error(Msg);
	}

	if (Rho > Mu) {
		sprintf(Msg, "Input: Rho (%d) > Mu (%d)", Rho, Mu);
		Error(Msg);
	}

	if ((C_rate < 0.0) || (C_rate > 1.0)) {
		sprintf(Msg, "Input: C_rate (%f) invalid", C_rate);
		Error(Msg);
	}
			
	if ((C_points > Length/2) || (C_points < 0)) {
		sprintf(Msg, "Input: C_points (%d) invalid", C_points);
		Error(Msg);
	}

	if (Totalexperiments < 1) {
		sprintf(Msg, "Input: Totalexperiments (%d) invalid", 
			Totalexperiments);
		Error(Msg);
	}

	if (Dump_freq < 0) {
		sprintf(Msg, "Input: Dump_freq (%d) invalid", Dump_freq);
			Error(Msg);
	}

	if (Maxspin < 0) {
		sprintf(Msg, "Input: Maxspin (%d) invalid", Maxspin);
		Error(Msg);
	}

	if ((Gapsize < 0.0) || (Gapsize > 1.0)) {
		sprintf(Msg, "Input: Gapsize (%f) invalid", Gapsize);
		Error(Msg);
	}

	if (Interval < 0) {
		sprintf(Msg, "Input: Interval (%d) invalid", Interval);
		Error(Msg);
	}

	if (PgmFrq < 0) {
		sprintf(Msg, "Input: Bitmap frequency (%d) invalid", PgmFrq);
		Error(Msg);
	}

	if ((M_rate < 0.0) || (M_rate > 1.0)) {
		sprintf(Msg, "Input: M_rate (%f) invalid", M_rate);
		Error(Msg);
	}

	if (Popsize < 1) {
		sprintf(Msg, "Input: Popsize (%d) invalid", Popsize);
		Error(Msg);
	}

	if ((Savesize < 1) || (Savesize > Popsize)) {
		sprintf(Msg, "Input: Savesize (%d) invalid", Savesize);
		Error(Msg);
	}

	if (Totaltrials < 1) {
		sprintf(Msg, "Input: Total number of trials (%d) invalid",
			Totaltrials);
		Error(Msg);
	}

	if (Windowsize < 0) {
		sprintf(Msg, "Input: Scaling window size (%d) invalid",
			Windowsize);
		Error(Msg);
	}

	switch (MttScm) {

		case STD_MTT:		/* standard */
			if (RecScm[1] != NOP_REC) {
				sprintf(Msg, 
					"Input: Illegal recombination (%s)",
					RecScm);
				Error(Msg);
			}
			if (NbrMttRts != 0) {
				sprintf(Msg,
					"Input: Illegal mutation rate (%d)",
					NbrMttRts);
				Error(Msg);
			}
			break;

		case ADT_MTT:		/* adaptive */
		case ADX_MTT:
			if ((M_bits > INTSIZE) || (M_bits < 0)) {
				sprintf(Msg, "Input: M_bits (%d) invalid", 
					M_bits);
				Error(Msg);
			}
			if ((NbrMttRts != 1) && (NbrMttRts != FctDim)) {
				sprintf(Msg, 
					"Input: NbrMttRts (%d) inconsistent", 
					NbrMttRts);
				Error(Msg);
			}
			MttLen = NbrMttRts * M_bits;	/* adjust MttLen */
			break;

		case OPT_MTT:		/* optimal mutation rate calculation */
			if (F_nbr != 11) {
				sprintf(Msg, 
					"Input: Mutation scheme (%c) invalid",
					MttScm);
				Error(Msg);
			}
			break;

		default:
			sprintf(Msg, "Input: Mutation scheme (%c) unknown",
				MttScm);
			Error(Msg);
			break;
	}

	for (i = 0; i < strlen(RecScm); i++) {
	
		switch (RecScm[i]) {	/* check recombination schemes */

			case STD_REC:	/* standard */
				break;		
		
			case DCT_REC:	/* discrete */
				if ((f_tab[F_nbr].dim == 0) && (i == 0)) {
					sprintf(Msg, "Input: Discrete recombination not allowed for binary functions");
					Error(Msg);
				}
				break;

			case IMD_REC:	/* intermediate */
			case RID_REC:
				if ((f_tab[F_nbr].dim == 0) && (i == 0)) {
					sprintf(Msg, "Input: Intermediate recombination not allowed for binary functions");
					Error(Msg);
				}
				break;

			case UFM_REC:	/* uniform */
			case NOP_REC:	/* no recombination */
				break;

			default:	
				sprintf(Msg, 
				    "Input: Recombination scheme (%s) unknown", 
					RecScm);
				Error(Msg);
				break;

		} /* end switch */

	} /* end for */

	switch (SltScm) {

		case PRP_SLT:
			break;

		case BZM_SLT:
			if ((CtlPar < 0.8) || (CtlPar > 1.0)) {
				sprintf(Msg, "Input: CtlPar (%f) invalid",
					CtlPar);
				Error(Msg);
			}
			if (ChnLgt < 1) {
				sprintf(Msg, "Input: ChnLgt (%d) invalid",
					ChnLgt);
				Error(Msg);
			}
			break;
	
		case BRK_SLT:
		case IRK_SLT:
			if ((Eta_max < 1.0) || (Eta_max > 2.0)) {
				sprintf(Msg, "Input: Eta_max (%f) invalid",
					Eta_max);
				Error(Msg);
			}
			break;

		case WRK_SLT:
			if (fabs(Eta_max - 1.0) < EPS) {
				sprintf(Msg, "Input: Eta_max (%f) invalid",
					Eta_max);
				Error(Msg);
			}
			if (Mu != Popsize) {
				sprintf(Msg, "Input: Mu (%d) must be equal to popsize (%d)",
					Mu, Popsize);
				Error(Msg);
			}
			if (Rho != 1) {
				sprintf(Msg, "Input: Rho (%d) must be equal to popsize (%d)",
					Rho, Popsize);
				Error(Msg);
			}
			break;

		case MLR_SLT:
			break;

		case MLC_SLT:
			break;

		default:
			sprintf(Msg, "Input: Selection scheme (%c) unknown",
				SltScm);
			Error(Msg);
			break;
	}

	if (DimChk(F_nbr) && 
	   (FctDim != f_tab[F_nbr].dim)) { /* wrong dimension */
		sprintf(Msg, "Input: Dimension error (-n %d vs. default %d)",
			FctDim, f_tab[F_nbr].dim);
		Error(Msg);
	}

	if (!DimChk(F_nbr)) { 		/* dimension may change */

		if (FctDim <= 0) {	/* wrong dimension */
			sprintf(Msg, "Input: Dimension error (%d)", FctDim);
			Error(Msg);
		}
		
		if ((NewLgt = INDEX(FctDim)) != Length) {
			sprintf(Msg, "Input: Warning - Length correction (%d -> %d)",
				Length, NewLgt);
			if ((fp = fopen(Logfile, "a")) != NULL) {
				fprintf(fp, "%s\n", Msg);
				fclose(fp);
			}
			Length = NewLgt;	/* correct length, dimension */
		}
		f_tab[F_nbr].dim = FctDim;	/* dimension update */
	}

} /* end ChkCst() */




void
AllStg()

{
	extern BUFFER		*CrtBuf();

	register int		 i;

	int			 Flg = 0,
				*AllInt();

	char		 	*calloc(),
				*malloc(),
				*AllChr();

	STRUCTURE		*AllPop();

	BESTSTRUCT		*AllBst();

	double			*AllDbl();

    	/* allocate storage for variable sized structures */

	Old = AllPop(Popsize);		/* population allocation */
	New = AllPop(Popsize);
 
    	for (i = 0; i < Popsize; i++) { /* allocate space for individuals */

		Old[i].Gene = AllChr(Length);	/* allocate genotype space */
		New[i].Gene = AllChr(Length);

		Flg = (FctDim > 0);		/* space for objvars needed */
		Old[i].ObjVar = AllDbl(Flg ? FctDim : 0);
		New[i].ObjVar = AllDbl(Flg ? FctDim : 0);

		Flg = (f_tab[F_nbr].MrkFct == PERM); 	/* space for perms */
		Old[i].PerMut = AllInt(Flg ? FctDim : 0);
		New[i].PerMut = AllInt(Flg ? FctDim : 0);

		Flg = (NbrMttRts > 0); 		/* space for mutation rates */
		Old[i].MttGen = AllChr(Flg ? MttLen : 0);
		New[i].MttGen = AllChr(Flg ? MttLen : 0);
		Old[i].MttRts = AllDbl(Flg ? NbrMttRts : 0);
		New[i].MttRts = AllDbl(Flg ? NbrMttRts : 0);

		Flg = 0;
	}

    	if (Windowsize) {	/* moving value for Worst */
		Window = AllDbl(Windowsize);
	}
 
    	if (Savesize) {		/* best structures array */

		Bestset = AllBst(Savesize);

		for (i = 0; i < Savesize; i++) {

			Bestset[i].Gene   = AllChr(Length);
			Bestset[i].ObjVar = AllDbl(FctDim);

			Flg = (NbrMttRts > 0);	/* space for mutation rates */
			Bestset[i].MttGen = AllChr(Flg ? MttLen : 0);
			Bestset[i].MttRts = AllDbl(Flg ? NbrMttRts : 0);

			Flg = (f_tab[F_nbr].MrkFct == PERM);
			Bestset[i].PerMut = AllInt(Flg ? FctDim : 0);
		}
	}

	if (NbrMttRts > 0)  { 		/* alloc mutation rate buffers */
		for (i = 0; i < BUFCNT; i++) {
			MttBuf[i] = CrtBuf(MttFil[i], NbrMttRts);
			ChgFmt(MttBuf[i], 0, "%5.0f ");
		}
	}

	if (FctDim > 0)  {	/* allocate object variable buffers */
		for (i = 0; i < BUFCNT; i++) {
			ValBuf[i] = CrtBuf(ValFil[i], FctDim);
			ChgFmt(ValBuf[i], 0, "%5.0f ");
		}
	}
	PfmBuf = CrtBuf(Outfile, DTACOL);
	PrbBuf = CrtBuf(PrbFil, Popsize);

} /* end AllStg() */



STRUCTURE *
AllPop(Siz)

register int		Siz;

{	
	/*
	 *	Allocate Siz individuals.
	 */

	STRUCTURE	*Pop;
	char		*calloc();

    	if ((Pop = (STRUCTURE *) calloc((unsigned) Siz, 
					sizeof(STRUCTURE))) == NULL) {
		Error("Input: No space");
	}
	return Pop;

} /* end AllPop */



BESTSTRUCT *
AllBst(Siz)

register int		Siz;

{	
	/*
	 *	Allocate Siz structures Beststruct.
	 */

	BESTSTRUCT	*Bst;
	char		*calloc();

    	if ((Bst = (BESTSTRUCT *) calloc((unsigned) Siz, 
					sizeof(BESTSTRUCT))) == NULL) {
		Error("Input: No space");
	}
	return Bst;

} /* end AllBst */


char *
AllChr(Len)

register int	 	 Len;

{
	/*
	 *	Allocate Len characters.
	 */

	char		*Chr,
			*calloc();

	if (Len == 0) 	return((char *) NULL);
	if ((Chr = calloc((unsigned) Len, sizeof(char))) == NULL) {
		      Error("Input: No space");
	}
	return Chr;

} /* end AllChr */



int *
AllInt(Len)

register int	 	 Len;

{
	/*
	 *	Allocate Len integers.
	 */

	char		*calloc();
	int		*Int;

	if (Len == 0) 	return((int *) NULL);
	if ((Int = (int *) calloc((unsigned) Len, sizeof(int))) == NULL) {
		      Error("Input: No space");
	}
	return Int;

} /* end AllInt */



double *
AllDbl(Len)

register int	 	 Len;

{
	/*
	 *	Allocate Len doubles.
	 */

	char		*calloc();
	double		*Dbl;

	if (Len == 0)	return((double *) NULL);
	if ((Dbl = (double *) calloc((unsigned) Len, sizeof(double))) == NULL) {
		      Error("Input: No space");
	}
	return Dbl;

} /* end AllDbl */



void
FreStg()

{
	extern int		DelBuf();

	register int		i;

    	/* free storage for variable sized structures */

	for (i = 0; i < Popsize; i++) {	/* free space for individuals */
		free((char *) Old[i].Gene);
		free((char *) New[i].Gene); 
		if (Old[i].ObjVar != (double *) NULL) {
			free((char *) Old[i].ObjVar);
			free((char *) New[i].ObjVar);
		}
		if (Old[i].MttGen != (char *) NULL) {
			free((char *) Old[i].MttGen);
			free((char *) New[i].MttGen);
		}
		if (Old[i].MttRts != (double *) NULL) {
			free((char *) Old[i].MttRts);
			free((char *) New[i].MttRts);
		}
		if (Old[i].PerMut != (int *) NULL) {
			free((char *) Old[i].PerMut);
			free((char *) New[i].PerMut);
		}
	}

    	if (Windowsize) 
		free((char *) Window);

    	if (Savesize) {
    		for (i = 0; i < Savesize; i++) {
			free((char *) Bestset[i].Gene);
			if (Bestset[i].ObjVar != (double *) NULL) {
				free((char *) Bestset[i].ObjVar);
			}
			if (Bestset[i].MttGen != (char *) NULL) {
				free((char *) Bestset[i].MttGen);
			}
			if (Bestset[i].MttRts != (double *) NULL) {
				free((char *) Bestset[i].MttRts);
			}
			if (Bestset[i].PerMut != (int *) NULL) {
				free((char *) Bestset[i].PerMut);
			}
		}
		free((char *) Bestset);
	}

	for (i = 0; i < BUFCNT; i++) {		/* free global buffers */
		if (NbrMttRts > 0)  {
			DelBuf(MttBuf[i]);	
		}
		if (FctDim > 0) {
			DelBuf(ValBuf[i]);
		}
	}
	DelBuf(PfmBuf);
	DelBuf(PrbBuf);

} /* end FreStg() */



/*** end of file ***/
