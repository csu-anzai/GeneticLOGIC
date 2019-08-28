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
 *	file:	ftab.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   maintaining of a table of functions which can be used
 *		in the ga directly.
 *
 *
 *	$Id$
 *
 */


#include "../extern.h"


	/* all objective functions must be declared here by a statement	*/
	/* of the common form						*/
	/*								*/
	/* extern int <f_i>();						*/
	/*								*/
	/* It is suggested, to name functions by the prefix f, followed	*/
	/* simply by a _ and a number.					*/

extern double	f_01();		/* Sphere model				*/
extern double	f_02();		/* Rosenbrock's function		*/
extern double	f_03();		/* Step function			*/
extern double	f_04();		/* Quartic function with Gaussian noise	*/
extern double	f_05();		/* Shekel's foxholes			*/
extern double	f_06();		/* Schwefel's problem No. 1.2		*/
extern double	f_07();		/* Generalized Rastrigin's function	*/
extern double	f_08();		/* Sphere model, changing environment	*/
extern double	f_09();		/* Ackley's multimodal ascending path	*/
extern double	f_10();		/* Krolak's 100 city TSP		*/
extern double	f_11();		/* low autocorrelation binary sequences	*/
extern double	f_12();		/* Hamming distance to string 0...0	*/
extern double	f_13();		/* Fractal objective function		*/
extern double	f_14();		/* Fully deceptive function		*/
extern double	f_15();		/* Weighted sphere model   		*/
extern double	f_16();		/* Fletcher Powell			*/
extern double	f_17();		/* Fletcher Powell, version by rudolph	*/
extern double	f_18();		/* Shekel_5				*/
extern double	f_19();		/* Shekel_7				*/
extern double	f_20();		/* Shekel_10				*/
extern double	f_21();		/* Griewank, n = 2			*/
extern double	f_22();		/* Griewank, n = 10			*/
extern double	f_23();		/* Galar, two optima			*/
extern double	f_24();		/* Kowalik, accodring to Schwefel 1977	*/

FUNCTION f_tab[] = {

	{30, VRBL, REAL, -5.12, 5.12, f_01, "f_01", 
	"Sphere model", 
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{2,  VRBL, REAL, -5.12, 5.12, f_02, "f_02", 
	"Rosenbrock's function",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{5,  STRC, REAL, -5.12, 5.12, f_03, "f_03", 
	"Step function",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{30, VRBL, REAL, -1.28, 1.28, f_04, "f_04", 
	"Quartic function with Gaussian noise",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{2,  STRC, REAL, -65.536, 65.536, f_05, "f_05", 
	"Shekel's foxholes",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{20, VRBL, REAL, -65.536, 65.536, f_06, "f_06", 
	"Schwefel's problem 1.2",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{20, VRBL, REAL, -5.12, 5.12, f_07, "f_07", 
	"Generalized Rastrigin's function",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{30, VRBL, REAL, -5.12, 5.12, f_08, "f_08", 
	"Sphere model, changing environment",
	{250.0, 4.0, 0.0}, 
	{"Optimum switching interval (generations)", 
	 "Alternative optimum location (all x_i)",
	 (char*) NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{20, VRBL, REAL, -32.768, 32.768, f_09, "f_09",
	"Ackley's multimodal descending path",
	{20.0, 0.2, 2.0 * M_PI},
	{"Factor A", "Factor B", "Factor C"},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{100, VRBL, PERM, -5.12, 5.12, f_10, "f_10", 
	"Krolak's 100 city TSP",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{0, EXCP, BINY, 0.0, 0.0, f_11, "f_11", 
	"Low autocorrelation binary sequences",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{0, EXCP, BINY, 0.0, 0.0, f_12, "f_12",
	"Hamming distance to the string 0...0",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{30, VRBL, REAL, -5.12, 5.12, f_13, "f_13",
	"Fractal Weierstrass-Mandelbrot function",
	{1.1, 1.5, (double) RDS_DEF},
	{"Box Dimension D", "Constant b", 
	 "Private RNG Seed"},
	{{0, {"All Angles 0", "Random Angles in [0,2Pi]", (char *) NULL}, 
	 "Angle Determination Mode"},
	 {0, {"Sphere Model", "Exponential Function", "No Superposition"}, 
	 "Superposition Function"},
	 {0, {"Subtraction Method [BL80]", "Division Method (Schwefel)", 
	      "No Elimination"}, 
	 "Trend Elimination Mode"}}},

	{0, EXCP, BINY, 0.0, 0.0, f_14, "f_14",
	"Fully deceptive binary function",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{30, VRBL, REAL, -5.12, 5.12, f_15, "f_15",
	"Weighted sphere model",
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{5, STRC, REAL, -M_PI, M_PI, f_16, "f_16",
	"Problem according to Fletcher and Powell (fixed matrix (a_ij), (b_ij))",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{5, VRBL, REAL, -M_PI, M_PI, f_17, "f_17",
	"Problem according to Fletcher and Powell (variable matrix (a_ij), (b_ij))", 
	{(double) 1472921, 0.0, 0.0},
	{"Private RNG Seed", (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{4, STRC, REAL, 0.0, 10.0, f_18, "f_18",
	"Shekel-5 according to Toern / Zilinskas: Global Optimization",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{4, STRC, REAL, 0.0, 10.0, f_19, "f_19",
	"Shekel-7 according to Toern / Zilinskas: Global Optimization",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{4, STRC, REAL, 0.0, 10.0, f_20, "f_20",
	"Shekel-10 according to Toern / Zilinskas: Global Optimization",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{2, STRC, REAL, -100.0, 100.0, f_21, "f_21",
	"Griewank-2 according to Toern / Zilinskas: Global Optimization",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{10, STRC, REAL, -600.0, 600.0, f_22, "f_22",
	"Griewank-10 according to Toern / Zilinskas: Global Optimization",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{10, VRBL, REAL, -2.0, 2.0, f_23, "f_23",
	"Galar's function with two optima",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{4, STRC, REAL, -5.0, 5.0, f_24, "f_24",
	"Nonlinear parameter estimation (Kowalik 1967, Schwefel 1977)",
	{0.0, 0.0, 0.0},
	{(char*) NULL, (char*) NULL, (char*)NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}},

	{0, VRBL, REAL, 0.0, 0.0, NULL, DUMMY, DUMMY,
	{0.0, 0.0, 0.0}, 
	{(char*) NULL, (char*) NULL, (char*) NULL},
	{{0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL},
	 {0, {(char *) NULL, (char *) NULL, (char *) NULL}, (char *) NULL}}}
	  

};


int
get_fdx(fnm)
char	*fnm;

	/* get the index of the function with filename <fnm>.c, if it	*/
	/* is included in f_tab. Otherwise, -1 is returned.		*/

{
	register int i    = 0;
	register int stop = 0;
	
	while (!stop && (strcmp(fnm, f_tab[i].fnm) != 0)) {

		stop = (strcmp(f_tab[i].fnm, DUMMY) == 0);
		i++;
	}

	if (stop)
		return -1;
	return i;
}


char *
get_fnm(i)
int	i;

	/* get the filename of the function with index i, if i is a	*/
	/* valid index. Otherwise, (char *) 0 is returned.		*/

{
	int	cnt_fct();

	if ((i > -1) && (i <= cnt_fct()))
		return (f_tab[i].fnm);

	return ((char *) 0);
}


int 
cnt_fct()

	/* return the highest index of function entries in f_tab.	*/

{
	register int i = 0;

	while (strcmp(f_tab[i++].fnm, DUMMY) != 0);

	return (i - 1);
}


int
DspFctHlp()

{
	register int	i,	
			j;


	int		cnt_fct(),
			_ftDspFctHlp();

	printf("Objective functions actually implemented:\n\n");
	for (i = 0, j = cnt_fct(); i < j; i++) {
		_ftDspFctHlp(i);
		printf("\n");
	}
	return (0);

} /* end DspFctHlp */




int
_ftDspFctHlp(i)
register int		i;

{
	/*
	 *  Print specific help information for the objective function
	 *  with index i in the function table.
	 */

	char		Chr;

	int		k,
			l,
			DimChk();

	printf("%s:\t %s\n", f_tab[i].fnm, f_tab[i].descr);
	
	/*
	 *  Display the general function informations, i.e. dimension
	 *  and interval bounds.
	 */

	if (f_tab[i].dim > 0) {
		if (DimChk(i))
			printf("\t n = %3d !\t\t", f_tab[i].dim);
		else
			printf("\t n = %3d\t\t", f_tab[i].dim);
		printf("  %G <= xi <= %G\n", f_tab[i].umin, f_tab[i].umax);
	}

	/*
   	 *  Display the continuous valued options, if any.
	 */

	if (f_tab[i].CstDsc[0] != (char *) NULL) {
		printf("\t Continuous valued options:\n");
		for (	k = 0, Chr = 'p'; 
			k < sizeof(f_tab[i].CstVal) / sizeof(double)
			&& f_tab[i].CstDsc[k] != (char *) NULL;
			k++, Chr++) {
			printf("\t '-%c': %s (default: % g)\n", Chr, 
				f_tab[i].CstDsc[k], f_tab[i].CstVal[k]);
		}
	}

	/*
	 *  Display the discrete valued options, if any.
	 */

	if (f_tab[i].FctOpt[0].OTtl != (char *) NULL) {
		printf("\t Discrete valued options (0,...,%d):\n", MOPT);
		for (	k = 0, Chr = 's';
			k < MOPT && f_tab[i].FctOpt[k].OTtl != (char *) NULL;
			k++, Chr++) {
			printf("\t '-%c': %s (default: %d)\n", Chr,
				f_tab[i].FctOpt[k].OTtl, 
				f_tab[i].FctOpt[k].OVal);
			for (	l = 0;
				l < MOPT 
				&& f_tab[i].FctOpt[k].ODsc[l] != (char *) NULL;
				l++) {
				printf("\t\t %d: %s\n", 
					l, f_tab[i].FctOpt[k].ODsc[l]);
			}
		}
	}

	/*
	 *  Display the interval bound change options, if the object
	 *  variables are real-valued.
	 */

	if (f_tab[i].MrkFct == REAL) {
		printf("\t Additional options:\n");
		printf("\t '-L': Lower interval bound ");
		printf("for object variables\n");
		printf("\t '-H': Upper interval bound ");
		printf("for object variables\n");
	}
	return(0);

} /* end _ftDspFctHlp */




int
DimChk(FctInd)
register int	FctInd;		/* index of function */

{	/* true, if dimension unchangeable */

	return (   (f_tab[FctInd].DimObl == STRC)
		|| (f_tab[FctInd].DimObl == EXCP));
}


#define	FCTOPT		"p:q:r:s:t:u:L:H:h"

#define FCTUSG		"%s\n\n\
Function-specific options:\n\
\t-p val1 \t\t# first  continuous option for function\n\
\t-q val2 \t\t# second continuous option for function\n\
\t-r val3 \t\t# third  continuous option for function\n\
\t-s val1 \t\t# first  discrete (0,1,2) option for function\n\
\t-t val2 \t\t# second discrete (0,1,2) option for function\n\
\t-u val3 \t\t# third  discrete (0,1,2) option for function\n\
\t-L lowbd\t\t# lower  interval bound\n\
\t-H hghbd\t\t# higher interval bound\n\
\t-h      \t\t# this help information\n\
\n"



int
_ftInzFct(FctInd, argc, argv)
int		FctInd;		/* Index of function */
int		argc;
char	      **argv;

{
	extern void 		Error();
	extern int 		getopt();
	extern int		optind, optopt;
	extern int		opterr, optbad;
	extern char	       *optarg;
	int			Opt,
				FOpt,
				LowFlg = 0,
				HghFlg = 0,
				_ftDspFctHlp();
	char			Msg[NSZ];
	double			Low,
				Hgh;

	Low = f_tab[FctInd].umin;
	Hgh = f_tab[FctInd].umax;

	while ((Opt = getopt(argc, argv, FCTOPT)) != -1) {
		switch (Opt) {
		    	case 'p': /* first external constant for function */
				sscanf( optarg, "%lf", 
					&(f_tab[FctInd].CstVal[0]));
				break;
			
		    	case 'q': /* second external constant for function */
				sscanf( optarg, "%lf", 
					&(f_tab[FctInd].CstVal[1]));
				break;
			
		    	case 'r': /* third external constant for function */
				sscanf( optarg, "%lf", 
					&(f_tab[FctInd].CstVal[2]));
				break;

			case 's': /* first three-value option for function */
				sscanf(optarg, "%d", &FOpt);
				if (OptChk(FOpt) != 0) {
					sprintf(Msg, "_ftInzFct: -%c %d",
						Opt, FOpt);
					Error(Msg);
				}
				f_tab[FctInd].FctOpt[0].OVal = FOpt;
				break;

			case 't': /* second three-value option for function */
				sscanf(optarg, "%d", &FOpt);
				if (OptChk(FOpt) != 0) {
					sprintf(Msg, "_ftInzFct: -%c %d",
						Opt, FOpt);
					Error(Msg);
				}
				f_tab[FctInd].FctOpt[1].OVal = FOpt;
				break;

			case 'u': /* third three-value option for function */
				sscanf(optarg, "%d", &FOpt);
				if (OptChk(FOpt) != 0) {
					sprintf(Msg, "_ftInzFct: -%c %d",
						Opt, FOpt);
					Error(Msg);
				}
				f_tab[FctInd].FctOpt[2].OVal = FOpt;
				break;

			case 'L': /* lower interval bound */
				sscanf(optarg, "%lf", &Low);
				if (f_tab[FctInd].MrkFct != REAL) {
					sprintf(Msg, "_ftInzFct: -%c f_%d",
						Opt, FctInd);
					Error(Msg);
				}
				LowFlg = 1;
				break;

			case 'H': /* higher interval bound */
				sscanf(optarg, "%lf", &Hgh);
				if (f_tab[FctInd].MrkFct != REAL) {
					sprintf(Msg, "_ftInzFct: -%c f_%d",
						Opt, FctInd);
					Error(Msg);
				}
				HghFlg = 1;
				break;

			case 'h': /* help information */
				printf("%s\n\n", GENESIS);
				_ftDspFctHlp(FctInd);
				exit(1);
				break;
		} /* end switch */
	} /* end while */

	if (optind < argc) {
		sprintf(Msg, "%s: too many parameters", *argv);
		Error(Msg);
	}
	if (LowFlg) f_tab[FctInd].umin = Low;
	if (HghFlg) f_tab[FctInd].umax = Hgh;
	if (Low > Hgh) {
		sprintf(Msg, "%s: Interval bounds invalid ([%e,%e])", 
			f_tab[FctInd].fnm, Low, Hgh);
		Error(Msg);
	}
	return(0);

} /* end _ftInzFct */




int
OptChk(Opt)
register int		Opt;

{
	/*
	 *  check whether option is valid.
	 */

	if ((Opt > -1) && (Opt <= MOPT))
		return (0);
	return (1);

} /* end OptChk */




/** end of file **/
