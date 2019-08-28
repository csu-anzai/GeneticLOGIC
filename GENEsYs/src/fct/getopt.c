/*
 *	getopt.c	Ho, 19.Mar.'89				MWC 3.0
 *			Ho, 18.Dec.'90				SunOS 4.0.3
 *
 *	BSD compatability function.
 *	get option letter from argument vector.
 *
 *	$Log: getopt.c,v $
 *	Revision 1.2  1991/01/15  11:51:08  iwan
 *	added option introducer variable opt1st
 *
 *	Revision 1.1  1990/12/31  16:17:59  iwan
 *	Initial revision
 *
 */

#ifndef	lint
static	char	_rcsid[] =
"$Id: getopt.c,v 1.2 1991/01/15 11:51:08 iwan Exp iwan $";
#endif

#include <stdio.h>


#define BADCH	(int)'?'	/* error return */
#define EMSG	""


int	opt1st = '-';		/* option introducer */
int	optbad = 0;		/* bad option flag, additional to BADCH */
int	opterr = 1;		/* enable error messages */
int	optind = 1;		/* index into parent argv vector */
int	optopt; 		/* character checked for validity */
char	*optarg;		/* argument associated with option */

static	char	*optstr = EMSG;	/* option letter processing */


/*
 *	BSD compatibility function getopt()
 */

int
getopt(nargc,nargv,ostr)
int	nargc;			/* argc */
char	**nargv;		/* argv */
char	*ostr;
{
	register char	*oli;		/* option letter list index */
	extern char	*strchr();

	if (optind < 1) {			/* init globals */
		optind = 1;
		optstr = EMSG;
	}
	optbad = 0;

	if (!*optstr) {				/* update scanning pointer */
		if (optind >= nargc
		|| *(optstr = nargv[optind]) != opt1st
		|| !*++optstr) {
			return(EOF);
		}
		if (*optstr == opt1st) {	/* found "--" */
			optstr= EMSG;
			++optind;
			return(EOF);
		}
	}					/* option letter okay? */
	if ((optopt = (int)*optstr++) == (int)':'
	||  !(oli = strchr(ostr,optopt))) {
		if (!*optstr)	++optind;
		if (opterr) {
			fprintf(stderr, "%s: illegal option '%c%c'\n",
				*nargv, opt1st,optopt);
		}
		optbad = 1;
		return(BADCH);
	}
	if (*++oli != ':') {			/* don't need argument */
		optarg = (char *)NULL;
		if (!*optstr) ++optind;
	}
	else {					/* need an argument */
		if (*optstr) optarg = optstr;	/* no white space */
		else if (nargc <= ++optind) {	/* no arg */
			optstr = EMSG;
			if (opterr) {
				fprintf(stderr,
		 		"%s: option %c%c requires an argument\n",
					*nargv, opt1st,optopt);
			}
			return(BADCH);
		}
		else {
			optarg = nargv[optind];	/* white space */
		}
		optstr = EMSG;
		++optind;
	}
	return(optopt); 		/* dump back option letter */
} /* end getopt */


/*
 *	skip to end of option list
 */

int
skip_getopt(argc,argv,ostr)
int	argc;
char	**argv;
char	*ostr;
{
	register int	ErrCnt = 0;

	while (getopt(argc,argv,ostr) != EOF) {
		if (optbad != 0 && opterr) {
			ErrCnt++;
			fprintf(stderr, "%s: illegal option '%c%c'\n",
				*argv, opt1st,optopt);
		}
	}
	return(ErrCnt);
} /* end skip_getopt */


/*
 *	allow for NESTED application of getopt():
 */

struct	state_getopt
{
	struct state_getopt	*sg_next;
	int			 sg_optind;
	char			*sg_optstr;
};

static	struct	state_getopt	*prev_state =	NULL;


/*
 *	save getopt()-state
 */

/*ARGSUSED*/
init_getopt(argc,argv)
int	argc;
char	**argv;
{
	extern   char			*malloc();
	register struct state_getopt	*state;

	if ((state= (struct state_getopt *)malloc(sizeof(struct state_getopt)))
								== NULL) {
		fprintf(stderr, "%s: out of memory for getopt()-state\n",
			*argv	);
		exit(1);
	}

	state->sg_next  =	prev_state;
	state->sg_optind=	optind;
	state->sg_optstr=	optstr;

	prev_state	=	state;
	optind		=	-1;		/* force init */
} /* end init_getopt */


/*
 *	restore getopt()-state
 */

/*ARGSUSED*/
term_getopt(argc,argv)
int	argc;
char	**argv;
{
	register struct state_getopt	*state = prev_state;

	if (prev_state == NULL) {
		fprintf(stderr,
			"%s: term_getopt() without matching init_getopt()\n",
			*argv);
	exit(1);
	}

	optind		= state->sg_optind;
	optstr		= state->sg_optstr;
	prev_state	= state->sg_next;

	free((char *)state);
} /* end term_getopt */

