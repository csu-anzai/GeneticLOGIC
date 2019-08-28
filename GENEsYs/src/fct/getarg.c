/*
 *	getarg.c	Ho, 12.July'90				SunOS 4.0.3
 *			Ho, 30.Sep.'90
 *			Ho, 04.Oct.'90
 *			Ho, 31.Oct.'90
 *			Ho, 06.Nov.'90
 *			Ho, 07.Dec.'90
 *			Ho, 10.Dec.'90
 *			Ho, 21.Dec.'90
 *			Ho, 23.Dec.'90
 *			Ho, 28.Dec.'90
 *
 *	get program arguments
 *
 *	$Log: getarg.c,v $
 *	Revision 1.5  1991/02/22  19:26:50  iwan
 *	enforced new module naming conventions
 *
 *	Revision 1.4  1991/01/24  21:25:29  iwan
 *	fixed unavailable option '-z'
 *
 *	Revision 1.3  1991/01/21  16:02:49  iwan
 *	added declaration of dump file extension (_mpExtDmp)
 *
 *	Revision 1.2  1991/01/20  21:32:27  iwan
 *	added test for extra command line arguments
 *
 *	Revision 1.1  1990/12/31  16:16:50  iwan
 *	Initial revision
 *
 */

#ifndef	lint
static	char	_rcsid[] =
"$Id: getarg.c,v 1.5 1991/02/22 19:26:50 iwan Exp iwan $";
#endif

#include <string.h>
#include "arglst.h"
#include "../extern.h"

int	  _mpArgC = 0;			/* copy of the well-known argc,argv[] */
char	**_mpArgV = NULL;


/*
 *	create option list from command line
 *	recognized word structures:
 *
 *		name{ hello world }
 *		name {hello world}
 *		name { hello world }
 */

ArgLst_t *
mpCrtOptLst(OptArg,argc,argv)
char	*OptArg;			/* identical to optarg */ 
int	 argc;				/* argument count */
char	**argv;				/* argument list */
{
	extern	ArgLst_t *_mpCrtOptLst();
	ArgLst_t	 *al = alCrtArgLst(OptArg);

	return( _mpCrtOptLst(al,OptArg,argc,argv) );
} /* end mpCrtOptLst */


ArgLst_t *
_mpCrtOptLst(al,OptArg, argc,argv)
register ArgLst_t	*al;		/* pre-defined argument list */
char	 		*OptArg;	/* identical to optarg */ 
int			 argc;		/* argument count */
char			**argv;		/* argument list */
{
	extern	int		 getopt();
	extern	int		 optind, optopt;
	extern	char		*optarg;
	extern	ArgLst_t	*_alLodArgLst();
	extern	char		*smDftFilNam();
	int			 Len;
	register char		*p;

	if (al->al_argv[0][(Len= strlen(OptArg))-1] == C_LBRACE) {
		al->al_argv[0][Len-1]= '\0';
	}
	
	/*
	 *	load monitor related command line options
	 */

	if (OptArg[strlen(OptArg)-1] != C_LBRACE) {	/* no left brace */
		if (optind >= argc || argv[optind][0] != C_LBRACE) {
			return(al);
		}
		if (argv[optind][1] != '\0') {		/* data behind brace */
			al= alAddArg(al,argv[optind]+1);
		}
		optind++;
	}

	/*	scan argument list	*/

	for (p= argv[optind]; optind < argc; p= argv[++optind]) {
		if (strcmp(p,S_RBRACE) == 0) {		/* lonely brace */
			break;
		}
		al = alAddArg(al,p);
		Len= strlen(p);

		if (p[Len-1] == C_RBRACE) {		/* trailing brace */
			al->al_argv[al->al_argc-1][Len-1]= '\0';
			break;				/* ... stripped off */
		}
	}

	if (optind >= argc) {				/* no brace found */
		fprintf(stderr, "%s: missing '%c' in option list for %s\n",
			*argv, C_RBRACE,al->al_argv[0]	);
		mpDltOptLst(al);
		return(NULL);
	}
	optind++;					/* advance to next */

	return(al);
} /* end _mpCrtOptLst */


int
mpDltOptLst(al)
ArgLst_t		*al;			/* option list */
{
	return( -(alDltArgLst(al) !=0) );
} /* end mpDltOptLst */



int					/* index to next argument */
mpCfgFct(OptArg, argc, argv)
char		       *OptArg;			/* monitor name */
int	 		argc;			/* argument count */
char		      **argv;			/* argument list */
{
	extern void		 Error();
	extern int 		 _ftInzFct();
	register ArgLst_t	*al;
	char			 Str[NSZ];
	int			 Argc;
	char		       **Argv;

	sprintf(Str, "%d", F_nbr);
	al= alCrtArgLst(Str);

	/*
	 *	add other options
	 */

	if ((al= _mpCrtOptLst(al,OptArg,argc,argv)) == NULL) {
		sprintf(Str, "%s: failed to get options of function %d\n",
			*argv, FctNbr);
		Error(Str);
	}
	Argc = al->al_argc;
	Argv = al->al_argv;

	init_getopt(Argc, Argv);
	if (_ftInzFct(F_nbr, Argc, Argv) != 0) {
		sprintf(Str, "%s: processing of function options failed %d\n",
			*argv, FctNbr);
		Error(Str);
	}
	term_getopt(Argc, Argv);

	return(0);

} /* end mpCfgFct */


