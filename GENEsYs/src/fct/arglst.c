/*
 *	arglst.c	Ho, 04.Oct.'90				SunOS 4.0.3
 *			Ho, 30.Oct.'90
 *			Ho, 07.Dec.'90
 *			Ho, 19.Dec.'90
 *
 *	argument lists
 *
 *	$Log: arglst.c,v $
 *	Revision 1.2  1991/02/22  19:27:15  iwan
 *	replaced stdio.h by io.h
 *
 *	Revision 1.1  1990/12/31  16:14:23  iwan
 *	Initial revision
 *
 */

#ifndef	lint
static	char	_rcsid[] =
"$Id: arglst.c,v 1.2 1991/02/22 19:27:15 iwan Exp iwan $";
#endif

#include <string.h>
#include "arglst.h"
#include "../extern.h"


/*
 *	create argument list
 */

ArgLst_t *
alCrtArgLst(Arg0)
char	*Arg0;
{
	extern	char	*calloc();
	extern	char	*strdup();
	ArgLst_t	*al;

	if (Arg0 == NULL) {
		Arg0= "<NULL>";
	}

	if ((al= (ArgLst_t *)calloc(1,sizeof(*al))) == NULL
	||  (al->al_argv= (char **)calloc(2,sizeof(char *))) == NULL) {
		fprintf(stderr, "out of memory for argument list\n");
		exit(1);
	}
	al->al_argc= 1;
	al->al_argv[0]= strdup(Arg0);
	al->al_argv[1]= NULL;

	alRstArgLst(al);		/* reset retrieval of argument list */
	
	return(al);
} /* end alCrtArgLst */


/*
 *	entry to argument list
 */

ArgLst_t *
alAddArg(al,Arg)
register ArgLst_t	*al;		/* argument list */
char			*Arg;		/* argument to add */
{
	extern	 char	*realloc();
	extern	 char	*strdup();
	register int	argc;

	argc= ++al->al_argc;
	al->al_argv= (char **)realloc((char *)al->al_argv,
					(unsigned)(argc+1)*sizeof(char *));
	if (al->al_argv == NULL) {
		fprintf(stderr, "out of memory for argument list\n");
		exit(1);
	}
	al->al_argv[argc-1]= strdup(Arg);
	al->al_argv[argc  ]= NULL;

	return(al);
} /* end alAddArg */


/*
 *	retrieve argument list
 */

char	*
alNxtArg(al)
register ArgLst_t	*al;
{
	if (al->al_next >= al->al_argc) {
		return(NULL);
	}
	return( al->al_argv[al->al_next++] );
} /* end alNxtArg */


/*
 *	reset argument list
 */

ArgLst_t *
alRstArgLst(al)
register ArgLst_t	*al;
{
	al->al_next= 1;
	return(al);
} /* end alRstArgLst */


/*
 *	clear argument list
 */

ArgLst_t *
alClrArgLst(al)
register ArgLst_t	*al;		/* argument list */
{
	extern	char	*realloc();

	al->al_argc= 1;
	al->al_argv= (char **)realloc((char *)al->al_argv,
				(unsigned)(al->al_argc+1)*sizeof(char *));
	if (al->al_argv == NULL) {
		fprintf(stderr, "out of memory for argument list\n");
		exit(1);
	}
	al->al_argv[al->al_argc]= NULL;

	return(al);
} /* end alClrArgLst */


/*
 *	close/deallocate argument list
 */

int
alDltArgLst(al)	
ArgLst_t	*al;
{
	register char	**av;

	for (av= al->al_argv; *av != NULL; av++) {
		free(*av);
	}
	free((char *)al);

	return(0);
} /* end alDltArgLst */



/*
 *	load argument list from file
 */

ArgLst_t *
alLodArgLst(al,ArgF)
ArgLst_t	*al;			/* argument list */
char		*ArgF;			/* input file name */
{
	extern	char	*strdup();
	ArgLst_t	*_alLodArgLst();

	if (al == NULL) {		/* create new argument list */
		al= alCrtArgLst(ArgF);
		if (_alLodArgLst(al,ArgF) == NULL) {
			alDltArgLst(al);
			return(NULL);
		}
		return(al);
	}

	/* recycle old argument list */

	alClrArgLst(al);		/* clear old contents */
	free(al->al_argv[0]);		/* release previous argv name */
	al->al_argv[0]= strdup(ArgF);	/* ... and replace by new one */

	return( _alLodArgLst(al,ArgF) );
} /* end alLodArgLst */


ArgLst_t *
_alLodArgLst(al,ArgF)
ArgLst_t	*al;			/* argument list */
char		*ArgF;			/* input file name */
{
	FILE	*fd;
	int	LinCnt;
	int	LinLen;
	char	LinBfr[256];

	if (ArgF == NULL || *ArgF == '\0'
	||  (fd= fopen(ArgF,"r")) == NULL) {
		return(NULL);
	}

	for (LinCnt= 1; fgets(LinBfr,sizeof(LinBfr),fd) != NULL; LinCnt++) {
		LinLen= strlen(LinBfr);
		if (LinBfr[LinLen-1] != '\n') {
			fprintf(stderr,
			"%s: overfull line %d ignored in argument file %s\n",
				al->al_argv[0], LinCnt, ArgF		);
		}
		else {
			LinBfr[LinLen-1]= '\0';
			alAddArgLin(al,LinBfr);
		}
	}

	fclose(fd);
	return(al);
} /* end _alLodArgLst */


/*
 *	add words of argument line
 */

ArgLst_t *
alAddArgLin(al,ArgLin)
ArgLst_t	*al;	 		/* argument list */
char		*ArgLin;		/* argument line */
{
	char	*strchr();
	char	*strtok();
	char	*Arg;
	char	*Tkn;			/* token */
	int	 Len;
	int	 StrDel;		/* string delimiter */
	char	*ws = S_WHITESPACE;	/* white space characters */
	char	*p;


	for (Arg= strtok(ArgLin,ws); Arg != NULL; Arg= strtok((char *)NULL,ws)){
		if (*Arg == C_COMMENT) {
			return(al);	/* ignore rest of line */
		}

		if (strchr(S_STRDEL,(StrDel= *Arg)) != NULL) {
			for (	Len= strlen(Tkn= ++Arg);
				Tkn[Len-1] != StrDel;
				Len= strlen(Tkn= p)
			) {
				if ((p= strtok((char *)NULL,ws)) == NULL) {
					fprintf(stderr,
					"%s: unmatched %c in token %s\n",
						al->al_argv[0], StrDel, Arg-1 );
					goto NxtArg;
				}
				Tkn[Len]= ' ';	/* remove '\0' termination */
			}
			Tkn[Len-1]= '\0';	/* strip string delimiter */
		}
		alAddArg(al,Arg);
NxtArg:		;
	}
	return(al);
} /* end alAddArgLin */

