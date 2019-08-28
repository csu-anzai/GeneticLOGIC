/*
 *	arglst.h	Ho, 04.Oct.'90				SunOS 4.0.3
 *			Ho, 19.Dec.'90
 *
 *	argument lists
 */

#ifndef	__ARGLST__
#define	__ARGLST__	1		/* prevent multiple declarations */


typedef	struct	ArgLst			/* argument list */
{
	int	  al_next;		/* index to next argument */
	int	  al_argc;		/* argument count */
	char	**al_argv;		/* argument vector */
} ArgLst_t;


/*
 *	symbolic values
 */

#define	AL_NEW	((ArgLst_t *)0)		/* alLodArgLst: create new one */


/*
 *	basic functions
 */

extern	ArgLst_t	*alCrtArgLst();	/* create argument list */
extern	int	 	 alDltArgLst();	/* delete argument list */
extern	ArgLst_t	*alClrArgLst();	/* clear  argument list */
extern	ArgLst_t	*alAddArg();	/* add    argument to argument list */

extern	ArgLst_t	*alRstArgLst();	/* reset  argument list */
extern	char		*alNxtArg();	/* retrieve next argument from list */

/*
 *	comfort functions
 */

extern	ArgLst_t	*alLodArgLst();	/* load	argument list from file */
extern	ArgLst_t	*alAddArgLin();	/* add	argument line */


#endif	/* __ARGLST__ */
