/*
* alerror.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <varargs.h>
#include "alcommp.h"


extern int	sys_nerr;
extern char *	sys_errlist[];


static FILE	*pErrStream = stdout;


void ALGripeErr( pWhere )
  char *	pWhere;
{

  if ( errno <= sys_nerr )
    ALGripe( pWhere, "%s", sys_errlist[errno] );
  else
    ALGripe( pWhere, "system error # %d", errno );
}


/*VARARGS1*/
void ALGripe(va_alist)
va_dcl
{
  va_list	varArgs;
  char *	pFmt;
  char *	pWhere;
  char          ebuf[120];

  va_start( varArgs );
  pWhere = va_arg( varArgs, char * );
  pFmt = va_arg( varArgs, char * );

#ifdef FRONTEND
  vsprintf( ebuf, pFmt, varArgs );
  FEError(-197,0,0,"AL Err: %s ",ebuf);
#else	/* if FRONTEND not def */
  if ( pWhere != NULL )	fprintf( pErrStream, "%s: ", pWhere );

  vfprintf( pErrStream, pFmt, varArgs );

  fflush( pErrStream );
#endif	/* ifdef frontend */
  va_end( varArgs );
}



/* end alerror.c */
