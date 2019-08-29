/*
* memory.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include "alcommp.h"
#include <malloc.h>


/*   For obvious reasons, the functions in this file should really be made  */
/* inline functions (where supported), or at least macros.  They are called */
/* so often I'd hate to leave them real functions!			    */

/*   Also, please remember that RISC systems have very strict alignment	 */
/* requirements; in most cases, memory allocation must be word-aligned   */
/* (ie: 4 byte boundary).  SunOS memory allocation routines perform this */
/* alignment automatically.  Remember to check when PORTING, however!    */

/*   SunOS memory allocation routines are not protected from signal   */
/* interruption.  Thus the following replacement routines which block */
/* certain frequently-used signals.				      */


char * ALMalloc( uiSize )
  u_int		uiSize;
{
  ALmProtectedFromSignals;
  register char	*	pMem;

  ALmBlockSignals();
  pMem = malloc( uiSize );
  ALmUnblockSignals();

  return pMem;
}


int ALFree( pMem )
  char *	pMem;
{
  ALmProtectedFromSignals;
  register int		iRet;

  ALmBlockSignals();
#ifdef ultrix
  free( pMem );
  iRet = 1;
#else
#ifdef __sgi
  free( pMem );
  iRet = 1;
#else
  iRet = free( pMem );
#endif
#endif
  ALmUnblockSignals();

  return iRet;
}


char * ALRealloc( pMem, uiSize )
  char *	pMem;
  u_int		uiSize;
{
  ALmProtectedFromSignals;
  register char	*	pMemOut;

  ALmBlockSignals();
  pMemOut = realloc( pMem, uiSize );
  ALmUnblockSignals();

  return pMemOut;
}


char * ALCalloc( uiNElem, uiElSize )
  u_int		uiNElem;
  u_int		uiElSize;
{
  ALmProtectedFromSignals;
  register char *	pMem;

  ALmBlockSignals();
  pMem = calloc( uiNElem, uiElSize );
  ALmUnblockSignals();

  return pMem;
}



/* end memory.c */
