/*
* almemory.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __ALMEMORY_H
#define __ALMEMORY_H


/*   For obvious reasons, the functions below should really be made inline */
/* functions (where supported), or at least macros.  They are called so    */
/* often I'd hate to leave them real functions!  (this note is duplicated  */
/* in memory.c)								   */

extern char *		ALMalloc P_(( ));
extern int		ALFree P_(( ));
extern char *		ALRealloc P_(( ));
extern char *		ALCalloc P_(( ));

#define	TRYMALLOC( p, ptyp, bsiz )	{				\
  if (((p)=(ptyp)ALMalloc(sizeof(bsiz)))==NULL) 			\
    ALGripe("trymalloc","%s(%d): no memory",__FILE__,__LINE__);		\
}

#define	TRYMALLOCBLK( p, ptyp, bsiz, num )	{			\
  if (((p)=(ptyp)ALMalloc((num)*sizeof(bsiz)))==NULL)			\
    ALGripe("trymallocblk","%s(%d): no memory",__FILE__,__LINE__);	\
}

#define	TRYCALLOC( p, ptyp, bsiz, num )	{				\
  if (((p)=(ptyp)ALCalloc((num),sizeof(bsiz)))==NULL)			\
    ALGripe("trycalloc","%s(%d): no memory",__FILE__,__LINE__);		\
}

#define	TRYREALLOCBLK( op, np, ptyp, bsiz, num ) {			\
  if (((np)=(ptyp)ALRealloc((op),((num)*sizeof(bsiz))))==NULL)		\
    ALGripe("tryreallocblk","%s(%d): no memory",__FILE__,__LINE__);	\
}


#endif  /* ifndef __ALMEMORY_H; Add nothing past this point */
