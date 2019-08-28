
/*************************************************************/
/*                                                           */
/*  Copyright (c) 1990                                       */
/*  Darrell L. Whitley                                       */
/*  Computer Science Department                              */
/*  Colorado State University                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
/* Include the math file here BECAUSE if it's not included,
 * no compiler error will occur, but randomain may break!
 */
#include <math.h>

/* find out 1/2147483647 val and multiply instead; cheaper! */
#define fracrand() ((double)random()/2147483647)

#define bitgen()  ((fracrand()>.5)? 1 : 0 )
#define bitgenc() ((fracrand()>.5)?'1':'0')
#define randomain(hi,lo) ((int) floor(fracrand()*((hi-lo)+0.999999))+lo)
