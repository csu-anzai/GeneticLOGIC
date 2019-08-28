/*
 *	LICE - LInear Cellular Evolution strategy
 *
 *	Copyright (C) 1994 Joachim Sprave
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *	See README for details of LICE.
 *	Send bugs (better: bug desciptions) and comments to
 *
 * /---------------------------------------------------------------------\
 * | Joachim Sprave                  joe@ls11.informatik.uni-dortmund.de |
 * |                      //////\\                                       |
 * | Univ. Dortmund      /        \        44221 Dortmund                |
 * | Dept. CS           _|  _   _ |_       Tel.: +49-231-755 4678        |
 * | Systems Analysis  |.|-(.)-(.)+.|      Fax : +49-231-755 2450        |
 * \------------------  \|    J   |/  -----------------------------------/
 *                       \   ---  /
 *                        \      /
 *                         "####"
 */


#include <stdlib.h>

#define MANDATORY 0
#define OPTIONAL  1


  typedef enum {
      STRING, DOUBLE, INT, BOOLEAN, ENUM
  } type_t;

#define MAXKEY 2048

  typedef struct {
      char key[MAXKEY];		/* keyword */
      char dsc[MAXKEY];
      type_t type;		/* type, one of STRING, DOUBLE, INT	*/
      char val[MAXKEY];
      int set;			/* == 1 if this entry must not be set, else 0 */
      size_t offset;		/* offset to the beg. of the par. struct */
  }
param_t;

#define END_PARAMS  { "","",INT,"",OPTIONAL,0}

void getparam(
		 void *result,	/* pointer to the parameter struct	*/
		 param_t * param,	
				/* array of parameter definitions, see (1)
						at the end of this file    */
		 char **argv	/* just argv */
);



#if !defined(offsetof)
#define offsetof(type,member) ((size_t) &(((type *)0)->member))
#endif


/*
 * (1) the first two elements (index 0 and 1) have to be string fields
 * to store the program name and the parameter file name to. See main.c.
 * The last element must contain an empty key field: ""
 */
