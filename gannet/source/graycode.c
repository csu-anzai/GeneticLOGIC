/*********************************************************************
* Source: graycode.c         Version 1.0
*
* Copyright (C) 1990  Jason J. Spofford
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 1, or (at your option)
* any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
* Purpose:
*     
*    This source code program provides two functions for coding 
* integers into graycoded integers and decoding graycoded integers back
* to the normal integer representation.
*
* For further information contact:
*       Professor Kenneth J. Hintz
*       Department of Electrical and Computer Engineering
*       George Mason University
*       4400 University Drive
*       Fairfax, VA 22030
*
*       email:   khintz@gmuvax2.gmu.edu
*       Tel:     (703)993-1592
*
*  Written by:  Captain Jason J. Spofford, USAF
*               ECE Graduate Student, George Mason University
* Written on: 17 June 1990
* Updated on: 21 June 1990
*
********************************************************************/
/* The following include files are requried */
#include <stdio.h>

/* Functions defined here */
int graycode(register int);
int degraycode(register int);

/*******************************************************************
* degraycode
*
*     This function takes a graycoded value and passes back
*  a normally encoded integer value.  The logic of the algoritm was
*  extracted from graycode.c written by:
*
*                            John J. Grefenstette         
*                     Navy Center for Applied Research in AI
*                         Naval Research Laboratory         
*
*     The alogritm was coded completely differently.  It now works on 
*  the binary level instead of with character arrays.
*
* Inputs : gray - the value to be converted from graycode.
* Outputs: passes back the interger value to the calling function.
*
*******************************************************************/

int degraycode(register int gray)

{
register int bit, regular = 0;
register int last = 0, mask;

for (bit = 31; bit >= 0; bit--)
  {
  mask = 1 << bit;
  if (gray & mask) regular = regular | (mask & (last-1));
  else regular = regular | (mask & (~(last-1)));
  last = ((regular & mask) == mask);
  }

return(regular);
} /* END of degraycode */

/*******************************************************************
* graycode
*
*     This function takes a normally encoded integer and passes back
*  the graycode equivalent value.  The logic of the algoritm was extracted
*  from graycode.c written by:
*
*                            John J. Grefenstette         
*                     Navy Center for Applied Research in AI
*                         Naval Research Laboratory         
*
*     The alogritm was coded completely differently.  It now works on 
*  the binary level instead of with character arrays.
*
* Inputs : regular - the value to be converted to graycode.
* Outputs: passes back the graycode value to the calling function.
*
*******************************************************************/

int graycode(register int regular)

{
register int gray = 0, bit;
register int mask, last = 0;

for (bit = 31; bit >= 0; bit--)
  {
   mask = 1 << bit;
   gray = gray | (mask & (~((((regular & mask) == mask) != last)-1)));
   last = ((regular & mask) == mask);
 }

return(gray);
} /* END of gray code */





