
/*
 *  file:	convert.cs
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	functions that translate between various representations
 *
 *  modified:	16 apr 86
 *		15 sep 90: translations for floating point representation
 */

#include "extern.h"

static char BIT[CHARSIZE] ={ '\200', '\100', '\040', '\020',
			       '\010', '\004', '\002', '\001'};


/* Itoc and Ctoi translate ints to strings and vice versa */

unsigned long int:physical PCtoi(instring, length)
char:physical instring[];		/* string representation	*/
int length;		                /* length of instring		*/
{
  register int i,index;		/* loop control			*/
  unsigned long n:physical;	/* accumulator for return value	*/
  
  with(physical) {
    n = (unsigned long:physical) 0;
    index = 0;
    for (i=0; i<length; i++)
      {
	n <<= 1;
	n += (instring[index++] - (int:physical) '0');
      }
    return(n);
  }
}

PItoc(n, outstring, length)
unsigned long int:physical n;	/* input int value		*/
char:physical outstring[];	/* string representation	*/
int length;		/* length of outstring		*/
{
  register int i;		/* loop control			*/
  
  with(physical)
    for (i=length-1; i>=0; i--)
      {
	outstring[i] = '0' + (n & 1);
	n >>= 1;
      }
}


/* Pack and Unpack translate between strings and (packed) bit arrays */


PPack(instring, outstring, length)
char:physical instring[];	/* string representation		*/
char:physical outstring[];	/* packed representation of instring	*/
int length;		        /* length of instring			*/
{
  static firstflag = 1 ;
  static full;	/* number of fully used bytes in outstring	*/
  static slop;	/* number of bits used in outstring's last byte	*/
  register i,j;	/* loop control					*/
  int inindex, outindex;

  if (firstflag)
    {
      full = length / CHARSIZE;
      slop = length % CHARSIZE;
      firstflag = 0;
    }

  inindex = 0;
  outindex = 0;
  with(physical) {
    for (i=0; i<full; i++, outindex++)
      {
	outstring[outindex] = '\0';
	for (j=0; j < CHARSIZE; j++)
	  where (instring[inindex++] == '1')  outstring[outindex] |= BIT[j];
      }
    if (slop)
      {
	outstring[outindex] = '\0';
	for (j=0; j < slop; j++)
	  where (instring[inindex++] == '1')  outstring[outindex] |= BIT[j];
      }
  }
}

PUnpack(instring, outstring, length)
char:physical instring[];	/* packed bit representation		*/
char:physical outstring[];	/* string representation of instring	*/
int length;		/* length of outstring			*/
{
  static firstflag = 1 ;
  static full;	/* number of fully used bytes in instring	*/
  static slop;	/* number of bits used in instring's last byte	*/
  register i,j;	/* loop control					*/
  int:physical inindex,outindex;
  bool:physical b;

  if (firstflag)
    {
      full = length / CHARSIZE;
      slop = length % CHARSIZE;
      firstflag = 0;
    }

  with(physical) {

    inindex = 0;
    outindex = 0;
    for (i=0; i<full; i++, inindex++)
      {
	for (j=0; j < CHARSIZE; j++) {
	  b = (instring[inindex] & BIT[j]);
	  where (b)
	    outstring[outindex++] = '1';
	  else 
	    outstring[outindex++] = '0';
	}
      }
    if (slop)
      {
	for (j=0; j < slop; j++) {
	  b = (instring[inindex] & BIT[j]);
	  where (b)
	    outstring[outindex++] = '1';
	  else 
	    outstring[outindex++] = '0';
	}
      }
    outstring[outindex++] = '\0';
  }
}


/* Translations between fixed point ints and reflected Gray code */


PGray(instring, outstring, length)
char:physical instring[];	/* string representing fixed point int  */
char:physical outstring[];	/* string representing Gray coded value	*/
register int length;	        /* length of strings			*/
{
  register int i;
  char:physical last;
  
  with(physical) {
    last = '0';
    for (i=0; i<length; i++)
      {
	outstring[i] = '0' + (instring[i] != last);
	last = instring[i];
      }
  }
}  

PDegray(instring, outstring, length)
char:physical instring[];/* string representing Gray coded int		*/
char:physical outstring[];/* string representing fixed point int	*/
register int length;	/* length of strings				*/
{
  register int i;
  int:physical last;
  
  with(physical) {
    last = 0;
    for (i=0; i<length; i++)
      {
	where(instring[i] == '1')
	  outstring[i] = '0' + (!last);
        else
	  outstring[i] = '0' + last;
	last = outstring[i] - '0';
      }
  }
}

/* Translations between string representation and floating point vectors */


PFloatRep(instring, vect, length)
char:physical instring[];	/* string representation		*/
double:physical vect[];		/* floating point representation	*/
int length;	        	/* length of vect (output array)	*/
{
  register int i,j;		/* loop control				*/
  unsigned long int n:physical;	/* decoded int value			*/
  register int pos;	/* position to start decoding		*/
  char:physical tmpstring[80];	/* used for gray code interpretation	*/
  char:physical tmpstring2[80];  

  pos = 0;
  with(physical) {
    for (i=0; i < length; i++)
      {
	if (Grayflag)
	  {
	    for(j=0;j<Gene[i].bitlength+1;j++)
	      tmpstring2[j] = instring[pos+j];
	    PDegray(tmpstring2, tmpstring, Gene[i].bitlength);
	    n = PCtoi(tmpstring, Gene[i].bitlength);
	  }
	else
	  {
	    for(j=0;j<Gene[i].bitlength+1;j++)
	      tmpstring[j] = instring[j+pos];
	    n = PCtoi(tmpstring, Gene[i].bitlength);
	  }
	vect[i] = Gene[i].min + n*Gene[i].incr;
	pos += Gene[i].bitlength;
      }
  }
}

PStringRep(vect, outstring, length)
double:physical vect[];		/* floating point representation	*/
char:physical outstring[];	/* string representation		*/
int length;		/* length of vect 			*/
{
  register int i;		/* loop control				*/
  unsigned long int:physical n;	/* index of vext[i] within legal range	*/
  register int pos;	/* next position for filling outstring	*/
  char:physical Ptmpstring[80];	/* used for gray code translation	*/
  
  with(physical) {
    pos = 0;
    for (i=0; i < length; i++)
      {
	/* convert floating value to an index */
	n = (int:physical)((vect[i] - (int:physical)Gene[i].min) / 
			   (double:physical)(Gene[i].incr + 0.5));
	
	/* encode n in char string */
	if (Grayflag)
	  {
	    /* convert to Gray code */
	    PItoc(n, Ptmpstring, Gene[i].bitlength);
	    PGray( Ptmpstring, &outstring[pos], Gene[i].bitlength);
	  }
	else
	  {
	    PItoc(n, &outstring[pos], Gene[i].bitlength);
	  }
	pos += Gene[i].bitlength;
      }
    outstring[pos] = '\0';
  }
}

/*** end of file ***/
