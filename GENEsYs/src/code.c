/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
 
/*
 *
 *	$Id$
 *	$Log$
 *
 *
 *      file:    code.c
 *
 *    author:    John J. Grefenstette
 *
 *   created:    3 feb 86
 *
 *   purpose:    translate between fixed point and reflected Gray code.
 *
 *  modified:    7 feb 86
 */
 
void 
Gray(inbuf, outbuf, length)
char 		*inbuf, 
     		*outbuf;
register int 	 length;

{
    	/* inbuf contains the fixed point integer, one bit per char.	*/
    	/* outbuf gets the Gray coded version, one bit per char.	*/
    
    	register int 	i,
    			last;
 
    	for (last = 0, i = 0; i < length; i++) {
        	outbuf[i] = (inbuf[i] != last);
        	last      = inbuf[i];
    	}

} /* end Gray */


 
void
Degray(inbuf, outbuf, length)
char 		*inbuf, 
		*outbuf;
register int 	 length;

{
    	/* inbuf contains the Gray code integer, one bit per char.  	*/
    	/* outbuf gets the corresponding fixed point integer.  		*/

    	register int 	i,
    			last;
 
    	for (last = 0, i = 0; i < length; i++) {
        	if (inbuf[i])  
			outbuf[i] = !last;
        	else 
			outbuf[i] = last;
        	last = outbuf[i];
    	}

} /* end Degray */



 
void
Itoc(n, outbuf, length)
register int 	 n;
register char 	*outbuf;
register int 	 length;

{
	/*
 	 *   purpose:    translate an integer into an unpacked array,
 	 *          	 storing one bit per char element
	 */

    	register int i;
 
    	for (i = length-1; i >= 0; i--) {
        	outbuf[i] = (n & 1);
        	n >>= 1;
    	}

} /* end Itoc */
 

 
int 
Ctoi(buf, length)
register char 	*buf;
register int 	 length;

{
 	/*
	 *   purpose:    Translate a char array "buf" into a binary integer.
 	 *        	 It is assumed that buf contains only 000 and 001's.
	 */

    	register int 	i,
    			answer;

    	for (answer = 0, i = 0; i < length; i++) {
        	answer <<= 1;
        	answer += *buf++;
    	}
    	return(answer);

} /* Ctoi */
 
 
/*** end of file ***/
