/*								 LETSEQ1.H 

This file, LETSEQ1.H, defines constants for the LETSEQ1 environment
(LETSEQ1 program and documentation, Copyright 1988 Rick L. Riolo).

This file should be #include-d in the LETSEQ1.DEF file.

*/

#define  LETMEMSZ	 4			  /* short-term memory size, i.e., how much of the sequence it keeps */
#define  LETMEMMX	 (LETMEMSZ-1)   /* in the systems 'hardware', unlearned memory.  */
 
#define  STRLETSZ	 7			  /* Number of right-most loci of message in which 
										letter is encoded (at detector/effector interfaces). 
										(This includes the 2 loci for 'type' and the rest for specific letter.)
									 */
#define  NMLETATT	 7
#define  BINLETMX	 127			/* Max binary integer of size STRLETSZ */

#define  SYMSUPSZ	 128
#define  SYMSUPMX	 (SYMSUPSZ-1)

#define  EFRHIBID	 0			 /* Effector resolution: use High Bidder */
#define  EFRHISUM	 1			 /* ditto: use highest sum of support (bids) */  
#define  EFRPRSUM	 2			 /* ditto: pick with probability proportional to sums */

#define  ENVLINSZ	 80			 /* maximum length of input line of letters. */
#define  ENVLINMX	 (ENVLINSZ-1)

#define  TYPENONE	 0			  /* illegal type */
#define  TYPEVOW	  1			  /* Character is a vowel */
#define  TYPECON	  2			  /*	"	  is a consonant */
#define  TYPEPUNC	 3			  /*	"	  is punctuation mark */

