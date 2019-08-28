
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


/****************************************************************/
/* LOC_CROSS: Does 3 things.  First, the dominant gene is       */
/*         determined by some heuristic.  Then the subordinant  */
/*         gene is remapped to the locations used by the        */
/*         dominant gene.   This is done in the last two steps. */
/*         First the subordinant string is mapped to            */
/*         B0, B1, ... Bn.   Next the subordinant string is     */
/*         remapped to use the same locations used by the       */
/*         dominant string.                                     */
/****************************************************************/

loc_cross(gene1, gene2, string_len)
GENEPTR  gene1, gene2;   
int      string_len;
    {
     char remap1[];
     char remap2[];
    
    /***************************************************/
    /* First determine which string is to be dominant  */
    /*  NO SET OR FAST RULE HERE -- EXPERIMENT         */
    /***************************************************/

    if (gene1->worth + gene1->repro_worth <
	gene2->worth + gene2->repro_worth)
	{
	dominant = gene1;
	  subord = gene2;
	  }
    else 
	{
	dominant = gene2;
	  subord = gene1;
	  }

    /***********************************************************/
    /*  Second map subordinant string to B0, B1, ... Bn.       */
    /***********************************************************/

     for(i=0; i<length; i++) 
     remap1[subord->tags[i]] = subord->bits??[i];

    /**********************************************/
    /*  Second, map to same locations used by r1. */
    /**********************************************/

     for(i=0; i<length; i++) 
     remap2[i] = remap1[dominant->tag[i]];

     cross(dominant, remap2, newr);

    }




























/*============================================================*/
/*  Cross:    The crossover operator.  It selects a break     */
/*            point and generates two new chromosones.        */
/*     This version of crossover narrows the range of the     */
/*            operator by finding the *front* position and    */
/*            *back* position where the two strings are       */
/*            different.  For example:                        */
/*                                                            */
/*                       0001010101001                        */
/*                       0000010010001                        */
/*                          ^     ^                           */
/*                      Front     Back                        */
/*                                                            */
/*            Crossover then occurs between the Front and     */
/*            Back positions.  As a result, no duplicates     */
/*            can be generated. It is also hoped that this    */
/*            will help to narrow the search space as         */
/*            convergence begins. By doing so it should       */
/*            actually prevent premature convergence.         */
/*============================================================*/

cross(r1,r2,newr)
	 int r1;
	 int r2;
	 int newr;
	 {
	    char *rule1;
	    char *rule2;
	    char *newrule;
	    int front, back;
	    int i, do_cross;
	    int xpoint;
            double worth;

            rule1 = ptr[rank(r1)].gene;
            rule2 = ptr[rank(r2)].gene;
            newrule = ptr[rank(newr)].gene;
            do_cross = TRUE;

/* Step One: establish front and back */

           i = 0;
           while (i<LENGTH)
	      {
              front = i;
	      if (rule1[i] != rule2[i]) i = LENGTH;
	      i++;
	      }

           if (front == LAST) do_cross = FALSE;

           else 
	       {i=LAST;
		back = LAST;

                while((back > front) && (i > 0))
		 {
		 back = i;
	         if (rule1[i] != rule2[i]) i = 0;
                 i--;
	         }
                }

           if (back < front+2) do_cross = FALSE;

           if (do_cross)
	     {
	     xpoint = randomain(back-1, front+1);
              
	     if (fracrand () > 0.5)
	       {
	       for(i=0; i<xpoint; i++) newrule[i]=rule1[i];
               for(i=xpoint; i<LENGTH; i++) newrule[i]=rule2[i];
               }
	      else
	       {
	       for(i=0; i<xpoint; i++) newrule[i]=rule2[i];
               for(i=xpoint; i<LENGTH; i++) newrule[i]=rule1[i];
	       }
              
	      }
	     ptr[rank(newr)].worth = eval(newrule,LENGTH);
          }

