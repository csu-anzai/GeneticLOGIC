
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

loc_cross(gene1, gene2, new_gene, string_len)
GENEPTR  gene1, gene2;   
GENEPTR  new_gene;
int      string_len;
    {
     GENEPTR dominant, subord;
     int     i; 
     char remap1[];
     char remap2[];
    
    /***************************************************/
    /* First determine which string is to be dominant  */
    /*  NO SET OR FAST RULE HERE -- EXPERIMENT         */
    /***************************************************/
    /***************************************************/
    /*  if (gene1->worth + gene1->repro_worth <        */
    /*	gene2->worth + gene2->repro_worth)             */
    /***************************************************/

    if (bitgen() == 0) 
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

     for(i=0; i<string_len; i++) 
        remap1[subord->tags[i]] = subord->string[i];

    /**********************************************/
    /*  Second, map to same locations used by r1. */
    /**********************************************/

     for(i=0; i<string_len; i++) 
        remap2[i] = remap1[dominant->tag[i]];
	  
     /******************************************
      * red_surrog_cross wants BUFFERS as input
      * (not actual population string memory).
      * So make a copy of dominant string in
      * remap 1 (remap2 is already set to go)
      ******************************************/

     for(i=0; i<string_len; i++) 
        remap1[i] = dominant->string[i];

     /***********************************
      * after call to red_surrog_cross,
      * children are in remap1 and remap2 
      ***********************************/
     red_surrog_cross(remap1, remap2, string_len);
     
     /*********************************************
      * copy chosen child info to new_gene--        
      * also, the child needs to be mapped to the
      * canonical ordering for evaluation         
      *********************************************/

     
     for(i=0; i<string_len; i++) 
	{
        new_gene->string[i] = remap2[i];
        new_gene->tag[i] = dominant->tag[i];
	remap1[dominant->tag[i]]
	}
	new_gene->worth = bin_eval (remap1, string_len);
	new_gene->repro_eval = 0;
	new_gene->repro_count = 1;

     /* Do inversion ??? */

     /* return offspring */

     return (new_gene);
    }


