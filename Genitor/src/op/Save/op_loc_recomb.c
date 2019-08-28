
#include <stdio.h>
#include <malloc.h>
#include "gene.h"
#include "ga_random.h"

loc_recomb (gene1, gene2, string_len)
GENEPTR     gene1, gene2;
int         string_len;
{
 int        i;
 GENEPTR    dominant,
			subord;

 /************************
  * pick dominant parent *
  ************************/

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

 /****************************** 
  * reorder subordinate parent *
  ******************************/
  {
   for (i=0; i<string_len; i++)
	   {
	   }
  }

 /* cross parents */

 red_surrog ();

 /* evaluate offspring */

 /* update reproductive worth of dominant parent */
}
