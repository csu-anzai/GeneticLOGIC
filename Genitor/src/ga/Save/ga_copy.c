#include <stdio.h>
#include "gene.h"
#include "ga_copy.h"

/***************************************************************************
 * FUNCTION: gene_copy
 *
 * DESCRIBE: This routine copies one gene to another
 *           (genes are of the same type)
 *
 * INPUT PARAMETERS: 2 gene pointers, string_length
 *
 * RETURN VALUE: none
 ****************************************************************************/
void
gene_copy (gene1, gene2, string_length)
GENEPTR    gene1, gene2;
int        string_length;
{
 int i;

 for (i=0; i<string_length; i++)
	 {
	 gene1->string[i] = gene2->string[i];
	   gene1->tags[i] = gene2->tags[i];
	 }
       gene1->worth = gene2->worth;
 gene1->repro_worth = gene2->repro_worth;
 gene1->repro_count = gene2->repro_count;
}

/***************************************************************************
 * FUNCTION: gene_copy_ptr
 *
 * DESCRIBE: This routine copies one gene to another;
 *           note that only pointers are copied
 *
 * INPUT PARAMETERS: 2 gene pointers, string_length
 *
 * RETURN VALUE: none
 ****************************************************************************/
void
gene_copy_ptr (gene1, gene2)
GENEPTR    gene1, gene2;
{
      gene1->string = gene2->string;
       gene1->worth = gene2->worth;
        gene1->tags = gene2->tags;
 gene1->repro_worth = gene2->repro_worth;
 gene1->repro_count = gene2->repro_count;
}
