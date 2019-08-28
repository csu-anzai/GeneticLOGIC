/*==================================================
       file : supports.c
   
    purpose : auxiliary files
 
  developed : 1991

     author : Kalyanmoy Deb
====================================================*/

#include "mga.ext"


int length_chrom(chr)
struct GENE *chr;
/*  calculates the length of a chromosome  */
{
	int len = 0;
	struct GENE *temp;

	temp = chr;
	/*  NIL chrom has a zero length  */
	if (temp != NIL)
	{
		for (len=1; temp->nextgene != NIL; )
		{
			len++;
			temp = temp->nextgene;
		}
	}
	return(len);
}


void fill_chrom(chr)
struct INDIVIDUAL *chr;
/*  fills up the remaining genes from the template  */
{
        register int i;
	BOOLEAN gene_flag[MAX_SIZE];
	struct GENE *temp;
	int id, rem, temp_gene, count = 0;
	unsigned temp_allele, shield, tempmask_gene, tempmask = 1;
	unsigned *rawchr = (*chr).fullchrom;
	unsigned *rawgn = (*chr).fullgene;

	/*   no gene has been considered yet  */
	for (i = 0; i < problem_length; i++)
		gene_flag[i] = 0;

	/*  assign the template alleles first */
	for (i = 0; i < bytesize; i++)
	{
		rawchr[i] = template[i];
		rawgn[i] = 0;
        }
	temp = (*chr).firstgene;

	/*  for each member in the chromosome  */
	while (temp != NIL) {
		temp_gene = temp->genenumber;
		temp_allele = temp->allele;

		/*  if not replaced already, (left to right scan)  */
		if (! gene_flag[temp_gene])
		{
			id = temp_gene/UNSINTSIZE;
			rem = temp_gene % UNSINTSIZE;
			shield = tempmask << rem;

			/*  replace the allele, if different  */
			if (((rawchr[id] >> rem) & tempmask)  != temp_allele)
				rawchr[id] = shield ^ rawchr[id];

                        /*  make fullgene */
                        tempmask_gene = (0 >> rem) | 1;
			tempmask_gene <<= rem;
			rawgn[id] |= tempmask_gene;
			count++;

			/*  set the allele flag off  */
			gene_flag[temp_gene] = 1;
		}
		temp = temp->nextgene;
	}
	(*chr).genelen = count;
}


void copy_chrom(chr1,chr2)
struct INDIVIDUAL chr1,*chr2;
/*  copy chr into another variable whose firstgene has been allocated */
{
	register int i,j;
	struct GENE *temp, *tchr;
	char msg[PAGEWIDTH];

	/*   copy genes   */
	temp = (*chr2).firstgene;
	tchr = chr1.firstgene;
	temp->genenumber = tchr->genenumber;
	temp->allele = tchr->allele;
	for (i = 1; i < chr1.chromlen; i++)
	{
		if (temp->nextgene == NIL)
		{
			if(!(temp->nextgene = (struct GENE *) \
						malloc(sizeof(struct GENE))))
			{
				sprintf(msg,"Insufficient memory for genes\n");
				print_error(msg);
			}
                        temp->nextgene->nextgene = NIL;
		}
		temp = temp->nextgene;
		tchr = tchr->nextgene;
		temp->genenumber = tchr->genenumber;
		temp->allele = tchr->allele;
	}

	/*   copy other members   */
	(*chr2).lastgene = temp;
	temp->nextgene = NIL;
	(*chr2).chromlen = chr1.chromlen;
	for (i = 0; i < bytesize; i++)
	{
		(*chr2).fullchrom[i] = chr1.fullchrom[i];
		(*chr2).fullgene[i] = chr1.fullgene[i];
        }
	(*chr2).genelen = chr1.genelen;
	(*chr2).fitness = chr1.fitness;
}


BOOLEAN isempty(stack)
STACKPTR stack;
/*  Is the stack empty?  */
{
	return (stack == NIL);
}


BOOLEAN pop_stack(stack, chr)
STACKPTR *stack;
struct INDIVIDUAL *chr;
/*  pops the top member in the stack; sends 1 if there is a member,
    0 otherwise */
{
	STACKPTR t = *stack;

	if (! isempty(t))
	{
		*chr = t->genefirst;
		*stack = t->nextmem;
		free(t);
		return(1);
	}
	else
		return(0);
}


void push_stack(stack, chr)
STACKPTR *stack;
struct INDIVIDUAL chr;
/*  push a member chr into the stack  */
{
	STACKPTR temp;
	char msg[PAGEWIDTH];

	if(!(temp = (STACKPTR)malloc(sizeof(struct STACKTYPE))))
	{
		sprintf(msg,"Insufficient memory for stack\n");
		print_error(msg);
	}
	temp->genefirst = chr;
	temp->nextmem = *stack;
	*stack = temp;
}


ALLELES pickallele(id,rawchr)
int id;
unsigned *rawchr;
/*  picks the allele value of the id-th gene from rawchr  */
{
	unsigned mask = 1;
	int num, rem;

	num  = id / UNSINTSIZE;
	rem  = id % UNSINTSIZE;
	if (((rawchr[num] >> rem) & mask) == 1)
		return(1);
	else 
		return(0);
}


void shuffle_pop()
/*  shuffles the shuffle array  */
{
	register int i;
	int num; 
	INDV_ID temp;

	for (i = 0; i < popsize; i++)
		shuffle[i] = i;
	for (i = 0; i < popsize-1; i++)
	{
		num = rnd(i, popsize-1);
		temp = shuffle[num];
		shuffle[num] = shuffle[i];
		shuffle[i] = temp;
	}
}


void assign_beststring_to_template()
/* assigns the best string to the template */
{
	register int i;
	long clock,time();
	char *ctime();
	FILE *fp, *fopen();

        for (i = 0; i < bytesize; i++)
		template[i] = best_indv.fullchrom[i];
	templatefitness = best_indv.fitness;
        
	/*  write the best string in output file  */
	fp = fopen(Outputfilename,"a");
        fprintf(fp,"\nThe best string:\n");
	writefullchrom(fp,best_indv.fullchrom);
	fprintf(fp,"Fitness = %10.3lf\n",best_indv.fitness);
	fprintf(fp,"String length = %5d\n",best_indv.chromlen);
	fprintf(fp,"Chromosome: ");
	fwritechrom(fp,best_indv,14);

	time(&clock);
	fprintf(fp,"\nEnd of run for era %d: %s\n",era,ctime(&clock));
	if (era != max_era) {
		page(fp);
		fprintf(fp,"\n");
	}
	fclose(fp);
}
