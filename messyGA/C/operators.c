/*=================================================     
       file : operators.c
 
    purpose : messy GA operators 
 
  developed : 1991

     author : Kalyanmoy Deb
====================================================*/

#include "mga.ext"


INDV_ID select_pop()
/*  determine which selection operator  */
{
	if (gen < init_select_gen)
		return (init_select());
	else 
	{
		if (thresholdingflag)
			return(thresholding_select());
		else
			return(normal_select());
	}
}


INDV_ID select(first, second)
INDV_ID first, second;
/*  selects the better of first and second  */
{
	if (oldpop[first].fitness == oldpop[second].fitness) {
		if (tiebreakingflag)
			return((oldpop[first].chromlen < \
				oldpop[second].chromlen) ? first : second);
		else 
			return(flip(0.5) ? first : second);
	}
	else if (oldpop[first].fitness > oldpop[second].fitness)
	{
		return((maximizationflag) ? first : second);
	}
	else 
	{
		return((maximizationflag) ? second : first);
	}
}


INDV_ID init_select()
/*  tournament selection operator without shuffling  */
{
	INDV_ID first,second;

	if (pick >= popsize-1)
		pick = 0;
	first = pick;
	second = pick + 1;
	pick += 2;
	return (select(first, second));
}


INDV_ID normal_select()
/*  tournament selection operator with shuffling  */
{
	INDV_ID first,second;

	if (pick >= popsize-1)
	{
		pick = 0;
		shuffle_pop();
	}
	first = shuffle[pick];
	second = shuffle[pick+1];
	pick += 2;
	return (select(first, second));
}


INDV_ID thresholding_select()
/*  tournament selection operator with thresholding  */
{
	register int i,j,k;
	INDV_ID first, second, fittest, secondid;
	int num1, num2, count, temp, threshold;
	BOOLEAN flag;
	unsigned *rawgn1, *rawgn2;

	if (pick >= popsize-1)
	{
		pick = 0;
		shuffle_pop();
	}
	/*  choose the first individual, default is first  */
	fittest = first = shuffle[pick];
	rawgn1 = oldpop[first].fullgene;
	num1 = oldpop[first].genelen;

	for (k=1,flag=0; (k<=shuffle_num && !flag); k++)
	{
		/*  choose the second individual  */
		secondid = pick + k;
		/*  if required, scroll to the begining of the array */
		if (k >= (popsize - pick))
			secondid = k - (popsize - pick);
		second = shuffle[secondid];
		rawgn2 = oldpop[second].fullgene;
		num2 =  oldpop[second].genelen;

		/*  count the number of matched genes  */
		count = matched_genes(rawgn1, rawgn2);

		/*  calculate the threshold value  */
		threshold = (num1 * num2);
		threshold /= problem_length;

		if (count > threshold)
		/*  if count > threshold, found a match, perform selection */
		{
			flag = 1;
			temp = shuffle[secondid];
			shuffle[secondid] = shuffle[pick + 1];
			shuffle[pick + 1] = temp;
			fittest = select(first, second);
			pick++;
		}
	}
	pick++;
	return (fittest);
}

int matched_genes(rawgn1,rawgn2)
unsigned *rawgn1;
unsigned *rawgn2;
/*  count the number of matched genes  */
{
	register int i, j;
	int num = 0;
	unsigned rawgn, mask = 1;

	for (i = 0; i < bytesize; i++)
	{
		/*  get the template of common genes */
		rawgn = (rawgn1[i] & rawgn2[i]);

		/*  count the number of 1's in the template  */
		for (j = 0; j < bytelimit[i]; j++)
		{
			if (((rawgn >> j) & mask) == 1)
				num++;
		}
	}
	return(num);
}


void cut_and_splice(chr1,chr2)
struct INDIVIDUAL chr1,chr2;
/*  cut and splice operators  */
{
	int chr3flag;
	struct INDIVIDUAL chr3;
	struct INDIVIDUAL chr4;

	/*  if first chromosome needs to be cut  */
	if (flip(cut_prob * chr1.chromlen))
	{
		cut(&chr1,&chr4);
		/* if chr4 is not empty first element in chrom_stack is chr4 */
		if (chr4.firstgene != NIL)
			push_stack(&chrom_stack,chr4);
	}

	chr3.firstgene = NIL;
	/*  if second chromosome needs to be cut  */
	if (chr3flag = flip(cut_prob * chr2.chromlen))
		cut(&chr2,&chr3);

	/*  next element in the chrom_stack is chr2  */
	push_stack(&chrom_stack,chr2);

	/*  if chr3 is not empty, next element in the stack is chr3  */
	if (chr3flag && chr3.firstgene != NIL)
		push_stack(&chrom_stack,chr3);

	/*  top most element is chr1  */
	push_stack(&chrom_stack,chr1);

	/*  check elements in the stack and splice  */
	do  {
	}   while (test_splice());
}


BOOLEAN test_splice()
/*  splice two members of the stack, send 1 if there is a member left,
    0 otherwise  */
{
	struct INDIVIDUAL chr1,chr2;

	/*  get the first member  */
	if (pop_stack(&chrom_stack,&chr1))
	{
		if (flip(splice_prob))
		{
			/*  check for another member in the stack  */
			/*  splice if yes  */
			if (pop_stack(&chrom_stack,&chr2))
				splice(&chr1,&chr2);
		}
		push_stack(&newchrom_stack,chr1);
		return(1);
	}
	else
		return(0);
}


void cut(chr1,chr2)
struct INDIVIDUAL *chr1,*chr2;
/*   cut chr1 into two pieces: chr1, chr2   */
{
	register int i;
	struct GENE *temp;
	int cut_site;
	char msg[PAGEWIDTH];

	temp = (*chr1).firstgene;

	/* find out cut site  */
	cut_site = rnd(1,(*chr1).chromlen);

	/*  cut chr1 at cut_site  */
	for (i = 1; i < cut_site; i++)
		temp = temp->nextgene;

	/*   assign other members of chr1 and chr2  */
	(*chr2).firstgene = temp->nextgene;
	(*chr2).lastgene = (*chr1).lastgene;
	(*chr1).lastgene = temp;
	temp->nextgene = NIL;
	(*chr2).chromlen = (*chr1).chromlen - cut_site;
	(*chr1).chromlen = cut_site;

	/*  allocate memory for chr2 fullchrom and fullgene  */
	if(!((*chr2).fullchrom = (unsigned *)malloc(bytesize*sizeof(unsigned)))) {
		sprintf(msg, "Insufficient memory for fullchrom\n");
		print_error(msg);
	}
	if(!((*chr2).fullgene = (unsigned *)malloc(bytesize*sizeof(unsigned)))) {
		sprintf(msg, "Insufficient memory for fullgene\n");
		print_error(msg);
	}
}


void splice(chr1,chr2)
struct INDIVIDUAL *chr1,*chr2;
/*   splice chr1 and chr2 to chr1  */
{
	/*  chr2 is appended behind chr1  */
	(*chr1).lastgene->nextgene = (*chr2).firstgene;
	(*chr1).lastgene = (*chr2).lastgene;
	(*chr1).chromlen = (*chr1).chromlen + (*chr2).chromlen;

	/*  destroy chr2 fullchrom and fullgene */
	free((*chr2).fullchrom);
	free((*chr2).fullgene);
}


void mutation(chr)
struct INDIVIDUAL *chr;
/*  if mutation probabalities are greater than zero, perform mutation  */
{
	register int i;
 	struct GENE *temp;

	if ((allelic_mut_prob > 0.0) || (genic_mut_prob > 0.0)) {
		temp = (*chr).firstgene;
		while (temp != NIL)
		{
			if (flip(allelic_mut_prob))
			/*   perform allelic mutation  */
			{
				if (temp->allele == 1)
					temp->allele = 0;
				else 
					temp->allele = 1;
				allelicmutation++;
			}
			if (flip(genic_mut_prob))
			/*  perform genic mutation  */
			{
				temp->genenumber = \
					genic_mutation(temp->genenumber);
				genicmutation++;
			}
			temp = temp->nextgene;
		}
	}
}


GENE_ID genic_mutation(num)
GENE_ID num;
/*  change the genenumber to other  */
{
	GENE_ID n;

	do {
		n = rnd(0, problem_length-1);
	}  while (n == num);
	return(n);
}
