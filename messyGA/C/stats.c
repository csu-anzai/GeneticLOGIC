/*==================================================
      file  : stats.c
 
    purpose : calculate statistics
 
  developed : 1991 

     author : Kalyanmoy Deb
===================================================*/

#include "mga.ext"


void statistics(pop)
struct INDIVIDUAL *pop;
/*  calculates statistics of the new population  */
{
	register int i,j;
	double sumfitness, fit, totlen;
	long stringlen;
	int maxid, minid, whether, ifbetter;
	int maxnum, minnum, num, best_id, abovetemplate = 0;
	char msg[PAGEWIDTH];
	FILE *fp, *fopen();

	TRACE("   Statistics entered");

	maxnum = minnum = maxid = minid = 0;
	stringlen = pop[0].chromlen;
	sumfitness = minfitness = maxfitness = pop[0].fitness;
	for (i = 1; i < popsize; i++)
	{
		fit = pop[i].fitness;
		sumfitness += fit;
		if (gen == 0) 
		{ /* proportion of best indv at initial pop */
			ifbetter = (maximizationflag) ? (fit > templatefitness)\
				   : (fit < templatefitness);
			if (ifbetter)
				abovetemplate++;
		}
		stringlen += pop[i].chromlen;
		/* calculate minimum fitness */
		if (minfitness > fit)
		{
			minfitness = fit;
			minid = i;
			minnum = 1;
		}
		else if (minfitness == fit)
			minnum++;
		/* calculate maximum fitness */
		if (maxfitness < fit)
		{
			maxfitness = fit;
			maxid = i;
			maxnum = 1;
		}
		else if (maxfitness == fit)
			maxnum++;
	}
	avgfitness = sumfitness/popsize;

	/* calculate average string length */
	totlen = stringlen;
	avgstrlen = totlen / popsize;

	/* determine the best individual and replace previous best */
	best_id = (maximizationflag) ? maxid : minid;
	whether = (maximizationflag) ? pop[best_id].fitness > best_indv.fitness\
		  : pop[best_id].fitness < best_indv.fitness;
	if (whether)
	{
		delete_chrom(best_indv.firstgene);
		if(!(best_indv.firstgene = (struct GENE *) \
						malloc(sizeof(struct GENE))))
		{
			sprintf(msg,"Insufficient memory for best_indv genes.\n");
			print_error(msg);
		}
		best_indv.firstgene->nextgene = NIL;
		copy_chrom(pop[best_id],&best_indv);
	}
	/* evaluate the proportion of the individuals with fitness
	   better than the template */
	if (gen == 0) 
	{
		if (abovetemplate == 0)  /* no indv. better than template */
			stopmgaflag = 2; 
		prop_bestindv = abovetemplate;
		prop_bestindv /= popsize;
	}

	/* check for convergence */
	num = (maximizationflag) ? maxnum : minnum;
	if ((num >= stopfactor * popsize) && (gen > prim_gen))
		stopmgaflag = 1;

	TRACE("   Statistics exited\n");
}
