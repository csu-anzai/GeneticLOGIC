/*==============================================   
      file : generate.c
 
   purpose : new population generation

 developed : 1991
 
    author : Kalyanmoy Deb
================================================*/

#include "mga.ext"


long change_popsize()
/*  find out popsize  */
{
	long size;

	size = popsize;
	if ((gen >= init_select_gen) && (gen <= cutpop_gen))
	{
		if ((gen == cutpop_gen) && (size > juxtpopsize))
			size = juxtpopsize;
		else if (gen <= cut_every_other_gen)
		/* cut at every other generation */
		{
			if (cutpopflag == 1)
			{
				size = popsize / 2;
				cutpopflag = 0;
			}
			else 
				cutpopflag = 1;
		}
		else  /* cut at every generation */
			size = popsize / 2;
	}
	return(size);
}


void primordial()
/*  primordial phase  */
{
	int pos,count;
	char msg[PAGEWIDTH];
	long newpopsize;

	TRACE("   Primordial entered");

	/*  check whether population size needs to be changed   */
	newpopsize = change_popsize();
	/* shuffle the population */
	pick = 0;
	shuffle_pop();
	count = 0;
	/*  create a new population  */
	while (count < newpopsize) {
		/*  select an individual  */
		pos = select_pop();

		/* copy it in the new population  */
		copy_chrom(oldpop[pos],&(newpop[count]));

		count++;
	}
	popsize = newpopsize;
	reallocate_memory(popsize);

	TRACE("   Primordial exited\n");
}


void juxtapositional()
/*  juxtapositional phase  */
{
	int pos1, pos2, count;
	register int i;
	struct INDIVIDUAL mate1, mate2;

	TRACE("   Juxtapositional entered");
	
	/* shuffle the population */
	pick = 0;
	shuffle_pop();
	count = 0;
	/* free memory allocated for new population */
	freenewpop();
	/*  create new population */
	do {
		/*  generate mate1 and mate2  */
		storepop(&mate1,1);

		/*  select two individuals for mating  */
		pos1 = select_pop();
		copy_chrom(oldpop[pos1],&mate1);

		storepop(&mate2,1);
		pos2 = select_pop();
		copy_chrom(oldpop[pos2],&mate2);

		/* cut and splice two individuals */
		cut_and_splice(mate1,mate2);

		/* put children individuals in new population  */
		while (count < popsize && ! isempty(newchrom_stack)) {
			if (pop_stack(&newchrom_stack,&(newpop[count])))
			{
				/*  perform mutation  */
				mutation(&(newpop[count]));

				/*  find out raw chrom  */
				fill_chrom(&(newpop[count]));

				/*  evaluate objective function value  */
				newpop[count].fitness = 
					objfunc(newpop[count].fullchrom);

				count++;
			}
		}
	}    while (count < popsize);

	TRACE("   Juxtapositional exited\n");
}

void generate()
/*   messy GA operation   */
{
	struct INDIVIDUAL *tempop;

	printf("\nera = %d, generation = %d\n\n",era,gen);
	TRACE("Generate entered");

	if (gen <= prim_gen)
		primordial();
	else
		juxtapositional();

	statistics(newpop);
	reportpop(newpop);

 	/* copy newpop to oldpop */
	tempop = oldpop;
	oldpop = newpop;
	newpop = tempop;

	TRACE("Generate exited");
}

