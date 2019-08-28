/*============================================================
      file : random.c
 
   purpose : random number generator
  
 developed : 1991 

    author : Kalyanmoy Deb

  (Translated in C from David Goldberg's SGA code in Pascal)
==============================================================*/

#include "mga.ext"


void advance_random()
/* Create next batch of 55 random numbers */
{
	register int j;
	double new_random;

	for(j = 0; j < 24; j++) {
		new_random = oldrand[j] - oldrand[j+31];
		if(new_random < 0.0)
			new_random = new_random + 1.0;
		oldrand[j] = new_random;
	}
	for(j = 24; j < 55; j++) {
		new_random = oldrand [j] - oldrand [j-24];
		if(new_random < 0.0)
			new_random = new_random + 1.0;
		oldrand[j] = new_random;
	}
}

void warmup_random(random_seed)
double random_seed;
/* Get random off and runnin */
{
	register int i, j;
	double new_random, prev_random;
	void advance_random();

	oldrand[54] = random_seed;
	new_random = 0.000000001;
	prev_random = random_seed;
	for(j = 1 ; j <= 54; j++) {
		i = (21 * j) % 54;
		oldrand[i] = new_random;
		new_random = prev_random-new_random;
		if(new_random < 0.0)
			new_random = new_random + 1.0;
		prev_random = oldrand[i];
	}
	advance_random();
	advance_random();
	advance_random();
	jrand = 0;
}

double random()
/* Fetch a single random number between 0.0 and 1.0 - Subtractive Method */
/* See Knuth, D. (1969), v. 2 for details */
{
	void advance_random();

	jrand++;
	if(jrand >= 55) {
		jrand = 1;
		advance_random();
	}
	return(oldrand[jrand]);
}


BOOLEAN flip(prob)
double prob;
/* Flip a biased coin - true if heads */
{
	return (random() <= prob ? 1 : 0);
}

int rnd(low, high)
int low, high;
/* Pick a random integer between low and high */
{
	int i;
	double random();

	if (low >= high)
		i = low;
	else {
		i = (random() * (high - low + 1)) + low;
		if(i > high)
			i = high;
	}
	return(i);
}

void randomize()
/* Get seed number for random and start it up */
{
	register int j;

	oldrand = (double *)malloc(55 * sizeof(double));
	do {
		for (j = 0; j <= 54; j++)
			oldrand[j] = 0.0;
		jrand=0;
		printf("\nEnter seed random number (0.0..1.0)-> ");
		scanf("%lf", &randomseed);
	}  while ((randomseed < 0.0) || (randomseed > 1.0));
	warmup_random(randomseed);
}
