/***************************************************************************/
/*																		   */
/*	BUGS software														   */
/*	Copyright 1990 Joshua R. Smith									       */
/* 	file: breed.c	5/10/90												   */
/*																		   */
/*	This is a simple interactive genetic design system.  You can grow 	   */
/*	creatures (graphs of Fourier Series, at this point) on the screen and  */
/*	specify which are fit enough to reproduce by clicking.  When you've	   */
/*	choosen the ones you want to reproduce, click breed.  breeder varies   */
/*	their genetic material and displays a new generation for you to 	   */
/*	inspect.  As of now, breeder uses only the three most basic variation  */
/*	mechanisms: reproduction, crossover, and mutation.  Reproduction 	   */
/*	means that only the fit will affect the next generation.  Those which  */
/*	are not fit do not reproduce and therefore do not affect the next 	   */
/*	generation.  In crossover, pairs of parents from the breeding (ie fit) */
/*	subpopulation are choosen.  We then choose a random chromosome locus   */
/*	(a locus is an index into the array holding the genes, ie an index	   */
/*	into the chromosome), snip both Chromosomes at that point, and cross   */
/*	the strands; that is, we glue the first piece from the first parent	   */
/*	onto the second piece from the second parent and the first piece	   */
/*	from the second parent onto the second piece from the first parent.	   */
/*	Got it?  Or wait... Did I get that backwards?  No.  Just Kidding.	   */
/*	During crossover, mutation can also occur.  If an allele (chromosome   */
/*	array element value) is to be mutated, we just add some noise-- a	   */
/*	gaussian random variable with a specified STD.						   */
/*																		   */
/*	Crossover and mutation do not happen every time.  There are variables  */
/*	specifying the probabilities of these events.  Right now, these and    */
/*	many other useful parameters are in the file Curves.h.  To experiment  */
/*	with them, modify the values set in Curves.h and make to recompile.    */
/*	Only breed.c should need to be recompiled.							   */
/*	(At some point in the near future, useful parameters will appear in    */
/*	the control panel so you'll be able to tinker with them without		   */
/*	recompiling.  At a later date, all or some parameters may be specified */
/*	BY THE GENETIC MATERIAL ITSELF!  Then things will really get		   */
/*	interesting.  If some variation mechanism is counter-productive in a   */
/*	particular application, the probability of that variation should	   */
/*	diminish and perhaps vanish.  Further down the road, hopefully the	   */
/*	genes will code (some of) the variation	mecahisms.  Then new ones will */
/*	be able to develop as required by the problem, the changing search     */
/*	space or the environment--whatever you want to call it.				   */
/*																		   */
/*																		   */
/*	This file is where the action is.  The main routine, which sets up the */
/*	interactive system and then lets it go, is in this file.  Also, all the*/
/*	genetic operators are implemented here.  Once again, parameters to     */
/*	tweak are in Curves.h.  The embryological stuff (routine to graph	   */
/*	polynomials) is in the file Grow.c.									   */
/*																		   */
/*	To run, type 'b' (the letter b).  This is a command  				   */
/*	script which executes 'breed' (compiled program) and passes it the time*/
/*	as a seed for the random number generator.  Hint: if you want 		   */
/*	everything to go faster, shrink the population window.  The smaller    */
/*	it is, the faster everything goes.									   */
/*																		   */
/*	Acknowledgements: the code for the user interface was largely		   */
/*	appropriated from GED, written by Mike McDougall, Rob Allen, me, and   */
/*	others at Williams College.  The Gaussian noise generator was written  */
/*	by Donald House and translated by Rob Allen.  And of course the idea   */
/*	for the program was inspired by Richard Dawkins' Blind Watchmaker.	   */
/*	This program was originally written as a project in Donald House's AI  */
/*	course.  Thanks to Duane Bailey and Don House for supervision and 	   */
/*	funding over the summer of 1990.									   */
/*																		   */
/***************************************************************************/




/* -------------------------------------------------------------------------*/
/*			 INCLUDES									 				    */
/* -------------------------------------------------------------------------*/

#include "Control.h"
#include "GA.h"
#include "Curves.h"
#include <math.h>

/* -------------------------------------------------------------------------*/
/*			GLOBAL VARIABLES		 										*/
/* -------------------------------------------------------------------------*/

/*	Genetic variables														*/
/* -------------------------------------------------------------------------*/
Population	G_Population[MAX_POP];	/* Population is an array of Organisms. */
Population	G_Kids_Pop[MAX_POP];	/* Next generation						*/

int			G_size_pop;				/* # organisms currently in population	*/
int			G_size_breeding_pop;	/* # organisms who will reproduce		*/
double		G_fit_thresh;			/* fitness threshold					*/
double		G_pCross;				/* probability of crossover				*/
double		G_pMutation;			/* probability of mutation				*/
double		G_mutation_std;			/* std of gaussian fn used to mutate	*/
double		G_weight[MAX_CHROM_SIZE];/*weight for each term					*/
double		G_sum_weights;			/* Sum of weights-- for yscaling		*/
int			G_generation;			/* # generations so far					*/

double		drand48();
double		Fitness();


/*	User interface variables												*/
/* -------------------------------------------------------------------------*/
Panel		G_control_panel;		/* Panel for program's control buttons	*/
Panel_item	G_file_field;			/* File names for I/O					*/
Panel_item  G_file_path_field;  	/* Default I/O path   			        */
Panel		G_fitness_panel;		/* Panel for fitness toggles			*/
Panel_item	G_fit_switch[INIT_POP];	/* Array holding fitness toggles		*/
int			G_switch_default;		/* Whether switches start on or off		*/
int			G_show_genes;			/* Whether gene window is displayed		*/
int			G_print_out;			/* Whether print out is enabled			*/
Frame		G_control_frame;		/* Frame containing the control panel	*/
Frame		G_population_frame;		/* Holds the whole environment			*/

Canvas		G_population_window;			/* A big Canvas for creatures	*/
Pixwin	   *G_population_pw;				/* A pixwin for all creatures	*/

Rect	   *G_population_rect;				/*  Used to lock screen			*/
Pixfont	   *G_population_font;				/*	This is the system font		*/

int			G_current_width;				/*  Current size of pixwin		*/
int			G_current_height;

int			G_org_height;					/*  Size in pixels of organism	*/
int			G_org_width;

int			G_x_scale;						/*	These are used for scaling	*/
int			G_y_scale;						/*	curves.						*/
int			G_x_trans;
int			G_y_trans;


/* DUMMY functions */
LoadPOP() {};
SavePOP() {};


/*--------------------------------------------------------------------------*/
/*																			*/
/*	MAIN																	*/
/*																			*/
/*--------------------------------------------------------------------------*/

main(argc, argv)
int argc;
char* argv[];
{
	if (argc > 1) init((double) atoi(argv[1]));
	else init(0.0);
	MakeWindows();
	window_main_loop(G_control_frame);	/*	begin event handling, and
											show windows.	*/
}



/*--------------------------------------------------------------------------*/
/*	INIT-- set initial variable values										*/
/*--------------------------------------------------------------------------*/

init(seed)
double	seed;
{

	G_size_pop		=	INIT_POP;
	G_fit_thresh	=	INIT_FIT_THRESH;
	G_switch_default=	INIT_SWITCH_DEF;
	G_show_genes	=	INIT_SHOW_GENES;
	G_print_out		=	INIT_PRINT_OUT;
	G_pCross		=	pCROSS;
	G_pMutation		=	pMUTATION;
	G_mutation_std	=	MUTATION_STD;
	G_generation	=	0;

	srand48(seed);
	Randomize_Pop();
}





/*--------------------------------------------------------------------------*/
/*	RANDOMIZE sets organism's genes to random values between -.5 and +.5	*/
/*--------------------------------------------------------------------------*/
/* 6/4/90, changed max and min gene values from -.5,.+5 to -1.0,+1.0		*/
Randomize (org, name, size_chrom)
Organism*	org;
int			name;
int			size_chrom;
{
	double	dpow();
	int		gene_i;

	org->name				= name;
	org->size_chrom			= size_chrom;

	for (gene_i = 0; gene_i < size_chrom; gene_i++) {
		org->X_Chrom[gene_i]= (2.0*drand48())-1.0;
		org->Y_Chrom[gene_i]= (2.0*drand48())-1.0;
		G_weight[gene_i]	= dpow(WEIGHT_BASE, gene_i);
		G_sum_weights		+= G_weight[gene_i];
	}

	org->fitness			= 0.0;
	org->mom				= 0;
	org->dad				= 0;

	G_sum_weights			= 0.0;
}



/*--------------------------------------------------------------------------*/
/*	RANDOMIZE_POP initializes the population								*/
/*--------------------------------------------------------------------------*/
Randomize_Pop()
{
	int			org_i;

	for (org_i = 0; org_i < G_size_pop; org_i++) {
		Randomize(&G_Population[org_i], org_i, INIT_CHROM);
	}
}




/*--------------------------------------------------------------------------*/
/*	RND--  a uniformly distributed random fn between low&high (inclusive)	*/
/*--------------------------------------------------------------------------*/
/* 6/7/90-- bug fixed */

Rnd (low, high)
int low, high;
{
	double	alpha;
	int		beta;

	alpha	= drand48(); /* alpha is a uniform rand dist var on [0.0, 1.0)	*/
	beta	= (int) (low + ((high-low+1)*alpha));
	return(beta+low);
}



/*--------------------------------------------------------------------------*/
/*	FLIP--  a boolean value with specified probability						*/
/*--------------------------------------------------------------------------*/
Flip(p)
double p;
{
	/* result of a weighted coin toss-- a Bernouli random variable			*/
	int result;

	if (p==1.0) {
		result = 1;
	}
	else {
		if (drand48() <= p) {
			result = 1;
		}
		else {
			result = 0;
		}
	}
	return(result);
}


/*--------------------------------------------------------------------------*/
/*	COPY genetic material from one organism to another						*/
/*--------------------------------------------------------------------------*/
Copy(org1, org2)
Organism* org1;
Organism* org2;
{
	int gene_i;

	org2->name					= org1->name;
	org2->size_chrom			= org1->size_chrom;
	for (gene_i= 0; gene_i		< org1->size_chrom; gene_i++) {
		org2->X_Chrom[gene_i]	= org1->X_Chrom[gene_i];
		org2->Y_Chrom[gene_i]	= org1->Y_Chrom[gene_i];
	}
	org2->mom					= org1->mom;
	org2->dad					= org1->dad;
}


/*--------------------------------------------------------------------------*/
/*	COPY_POP-- copy genes from one population to another					*/
/*--------------------------------------------------------------------------*/
Copy_Pop(pop1, pop2, size_pop)
Population* pop1;
Population* pop2;
int			size_pop;
{
	int org_i;

	for (org_i= 0; org_i < size_pop; org_i++) {
		Copy(&pop1[org_i], &pop2[org_i]);
	}
}



/*--------------------------------------------------------------------------*/
/*	ERASE genetic material													*/
/*--------------------------------------------------------------------------*/
Erase(org)
Organism* org;
{
	int gene_i;

	for (gene_i= 0; gene_i < org->size_chrom; gene_i++) {
		org->X_Chrom[gene_i]= 0.0;
		org->Y_Chrom[gene_i]= 0.0;
		org->size_chrom		= 0;
		org->fitness		= 0.0;
	}
}



/*--------------------------------------------------------------------------*/
/*	SELECT-- pick out a single eligible individual		 					*/
/*--------------------------------------------------------------------------*/
Select(size_pop)
int	size_pop;
{
	return(Rnd(0,size_pop-1));
}




/*--------------------------------------------------------------------------*/
/*	MUTATION-- agggggh!!!!!													*/
/*--------------------------------------------------------------------------*/
double Mutation(allele)
double allele;
{
extern	double	noise();

	if (Flip(G_pMutation)==1) {
		allele = noise(allele, G_mutation_std);
	}
	return(allele);
}



/*--------------------------------------------------------------------------*/
/*	CROSSOVER-- i.e. SEX!!	XXX!!!											*/
/*--------------------------------------------------------------------------*/
/* Note-- if we removed possibility of combining chromosomes of different	*/
/* lengths and it were still possible for chrom length to change then we'd	*/
/* have...  speciation!														*/

/* Should try to have GENES specify how genetic material is  recombined.	*/

/*	This is how Crossover works: choose a crossover point, i_cross.
	That partitions the parents, p1 and p2, into two strings:
	p1 = p1a.p1b	p2 = p2a.p2b.

	Then the children c1 and c2, are:
	c1 = p1a.p2b	c2 = p2a.p1b
*/


Crossover(parent1, parent2, child1, child2, first_born_name)
Organism*	parent1;
Organism*	parent2;
Organism*	child1;
Organism*	child2;
int			first_born_name;
{
	int	gene_i, i_cross;
	int	short_chrom, long_chrom;
/*	
	printf("Mom: \n");
	Print_Org(parent1);
	printf("Dad: \n");
	Print_Org(parent2);
*/

	child1->name = first_born_name;
	child2->name = first_born_name+1;

	/* We will use size of smaller chromosome as max crossover locus		*/
	if (parent1->size_chrom > parent2->size_chrom) {
		long_chrom	= parent1->size_chrom;
		short_chrom	= parent2->size_chrom;
	}
	else {
		long_chrom	= parent2->size_chrom;
		short_chrom	= parent1->size_chrom;
	}

/*	Part changed when 2nd chrom added										*/
/* ------------------------------------------------------------------------	*/
/* Now we vary X and Y Chroms completely seperately.  They don't cross over */
/* at the same points or at the same times.  Also, we don't use any inter-	*/
/* chromosomal variation operators like segregation and translocation.		*/

/*	X_CHROM																	*/
	if (Flip(G_pCross)==1) {			/* Crossover occurs with p(G_pCross)*/
		i_cross = Rnd(0,short_chrom-1);	/* Randomly pick a crossover locus	*/
	}
	else {
		i_cross = short_chrom;	/*INCORRECT if diff Chrom sizes!!!			*/
	}

/*
	printf("i_cross = %d \n", i_cross);
*/
	/* Copy first part of Chroms directly to children						*/
	for (gene_i = 0; gene_i < i_cross; gene_i++) {
		child1->X_Chrom[gene_i] = Mutation(parent1->X_Chrom[gene_i]);
		child2->X_Chrom[gene_i] = Mutation(parent2->X_Chrom[gene_i]);
	}

	/* parent2 to child 1	(a cross)										*/
	for (gene_i = i_cross;	gene_i </*= ?*/ short_chrom; gene_i++) { 
		child1->X_Chrom[gene_i] = Mutation(parent2->X_Chrom[gene_i]);
	}

	/* parent1 to child 2	(a cross)										*/
	for (gene_i = i_cross;	gene_i </*= ?*/ long_chrom; gene_i++) { 
		child2->X_Chrom[gene_i] = Mutation(parent1->X_Chrom[gene_i]);
	}


/*	Y_CHROM																	*/
	if (Flip(G_pCross)==1) {			/* Crossover occurs with p(G_pCross)*/
		i_cross = Rnd(0,short_chrom-1);	/* Randomly pick a crossover locus	*/
	}
	else {
		i_cross = short_chrom;	/*INCORRECT if diff Chrom sizes!!!			*/
	}

/*
	printf("i_cross = %d \n", i_cross);
*/
	/* Copy first part of Chroms directly to children						*/
	for (gene_i = 0; gene_i < i_cross; gene_i++) {
		child1->Y_Chrom[gene_i] = Mutation(parent1->Y_Chrom[gene_i]);
		child2->Y_Chrom[gene_i] = Mutation(parent2->Y_Chrom[gene_i]);
	}

	/* parent2 to child 1	(a cross)										*/
	for (gene_i = i_cross;	gene_i </*= ?*/ short_chrom; gene_i++) { 
		child1->Y_Chrom[gene_i] = Mutation(parent2->Y_Chrom[gene_i]);
	}

	/* parent1 to child 2	(a cross)										*/
	for (gene_i = i_cross;	gene_i </*= ?*/ long_chrom; gene_i++) { 
		child2->Y_Chrom[gene_i] = Mutation(parent1->Y_Chrom[gene_i]);
	}





	child1->size_chrom	= parent2->size_chrom;
	child2->size_chrom	= parent1->size_chrom;

	child1->fitness		= 0.0;
	child2->fitness		= 0.0;

	child1->mom			= parent1->name;
	child2->mom			= parent1->name;

	child1->dad			= parent2->name;
	child2->dad			= parent2->name;

/*
	printf("Fred: \n");
	Print_Org(child1);
	printf("Jane: \n");
	Print_Org(child2);
*/

}



/*--------------------------------------------------------------------------*/
/*	BREED-- mix up the genes												*/
/*--------------------------------------------------------------------------*/

Breed() 
{
	int org_i, size_eligible_pop;
	int	Mom, Dad;

	size_eligible_pop = 0;
	/* Isolate the breeding subpopulation									*/
	for (org_i= 0; org_i < G_size_pop; org_i++) {
		/* Set fitness values */
		G_Population[org_i].fitness = Fitness(org_i);
		if (Fitness(org_i) > .5) size_eligible_pop++;
		SetToggleToDefault(org_i);
	}
	if (size_eligible_pop < 2) size_eligible_pop = G_size_pop;
	else {
		size_eligible_pop = 0;
		for (org_i= 0; org_i < G_size_pop; org_i++) {
			if (G_Population[org_i].fitness > G_fit_thresh) {
/*
				printf("Copying %d to %d \n",org_i, size_eligible_pop);
				printf("original %d: \n",org_i);
				Print_Org(&G_Population[org_i]);
				printf("\n");
				printf("original %d: \n",size_eligible_pop);
				Print_Org(&G_Population[size_eligible_pop]);
				printf("\n");
*/
				/* This is a tiny optimization-- don't copy to same place	*/
				if (org_i != size_eligible_pop) {
					Copy(&G_Population[org_i], &G_Population[size_eligible_pop]);
				}
/*
				printf("%d after copy: \n",size_eligible_pop);
				Print_Org(&G_Population[size_eligible_pop]);
				printf("\n");
*/
				size_eligible_pop++;  /* Increment exactly once per FIT organism*/
			}
			G_Population[org_i].fitness = 0.0;
		}
	}
	/* Each couple has two kids.  Nuclear families!							*/
	for (org_i = 0; org_i < G_size_pop; org_i += 2) {
		Dad = Select(size_eligible_pop);
		do {
			Mom = Select(size_eligible_pop);
		}
		while (Mom == Dad);

		Crossover(&G_Population[Mom], &G_Population[Dad],
				  &G_Kids_Pop[org_i], &G_Kids_Pop[org_i+1], org_i);
	}

	/* Now the kids grow up-- make the new generation the current one		*/
	Copy_Pop(G_Kids_Pop, G_Population, G_size_pop);

	/* Here we actually put the kids on the screen							*/
	Grow_Pop();
}




/*--------------------------------------------------------------------------*/
/*	FITNESS-- calculate fitness; in this case by checking toggles			*/
/*--------------------------------------------------------------------------*/

double Fitness(org_i)
int org_i;
{
	int fit;

	fit = (int) panel_get_value(G_fit_switch[org_i]);
	return (double) fit;
}


/*--------------------------------------------------------------------------*/
/*	SET TOGGLE TO DEFAULT-- clear ot set a toggle switch					*/
/*--------------------------------------------------------------------------*/

SetToggleToDefault(org_i)
int org_i;
{
	panel_set_value(G_fit_switch[org_i],G_switch_default);
}


/*--------------------------------------------------------------------------*/
/*	PRINT_POP-- print population											*/
/*--------------------------------------------------------------------------*/


Print_Pop()
{
	int org_i;
	for (org_i = 0; org_i < G_size_pop; org_i++) {
		Print_Org(&G_Population[org_i]);
	}
	printf("\n");
}



/*--------------------------------------------------------------------------*/
/*	PRINT_ORG-- print out an organism										*/
/*--------------------------------------------------------------------------*/


Print_Org(org)
Organism* org;
{
	int gene_i;
	printf("%d of %d and %d: ",org->name, org->mom, org->dad);
	for (gene_i = 0; gene_i < org->size_chrom; gene_i++) {
		printf("\t(%1.3g, %1.3g)    ",org->X_Chrom[gene_i],org->Y_Chrom[gene_i]);
	}
	printf("\t%g", org->fitness);
	printf("\n");
}
