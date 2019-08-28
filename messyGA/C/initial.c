/*========================================================
       file : initial.c 
  
    purpose : create initial population and initialize parameters
 
  developed : 1991

     author : Kalyanmoy Deb
==========================================================*/

#include "mga.ext"


void get_input()
/* read and set input parameters */
{
	register int i;
	FILE *fp, *finput, *fopen();
	long clock, time();
	char *ctime(), msg[PAGEWIDTH];

	gen_file("Parameters",         Inputfilename,    "parameters");
	gen_file("Era",                Erafilename,      "era");
	gen_file("Objective function", Objfilename,      "subfunc");
	gen_file("Template",           Templatefilename, "template");
	gen_file("Partition input",    Partinfilename,   "partitions");
	gen_file("Pop record input",   Poprinfilename,   "poprecin");
	gen_file("Extra members",      Extrafilename,    "extra");
	gen_file("Output",             Outputfilename,   "output");
	gen_file("Pop record output",  Poproutfilename,  "poprecout");
	gen_file("Partition output",   Partoutfilename,  "partout");
	gen_file("Plotting",           Plotfilename,     "plot");

	/*  check input files  */
	check_input_file(Inputfilename);

	/*  read input parameters  */
	finput = fopen(Inputfilename, "r");
	if (fscanf(finput, FORMAT_1, VARS_1) != 17) {
		sprintf(msg,"Error in the input file: parameters.\n");
		print_error(msg);
	}

	check_input_file(Erafilename);
	check_input_file(Objfilename);
	if (extrapopflag)
		check_input_file(Extrafilename);

	/*  open output files  */
	scratch_file(Outputfilename);
	if (plotstatflag)
		scratch_file(Plotfilename);
	if (popprintflag) {
		check_input_file(Poprinfilename);
		scratch_file(Poproutfilename);
	}
	if (partitionflag) {
		check_input_file(Partinfilename);
		scratch_file(Partoutfilename);
	}

	/*  set up the clock  */
	fp = fopen(Outputfilename, "a");
	time(&clock);
	fprintf(fp,"Start-up time is : %s\n",ctime(&clock));
	fclose(fp);

	/*  generate random numbers  */
	randomize();

	/*  get the template  */
	get_template();

	fscanf(finput,"Copies (10 1 1) =");
	if(!(copies = (int *)malloc((max_era+1) * sizeof(int))))
	{
		sprintf(msg,"Insufficient memory for variable, copies.\n");
		print_error(msg);
	}

	for (i = era; i <= max_era; i++) {
		copies[i] = 1;     /* default */
		fscanf(finput,"%d",&copies[i]);
	}
	fclose(finput);


	/*  get the objective function information  */
	function_evaluations = 0.0;
	objfunc_info();

	/*  get the auxiliary information for report  */
	if (partitionflag)  
		partition_info();
	if (popprintflag)
		poprec_info();

	/* writes the problem information */
	problem_rep();
}


void initialize()
/* initialize parameters */
{
	char msg[PAGEWIDTH];

	if (thresholdingflag)
		shuffle_num = problem_length;

	if (popprintflag)
	{
		/* set the counter to the first generation */
		nextpopstatgen = sortpopstatgen[0];
	}

	/*  get other input parameters  */
	if (fscanf(fin, FORMAT_2, VARS_2) != 2) {
		sprintf(msg,"Error in input file era.\n");
		print_error(msg);
	}

	/*  generate initial population, get stats, and write report  */
	initialize_pop();
	statistics(oldpop);
	/*  set up the population sizing in primordial phase  */
	setup_popsize();
	initreport();
}


void initialize_pop()
/* setup parameters for initial population */
{
	register int i;
	long member_id = 0;
	int temp_order, allele_combo;
	long max_size, total_size = 0, population_size[MAX_ORDER];
	char msg[PAGEWIDTH];

	gen = 0;
	stopmgaflag = 0;
	/* reset the population size array */
	for (i=0; i <= era; i++) population_size[i] = 0;

	/* only order era strings are created */
	order = temp_order = era;

	/* but if tiebreaking is on, 
	   all strings of order less than era are created */
	if (tiebreakingflag) {
		order = temp_order = 1;
	}
	while (order <= era) {
	 	/* if reduction in initpop is desired */
		if (r_initpop_flag)
			allele_combo = 1;
		else
			allele_combo = power(2,order);
		max_size = copies[order] * allele_combo * \
				choose(problem_length,order);
		population_size[order] = max_size;
		total_size += max_size;
		order++;
	}
	/*  storage allocation for */
	/*  population array  */
	if(!(newpop=(struct INDIVIDUAL *)malloc(total_size * \
				sizeof(struct INDIVIDUAL))))
	{
		sprintf(msg,"Insufficient memory for newpop.\n");
		print_error(msg);
	}
	if(!(oldpop=(struct INDIVIDUAL *)malloc(total_size * \
				sizeof(struct INDIVIDUAL))))
	{
		sprintf(msg,"Insufficient memory for oldpop.\n");
		print_error(msg);
	}
	/*  storage for shuffle array  */
	if (!(shuffle = (int *)malloc(total_size*sizeof(int *))))
	{
		sprintf(msg,"Insufficient memory for shuffle array.\n");
		print_error(msg);
	}
	order = temp_order;
	while (order <= era) {
		initpop(&member_id, population_size[order]);
		order++;
	}
	if (extrapopflag)
		extra_pop(&member_id);
	order--;
	storepop(&best_indv,era);
	copy_chrom(oldpop[0],&best_indv);

	popsize = member_id;
	templatefitness = objfunc(template);
}

void extra_pop(last_member)
long *last_member;
/*  creates extra population members  */
{
	register int i,j,k,m;
	int flag=0, count=0, extra_size=0;
	long size, cnt;
	int patsize[MAX_PARTITIONS], extra_copies[MAX_PARTITIONS];
	GENE_ID pattern[MAX_PARTITIONS][MAX_ORDER];
	ALLELES allele_list[MAX_ORDER];
	struct GENE *tailold;
	char dummy[PAGEWIDTH];
	FILE *fp, *fopen();

	fp = fopen(Extrafilename,"r");
	get_string(fp, dummy, PAGEWIDTH);
	/* read gene information */
	while (fscanf(fp,"%d",&patsize[count]) == 1) {
		for (i=0; i<patsize[count]; i++)
			fscanf(fp,"%d",&pattern[count][i]);
		fscanf(fp,"%d\n",&extra_copies[count]);
		extra_size += extra_copies[count] * power(2,patsize[count]);
		count++;
        }
	fclose(fp);
	size = (*last_member) + extra_size;
	/* reallocate memory size and allocate memory for extra individuals */
	reallocate_memory(size);
	cnt = (*last_member);
	for (i=0; i<count; i++) {
		for (j=0; j<patsize[i]; j++)
			allele_list[j] = 0;
		for (j=0; j<power(2,patsize[i]); j++) {
                        if (j != 0)
				next_allele_comb(allele_list,patsize[i],patsize[i]);
			for (k=0; k<extra_copies[i]; k++) {
				storepop(&(oldpop[cnt]),patsize[i]);
				storepop(&(newpop[cnt]),patsize[i]);
				tailold = oldpop[cnt].firstgene;
				tailold->genenumber = pattern[i][0];
				tailold->allele = allele_list[0];

				for (m=1; m<patsize[i]; m++) {
					tailold = tailold->nextgene;
					tailold->genenumber = pattern[i][m];
					tailold->allele = allele_list[m];
				}
				fill_chrom(&(oldpop[cnt]));
				oldpop[cnt].fitness = \
					objfunc(oldpop[cnt].fullchrom);
				cnt++;
			}
		}
	}
        *last_member = cnt;
}


void randomtemplate(temp)
unsigned *temp;
/*  create a random template  */
{
	register int i,j;
	unsigned mask = 1;

	for (i = 0; i < bytesize; i++)
	{
		temp[i] = 0;
		for (j = 0; j < bytelimit[i]; j++)
		{
			if (flip(0.5))
				temp[i] |= mask;
			if (j < bytelimit[i]-1)
				temp[i] <<= 1;
		}
	}
}


void next_genic_comb(list, n)
GENE_ID *list;
int n;
/*    calculates the next gene combination  */
{
	register int j, s, k;
	BOOLEAN exitflag = 0;
	int pos;

	pos = n-1;
	if (list[pos] < problem_length - 1)
		(list[pos])++;
	else
	{
		for (j = pos; j > 0 && !exitflag; j--)
		{
			if (list[j-1] < problem_length - pos + j - 2)
			{
				(list[j-1])++;
				for (s = j; s <= pos; s++)
				{
					list[s] = list[j-1] + s - j + 1;
					exitflag = 1;
				}
			}
		}
	}
}


void next_allele_comb(list,size,len)
ALLELES *list;
int size, len;
/*  calculates the next bit combination  */
{
	register int i;
	int total=0, pos;
	ALLELES allele_max = 1;

	pos = size-1;
	for (i = 0; i < pos+1; i++)
	{
		if (list[i])  total++;
	}
	if (list[pos] < allele_max)
		(list[pos])++;
	else if (total == pos+1)
		reset_list(list,size);
	else if ((list[pos-1] >= allele_max) && (list[pos] >= allele_max))
		next_allele_comb(list, pos, len);
	else if (list[pos] >= allele_max)
	{
		for (i = pos; i < len; i++)
			list[i] = 0;
		(list[pos-1])++;
	}
}


void storepop(pop, n)
struct INDIVIDUAL *pop;
int n;
/*  allocates memory for an individual */
{
	register int j;
	struct GENE *tail;
	char msg[PAGEWIDTH];

	/*  storage allocation for */
	/*  genes */
	if(!((*pop).firstgene = (struct GENE *)malloc(sizeof(struct GENE))))
	{
		sprintf(msg,"Insufficient memory for firstgene.\n");
		print_error(msg);
	}

	/*  initialize genes  */
	tail = (*pop).firstgene;

	/*  for each gene  */
	for (j = 1; j < n; j++)
	{
		if (!(tail->nextgene = (struct GENE *)malloc(sizeof(struct GENE))))
		{
			sprintf(msg,"Insufficient memory for making a gene.\n");
			print_error(msg);
		}
		tail = tail->nextgene;
	}

	/*  assign last gene pointer and chromosome length */
	(*pop).lastgene = tail;
	tail->nextgene = NIL;
	(*pop).chromlen = n;
	(*pop).genelen  = n;

	/*  allocate storage for fullchrom  */
	if(!((*pop).fullchrom = (unsigned *)malloc(bytesize*sizeof(unsigned))))
	{
		sprintf(msg,"Insufficient memory for fullchrom.\n");
		print_error(msg);
	}

	/*  allocate storage for fullgene  */
	if(!((*pop).fullgene = (unsigned *)malloc(bytesize*sizeof(unsigned))))
	{
		sprintf(msg,"Insufficient memory for fullgene.\n");
		print_error(msg);
	}
}


void initpop(last_member, n)
long *last_member;
long n;
/*  generates initial population  */
{
	register int i, j, m, count;
	long current_size;
	int allele_combo;
	GENE_ID genenumber_list[MAX_ORDER];
	ALLELES allele_list[MAX_ORDER];
	struct GENE *tailold;
	char msg[PAGEWIDTH];

        sprintf(msg,"   Initpop entered for order %d",order);
	TRACE(msg);

        /* check if reduced population is desired */
	if (r_initpop_flag) 
		allele_combo = 1;
	else
		allele_combo = power(2, order);
	current_size = (*last_member) + n;
	for (i= (*last_member); i < current_size; i++) {
		/*  store oldpop and newpop  */
		storepop(&(oldpop[i]),order);
		storepop(&(newpop[i]),order);
	}
	/*   assign values  */
	for (i = 0; i < order; i++)
		genenumber_list[i] = i;

	for (count=(*last_member); count < current_size; ) {
		for (i = 0; i < order; i++)
			allele_list[i] = 0;
		for (j = 0; j < allele_combo; j++) {
			if (j != 0)
				/*   get the next allele combination  */
				next_allele_comb(allele_list, order, order);

			for (m = 0; m < copies[order]; m++)
			{
				tailold = oldpop[count].firstgene;
				tailold->genenumber = genenumber_list[0];
				if (r_initpop_flag)
					tailold->allele = mut( \
					pickallele(genenumber_list[0], template));
				else 
					tailold->allele = allele_list[0];

				for (i = 1; tailold->nextgene != NIL; i++) {
					tailold = tailold->nextgene;
					tailold->genenumber = genenumber_list[i];
					if (r_initpop_flag)
						tailold->allele = mut( \
						pickallele(genenumber_list[i],\
								template));
					else
						tailold->allele = allele_list[i];
				}

				/*  fill up the under-specified genes  */
				fill_chrom(&(oldpop[count]));

				/*  evaluate objective function value  */
				oldpop[count].fitness = \
					objfunc(oldpop[count].fullchrom);

				count++;
			}
		}
		/*  get the next gene combination  */
		next_genic_comb(genenumber_list, order);
	}
	*last_member = current_size;

	TRACE("   Initpop exited\n");
}


int mut(bit)
int bit;
/* returns a 1 if the bit is 0 and vice versa */
{
	return((bit == 1) ? 0 : 1);
}


void objfunc_info()
/*  reads objective function information  */
{
	register int i, j, k;
	ALLELES allele_max = 1;
	int sum, maxallele, bit, string_length, curr_len;
	int numtable, numstrings, tablenum;
	GENE_ID dummygene[MAX_ORDER];
	double perfm;
	char str[PAGEWIDTH], dummy[PAGEWIDTH], msg[PAGEWIDTH];
	FILE *fp, *fopen();

	fp = fopen(Objfilename,"r");

	/*  read subfunction table information, skip a line  */
	get_string(fp,dummy,PAGEWIDTH);

	maxallele = allele_max + 1;

	fscanf(fp,"%d\n",&numtable);

	if(!(taborfunc = (BOOLEAN *)malloc(numtable * sizeof(BOOLEAN))))
	{
		sprintf(msg,"Insufficient memory for taborfunc.\n");
		print_error(msg);
	}
	if(!(str_length = (int *)malloc(numtable * sizeof(int))))
	{
		sprintf(msg,"Insufficient memory for str_length.\n");
		print_error(msg);
	}
	if(!(chromfitness = (double **)malloc(numtable * sizeof(double *))))
	{
		sprintf(msg,"Insufficient memory for chromfitness.\n");
		print_error(msg);
	}

	get_string(fp,dummy,PAGEWIDTH);
	for (k = 0; k < numtable; k++)
	{
		/* read size of the subfunction */
		fscanf(fp,"%d\n",&tablenum);
		fscanf(fp,"%s\n",&str[0]);
		fscanf(fp,"%d\n",&string_length);
		str_length[tablenum] = string_length;
		taborfunc[tablenum] = 0;

		if ((strcmp(str,"Table") == 0) || (strcmp(str,"table") == 0))
		/* read a table, if the subfunction is a table  */
		{
			taborfunc[tablenum] = 1;
			numstrings = power(maxallele,string_length);
			if(!(chromfitness[tablenum] = (double *) \
					malloc(numstrings * sizeof(double))))
			{
				sprintf(msg,"Insufficient memory for chromfitness.\n");
				print_error(msg);
			}

			for (i = 0; i < numstrings; i++)
			{
				for (j = 0,sum = 0; j < string_length; j++)
				{
					/*   read each bit  */
					fscanf(fp,"%d",&bit);
					sum += power(maxallele,\
						string_length-j-1) * bit;
				}
				/*  read the corresponding objective function value  */
				fscanf(fp,"%lf\n",&perfm);
				chromfitness[tablenum][sum] = perfm;
			}
		}
		get_string(fp,dummy,PAGEWIDTH);
	}
	/*  read subfunction information, skip a line  */
	get_string(fp,dummy,PAGEWIDTH);

	/*  read number of subfunctions */
	fscanf(fp,"%d",&numsubfunc);

	if(!(table_id = (int *)malloc(numsubfunc * sizeof(int))))
	{
		sprintf(msg,"Insufficient memory for table_id.\n");
		print_error(msg);
	}
	if(!(scale = (float *)malloc(numsubfunc * sizeof(float))))
	{
		sprintf(msg,"Insufficient memory for scale.\n");
		print_error(msg);
	}

	if(!(genesets = (GENE_ID **)malloc(numsubfunc * sizeof(GENE_ID *))))
	{
		sprintf(msg,"Insufficient memory for variable, genesets.\n");
		print_error(msg);
	}

	/*  for each subfunction  */
	for (i = 0; i < numsubfunc; i++)
	{
		/*  read  the table number  */
		fscanf(fp,"\n%d",&table_id[i]);
		fscanf(fp,"%f",&scale[i]);

		curr_len = str_length[table_id[i]];
		read_subfunction(fp,dummygene,curr_len);
		if(!(genesets[i] = (GENE_ID *)malloc(curr_len * sizeof(GENE_ID))))
		{
			sprintf(msg,"Insufficient memory for genes.\n");
			print_error(msg);
		}
		for (j = 0; j < curr_len; j++)
			genesets[i][j] = dummygene[j];
	}
	fclose(fp);
}

void read_subfunction(fp,dummygene,nstruc)
FILE *fp;
GENE_ID dummygene[];
int nstruc;
/*  reads subfunction data  */
{
	register int i;
	int count = 0, sets = 0, prev = 0;

	do {
		prev = sets;
		fscanf(fp,"%d",&sets);
		if (sets >= 0) {
			dummygene[count] = sets;
			count++;
		}
		else {
			sets *= -1;
			for (i = 0; i <= sets - prev; i++) {
				dummygene[count+i] = prev + i + 1;
			}
			count += sets - prev;
		}
	}   while (count < nstruc);
}


void partition_info()
/*  reads partition information for analysis  */
{
	register int i, j;
	FILE *fp, *fopen();
	GENE_ID dummygene[MAX_PARTITIONSIZE];
	char dummy[PAGEWIDTH], msg[PAGEWIDTH];

	fp = fopen(Partinfilename,"r");

	/*  read a comment  */
	get_string(fp, dummy, PAGEWIDTH);

	/* read the number of partitions */
	fscanf(fp,"%d",&numpartition);
	if(!(partition_len = (int *)malloc(numpartition * sizeof(int))))
	{
		sprintf(msg,"Insufficient memory for variable partition_len.\n");
		print_error(msg);
	}
	if(!(partition_genes = (GENE_ID **)malloc(numpartition * \
							sizeof(GENE_ID *))))
	{
		sprintf(msg,"Insufficient memory for partition genes.\n");
		print_error(msg);
	}	

	/*  for each structure  */
	for (i = 0; i < numpartition; i++)
	{
		fscanf(fp,"\n%d",&partition_len[i]);

		read_subfunction(fp,dummygene,partition_len[i]);

		if(!(partition_genes[i] = (GENE_ID *)malloc(partition_len[i] * \
							sizeof(GENE_ID))))
		{
			sprintf(msg,"Insufficient memory for partition genes.\n");
			print_error(msg);
		}

		for (j = 0; j < partition_len[i]; j++)
			partition_genes[i][j] = dummygene[j];
	}
	fclose(fp);
}

void poprec_info()
/* reads generations for population record */
{
	register int i;
        int numpopstatgen;
	FILE *fp, *fopen();
	char msg[PAGEWIDTH];

	fp = fopen(Poprinfilename,"r");

	get_string(fp, msg, PAGEWIDTH);

	/*  read the generation numbers for population dump  */
	fscanf(fp,"%d\n",&numpopstatgen);

	if (!(sortpopstatgen = (int *)malloc(numpopstatgen*sizeof(int))))
	{
		sprintf(msg,"Insufficient memory for poprecfile.\n");
		print_error(msg);
	}

	for (i = 0; i < numpopstatgen; i++)
	{
		fscanf(fp,"%d",&(sortpopstatgen[i]));
	}

	/*  sort the numbers in ascending order  */
	sortnum(numpopstatgen, sortpopstatgen);

	fclose(fp);
}

void setup_popsize()
/*  sets up the popsize pattern in primordial phase  */
{
	int cal_cut, noofcut;
	double init_prop, init_prop_1, init_prop_2;

	init_select_gen = 1;
	/*  initial proportion of the best individual  */
	init_prop_1 = problem_length / era;
	init_prop_1 /= (popsize / copies[era]);

	init_prop_2 = prop_bestindv;

	/* choose the greater of two */
	init_prop = (init_prop_1 > init_prop_2) ? init_prop_1 : init_prop_2;
	/* duration of primordial phase: (under tournament selection) is
	   calculated to have the proportion of best individual equals to 0.5 */
	prim_gen = 0;
	if (init_prop < 0.5)
		prim_gen = floor(log((1.0 - init_prop) / init_prop) / log(2.0));

	cal_cut = prim_gen - (init_select_gen - 1);

	if (popsize < juxtpopsize) juxtpopsize = popsize;
	/*  theoretical number of cut from juxtpopsize and initial popsize */
	noofcut = round((log(popsize) - log(juxtpopsize))/log(2.0));

	if (cal_cut <= 0 || noofcut <= 0) {
		/* if no cut is required */
		cutpop_gen = 0;
		juxtpopsize = popsize;
		init_select_gen = 0;
	}
	else if (noofcut > cal_cut) {
		/*  too many cut is requested  */
		printf("Given juxtapositional popsize is too small %d, ",\
						juxtpopsize);
		juxtpopsize = popsize/power(2,cal_cut);
		printf("using %d instead.\n",juxtpopsize);
		cut_every_other_gen = 0;
		cutpop_gen = prim_gen;
	}
	else if (noofcut >= cal_cut / 2) {
		/*  some cut needs to be at every generation  */
		cut_every_other_gen = init_select_gen - 1 + 2 * \
					(cal_cut - noofcut);
		cutpop_gen = prim_gen;
	}
	else {
		/*  only cut at every other generation  */
		cut_every_other_gen = init_select_gen - 1 + 2 * noofcut;
		cutpop_gen = cut_every_other_gen;
	}
}


void get_template()
/*  get template information and create a template  */
{
	register int i, j;
	unsigned mask = 1;
	int id, sum; 
	ALLELES dummytemplate[MAX_SIZE];
	char msg[PAGEWIDTH];
	FILE *fp, *fopen();

	/*  set bytesize  */
	id  = problem_length/UNSINTSIZE;
	bytesize = (problem_length % UNSINTSIZE) ? id+1 : id;

	/*  set up bytelimit array  */
	if(!(bytelimit = (int *)malloc(bytesize*sizeof(int)))) {
		sprintf(msg, "Insufficient memory for bytelimit\n");
		print_error(msg);
	}	
	for (i = 0; i < bytesize; i++)
	{
		if (i == bytesize-1)
			bytelimit[i] = problem_length - (i*UNSINTSIZE);
		else
			bytelimit[i] = UNSINTSIZE;
	}

	/*  memory for template is allocated  */
	if(!(template = (unsigned *)malloc(bytesize*sizeof(unsigned)))) {
		sprintf(msg, "Insufficient memory for template\n");
		print_error(msg);
	}

	if (levelmgaflag)
	{
		era = 1;
		/*  get a random template  */
		randomtemplate(template);
	}
	else
	{
		era = max_era;

		/*  read template  */
		check_input_file(Templatefilename);
		fp = fopen(Templatefilename,"r");
		for (i = 0; i < problem_length; i++)
			fscanf(fp,"%d",&dummytemplate[i]);
		fclose(fp);

		/*  make template  */
		for (i = 0,sum = 0; i < bytesize; i++)
		{
			sum += bytelimit[i];
			template[i] = 0;
			for (j = 0; j < bytelimit[i]; j++)
			{
				if (dummytemplate[sum - 1 - j] == 1)
					template[i] |= mask;
				if (j < bytelimit[i] - 1)
					template[i] <<= 1;
			}
		}
	}
}
