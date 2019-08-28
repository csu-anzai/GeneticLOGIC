/*==============================================
       file  : report.c 
 
     purpose : write reports 
 
   developed : 1991

      author : Kalyanmoy Deb
=================================================*/

#include "mga.ext"


void reportpop(pop)
struct INDIVIDUAL *pop;
/*  controls the output files  */
{
	register int i,j;

	TRACE("   Report entered");

	general_rep();

	if (plotstatflag)
		plot_rep();

	if (popprintflag) {
		if (gen == nextpopstatgen) {
			writepop(pop);
			countpopstatgen++;
			nextpopstatgen = *(sortpopstatgen + countpopstatgen);
		}
	}

	if (partitionflag)
		partitionprop(pop);

	printf("      Best String so far: ");
	writefullchrom(stdout,best_indv.fullchrom);
	printf("                 fitness: %lf\n",best_indv.fitness);

	TRACE("   Report exited");
}


void general_rep()
/* writes the population statistics */ 
{
	FILE *fp, *fopen();

	fp = fopen(Outputfilename,"a");
	fprintf(fp,"%-3d    %6d    %7.2lf  %9.2lf  %9.2lf  %9.2lf %14.4e\n",\
           gen,popsize,avgstrlen,maxfitness,minfitness,avgfitness,\
	   function_evaluations);
	if (stopmgaflag == 1) {
		fprintf(fp,"        Termination: convergence of the population\n");
	}
	else if (stopmgaflag == 2) {
		fprintf(fp,"        Termination: no individual in the initial \
					population is better \n");
		fprintf(fp,"        than the template\n");
	}
	if (gen == prim_gen) {
		repchar(fp,"-",23);
		fprintf(fp," end of primordial phase ");
		repchar(fp,"-",23);
		fprintf(fp,"\n");
	}
	fclose(fp);
}


void plot_rep()
/* writes population statistics for plotting purpose */
{
	FILE *fp, *fopen();

	fp = fopen(Plotfilename,"a");
	fprintf(fp,"%-3d    %6d    %7.2lf  %9.2lf  %9.2lf %14.4e\n",\
	       gencount,popsize,avgstrlen,best_indv.fitness,avgfitness,\
	       function_evaluations);
	gencount++;
	fclose(fp);
}


void fwritechrom(fp,chr,skipchar)
FILE *fp;
struct INDIVIDUAL chr;
int skipchar;
/*  chr is the pointer of the firstgene of a chromosome  */
{
	struct GENE *tail;
	int count = 0;
	int limit;

	limit = PAGEWIDTH - skipchar - 8;
	tail = chr.firstgene;
	fprintf(fp,"( ");
	if (tail != NIL) {
		do {
			if (count >= limit) {
				count = 0;
				fprintf(fp,"\n");
				skip_space(fp,skipchar);
			}
			count += 8;
			fprintf(fp,"(%-3d %1d) ", tail->genenumber, \
						tail->allele);
		}      while ((tail = tail->nextgene) != NIL);
	}
	fprintf(fp,")\n");
}


void writefullchrom(fp,chr)
FILE *fp;
unsigned *chr;
/* Write a chromosome as a string of 1's (true's) and 0's (false's) */
{
	register int j,k;
	unsigned mask = 1;
	unsigned temp;

	for(k = 0; k < bytesize; k++) {
		temp = chr[k];
		for(j = 0; j < bytelimit[k]; j++) {
			if((temp & mask) == 0)
				fprintf(fp,"0");
			else
				fprintf(fp,"1");
			temp >>= 1;
		}
		fprintf(fp," ");
	}
	fprintf(fp,"\n");
}


void problem_rep()
/* writes the problem information */
{
	register int i,j;
	FILE *fp, *fopen();

	fp = fopen(Outputfilename,"a");
	fprintf(fp,"Problem length         = %d\n",problem_length);
	fprintf(fp,"Number of subfunctions = %d\n",numsubfunc);
	fprintf(fp,"Subfunction            = ");
	for (i = 0; i < numsubfunc; i++)
		fprintf(fp,"%3d ",i);
	fprintf(fp,"\n");
	fprintf(fp,"Table or function id   = ");
	for (i = 0; i < numsubfunc; i++)
		fprintf(fp,"%3d ",table_id[i]);
	fprintf(fp,"\n");
	fprintf(fp,"Subfunction size       = ");
	for (i = 0; i < numsubfunc; i++)
		fprintf(fp,"%3d ",str_length[table_id[i]]);
	fprintf(fp,"\n");
	fprintf(fp,"Constitutive Genes:\n");
	for (i = 0; i < numsubfunc; i++) {
		fprintf(fp,"       Subfunction %3d = ",i);
		for (j = 0; j < str_length[table_id[i]]; j++) 
			fprintf(fp,"%3d ",genesets[i][j]);
		fprintf(fp,"\n");
	}
	fprintf(fp,"\n");
	fprintf(fp,"Splice probability     = %lf\n",splice_prob);
	fprintf(fp,"Cut probability        = %lf\n",cut_prob);
	fprintf(fp,"Allelic mutation prob. = %lf\n",allelic_mut_prob);
	fprintf(fp,"Genic mutation prob.   = %lf\n\n",genic_mut_prob);
	fprintf(fp,"Thresholding (1 if on) = %d\n",thresholdingflag);
	fprintf(fp,"Tiebreaking  (1 if on) = %d\n\n",tiebreakingflag);
	fprintf(fp,"Random seed            = %lf\n",randomseed);
	repchar(fp,"-",PAGEWIDTH);
	fprintf(fp,"\n\n");
	fclose(fp);
}

void initreport()
/* makes an initial report */
{
	register int i,j;
	FILE *fp, *fopen();

	fp = fopen(Outputfilename,"a");
	fprintf(fp,"Era                    = %d\n",era);
	fprintf(fp,"Init_select_gen        = %d\n",init_select_gen);
	fprintf(fp,"Cutpop_gen             = %d\n",cutpop_gen);
	fprintf(fp,"Primordial phase       = %d\n",prim_gen);
	fprintf(fp,"Initial popsize        = %d\n",popsize);
	fprintf(fp,"Popsize in juxt. phase = %d\n",juxtpopsize);
	fprintf(fp,"Maximum generation     = %d\n\n",maxgen);
	fprintf(fp,"Template               = ");
	writefullchrom(fp,template);
	fprintf(fp,"\n");

	repchar(fp,"*",31);
	fprintf(fp," era = %d ",era);
	repchar(fp,"*",31);  
	fprintf(fp,"\n");

	fprintf(fp,\
	"Gen  Population  Average   Maximum    Minimum    Average       Fuction\n");
	fprintf(fp,\
	"        size    str. len.  objfunc    objfunc    objfunc     evaluations\n\n");
	fclose(fp);

	if (partitionflag) {
		fp = fopen(Partoutfilename,"a");
		fprintf(fp,"Number of structures = %d\n",numpartition);
		fprintf(fp,"Number   Size   Partitions\n");
		for (i = 0; i < numpartition; i++) {
			fprintf(fp," %3d     %3d  ",i,partition_len[i]);
			for (j = 0; j < partition_len[i]; j++)
				fprintf(fp," %3d",partition_genes[i][j]);
			fprintf(fp,"\n");
		}
		fclose(fp);
	}

	reportpop(oldpop);
}


void writepop(pop)
struct INDIVIDUAL *pop;
/* writes all population members */
{
	long i;
	FILE *fp, *fopen();

	fp = fopen(Poproutfilename,"a");
	fprintf(fp,"Era = %d\n",era);
	fprintf(fp,"Generation = %d\n",gen);
	fprintf(fp,"Population size = %ld\n\n",popsize);
	fprintf(fp,"Number   Obj. Function  string    Chromosome\n");
	fprintf(fp,"             value      length \n");
	for (i = 0; i < popsize; i++) {
		fprintf(fp,"  %-5ld ",i);
		fprintf(fp,"%10.3lf ",pop[i].fitness);
		fprintf(fp,"   %5d   ",pop[i].chromlen);
		fwritechrom(fp,pop[i],32);
	}
	fprintf(fp,"\n\n");
	fclose(fp);
}


void partitionprop(pop)
struct INDIVIDUAL *pop;
/*  calculates proportion of all strings  */
{
	register int i,j,k;
	int powerof2,sum[MAX_PARTITIONS],numgenetable[MAX_PARTITIONS];
	int len, count, limit;
	BOOLEAN gene_flag[MAX_SIZE];
	ALLELES temp_allele, allele_max = 1;
	GENE_ID sub_list[MAX_PARTITIONS][MAX_PARTITIONSIZE], temp_gene;
	struct GENE *temp;
	FILE *fp, *fopen();

	/*  initialize proportion array  */
	for (i = 0; i < numpartition; i++) {
		for (j = 0; j < power(2,partition_len[i]); j++)
			sub_list[i][j] = 0;
	}

	/*  for each population member */
	for (i = 0; i < popsize; i++) {
		/*  for each partition  */
		for (j = 0; j < numpartition; j++) {
			/*  first initialize string counter and number of gene entry  */
			sum[j] = 0;
			numgenetable[j] = 0;
		}
		/*  then, for each gene along the problem length  */
		for (j = 0; j < problem_length; j++) {
			/*  set no gene has been considered yet  */
			gene_flag[j] = 0;
		}
		/*  Starting from the first gene  */
		temp = pop[i].firstgene;
		while (temp != NIL) {      /* till the last gene */
			temp_gene = temp->genenumber;
			temp_allele = temp->allele;

			/*  check whether the gene has been considered already  */
			if (!gene_flag[temp_gene]) {
				gene_flag[temp_gene] = 1;

				/*  for each partition  */
				for (j = 0; j < numpartition; j++) {
					len = partition_len[j];
					powerof2 = power(2,len - 1);
					for (k = 0; k < len; k++) {
					/*  if gene matches with a gene in the partition */ 
						if (temp_gene == \
							partition_genes[j][k]) {
						/* if yes, increment matched gene \
						counter for the partition and \
						set up the array  */
							numgenetable[j]++;
							sum[j] += powerof2 * \
								temp_allele;
						}
						powerof2 /= 2;
					}
				}
			}
			/*  consider the next gene  */
			temp = temp->nextgene;
		}

		/*  for each partition  */
		for (j = 0; j < numpartition; j++) {
			/*  if all members of the partition has been found */
			if (numgenetable[j] == partition_len[j]) {
				/*  increment the right array  */
				sub_list[j][sum[j]]++;
			}
		}
	}

	/*   write proportion of members of each structure  */

	fp = fopen(Partoutfilename,"a");
	fprintf(fp,"\nEra %d, Generation %d, Population size %ld\n",\
	              era, gen, popsize);
	fprintf(fp,"Partition   Number of strings in the population\n");

        limit = PAGEWIDTH - 24;
	/*  for each partition  */
	for (i = 0; i < numpartition; i++) {
		fprintf(fp,"   %3d      ",i);
		/*  print proportion of all members in the partition */
		for (j = 0,count = 0; j < power(allele_max+1, partition_len[i]);\
			j++) 
		{
			if (count > limit) {
				count = 7;
				fprintf(fp,"\n");
				skip_space(fp, 19);
			}
			count += 7;
			fprintf(fp,"%-6d ",sub_list[i][j]);
		}
		fprintf(fp,"\n");
	}
	fclose(fp);
}
