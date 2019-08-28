/*=================================================  
    file    : utility.c
   
    purpose :  utility funations used in other files 
 
  developed : 1991

     author : Kalyanmoy Deb 
===================================================*/

#include "mga.ext"


void gen_file(file, filevar, name)
char *file, *filevar, *name;
/*  name input-output files */
{
	char newname[30];
	int limit = 30;

	sprintf(filevar, "%s", name);
	/* find out if a different file name is asked for */
	printf("\n%s file name (%s) = ",file,filevar);
	get_string(stdin,newname,limit);
	if (strlen(newname) != 0)
		strcpy(filevar,newname);
}


void get_string(txt, s, n)
FILE *txt;
char *s;
int n;
/*  read a string */
{
	register int c;
	while ((c = getc(txt)) != EOF && --n > 0) {
		if (c != '\n')
			*s++ = c;
		else
			break;
	}
	*s = '\0';
}


void check_input_file(filename)
char *filename;
/*  check an input file */
{
	FILE *fp, *fopen();
	char msg[80];

	if ((fp = fopen(filename,"r")) == NIL)
	{
		sprintf(msg, "Input file: can not open %s",filename);
		print_error(msg);
		exit(1);
	}
	fclose(fp);
}


void scratch_file(filename)
char *filename;
/* scratch a file */
{
	FILE *fp, *fopen();

	fp = fopen(filename,"w");
	fclose(fp);
}


void print_error(msg)
char *msg;
/*  send error message and exit  */
{
	fprintf(stderr, "\n***** %s\n", msg);
	exit(1);
}


void sortnum(num,list)
int num;
int *list;
/*  sort an array of integers in ascending order  */
{
	register int i,j;
	int temp;

	for (i = 0; i < num; i++)
	{
		for (j=i+1; j<num; j++)
		{
			if (list[i] > list[j])
			{
				temp = list[i];
				list[i] = list[j];
				list[j] = temp;
			}
		}
	}
}


void freeallmemory()
/*  free all memory stored  */
{
	register int i;

	for (i = 0; i < popsize; i++)
	{ /* delete chromosome, fullchrom, and fullgenes */
		delete_chrom(oldpop[i].firstgene);
		delete_chrom(newpop[i].firstgene);
		free(oldpop[i].fullchrom);
		free(newpop[i].fullchrom);
		free(oldpop[i].fullgene);
		free(newpop[i].fullgene);
	}
	free(newpop);
	free(oldpop);

	free(shuffle);
	delete_chrom(best_indv.firstgene);
	free(best_indv.fullchrom);
	free(best_indv.fullgene);
}


void delete_chrom(chrom)
struct GENE *chrom;
/*  delete a chromosome starting at pointer temp  */
{
	if (chrom != NIL)
	{
		delete_chrom(chrom->nextgene);
		free(chrom);
	}
}


void freenewpop()
{
	register int i;
	struct GENE *p, *p2;

	for (i = 0; i < popsize; i++) {
		p = newpop[i].firstgene;
		while (p != NIL) {
			p2 = p;
			free(p);
			p = p2->nextgene;
		}
		free(newpop[i].fullchrom);
		free(newpop[i].fullgene);
	}
}

void reallocate_memory(size)
/*  reallocates storage for arrays, used only in primordial phase  */
long size;
{
	newpop = (struct INDIVIDUAL *)realloc(newpop,size * \
						sizeof(struct INDIVIDUAL));
	oldpop = (struct INDIVIDUAL *)realloc(oldpop,size * \
						sizeof(struct INDIVIDUAL));
	shuffle = (int *)realloc(shuffle,size*sizeof(int *));
}


int round(num)
double num;
/*  round off the number  */
{
	int a1;
	double a2;

	a1 = num;
	a2 = num - a1;
	return ((a2 > 0.5) ? a1+1 : a1);
}


int maximum(num, list)
int num;
int *list;
/*  calculates the maximum value in an array  */
{
	register int i;
	int max;

	for (i = 1, max = list[0]; i < num; i++) {
		if (list[i] > max)
			max = list[i];
	}
	return(max);
}


void repchar (out, ch, repcount)
FILE *out;
char *ch;
int repcount;
/* Repeatedly write a character to an output device */
{
	register int j;

	for (j = 1; j <= repcount; j++)
		fprintf(out, "%s", ch);
}

void page(out)
FILE *out;
/* Issue form feed to device or file */
{
	repchar(out,"\f",1);
}


void skip(out, skipcount)
FILE *out;
int skipcount;
/* Skip skipcount lines on device out */
{
	repchar(out,"\n",skipcount);
}


void skip_space(out, skipcount)
FILE *out;
int skipcount;
/*  skip skipcount characters  */
{
	repchar(out," ",skipcount);
}


long fact(n)
int n;
/*   calculates factorial of n  */
{
	register int i;
	long prod;

	for (i = 1, prod = 1; i <= n; i++)
		prod *= i;
	return(prod);
}


long choose(n,k)
int n;
int k;
/*   calculates n choose k   */
{
	register int i;
	long prod;

	for (i = n, prod = 1; i > n-k; i--)
		prod *= i;
	return(prod / fact(k));
}

void reset_list(list,nlist)
ALLELES *list;
int nlist;
/*   resets the elements of list to zero  */
{
	register int i;

	for (i = 0; i < nlist; i++)
		list[i] = 0;
}


long power(a,b)
int a;
int b;
/*  calculates a^b  */
{
	register int i;
	long prod;

	for (i = 0, prod = 1; i < b; i++)
		prod *= a;
	return(prod);
}

BOOLEAN parity(num)
int num;
/* whether num is even */
{
	BOOLEAN par;

	if ((num % 2) == 0) par = 1;
	else par = 0;
	return(par);
}

int ones(chr,len)
int chr[];
int len;
/* count the number of 1's */
{
	register int i;
	int num = 0;

	for (i = 0; i < len; i++)
		if (chr[i] == 1)  num++;
	return(num);
}

double decode(chr,len)
int chr[];
int len;
/* calculate the decoded value of a binary string */
{
	register int i;
	int powerof2;
	double val = 0.0;

	powerof2 = power(2,len-1);
	for (i = 0; i < len; i++) {
		if (chr[i] == 1)
			val += powerof2;
		powerof2 /= 2;
	}
	return(val);
}

double map(min, max, x, len)
double min, max, x;
int len;
{
	int dec_max;

	dec_max = power(2, len) - 1;
	return (min + ((max - min) * x) / dec_max);
}
