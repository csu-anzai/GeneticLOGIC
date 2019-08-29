/* geniob.c   27-3-92 genebank input/output routines for Beagle */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#define INSTD aid

#include "beagle.h"
#include "externb.h"

#define WritEcoS(bits)		WritEcoB(bits, mes[9])

/*
 * read_head - read header from a genebank archive
 */

head_t read_head(fp)
    FILE *fp;
{
    head_t t;

    if ((fp != NULL) && !fseek(fp, 0, 0)) fread(&t, sizeof(head_t), 1, fp);
    else {
	fprintf(stderr, "read_head file access failed");
	exit(errno);
       }
   if (GFormat < 0) 
      GFormat = t.magic[3] -'0';	/* autoselect */
   return t;
}

/*
 * read_indx - read the index from a genebank archive
 */

indx_t *read_indx(fp, head)
    FILE *fp;
    head_t *head;
{
    indx_t *t = 0;

    if (!fseek(fp, sizeof(head_t), 0)) {
	t = (indx_t *) farcalloc(head->n_alloc, sizeof(indx_t));
	fread(t, sizeof(indx_t), head->n, fp);
    }
    else {
	fprintf(stderr, "read_index file access failed");
	exit(errno);
         }
    return t;
}

/*
 * find_gen - find the index of a genome in an archive by its 3 letter name
 *
 * will return n (number of genomes) if not found, otherwise the position
 * (0 - n-1) in archive
 */

I32s find_gen(indx, gen, n)
    indx_t indx[];
    I8s *gen;
    I32s n;
{
    I32s i;

    for (i = 0; i < n; i++) if (!strncmp(indx[i].gen, gen, 3)) break;
    return i;
}

/*
 * get_gen - read a genome from genebank archive and return a pointer
 *     to a struct g_list containing all saved info.
 *
 *     fp - pointer to open archive file
 *     head - archive header
 *     indxn - index entry of desired genome
 *     n - position of desired genome in archive
 *
 * reads the genome and reformats its other args into the form used
 * internally by tierra. the genotype must be in archive. n can be
 * determined by find_gen(). currently no error checking
 */

Pgl get_gen(fp, head, indxn, n)
    FILE *fp;
    indx_t *indxn;
    head_t *head;
    I32s n;
{
    Pgl t = (Pgl) calloc(1, sizeof(GList));

    fseek(fp, head->g_off +
	  (n * head->size * (sizeof(Instruction) + sizeof(GenBits))), 0);
    t->genome = (FpInst) calloc(head->size, sizeof(Instruction));
    t->gbits = (FpGenB) calloc(head->size, sizeof(GenBits));
    fread(t->genome, head->size * sizeof(Instruction), 1, fp);
    fread(t->gbits, head->size * sizeof(GenBits), 1, fp);
    t->gen.size = head->size;
    strncpy(t->gen.label, indxn->gen, 3);
    t->parent.size = indxn->psize;
    strncpy(t->parent.label, indxn->pgen, 3);
    t->bits = indxn->bits;
    t->d1 = indxn->d1;
    t->d2 = indxn->d2;
    t->originI = indxn->originI;
    t->originC = indxn->originC;
    t->MaxPropPop = (float) indxn->mpp / 10000.;
    t->MaxPropInst = (float) indxn->mpi / 10000.;
    t->mpp_time = indxn->mppT;
    t->ploidy = (indxn->pt & 0360) >> 4;
    t->track = indxn->pt & 017;
    return t;
}

void WritEcoB(bits, buf)
    I32u bits;
    I8s *buf;	/* changed by DAN */
{
    I32u i, j;
    
    if(!buf) return;
    sprintf(buf,"EX      TC      TP      MF      MT      MB      ");
    for (i = 0, j = 0; i < 6; i++, j = 0) {
	if (IsBit(bits, (I32u) (5 * i + 2)))
	    buf[2+(i*8)+j++] = 's';
	if (IsBit(bits, (I32u) (5 * i + 3)))
	    buf[2+(i*8)+j++] = 'd';
	if (IsBit(bits, (I32u) (5 * i + 4)))
	    buf[2+(i*8)+j++] = 'o';
	if (IsBit(bits, (I32u) (5 * i + 5)))
	    buf[2+(i*8)+j++] = 'f';
	if (IsBit(bits, (I32u) (5 * i + 6)))
	    buf[2+(i*8)+j++] = 'h';
    }
}

void SetBit(seed, bit, value)
    I32u *seed, bit, value;
{
    if (value)
	(*seed) |= (1 << bit);
    else
	(*seed) &= (~(1 << bit));
}

I16s id_compare(i,j)
ArgInstDef   *i, *j;
{   return(i->op - j->op);
}

void GetAMap(file)
    I8s file[85];
{   FILE  *afp;
    char  data[85];
    I32s  i;

    if((afp = fopen(file,"r")) == NULL)
    {   fprintf(stderr,"unable to open IMapFile  - %s", file);
        exit(-666);
    }
    fgets(data, 84, afp);
    i = 0;
    while (strlen(data) > 3)
    {   if ((( sscanf(data,"%*[^x]x%lx%*[^\"]\"%[^\"]", &aid[i].op,
            aid[i].mn)) >= 2) && ((aid[i].op >= 0) &&
            (aid[i].op < INSTNUM )))
        {   i++;
            if (!strcmp(aid[i].mn, "nop0"))
            {   Nop0 = aid[i].op;
                NopS = Nop0 + Nop1;
            }
            if (!strcmp(aid[i].mn, "nop1"))
            {   Nop1 = aid[i].op;
                NopS = Nop0 + Nop1;
            }
        }
        if (fgets(data, 84, afp) == NULL)
            break ;
    }
    fclose(afp);
    qsort(aid,INSTNUM, sizeof(ArgInstDef), id_compare);
}
