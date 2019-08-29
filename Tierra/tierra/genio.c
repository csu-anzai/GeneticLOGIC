/* genio.c   9-9-92 genebank input/output routines */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)genio.c	1.5 7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#ifdef MEM_CHK
#include <memcheck.h>
#endif

#define WritEcoS(bits)                WritEcoB(bits, mes[9])

/*
 * open_ar - open a genebank archive
 *
 *     file - the filename;
 *     size - the creature size
 *     format - a byte, usually the instruction set number
 *     mode - 0 if the file exists its contents should be preserved,
 *            < 0, if doesn't exit create it
 *              else if the file should be created (or truncated). mode > 1
 *              is taken as the number of index entries to allocate
 *            (will be rounded to the next highest # such that
 *            index + header is a multiple of 1K)
 *
 * returns a pointer to a file opened for update, or NULL if unsuccessful.
 * open_ar fails if size or format are incompatible with the archive.
 */

FILE *open_ar(file, size, format, mode)
    I8s *file;
    I32s size, mode, format;
{
    FILE *fp = NULL;
    head_t head;
    struct stat buf; 

    if (( mode > 0) || (mode < 0 && (stat(file, &buf) == -1)))
    {   if (mode < 0) /* if file doesn't exist, no entries */
            mode = 0;
        if (fp = fopen(file, "w+b"))
        {   strcpy(head.magic, "tie");
            head.magic[3] = '0' + format;
            head.size = size;
            head.n = 0;
            head.n_alloc = (((int) ((sizeof(head_t) + mode * sizeof(indx_t)) /
                      1024.0) + 1) * 1024 - sizeof head) / sizeof(indx_t);
            head.g_off = sizeof(head_t) + head.n_alloc * sizeof(indx_t);
            write_head(fp, &head);
        }
    }
    else if (mode < 1 && (fp = fopen(file, "r+b")))
    {   head = read_head(fp);
        if (head.size != size || (format > -1) && head.magic[3] !=
            format + '0' || strncmp(head.magic, "tie", 3))
        {   fclose(fp);
            fp = NULL;
            errno = EINVAL;
        }
    }
    return fp;
}

/*
 * read_head - read header from a genebank archive
 */

head_t read_head(fp)
    FILE *fp;
{
    head_t t;

    if ((fp != NULL) && !fseek(fp, 0, 0))
        fread(&t, sizeof(head_t), 1, fp);
    else
    {
#ifdef ARG
        fprintf(stderr, "Tierra read_head() file access failed");
        exit(errno);
#else
        FEError(-400,EXIT,NOWRITE, "Tierra read_head() file access failed");
#endif
    }
    if (GFormat < 0) 
        GFormat = t.magic[3] -'0';        /* autoselect */
    return t;
}

/*
 * write_head - write header to a genebank archive
 */

void write_head(fp, head)
    FILE *fp;
    head_t *head;
{
    if (!fseek(fp, 0, 0))
        fwrite(head, sizeof(head_t), 1, fp);
    else
    {
#ifdef ARG
        fprintf(stderr, "Tierra write_head() file access failed");
        exit(errno);
#else
        FEError(-401,EXIT,NOWRITE, "Tierra write_head() file access failed");
#endif
    }
}

/*
 * read_indx - read the index from a genebank archive
 */

indx_t *read_indx(fp, head)
    FILE *fp;
    head_t *head;
{
#ifdef ERROR
    I32s  i;
#endif /* ERROR */
    indx_t *t = 0;

#ifdef __TURBOC__ 
    t = &GIndx;
#else /* __TURBOC__ */
    if (!fseek(fp, sizeof(head_t), 0))
    {   t = (indx_t *) thcalloc(head->n_alloc, sizeof(indx_t));
        if (t != NULL)
            fread(t, sizeof(indx_t), head->n, fp);
    }
    else
    {
#ifdef ARG
        fprintf(stderr, "Tierra read_index() file access failed");
        exit(errno);
#else /* ARG */
        FEError(-402,EXIT,NOWRITE, "Tierra read_index() file access failed");
#endif /* ARG */
    }
#ifdef ERROR
    if (t != NULL)
        for (i = 0; i < head->n; i++)
            if (!(t + i)->psize)
#ifdef ARG
            {   fprintf(stderr, "Tierra read_index() indx array corrupted");
                exit(errno);
            }
#else  /* ARG */
                FEError(-403,EXIT,NOWRITE,
                    "Tierra read_index() indx array corrupted");
#endif /* ARG */
#endif /* ERROR */
#endif /* __TURBOC__ */
    return t;
}

/*
 * write_indx - write the index to a genebank archive
 */

void write_indx(fp, head, indx)
    FILE *fp;
    head_t *head;
    indx_t *indx;
{
#ifdef ERROR
    I32s  i;
#endif /* ERROR */
#ifdef __TURBOC__ 
    return ;
#endif /* __TURBOC__ */

#ifdef ERROR
    for (i = 0; i < head->n; i++)
        if (!(indx + i)->psize)
#ifdef ARG
        {   fprintf(stderr, "Tierra write_index() indx array corrupted");
            exit(errno);
        }
#else  /* ARG */
            FEError(-404,EXIT,NOWRITE,
                "Tierra write_index() indx array corrupted");
#endif /* ARG */
#endif /* ERROR */
    if (!fseek(fp, sizeof(head_t), 0))
        fwrite(indx, sizeof(indx_t), head->n_alloc, fp);
    else
    {
#ifdef ARG
        fprintf(stderr, "Tierra write_index() file access failed");
        exit(errno);
#else
        FEError(-405,EXIT,NOWRITE, "Tierra write_index() file access failed");
#endif
    }
}

/*
 * find_gen - find the index of a genome in an archive by its 3 letter name
 *
 * will return n (number of genomes) if not found, otherwise the position
 * (0 - n-1) in archive
 */

I32s find_gen(fp, indx, gen, n)
    FILE    *fp;
    indx_t  *indx;
    I8s     *gen;
    I32s    n;
{
    I32s i;

#ifdef __TURBOC__
    if (!strncmp("---", gen, 3))
    {   fseek(fp, sizeof(head_t) + (n * sizeof(indx_t)), 0);
        fread(indx, sizeof(indx_t), 1, fp);
        i = n;
    }
    else
    {   fseek(fp, sizeof(head_t), 0);      /* seek past head */
        for(i = 0; i < n; i++)       /* scan index for gen */
        {   fread(indx, sizeof(indx_t), 1, fp);
            if (!strncmp(indx->gen, gen, 3))
                break;
        }
    }
#else /* __TURBOC__ */
    for (i = 0; i < n; i++)
        if (!strncmp((indx + i)->gen, gen, 3))
            break;
#endif /* __TURBOC__ */
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
    Pgl t = (Pgl) tcalloc(1, sizeof(GList));
    fseek(fp, head->g_off +
          (n * head->size * (sizeof(Instruction) + sizeof(GenBits))), 0);
    t->genome = (FpInst) tcalloc(head->size, sizeof(Instruction));
    t->gbits = (FpGenB) tcalloc(head->size, sizeof(GenBits));
    fread(t->genome, head->size * sizeof(Instruction), 1, fp);
    fread(t->gbits, head->size * sizeof(GenBits), 1, fp);
    t->gen.size = head->size;
    strncpy(t->gen.label, indxn->gen, 3);
    t->parent.size = indxn->psize;
    strncpy(t->parent.label, indxn->pgen, 3);
    t->bits = indxn->bits;
    t->hash = indxn->hash;
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

/*
 * add_gen - replace or add a genotype to end of genebank archive
 *
 *     fp - pointer to open archive file
 *     head - header of archive
 *     indx - index of archive
 *     gen - genotype to be added
 *
 * reformats the genotype and replaces it in the archive, or adds it to
 * the end if not found. args head & indx are modified by this fn.
 * returns 0 on add, and 1 on replace.
 *
 * Scheme of add_gen():
 *
 * 1) find the gen in the file, or find out if it is not in the file.
 *    This may be done with find_gen() if we have indx in RAM, or by
 *    by repeated freads if we don't have indx.
 * 2) If gen is not in file and file is full, we must enlarge the file by
 *    moving the genotypes out to make room for more indexes.
 *    Three strategies are used here.  In Unix, we read the entire index
 *    and bank of genomes into RAM, then write them back out into their
 *    new locations.  In DOS, when memory is available, we will copy
 *    the genomes one by one from their old to their new locations on disk,
 *    then we write the free index structures over with zeros.  In DOS
 *    when memory is not available, we copy the genomes byte by byte.
 * 3) We write the new genome and genbits to the file.
 * 4) We determine if this was an add or a replace.
 * 5) We write the new updated head.
 * 6) We update the index of the new gen in RAM.
 * 7) We write the new index to the file.
 * 8) We return add or replace.
 *
 */

I32s add_gen(fp, head, indx, gen)
    FILE *fp;
    head_t *head;
    indx_t **indx;
    Pgl gen;
{
    I8s     t_buf = '\0';
    I32s    n, s, ret, oldoff, gensiz, old_n_alloc, i;
    I32s    wsiz = 0, rsiz = 0, segs, rem;
    indx_t  t_indx, *tp_indx, *tindx;
#ifdef __TURBOC__
    I8s     buf[512];
#else  /* __TURBOC__ */
    I8s     *buf;
#endif /* __TURBOC__ */

    Swap = 0; /* to avoid recursion */
    gensiz = (head->size * (sizeof(Instruction) + sizeof(GenBits)));

/* 1) find the gen in the file, or find out if it is not in the file.
 *    This may be done with find_gen() if we have indx in RAM, or by
 *    by repeated freads if we don't have indx.  */

    n = find_gen(fp, *indx, gen->gen.label, head->n); /* does not alloc */

/* 2) If gen is not in file and file is full, we must enlarge the file by
 *    moving the genotypes out to make room for more indexes.
 *    Three strategies are used here.  In Unix, we read the entire index
 *    and bank of genomes into RAM, then write them back out into their
 *    new locations.  In DOS, when memory is available, we will copy the
 *    genomes one by one from their old to their new locations on disk,
 *    then we write over with zeros, the free index structures.  In DOS
 *    when memory is not available, we copy the genomes byte by byte. */

    /* n == head->n means not in index */
    /* head->n == head->n_alloc means file is full */
    /* if not in file and file is full */
    if (n == head->n && head->n == head->n_alloc)
    {   old_n_alloc = head->n_alloc;
        head->n_alloc += 1024 / sizeof(indx_t);
        oldoff = head->g_off;
        head->g_off = sizeof(head_t) + (head->n_alloc * sizeof(indx_t));
#ifdef __TURBOC__
        fseek(fp, 0, SEEK_END); /* make room at end for slide */
        fwrite(&t_buf, sizeof(I8s), head->g_off - oldoff, fp);/* write zeros*/
        segs = (gensiz * head->n) / 512;
        rem  = (gensiz * head->n) % 512;
        if (rem)
        {   fseek(fp, (segs * 512) + oldoff, 0);
            rsiz += fread(buf, sizeof(I8s), rem, fp);
            fseek(fp, (segs * 512) + head->g_off, 0);
            wsiz += fwrite(buf, sizeof(I8s), rem, fp);
        }
        if (segs) for(i = segs - 1; i >= 0; i--)
        {   fseek(fp, (i * 512) + oldoff, 0);
            rsiz += fread(buf, sizeof(I8s), 512, fp);
            fseek(fp, (i * 512) + head->g_off, 0);
            wsiz += fwrite(buf, sizeof(I8s), 512, fp);
        }
#ifdef ERROR
        if ((rsiz != gensiz * head->n) || (wsiz != gensiz * head->n))
#ifdef ARG
        {   fprintf(stderr, "Tierra add_gen() rsiz or wsiz bad");
            exit(errno);
        }
#else  /* ARG */
            FEError(-406,EXIT,NOWRITE, "Tierra add_gen() rsiz or wsiz bad");
#endif /* ARG */
#endif /* ERROR */
#else  /* __TURBOC__ */
        fseek(fp, oldoff, 0);
        buf = (I8s *) thcalloc(s = gensiz * head->n , 1);
        fread(buf, s, 1, fp);
        fseek(fp, head->g_off, 0);
        fwrite(buf, s, 1, fp);
        if (buf)
        {   thfree(buf);
            buf = NULL;
        }
        tindx = (indx_t *) threcalloc(*indx,
            sizeof(indx_t) * head->n_alloc,
            sizeof(indx_t) * old_n_alloc);
        if (tindx)
        {   *indx = tindx;
            for (i = old_n_alloc * sizeof(indx_t);
                i < head->n_alloc * sizeof(indx_t); i++)
                ((I8s  *) *indx)[i] = (I8s) 0;
        }
        else if (*indx)
        {   thfree(*indx);
            *indx = NULL;
        }
#endif /* __TURBOC__ */
    }

/* 3) We write the new genome and genbits to the file. */

    fseek(fp, head->g_off + (n * gensiz), 0);
    fwrite(gen->genome, head->size * sizeof(Instruction), 1, fp);
    fwrite(gen->gbits, head->size * sizeof(GenBits), 1, fp);

/* 4) We determine if this was an add or a replace. */

    head->n += ret = (n == head->n);

/* 5) We write the new updated head. */

    write_head(fp, head); /* no alloc */

/* 6) We update the index of the new gen in RAM. */

#ifdef __TURBOC__
    tp_indx = *indx; /* fake pointers for meat of function */
#else  /* __TURBOC__ */
    tp_indx = &((*indx)[n]);
#endif /* __TURBOC__ */
    strncpy((tp_indx)->gen, gen->gen.label, 3);
    (tp_indx)->psize = gen->parent.size;
    strncpy((tp_indx)->pgen, gen->parent.label, 3);
    (tp_indx)->bits = gen->bits;
    (tp_indx)->hash = gen->hash;
    (tp_indx)->d1 = gen->d1;
    (tp_indx)->d2 = gen->d2;
    (tp_indx)->originI = gen->originI;
    (tp_indx)->originC = gen->originC;
    (tp_indx)->mpp = (short) (gen->MaxPropPop * 10000);
    (tp_indx)->mpi = (short) (gen->MaxPropInst * 10000);
    (tp_indx)->mppT =  (gen->mpp_time);
    (tp_indx)->pt = (gen->ploidy << 4) + gen->track;

/* 7) We write the new index to the file. */

#ifdef ERROR
#ifdef __TURBOC__
    if (!tp_indx->psize)
#ifdef ARG
    {   fprintf(stderr, "Tierra add_gen() tp_indx corrupted");
        exit(errno);
    }
#else  /* ARG */
        FEError(-407,EXIT,NOWRITE, "Tierra add_gen() tp_indx corrupted");
#endif /* ARG */
#else /* __TURBOC__ */
    for (i = 0; i < head->n; i++)
        if (!(*indx)[i].psize)
#ifdef ARG
        {   fprintf(stderr, "Tierra add_gen() indx array corrupted");
            exit(errno);
        }
#else  /* ARG */
            FEError(-408,EXIT,NOWRITE,
                "Tierra add_gen() indx array corrupted");
#endif /* ARG */
#endif /* __TURBOC__ */
#endif /* ERROR */
#ifdef __TURBOC__
    fseek(fp,((n * sizeof(indx_t)) + sizeof(head_t)), 0);
    fwrite(tp_indx, sizeof(indx_t), 1, fp);
#else /* __TURBOC__ */
    write_indx(fp, head, *indx);
#endif /* __TURBOC__ */

/* 8) We return add or replace. */

    Swap = 1;
    return !ret;
}

I16s GetAscGen(g, ifile)
    Pgl g;
    I8s ifile[];
{
    I8s bit[4], chm[4], buf[81], tbuf[81], *gdat, inst[9];
    I16s t1, format;
    I32s j, k, p, stl;
    I8u ti, dontfgets = 1;
    FILE *inf;

    inf = fopen(ifile, "r");
    if (inf == NULL)
    {
#ifdef ARG
        fprintf(stderr,"Tierra GetAscGen() file %s not opened, exiting", ifile);
        exit(errno);
#else
        FEError(-409,EXIT,WRITE,
            "Tierra GetAscGen() file %s not opened, exiting", ifile);
#endif
    }
    gdat = (I8s *) tcalloc(85, sizeof(I8s));
    g->ploidy = (I8s) 1;   /* default ploidy */
    fgets(gdat, 84, inf);
    while (strlen(gdat) < 3)  /* eat blank lines */
        fgets(gdat, 84, inf);
    while (1)
    {   if (dontfgets)
            dontfgets = 0;
        else
            fgets(gdat, 84, inf);
        if (strlen(gdat) < 3) break;     /* get a blank line and break */
        sscanf(gdat, "%s", buf);
        if (!strcmp(buf, "format:"))
        {   sscanf(gdat, "%*s%hd%*s%lu", &format, &g->bits);
            if (GFormat < 0 ) GFormat = format;
            continue;
        }
        if (!strcmp(buf, "genotype:"))
        {   sscanf(gdat, "%*s%ld%s%*s%*s%ld%s", &g->gen.size,
                g->gen.label, &g->parent.size, g->parent.label);
            continue;
        }
        if (!strcmp(buf, "1st_daughter:"))
        {   sscanf(gdat, "%*s%*s%ld%*s%ld%*s%ld%*s%hd",
                &g->d1.flags, &g->d1.inst, &g->d1.mov_daught, &t1);
            g->d1.BreedTrue = t1;
            continue;
        }
        if (!strcmp(buf, "2nd_daughter:"))
        {   sscanf(gdat, "%*s%*s%ld%*s%ld%*s%ld%*s%hd",
                &g->d2.flags, &g->d2.inst, &g->d2.mov_daught, &t1);
            g->d2.BreedTrue = t1;
            continue;
        }
        if (!strcmp(buf, "InstExe.m:"))
        {   sscanf(gdat, "%*s%ld%%*s%ld%*s%ld",
                &g->originI.m, &g->originI.i, &g->originC);
            continue;
        }
        if (!strcmp(buf, "MaxPropPop:"))
        {   if(format < 2)
                sscanf(gdat, "%*s%f%*s%f", &g->MaxPropPop, &g->MaxPropInst);
            else
               sscanf(gdat, "%*s%f%*s%f%*s%ld,%ld", 
                   &g->MaxPropPop, &g->MaxPropInst,
                   &g->mpp_time.m, &g->mpp_time.i);
            continue;
        }
        if (!strcmp(buf, "Origin:"))
        {   sscanf(gdat, "%*s%ld,%ld%*s%ld",
                &g->originI.m, &g->originI.i, &g->originC);
            continue;
        }
        if (!strcmp(buf, "ploidy:"))
        {   sscanf(gdat, "%*s%ld%*s%ld", &j, &k);
            g->ploidy = (I8s) j;
            g->track = (I8s) k;
            continue;
        }
        if (!strcmp(buf, "comments:"))
        {   do
            {   fgets(gdat, 84, inf);
                sscanf(gdat ,"%s", tbuf);
            }   while (strlen(gdat) > 2 && tbuf[strlen(tbuf) - 1] != ':');
            dontfgets = 1;
        }
    }
    g->genome = (FpInst) tcalloc(g->gen.size, sizeof(Instruction));
    g->gbits = (FpGenB) tcalloc(g->gen.size, sizeof(GenBits));
#if PLOIDY == 1
    fgets(gdat, 84, inf);
    fgets(gdat, 84, inf);
    for (j = 0; j < g->gen.size; j++)
    {   fgets(gdat, 84, inf);
        stl = sscanf(gdat, "%s%*s%s%s", inst, chm, bit);
        if (stl > 1 && strlen(chm) == 3)
        {   g->genome[j].read = chm[2] - '0';
            g->genome[j].write = chm[1] - '0';
            g->genome[j].exec = chm[0] - '0';
        }
        if (stl > 2 && strlen(bit) == 3)
        {   if (bit[0] - '0') g->gbits[j] |= (I8s) 1;
            if (bit[1] - '0') g->gbits[j] |= (I8s) (1 << 1);
            if (bit[2] - '0') g->gbits[j] |= (I8s) (1 << 2);
        }
        for (k = 0; k < INSTNUM; k++)
        {   if (!strcmp(inst, id[k].mn))
            {   ti = id[k].op;
                break;
            }
        }
        if (k == INSTNUM)
        {
#ifdef ARG
           fprintf(stderr,
               "Tierra GetAscGen() mnemonic %s not recognized\n", inst);
#else
            FEError(-410,NOEXIT,WRITE,
               "Tierra GetAscGen() mnemonic %s not recognized\n", inst);
#endif
            ti = 0;
        }
        g->genome[j].inst = ti;
    }
#else /* PLOIDY > 1 */
    for (p = 0; p < PLOIDY; p++)
    {   if (p) fgets(gdat, 84, inf);
        fgets(gdat, 84, inf);
        fgets(gdat, 84, inf);
        for (j = 0; j < g->gen.size; j++)
        {   fgets(gdat, 84, inf);
            stl = sscanf(gdat, "%s%*s%s%s", inst, chm, bit);
            if (stl > 1 && strlen(chm) == 3)
            {   g->genome[j][p].read = chm[2] - '0';
                g->genome[j][p].write = chm[1] - '0';
                g->genome[j][p].exec = chm[0] - '0';
            }
            if (stl > 2 && strlen(bit) == 3)
            {   if (bit[0] - '0') g->gbits[j][p] |= (I8s) 1;
                if (bit[1] - '0') g->gbits[j][p] |= (I8s) (1 << 1);
                if (bit[2] - '0') g->gbits[j][p] |= (I8s) (1 << 2);
            }
            for (k = 0; k < INSTNUM; k++)
            {   if (!strcmp(inst, id[k].mn))
                {   ti = id[k].op;
                    break;
                }
            }
            if (k == INSTNUM)
            {
#ifdef ARG
                fprintf(stderr,
                    "Tierra GetAscGen() mnemonic %s not recognized\n", inst);
#else
                FEError(-411,NOEXIT,WRITE,
                    "Tierra GetAscGen() mnemonic %s not recognized\n", inst);
#endif
                ti = 0;
            }
            g->genome[j][p].inst = ti;
        }
    }
#endif /* PLOIDY >  1 */
    fclose(inf);
    g->hash = Hash(g->gen.size, g->genome);
#ifdef IBM3090
    Ebcdic2Ascii(g->gen.label);
    Ebcdic2Ascii(g->parent.label);
#endif
    if (gdat)
    {   tfree(gdat);
        gdat = NULL;
    }
    return 1;
}

I8s WritAscFile(g, file)
    Pgl g;
    I8s *file;
{
    I8s bit[4], chm[4];
    I16s t1;
    I16u di, t, j;
    I8s format = GFormat;
    long int tp;
    FILE *fp;

#ifdef IBM3090
    I8s lbl[4], plbl[4], *comnts;

#endif
    if (format < 0) format = INST;
    if (!strcmp(file, "-"))
        fp = stdout;
    else if (!(fp = fopen(file, "w"))) {
#ifdef ARG
        fprintf(stderr,
            "Tierra WritAscFile() unable to open WritAscFile file %s",file);
        exit(errno);
#else
        FEError(-412,EXIT,NOWRITE,
            "Tierra WritAscFile() unable to open WritAscFile file %s",file);
#endif
    }
    WritEcoB(g->bits, mes[9]);
    fprintf(fp, "\nformat: %hd  bits: %lu  %s\n", format, g->bits,mes[9]);
#ifdef IBM3090
    strcpy(lbl, g->gen.label);
    strcpy(plbl, g->parent.label);
    Ascii2Ebcdic(lbl);
    Ascii2Ebcdic(plbl);
    fprintf(fp, "genotype: %04ld%s  parent genotype: %04ld%s\n",
            g->gen.size, lbl, g->parent.size, plbl);
#else
    fprintf(fp, "genotype: %04ld%s  parent genotype: %04ld%s\n",
            g->gen.size, g->gen.label, g->parent.size, g->parent.label);
#endif
    t1 = g->d1.BreedTrue;
    fprintf(fp, "1st_daughter:  flags: %ld  inst: %ld  mov_daught: %ld  \
        breed_true: %hd\n", g->d1.flags, g->d1.inst, g->d1.mov_daught, t1);
    t1 = g->d2.BreedTrue;
    fprintf(fp, "2nd_daughter:  flags: %ld  inst: %ld  mov_daught: %ld  \
        breed_true: %hd\n", g->d2.flags, g->d2.inst, g->d2.mov_daught, t1);
    tp = g->originC;
    if(format < 2)
       {
       fprintf(fp, "InstExe.m: %ld InstExe.i: %ld  origin: %ld  %s",
            g->originI.m, g->originI.i, g->originC, ctime(&tp));
       fprintf(fp, 
       "MaxPropPop: %g  MaxPropInst: %g \n", g->MaxPropPop, g->MaxPropInst);
       }
    else
       {
       fprintf(fp, "Origin: InstExe: %ld,%ld  clock: %ld  %s",
            g->originI.m, g->originI.i, g->originC, ctime(&tp));
       fprintf(fp, 
       "MaxPropPop: %g  MaxPropInst: %g mpp_time: %ld,%ld \n", 
            g->MaxPropPop, g->MaxPropInst,g->mpp_time.m,g->mpp_time.i);
        }
    fprintf(fp, "ploidy: %ld  track: %ld\n", (I32s) g->ploidy,
            (I32s) g->track);
    fprintf(fp, "\n");
    chm[3] = bit[3] = 0;

#if PLOIDY == 1
        fprintf(fp, "track 0: prot\n          xwr\n" );
        for (t = 0; t < g->gen.size; t++) {
            di = g->genome[t].inst;
            bit[0] = IsBit(g->gbits[t], 0) ? '1' : '0';
            bit[1] = IsBit(g->gbits[t], 1) ? '1' : '0';
            bit[2] = IsBit(g->gbits[t], 2) ? '1' : '0';
            chm[0] = '0' + g->genome[t].exec;
            chm[1] = '0' + g->genome[t].write;
            chm[2] = '0' + g->genome[t].read;
            fprintf(fp, "%-8s; %s %s %02x %3u\n", id[di].mn, chm, 
                    bit, di, t);
        }
#else /* PLOIDY > 1 */
    for (j = 0; j < PLOIDY; j++) {
        if (j)
            fprintf(fp, "\n");
        fprintf(fp, "track %ld: prot\n          xwr\n", j);
        for (t = 0; t < g->gen.size; t++) {
            di = g->genome[t][j].inst;
            bit[0] = IsBit(g->gbits[t][j], 0) ? '1' : '0';
            bit[1] = IsBit(g->gbits[t][j], 1) ? '1' : '0';
            bit[2] = IsBit(g->gbits[t][j], 2) ? '1' : '0';
            chm[0] = '0' + g->genome[t][j].exec;
            chm[1] = '0' + g->genome[t][j].write;
            chm[2] = '0' + g->genome[t][j].read;
            fprintf(fp, "%-8s; %s %s %02x %3u\n", id[di].mn, chm, 
                    bit, di, t);
        }
    }
#endif /* PLOIDY > 1 */
if (fp!= stdout) {fflush(fp);fclose(fp);}
}

#ifdef IBM3090
static unsigned char a2e[] = {
    0000, 0001, 0002, 0003, 0067, 0055, 0056, 0057, 0026, 0005, 0045, 0013, 0014, 0015, 0016,
    0017, 0020, 0021, 0022, 0023, 0074, 0075, 0062, 0046, 0030, 0031, 0077, 0047, 0034, 0035,
    0036, 0037, 0100, 0117, 0177, 0173, 0133, 0154, 0120, 0175, 0115, 0135, 0134, 0116, 0153,
    0140, 0113, 0141, 0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367, 0370, 0371, 0172, 0136,
    0114, 0176, 0156, 0157, 0174, 0301, 0302, 0303, 0304, 0305, 0306, 0307, 0310, 0311, 0321,
    0322, 0323, 0324, 0325, 0326, 0327, 0330, 0331, 0342, 0343, 0344, 0345, 0346, 0347, 0350,
    0351, 0112, 0340, 0132, 0137, 0155, 0171, 0201, 0202, 0203, 0204, 0205, 0206, 0207, 0210,
    0211, 0221, 0222, 0223, 0224, 0225, 0226, 0227, 0230, 0231, 0242, 0243, 0244, 0245, 0246,
    0247, 0250, 0251, 0300, 0152, 0320, 0241, 0007, 0040, 0041, 0042, 0043, 0044, 0025, 0006,
    0027, 0050, 0051, 0052, 0053, 0054, 0011, 0012, 0033, 0060, 0061, 0032, 0063, 0064, 0065,
    0066, 0010, 0070, 0071, 0072, 0073, 0004, 0024, 0076, 0341, 0101, 0102, 0103, 0104, 0105,
    0106, 0107, 0110, 0111, 0121, 0122, 0123, 0124, 0125, 0126, 0127, 0130, 0131, 0142, 0143,
    0144, 0145, 0146, 0147, 0150, 0151, 0160, 0161, 0162, 0163, 0164, 0165, 0166, 0167, 0170,
    0200, 0212, 0213, 0214, 0215, 0216, 0217, 0220, 0232, 0233, 0234, 0235, 0236, 0237, 0240,
    0252, 0253, 0254, 0255, 0256, 0257, 0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267, 0270,
    0271, 0272, 0273, 0274, 0275, 0276, 0277, 0312, 0313, 0314, 0315, 0316, 0317, 0332, 0333,
    0334, 0335, 0336, 0337, 0352, 0353, 0354, 0355, 0356, 0357, 0372, 0373, 0374, 0375, 0376,
0377};

static unsigned char e2a[] = {
    0000, 0001, 0002, 0003, 0234, 0011, 0206, 0177, 0227, 0215, 0216, 0013, 0014, 0015, 0016,
    0017, 0020, 0021, 0022, 0023, 0235, 0205, 0010, 0207, 0030, 0031, 0222, 0217, 0034, 0035,
    0036, 0037, 0200, 0201, 0202, 0203, 0204, 0012, 0027, 0033, 0210, 0211, 0212, 0213, 0214,
    0005, 0006, 0007, 0220, 0221, 0026, 0223, 0224, 0225, 0226, 0004, 0230, 0231, 0232, 0233,
    0024, 0025, 0236, 0032, 0040, 0240, 0241, 0242, 0243, 0244, 0245, 0246, 0247, 0250, 0133,
    0056, 0074, 0050, 0053, 0041, 0046, 0251, 0252, 0253, 0254, 0255, 0256, 0257, 0260, 0261,
    0135, 0044, 0052, 0051, 0073, 0136, 0055, 0057, 0262, 0263, 0264, 0265, 0266, 0267, 0270,
    0271, 0174, 0054, 0045, 0137, 0076, 0077, 0272, 0273, 0274, 0275, 0276, 0277, 0300, 0301,
    0302, 0140, 0072, 0043, 0100, 0047, 0075, 0042, 0303, 0141, 0142, 0143, 0144, 0145, 0146,
    0147, 0150, 0151, 0304, 0305, 0306, 0307, 0310, 0311, 0312, 0152, 0153, 0154, 0155, 0156,
    0157, 0160, 0161, 0162, 0313, 0314, 0315, 0316, 0317, 0320, 0321, 0176, 0163, 0164, 0165,
    0166, 0167, 0170, 0171, 0172, 0322, 0323, 0324, 0325, 0326, 0327, 0330, 0331, 0332, 0333,
    0334, 0335, 0336, 0337, 0340, 0341, 0342, 0343, 0344, 0345, 0346, 0347, 0173, 0101, 0102,
    0103, 0104, 0105, 0106, 0107, 0110, 0111, 0350, 0351, 0352, 0353, 0354, 0355, 0175, 0112,
    0113, 0114, 0115, 0116, 0117, 0120, 0121, 0122, 0356, 0357, 0360, 0361, 0362, 0363, 0134,
    0237, 0123, 0124, 0125, 0126, 0127, 0130, 0131, 0132, 0364, 0365, 0366, 0367, 0370, 0371,
    0060, 0061, 0062, 0063, 0064, 0065, 0066, 0067, 0070, 0071, 0372, 0373, 0374, 0375, 0376,
0377};

Ascii2Ebcdic(s)
    char *s;
{
    while (*s = a2e[*s])
        s++;
}

Ebcdic2Ascii(s)
    char *s;
{
    while (*s = e2a[*s])
        s++;
}

#endif

void WritEcoB(bits, buf)
    I32u bits;
    I8s *buf;        /* changed by DAN */
{
    int i, j;
    
    if(!buf) return;
    sprintf(buf,"EX      TC      TP      MF      MT      MB      ");
    for (i = 0, j = 0; i < 6; i++, j = 0) {
        if (IsBit(bits, 5 * i + 2))
            buf[2+(i*8)+j++] = 's';
        if (IsBit(bits, 5 * i + 3))
            buf[2+(i*8)+j++] = 'd';
        if (IsBit(bits, 5 * i + 4))
            buf[2+(i*8)+j++] = 'o';
        if (IsBit(bits, 5 * i + 5))
            buf[2+(i*8)+j++] = 'f';
        if (IsBit(bits, 5 * i + 6))
            buf[2+(i*8)+j++] = 'h';
    }
}

void SetBit(seed, bit, value)
    I32u *seed, bit, value;
{
    if (value)
        (*seed) |= (ONE << bit);
    else
        (*seed) &= (~(ONE << bit));
}

I16s id_compare(i,j)
InstDef   *i,*j;
{   return(i->op - j->op);
}

void GetAMap(file)
    I8s file[85];
{   FILE  *afp;
    char  data[85], mn[9];
    I32s  i, opc;

    if((afp = fopen(file,"r")) == NULL)
    {   fprintf(stderr,"unable to open IMapFile  - %s", file);
        exit(-666);
    }
    fgets(data, 84, afp);
    i = opc = 0;
    while (strlen(data) > 3)
    {   if (((sscanf(data,"%*[^x]x%lx%*[^\"]\"%[^\"]", &opc, mn))
            >= 2) && ((opc >= 0) && (opc < INSTNUM )))
        {
#ifdef ARG
            i = opc;
#else  /* ARG */
            for (i = 0; strcmp(mn,id[i].mn); i++)
                if (i >= INSTNUM)
                    FEError(-400,EXIT,NOWRITE,
                        "Tierra GetAMap() opcode \"%s\" not recognized", mn);
#endif /* ARG */
            id[i].op = opc;
            strcpy(id[i].mn,mn);
            if (!strcmp(id[i].mn, "nop0"))
            {   Nop0 = id[i].op;
                NopS = Nop0 + Nop1;
            }
            if (!strcmp(id[i].mn, "nop1"))
            {   Nop1 = id[i].op;
                NopS = Nop0 + Nop1;
            }
        }
        if (fgets(data, 84, afp) == NULL)
            break ;
    }
    fclose(afp);
    qsort(id,INSTNUM, sizeof(InstDef), id_compare);
}

I32s Hash(size, v)
    I32s    size;
    FpInst  v;
{   I32s  h = 0;
    I32s  i, j;

    for (i = 0; i < size; i++) 
#if PLOIDY == 1
        h = (3 * h + (v + i)->inst) % 277218551L; /* 277218551 is prime */
#else /* PLOIDY > 1 */
        for (j = 0; j < PLOIDY; j++)
            h = (3 * h + (v + i)[j]->inst) % 277218551L;
#endif /* PLOIDY > 1 */
    return h;
}
