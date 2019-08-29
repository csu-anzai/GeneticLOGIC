/*-------------------------------------------------------------------------*/
/* arg.c: 9-9-92 genebank archive utility, by T Uffner, D Pirone, & T Ray */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */
/*-------------------------------------------------------------------------*/

#ifndef lint
static char sccsid[] = "@(#)arg.c	1.5     7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "declare.h"
#include "arginst.h"
#include <errno.h>

#ifdef __TRS6000__
void *malloc();
#else
#ifdef unix
char *malloc();
#endif
#endif

int  hangup = 0;

int main(argc, argv)
    int   argc;
    char  *argv[];
{
    I8s  com, *mod, *u = "usage:  arg c|r[v12] afile size file [file...]\
        \n\targ x[v12] afile [genotype [genotype...]]\n\targ t[v] afile\n";
    I8s  *file = 0, m[20],data[85];
    I32s i, v = 0, f = GFormat, size = 0;
    FILE *afp = 0;
#ifdef __TURBOC__
    float  z = sin(0);
#endif

    GList *g = 0;
    head_t head;
    indx_t *indx, *tindx, gindx;

    if (argc < 3)
    {   fprintf(stderr, u);
        exit(1);
    }
    GetAMap("opcode.map");
    switch (com = *argv[1])
    {   case 'c':
        case 'r':
        if (argc < 5)
        {   fprintf(stderr, u);
            exit(2);
        }
        if (!sscanf(argv[3], "%ld", &size))
        {   fprintf(stderr, u);
            exit(3);
        }
        case 't':
        case 'x':
            break;
        default:
            fprintf(stderr, u);
        exit(4);
    }
    for (mod = ++argv[1]; *mod; mod++)
        switch (*mod)
        {   case 'v':
                ++v;
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                f = *mod - '0';
                break;
            default:
                fprintf(stderr, u);
            exit(5);
        }
    file = argv[2];

    switch (com)
    {   case 'c':
            fprintf(stdout, "creating archive \"%s\"\n", file);
            g = (GList *) calloc(1, sizeof(GList));
            GetAscGen(g,argv[4]);
            f = GFormat;
        case 'r':
            if (!(afp = open_ar(file, size, f , com == 'c' ? 1 : 0)))
            {   perror(argv[0]);
                exit(6);
            }
            head = read_head(afp);
#ifdef __TURBOC__
            indx = &gindx;
#else  /* __TURBOC__ */
            indx = read_indx(afp, &head);
#endif /* __TURBOC__ */
            if (g)
            {   free(g);
                g = NULL;
            }
            g = (GList *) calloc(1, sizeof(GList));
            for (i=4; i < argc; i++)
            {   int j;
                GetAscGen(g, argv[i]);
                if (head.size != g->gen.size)
                {   fprintf(stderr, "%s is wrong size\n", g->gen.label);
                    continue;
                }
                j = add_gen(afp, &head, &indx, g);
                if (v)
                {   fprintf(stdout, "%c - %04ld%3s %.3f %.3f", j?'r':'a',
                        size, g->gen.label, g->MaxPropPop, g->MaxPropInst);
                    if (IsBit(g->bits,0))
                    {   WritEcoB(g->bits, mes[9]);
                        fprintf(stdout," 1 %s\n",mes[9]);
                    }
                    else fprintf(stdout," 0\n");
                }
                else fprintf(stdout, "%c - %3s\n", j?'r':'a', g->gen.label);
            }
            if (g)
            {   free(g);
                g = NULL;
            }
            fclose(afp);
            break;
        case 't':
            if (!(afp = fopen(file, "rb")))
            {   perror(argv[0]);
                exit(7);
            }
            head = read_head(afp);
            if (strncmp(head.magic, "tie", 3) 
                || (f > -1 && head.magic[3] - '0' != f))
            {   fprintf(stderr, "%s: bad magic number", *argv);
                exit(8);
            }
#ifdef __TURBOC__
            indx = &gindx;
#else  /* __TURBOC__ */
            indx = read_indx(afp, &head);
#endif /* __TURBOC__ */
            sprintf(m, "%4s",head.magic);
            m[4]='\0';  /* insanity for not having null in the struct */
            fprintf(stdout, "Format: %s, Size: %hd, # of entries: %hd\n\n",
                m, head.size, head.n);
            for (i = 0; i < head.n; i++)
            {
#ifdef __TURBOC__
                find_gen(afp, indx, "---", i);
                tindx = indx;
#else  /* __TURBOC__ */
                tindx = &indx[i];
#endif /* __TURBOC__ */
                if (v)
                {   g = get_gen(afp, &head, tindx, i);
                    sprintf(m,"%3s",tindx->gen);
                    fprintf(stdout, "%04hd%.3s %.3f %.3f", head.size,
                        m, tindx->mpp / 10000., tindx->mpi / 10000.);
                    if (IsBit(tindx->bits, 0))
                    {   WritEcoB(tindx->bits, mes[9]);
                        fprintf(stdout," 1 %s\n",mes[9]);
                    }
                    else fprintf(stdout," 0\n");
                }
                else fprintf(stdout, "%.3s\n", tindx->gen);
            }
            break;
        case 'x':
            if (!(afp = fopen(file, "rb")))
            {   perror(argv[0]);
                exit(9);
            }
            head = read_head(afp);
            if (strncmp(head.magic, "tie", 3) 
                || (f > -1 && head.magic[3] - '0' != f))
            {   fprintf(stderr, "%s: bad magic number", *argv);
                exit(10);
            }
#ifdef __TURBOC__
            indx = &gindx;
#else  /* __TURBOC__ */
            indx = read_indx(afp, &head);
#endif /* __TURBOC__ */
            if (argc > 3)
                for (i=3; i<argc; i++)
                {   int j;
                    if ((j = find_gen(afp, indx, argv[i], head.n)) == head.n)
                    {   fprintf(stderr, "%s not in archive\n", argv[i]);
                        continue;
                    }
#ifdef __TURBOC__
                    tindx = indx;
#else  /* __TURBOC__ */
                    tindx = &indx[j];
#endif /* __TURBOC__ */
                    g = get_gen(afp, &head, tindx, j);
                    file = malloc(8);
                    sprintf(file, "%04hd%3s", head.size, g->gen.label);
                    WritAscFile(g, file);
                    if (v)
                    {   fprintf(stdout, "x - %04ld%3s %.3f %.3f", g->gen.size,
                            g->gen.label, g->MaxPropPop, g->MaxPropInst);
                        if (IsBit(g->bits,0))
                        {   WritEcoB(g->bits, mes[9]);
                            fprintf(stdout," 1 %s\n",mes[9]);
                        }
                        else fprintf(stdout," 0\n");
                    }
                    else fprintf(stdout, "x - %3s\n", g->gen.label);
                    if (file)
                    {   free(file);
                        file = NULL;
                    }
                    if (g)
                    {   if (g->genome)
                        {   free(g->genome);
                            g->genome = NULL;
                        }
                        if (g->gbits)
                        {   free(g->gbits);
                            g->gbits = NULL;
                        }
                        free(g);
                        g = NULL;
                    }
                }
            else
                for (i=0; i<head.n; i++)
                {
#ifdef __TURBOC__
                    find_gen(afp, indx, "---", i);
                    tindx = indx;
#else  /* __TURBOC__ */
                    tindx = &indx[i];
#endif /* __TURBOC__ */
                    g = get_gen(afp, &head, tindx, i);
                    file = malloc(12);
                    sprintf(file, "%04hd%3s", head.size, g->gen.label);
                    WritAscFile(g, file);
                    if (v)
                    {   fprintf(stdout, "x - %04ld%3s %.3f %.3f", g->gen.size,
                            g->gen.label, g->MaxPropPop, g->MaxPropInst);
                        if (IsBit(g->bits,0))
                        {   WritEcoB(g->bits, mes[9]);
                            fprintf(stdout," 1 %s\n",mes[9]);
                        }
                        else fprintf(stdout," 0\n");
                    }
                    else fprintf(stdout, "x - %3s\n", g->gen.label);
                    if (file)
                    {   free(file);
                        file = NULL;
                    }
                    if (g)
                    {   if (g->genome)
                        {   free(g->genome);
                            g->genome = NULL;
                        }
                        if (g->gbits)
                        {   free(g->gbits);
                            g->gbits = NULL;
                        }
                        free(g);
                        g = NULL;
                    }
                }
            break;
    }
    exit(0);
}
