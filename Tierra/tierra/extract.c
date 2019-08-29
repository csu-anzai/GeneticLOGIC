/* extract.c   9-9-92  genome extraction for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)extract.c	1.5    7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"


#ifdef MEM_CHK
#include <memcheck.h>
#endif

void extract(ce)
    Pcells ce;
{
    I16u i, j;
    I32s size;
    I32s ip;
    Pgl g;
    FILE *fp;
    head_t head;
    indx_t *indx, gindx;

    if (!GeneBnker)
        return;
    isolate = 0;
#ifdef IBM3090
    sprintf(Buff, "%04ld.gen.d", ce->d.gen.size);
#else
    sprintf(Buff, "%s%04ld.gen", GenebankPath, ce->d.gen.size);
#endif
    size = ce->d.gen.size;
    g = sl[size]->g[ce->d.gi];
    sprintf(ExtrG, "%04ld%s @ %ld", g->gen.size, g->gen.label, g->pop);
#if FRONTEND == STDIO
sprintf(mes[0], "extract: %s", ExtrG);
FEMessage(1,mes);
#else /* FRONTEND == STDIO */
    if (Log) fprintf(tfp_log, "ex = %s\n", ExtrG);
#endif /* FRONTEND == STDIO */

/* DAN open an archive, if it does not exist, create it */
    if (!(fp = open_ar(Buff, ce->d.gen.size, GFormat, -1)))
    {   FEError(-200,EXIT,NOWRITE,
            "Tierra extract() Unable to open extract file %s",Buff);
    }
    head = read_head(fp);
#ifdef __TURBOC__
    indx = &gindx;
#else  /* __TURBOC__ */
    indx = read_indx(fp, &head);
#endif /* __TURBOC__ */
    add_gen(fp, &head, &indx, g);
#ifndef __TURBOC__
    if (indx)
    {   thfree(indx);
        indx = NULL;
    }
#endif /* __TURBOC__ */
    fclose(fp);
    NumGenDG++;
}
