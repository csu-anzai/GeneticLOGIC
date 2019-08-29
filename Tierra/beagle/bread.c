/* bread.c  27-3-92  reads tierra.run data */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "stdio.h"
#include "string.h"
#include "ctype.h"

typedef unsigned int   Uint;
typedef unsigned long  Ulong;

struct last_out {
    Ulong  time;
    Ulong  ctime;
    char   bd;
    Uint   size;
    char   label[4];
    } ;

int t_read(char  data[], struct last_out  *lo, int  *first, int  *genotypes)
{   struct last_out  ti;
    int    nargs;
    char   v2[9], v3[9], v4[9];

    sscanf(data,"%s", v2);
    if(!strcmp(v2,"num_sp")) return 0;
    nargs = sscanf(data,"%lx%s%s%s", &ti.time, v2, v3, v4);
    if(*first)
    {   *first = 0;
        if(nargs == 4) *genotypes = 1;
        else *genotypes = 0;
        lo->time += ti.time;     /* assumes lo structure initialized to zero */
        if(lo->time >= 1000000L)
        {   lo->time %= 1000000L;
            lo->ctime++;
        }
        lo->bd = v2[0];
        sscanf(v3,"%u", &lo->size);
        if(*genotypes) strcpy(lo->label,v4);
        else strcpy(lo->label,"");
    }
    else
    {   lo->time += ti.time;
        if(lo->time >= 1000000L)
        {   lo->time %= 1000000L;
            lo->ctime++;
        }
        if(*genotypes) switch(nargs)
        {   case 1: break;
            case 2:
            {   if(isdigit(v2[0]))
                {   sscanf(v2,"%u", &lo->size); break; }
                else
                {   if(strlen(v2) == 1)
                    {   lo->bd = v2[0]; break; }
                    else
                    {   strcpy(lo->label,v2); break; }
                }
            }
            case 3:
            {   if(isdigit(v2[0]))
                {   sscanf(v2,"%u", &lo->size);
                    strcpy(lo->label,v3);
                }
                else
                {   lo->bd = v2[0];
                    if(isdigit(v3[0]))
                        sscanf(v3,"%u", &lo->size);
                    else
                        strcpy(lo->label,v3);
                }
                break;
            }
            case 4:
            {   lo->bd = v2[0];
                sscanf(v3,"%u", &lo->size);
                strcpy(lo->label,v4);
                break;
            }
        }
        else switch(nargs)
        {   case 1: break;
            case 2:
            {   if(isdigit(v2[0]))
                    sscanf(v2,"%u", &lo->size);
                else
                    lo->bd = v2[0];
                break;
            }
            case 3:
            {   lo->bd = v2[0];
                sscanf(v3,"%u", &lo->size);
                break;
            }
        }
    }
    return 1;
}
