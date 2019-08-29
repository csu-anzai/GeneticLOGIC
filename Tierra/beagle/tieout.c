/* 92-4-1  makes x,y list of parameters from tierra.log files */
/* Copyright (c) Daniel Pirone & Virtual Life 1991, 1992 */

#include <stdio.h>

void main(ac,av)
int  ac;
char  *av[];
{   int  x, y;
    char  data[81], buf[81], xbuf[255], ybuf[255];
    FILE  *inf;

    if(ac != 4)
    {   fprintf(stderr,"syntax:  %s infile xfield yfield \n",av[0]);
        exit(-666);
    }
    if (av[1][0] == '-')
        inf = stdin;
    else
    {   inf = fopen(av[1],"r");
        if(inf == NULL)
        {   fprintf(stderr,"tieout error: file %s not opened\n", av[1]);
            exit(0);
        }
    }
    printf("\n\"%s_%s\n",av[2],av[3]);
    x = y = -1;
    if (!strcmp(av[3],"ex")) 
        for(;;)
        {   if(fscanf(inf,"%s",buf) == EOF)
                break ;
            if(!strncmp(buf,av[2],2))
                sscanf(&buf[2],"%d", &x);
            if(!strncmp(buf,av[3],2))
            {   fscanf(inf,"%*[^0-9]%d", &y);
                printf("%d %d\n", x, y);
            }
     
        }
    else
        for(;;)
        {   XXX: if(fscanf(inf,"%s",buf) == EOF)
                break ;
            if(!strncmp(buf,av[2],2))
                sscanf(&buf[2],"%d", &x);
            else goto XXX ;
            YYY: if (fscanf(inf,"%s",buf) == EOF)
                break ;
            if(!strncmp(buf,av[3],2))
            {   sscanf(&buf[2],"%d", &y);
                printf("%d %d\n", x, y);
            }
	    else goto YYY;
     
        }
    fclose(inf);
}
