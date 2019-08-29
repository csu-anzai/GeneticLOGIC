/* divavg.c  4-6-92  computes averages for diversity indices */
/*** Version 1.0  Copyright (c) 1992  Tom Ray ***/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>

typedef unsigned long  Ulong;

struct siz {
    long   Time;
    long   NumCell;
    long   NumSize;
    float  SizeDiv;
    long   AgeSize;
    } si;

struct gen {
    long   NumGeno;
    float  GenoDiv;
    long   AgeGeno;
    } ge;

struct cum {
    double  Time;
    double  NumCell;
    double  NumSize;
    double  SizeDiv;
    double  AgeSize;
    double  NumGeno;
    double  GenoDiv;
    double  AgeGeno;
    } cu;

void main (argc, argv)
int  argc;
char  **argv;
{   char  ifile[30], ofile[30], bifile[30], format, data[100];
    Ulong   otime = 0, ctime = 0;
    int     binum = 1, genotypes;
    double  ttime = 0;
    FILE    *inf, *ouf;

    if(argc == 1)
    {   printf (" Usage: %s in_file out_file\n",argv[0]);
        exit (1);
    }
    if(argc == 2)
        sprintf(ofile,"divavgs");
    if(argc == 3)
        sprintf(ofile,argv[2]);
    strcpy(ifile,argv[1]);
    sscanf(ifile,"%[^.]", bifile);
    inf = fopen(ifile,"rb");
    if(inf == NULL)
    {   printf("\n Input File %s NOT found. \n", ifile);
        exit(1);
    }
    ouf = fopen(ofile,"w");
    fread(&format,sizeof(char),1,inf);
    fread(&genotypes,sizeof(char),1,inf);
    if(format)
    {   fclose(inf);
        inf = fopen(ifile,"r");
        fgets(data,80,inf);
    }
    si.Time = 0.;
    cu.Time    = 0.; cu.NumCell = 0.; cu.NumSize = 0.; cu.SizeDiv = 0.;
    cu.AgeSize = 0.; cu.NumGeno = 0.; cu.GenoDiv = 0.; cu.AgeGeno = 0.;
    printf("Time = %4ld million\r", ctime);
    for(;;)
    {   if(format)  /* ascii format */
        {   if(fgets(data,80,inf) == NULL)
            {   binum++;
                sprintf(ifile,"%s.%d", bifile, binum);
                fclose(inf);
                inf = fopen(ifile,"r");
                if(inf == NULL)
                    break ;
                if(fgets(data,80,inf) == NULL)
                    break ;
            }
            if(genotypes)
                sscanf(data,"%lx%ld%ld%f%lx%ld%f%lx",
                    &si.Time, &si.NumCell, &si.NumSize, &si.SizeDiv,
                    &si.AgeSize, &ge.NumGeno, &ge.GenoDiv, &ge.AgeGeno);
            else
                sscanf(data,"%lx%ld%ld%f%lx", &si.Time, &si.NumCell,
                    &si.NumSize, &si.SizeDiv, &si.AgeSize);
        }
        else  /* binary format */
        {   if(!fread(&si,sizeof(struct siz),1,inf))
            {   binum++;
                sprintf(ifile,"%s.%d", bifile, binum);
                fclose(inf);
                inf = fopen(ifile,"rb");
                if(inf == NULL)
                    break ;
                if(!fread(&si,sizeof(struct siz),1,inf))
                    break ;
            }
            if(genotypes)
            fread(&ge,sizeof(struct gen),1,inf);
        }
        ttime += (double) si.Time;
        ctime = (Ulong) ttime / 1000000uL;
        if(ctime > otime)
        {   printf("Time = %4ld million\r", ctime);
            fflush(stdout);
            otime = ctime;
        }
        cu.Time    += (double) si.Time;
        cu.NumCell += (double) si.Time * si.NumCell;
        cu.NumSize += (double) si.Time * si.NumSize;
        cu.SizeDiv += (double) si.Time * si.SizeDiv;
        cu.AgeSize += (double) si.Time * si.AgeSize;
        if (genotypes)
        {   cu.NumGeno += (double) si.Time * ge.NumGeno;
            cu.GenoDiv += (double) si.Time * ge.GenoDiv;
            cu.AgeGeno += (double) si.Time * ge.AgeGeno;
        }
    }
    printf("\nTime    = %g\n", cu.Time);
    printf("NumCell = %g\n", cu.NumCell / cu.Time);
    printf("NumSize = %g\n", cu.NumSize / cu.Time);
    printf("SizeDiv = %g\n", cu.SizeDiv / cu.Time);
    printf("AgeSize = %g\n", cu.AgeSize / cu.Time);
    if (genotypes)
    {   printf("NumGeno = %g\n", cu.NumGeno / cu.Time);
        printf("GenoDiv = %g\n", cu.GenoDiv / cu.Time);
        printf("AgeGeno = %g\n", cu.AgeGeno / cu.Time);
    }
    fprintf(ouf, "\n\nTime    = %g\n", cu.Time);
    fprintf(ouf, "NumCell = %g\n", cu.NumCell / cu.Time);
    fprintf(ouf, "NumSize = %g\n", cu.NumSize / cu.Time);
    fprintf(ouf, "SizeDiv = %g\n", cu.SizeDiv / cu.Time);
    fprintf(ouf, "AgeSize = %g\n", cu.AgeSize / cu.Time);
    if (genotypes)
    {   fprintf(ouf, "NumGeno = %g\n", cu.NumGeno / cu.Time);
        fprintf(ouf, "GenoDiv = %g\n", cu.GenoDiv / cu.Time);
        fprintf(ouf, "AgeGeno = %g\n", cu.AgeGeno / cu.Time);
    }
    fclose(ouf);
}
