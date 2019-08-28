/*----------------------------------------------------------------------------*/
/* utility.c - utility routines, contains copyright,  repchar, skip           */
/*----------------------------------------------------------------------------*/

#include "external.h"

void copyright()
{
    void repchar(), skip();
    int iskip;
    int ll = 51;

    iskip = (LINELENGTH - ll)/2;
    skip(1);
    repchar(" ",iskip); repchar("-",ll); skip(1);
    repchar(" ",iskip); 
    fprintf(outfp,"|   PSGA - A Parallel Simple Genetic Algorithm    |\n");
    repchar(" ",iskip); 
    fprintf(outfp,"| (c) David E. Goldberg 1986, All Rights Reserved |\n");
    repchar(" ",iskip); 
    fprintf(outfp,"|   C version by Robert E. Smith, U. of Alabama   |\n");
    repchar(" ",iskip); 
#ifdef NCUBE
    fprintf(outfp,"|   nCUBE port by Jeff Earickson, Boeing Company  |\n");
#else
    fprintf(outfp,"|    UNIX port by Jeff Earickson, Boeing Company  |\n");
#endif
    repchar(" ",iskip); repchar("-",ll); skip(2);
}


void repchar (ch,repcount)
/* Repeatedly write a character to stdout */
char *ch;
int repcount;
{
    int j;

    for (j = 1; j <= repcount; j++) fprintf(outfp,"%s", ch);
}


void skip(skipcount)
/* Skip skipcount lines */
int skipcount;
{
    int j;

    for (j = 1; j <= skipcount; j++) fprintf(outfp,"\n");
}


int ithruj2int(i,j,from)
/* interpret bits i thru j of a individual as an integer      */ 
/* j MUST BE greater than or equal to i AND j-i < UINTSIZE-1  */
/* from is a chromosome, represented as an array of unsigneds */
int i,j;
int *from;
{
    unsigned mask, temp;
    int bound_flag;
    int iisin, jisin;
    int i1, j1, out;
  
    if(j < i)
    {
        fprintf(stderr,"Error in ithruj2int: j < i\n");
        exit(-1);
    }
    if(j-i+1 > UINTSIZE)
    {
        fprintf(stderr,"Error in ithruj2int: j-i+1 > UINTSIZE\n");
        exit(-1);
    }
  
    iisin = i/UINTSIZE;
    jisin = j/UINTSIZE;

    i1 = i - (iisin*UINTSIZE);
    j1 = j - (jisin*UINTSIZE);

    /* check if bits fall across a word boundary */    
    if(iisin == jisin)
        bound_flag = 0;
    else
        bound_flag = 1;

    if(bound_flag == 0)
    {
        mask = 1;
        mask = (mask<<(j1-i1+1))-1;
        mask = mask<<(i1-1);
        out = (from[iisin]&mask)>>(i1-1);
        return(out);
    }
    else
    {
        mask = 1;
        mask = (mask<<j1+1)-1;
        temp = from[jisin]&mask;
        mask = 1;
        mask = (mask<<(UINTSIZE-i1+1))-1;
        mask = mask<<(i1-1);
        out = ((from[iisin]&mask)>>(i1-1)) | temp<<(UINTSIZE-i1);
        return(out);
    }
}
