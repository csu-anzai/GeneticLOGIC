
/*
 * ==================================================
 *
 *    Distributed GENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@lamport.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    1993
 *
 * --------------------------------------------------
 *
 *
 *  file:       misc.c
 *
 *  purpose:    miscelaneous functions
 *
 */


/*#include <stdio.h>*/
#include "extern.h"


/*
 * Reads a line from file 'fp', ignores comment lines starting with '#'.
 * Returns NULL on end of file.
 */
char *ReadLine(fp, line, length)
FILE *fp;
char *line;
int length;
{
	char *status;

	do
		status = fgets(line, length, fp);
	while (line[0] == '#' && status != NULL);

	return status;
}



/*
 * counts lines in file, ignores commented lines
 */
int CountLines(file)
char *file;
{
        register int lines = 0;
        char line[100];
        char msg[80];  /* error message */
        char *status;
        FILE *fp;

        if ((fp = fopen(file, "r")) == NULL){
                sprintf("CountLines can't open file %s", file);
                IOError(msg);
        }

        status = ReadLine(fp, line, 100);
        while(status != NULL){
                lines++;
                status = ReadLine(fp, line, 100);
        }

        fclose(fp);

        return lines;
}


int     ilog2 (n)
unsigned long   n;
{
        register int    i;

        if (n <= 0) {
                printf ("Help! values is %d, must be positive!\n", n);
                abort ();
        }

        i = 0;
        while ((int) (n & 1) == 0) {
                n >>= 1;
                i++;
        }
        return (i);
}
              

int compstruct(s1, s2)
STRUCTURE *s1;
STRUCTURE *s2;
{
	if (BETTER(s1->Perf, s2->Perf)) return -1;
        if (BETTER(s2->Perf, s1->Perf)) return 1;
        return 0;
}

void SortPopulation(pop)
STRUCTURE *pop;
{
	qsort(pop, Popsize, sizeof(STRUCTURE), compstruct);
}


/*** end of file ***/

