
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
 *  file:       links.c
 *
 *  purpose:    produce files with link info based in topology selected
 *              interactively by the user
 *
 */



#include <stdio.h>
#include <stdlib.h>
#include "global.h"


/*
 * creates the links describing a hypercube of specified dimension
 */
void hypercube (links, dim)
unsigned long int      *links;
int     dim;
{
	register unsigned long int      i;
	register int    j;
	int     nodes;		/* nodes in hypercube */
	char   *strrep;		/* string representation of each node id */

	nodes = 0x01 << dim;

	strrep = (char *) calloc (nodes, sizeof (char));
	if (strrep == NULL)
		Error ("links: can't allocate memory for specified hypercube");

	for (i = 0; i < nodes; i++)
		for (j = 0; j < dim; j++) {
			Itoc (i, strrep, dim);
			strrep[j] = (strrep[j] == '0' ? '1' : '0');
			*links++ = i;
			*links++ = Ctoi (strrep, dim);
		}

	free (strrep);
}



/*
 * makes the links describing a ring topology, the ring can be unidirecional
 * or bidirectional (dir = 1 or dir = 2, respectively)
 */
void ring (links, nodes, dir)
unsigned long int      *links;
int     nodes;
int     dir;
{
	register int    i;

	for (i = 0; i < nodes - 1; i++) {
		*links++ = i;
		*links++ = i + 1;
	}
	/* the last node is linked with the first */
	*links++ = nodes - 1;
	*links++ = 0;

	/* if ring is bidirectional, create additional links */
	if (dir == 2) {
		for (i = 0; i < nodes - 1; i++) {
			*links++ = i + 1;
			*links++ = i;
		}
		/* the first node is linked with the last */
		*links++ = 0;
		*links++ = nodes - 1;
	}

}



/*
 * creates the links for a square mesh topology, parameter n is the dimension
 * of the mesh
 */
void mesh (links, n)
unsigned long int      *links;
int     n;
{
	register int    i;

	for (i = 0; i < n * n; i++) {
		/* up */
		*links++ = i;
		if (i / n == 0)
			*links++ = i + n * (n - 1);
		else
			*links++ = i - n;

		/* down */
		*links++ = i;
		if (i / n == n - 1)
			*links++ = i - n * (n - 1);
		else
			*links++ = i + n;

		/* right */
		*links++ = i;
		if (i % n == n - 1)
			*links++ = i - (n - 1);
		else
			*links++ = i + 1;


		/* left */
		*links++ = i;
		if (i % n == 0)
			*links++ = i + (n - 1);
		else
			*links++ = i - 1;
	}
}



void complete (links, n)
unsigned long int      *links;
int     n;
{
	register int    i, j;

	for (i = 0; i < n; i++)
		for (j = 0; j < n; j++)
			if (j != i) {
				*links++ = i;
				*links++ = j;
			}
}



void main (argc, argv)
int     argc;
char   *argv[];
{
	register int    i;	/* loop control	 */
	int    opt;		/* menu option	 */
	int     n;		/* dimension of the desired topology	 */
	char    exp[80];	/* name of experiment */
	char    linkfile[30];	/* file to write the links	 */
	int     migint;		/* migration interval */
	double   migrate;	/* migration rate */
	int     nLinks;		/* number of links in the topology selected */
	unsigned long int      *pLinks, *pTemp;	/* array of links	 */
	char	topname[80];	/* topology name */
	char    dir[2];
	FILE 	*fp;


	printf ("\n\tDGENESIS linkfile Generator Program\n\n");
	printf ("Default values are displayed in brackets\n\n");

	/* set up linkfile name */
	if (argc < 2) {
		exp[0] = 0;
		INPUTSTR ("\nExperiment name", exp);
	}
	else {
		sprintf (exp, argv[1]);
	}
	sprintf (linkfile, "link.%s", exp);


	/* print the menu */
	printf ("\n\n\t1 - Hypercube\n");
	printf ("\t2 - Ring\n");
	printf ("\t3 - Mesh\n");
	printf ("\t4 - Complete\n\n");
	opt = 1;
	INPUT ("Select an Option", "%d", opt);


	switch (opt) {

		case 1:
			n = 4;
			INPUT ("Dimension of the hypercube", "%d", n);
			nLinks = (1 << n) * n;
			pLinks = (unsigned long int *) calloc (nLinks * 2, sizeof (unsigned long int));
			hypercube (pLinks, n);
			sprintf(topname, "Hypercube topology, dimension %d", n);
			break;

		case 2:
			n = 8;
			INPUT ("Number of nodes", "%d", n);
			sprintf (dir, "y");
			INPUTSTR ("Bidirectional", dir);
			nLinks = n * (dir[0] == 'y' ? 2 : 1);
			pLinks = (unsigned long int *) calloc (nLinks * 2, sizeof (unsigned long int));
			ring (pLinks, n, (dir[0] == 'y' ? 2 : 1));
			if (dir[0] == 'y')
				sprintf(topname, "Bidirectional Ring Topology between %d nodes", n);
			else
				sprintf(topname, "Unidirectional Ring Topology between %d nodes", n);

			break;

		case 3: 
			n = 4;
			INPUT ("Dimension of the mesh", "%d", n);
			nLinks = n * n * 4;
			pLinks = (unsigned long int *) calloc (nLinks * 2, sizeof (unsigned long int));
			mesh (pLinks, n);
			sprintf(topname, "Square Mesh Topology of dimension %d", n);
			break;

		case 4: 
			n = 4;
			INPUT ("Number of nodes", "%d", n);
			nLinks = n * (n - 1);
			pLinks = (unsigned long int *) calloc (nLinks * 2, sizeof (unsigned long int));
			complete (pLinks, n);
			sprintf(topname, "Complete Topology between %d nodes", n);
			break;

	}

	pTemp = pLinks;

	migrate = 0.20;
	INPUT ("Migration rate", "%lf", migrate);
	migint = 20;
	INPUT ("Migration interval", "%d", migint);


	/* write the linkfile */

	fp = fopen (linkfile, "w");
	if (fp == NULL)
		printf ("links: can't open linkfile");

	fprintf (fp, "#\n# file: %s\n", linkfile);
	fprintf (fp, "# %s\n#\n", topname);

	for (i = 0; i < nLinks; i++) {
		fprintf (fp, "%d\t", *pLinks++);
		fprintf (fp, "%d\t", *pLinks++);
		fprintf (fp, "%d\t%0.2f\n", migint, migrate);
	}
	fclose (fp);

	printf ("\nfile \"%s\" created succesfully\n\n", linkfile);

	free ((char *) pTemp);
}

/*** end of file ***/
