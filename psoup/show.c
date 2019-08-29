/* Utility to dump individuals and genomes for Primordial Soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 21Jan92 MdG	0.02b		File created.
 */

#include <ctype.h>
#include <stdio.h>
#include "soup.h"
#include "gbase.h"
#include "organism.h"

/* Function prototypes */

#ifdef PROTOTYPES
#define _(x) x
#else
#define _(x) ()
#endif

void main _((int, char * []));
void show_organisms _((void));
void get_genome _((char *));
void show_genome _((char *));
void get_organism _((long));
void show_organism _((char *));
void show_census _((int, char * []));
void show_lineage _((int, char * []));
void usage _((void));

genomerecord g;			/* Genome record read from file	*/
char bytes[MAXBYTES];		/* Genome's code read from file */
int length;			/* Length of genome's code		*/
genealogyrecord org;		/* Genealogy record from file	*/
FILE *genealogyfile;

void
main(argc, argv)
int argc;
char *argv[];
{
	char *p;		/* Pointer to first argument	*/

	if (argc <  2) {
		usage();
		exit(1);
	}

	p = argv[1];

	if (strcmp(p, "organisms") == 0) {
		show_organisms();
		exit(0);
	}
	if (strcmp(p, "census") == 0) {
		show_census(argc, argv);
		exit(0);
	}
	if (strcmp(p, "lineage") == 0) {
		show_lineage(argc, argv);
		exit(0);
	}

	if (isalpha(p[0]) && isalpha(p[1]) && isalpha(p[2]) &&
			isalpha(p[3]) && p[4] == '.' && isdigit(p[5])) {
		show_genome(p);
	} else {
		show_organism(p);
	}
	exit(0);
}

void
show_organisms()
{
	long maxnum;
	long i;

	if ( (genealogyfile = fopen(GENEALOGYFILE, RMODE)) == NULL) {
		printf("Can't open genealogy file %s.\n", GENEALOGYFILE);
		exit(1);
	}
	fseek(genealogyfile, -(sizeof(org)), 2);
	fread(&org, sizeof(org), 1, genealogyfile);
	maxnum = org.number;
	rewind(genealogyfile);
	for (i = 0; i < maxnum; i++) {
		fseek(genealogyfile, i * (long) sizeof(org), 0);
		if (fread(&org, sizeof(org), 1, genealogyfile) == 0) {
			printf("Organism %ld not found, or %s not found.\n",
				i, GENEALOGYFILE);
			exit(1);
		}
		printf("Organism:%6ld/%-8s Parent:%6ld PC:%8.8lX ",
			org.number, org.genome, org.parent, org.initpc);
		printf("b:%8ld d:%8ld a:%3ld\n",
			org.birthtick, org.deathtick, org.deathage);
	}
	fclose(genealogyfile);
}

void
show_genome(p)
char *p;
{
	long i;

	get_genome(p);

	/* Disassemble the code */
	for(i = 0; i < (long) length;) {
		printf("%8lX: ", i);
		i += disassemble(&bytes[i], i, (short) 0, (short) 0);
		printf("\n");
	}
}

/*
 * Given the genome name in p, read the genome code into the
 * global array, "bytes".
 */
void
get_genome(p)
char *p;
{
	char genomefilename[20];
	FILE *genomefile;

	length = atoi(&p[5]);
	sprintf(genomefilename, "%s.%s", GENOMEFILESTART, &p[5]);
	if ( (genomefile = fopen(genomefilename, RMODE)) == NULL) {
		printf("Can't open %s.\n", genomefilename);
		exit(1);
	}
	for(;;) {

		/* Read the next genome header.  If end-of-file,
		 * we didn't find the genome.
		 */
		if (fread(&g, sizeof(g), 1, genomefile) == 0) {
			printf("%s not found in database.\n");
			exit(1);
		}

		/* If genome string in file matches calling arg,
		 * we found it.  Break out of the loop.
		 */
		if (strcmp(p, g.genome) == 0) {
			break;
		}

		/* Seek to next entry in file */
		fseek(genomefile, (long) length, 1);
	}

	/* We found the genome.  Read the code for it. */
	if (fread(bytes, sizeof(char), length, genomefile) < length) {
		printf("Couldn't read code for genome.\n");
	}

	/* Close the file */
	fclose(genomefile);
}

void
show_organism(p)
char *p;
{
	long orgnum;
	long i;

	orgnum = atol(p);

	get_organism(orgnum);

	printf("Organism: %ld  Parent: %ld  PC: %lX  Genome: %s\n",
			org.number, org.parent, org.initpc, org.genome);
	get_genome(org.genome);

	/* Disassemble the code */
	for(i = 0; i < (long) length;) {
		printf("%8lX: ", i + org.initpc);
		i += disassemble(&bytes[i], i + org.initpc,
						(short) 0, (short) 0);
		printf("\n");
	}
}

void
get_organism(orgnum)
long orgnum;
{
	if ( (genealogyfile = fopen(GENEALOGYFILE, RMODE)) == NULL) {
		printf("Can't open genealogy file %s.\n", GENEALOGYFILE);
		exit(1);
	}
	fseek(genealogyfile, orgnum * (long) sizeof(org), 0);

	if (fread(&org, sizeof(org), 1, genealogyfile) == 0) {
		printf("Organism %ld not found, or %s not found.\n",
			orgnum, GENEALOGYFILE);
		exit(1);
	}
}

/*
 * Show a count of the number of organisms spawned for
 * each genome.
 */
void
show_census(argc, argv)
int argc;
char *argv[];
{
	censusrecord	*head = (censusrecord *) 0;	/* Start of list */
	censusrecord	*cp;		/* Pointer to record */
	genealogyrecord	org;		/* Rec from genealogy file */
	long		mincount = 1;	/* Minimum count to print	*/

	if ( (genealogyfile = fopen(GENEALOGYFILE, RMODE)) == NULL) {
		printf("Can't open genealogy file %s.\n", GENEALOGYFILE);
		exit(1);
	}

	/* Loop, reading genealogy records, possibly malloc'ing
	 * a new census record (if a genome is encountered for
	 * the first time, and incrementing the count in the
	 * census record.
	 */
	for (;;) {
		/* If end of file, close file and get out of loop */
		if (fread(&org, sizeof(org), 1, genealogyfile) == 0) {
			fclose(genealogyfile);
			break;
		}

		/* Search for the genome in the linked list */
		cp = head;
		while (cp != (censusrecord *) 0) {
			if (strcmp(org.genome, cp->genome) == 0) {
				break;
			}
			cp = cp->cp;	/* Point at next census record */
		}

		/* if not found, malloc another census record
		 * and add to linked list.  If found, increment organism count.
		 */
		if (cp == (censusrecord *) 0) {
			if ( (cp = (censusrecord *)
					malloc(sizeof(censusrecord))) == 0) {
				printf("Malloc failed.\n");
				exit(1);
			}
			cp->cp = head;	/* Point link at old head of list */
			head = cp;	/* This record becomes new head */
			cp->count = 1;	/* Initialize organism count	*/
			strcpy(cp->genome, org.genome);
		} else {	/* If we found genome in census records */
			cp->count++;		/* Bump organism count	*/
		}
	}
	/* If there was a numeric command line argument, we only print
	 * those counts that are >= to the argument.
	 */
	if (argc > 2) {
		mincount = atol(argv[2]);
	}

	/* Print the genomes, one to a line, most recent to least recent */
	cp = head;
	while(cp != (censusrecord *) 0) {
		if ( cp->count >= mincount ) {
			printf("%-8s %d\n", cp->genome, cp->count);
		}
		cp = cp->cp;
	}
}
	
void
show_lineage(argc, argv)
int argc;
char *argv[];
{
	genealogyrecord		org;
	long				orgnum;
	char				lastg[GENOMELEN];

	/* If there's no numeric argument, print usage and exit */
	if (argc < 3) {
		usage();
		exit(1);
	}

	if ( (genealogyfile = fopen(GENEALOGYFILE, RMODE)) == NULL) {
		printf("Can't open genealogy file %s.\n", GENEALOGYFILE);
		exit(1);
	}

	/* Get organism number */
	orgnum = atol(argv[2]);

	if (argc >= 4 && strcmp(argv[3], "genomes") == 0) {
		do {
			/* Read organism's record from genealogy file */
			fseek(genealogyfile,
				orgnum * (long) sizeof(genealogyrecord), 0);
			if (fread(&org, sizeof(genealogyrecord),
						1, genealogyfile) == 0) {
				printf("Can't read genealogy file %s.\n", 
								GENEALOGYFILE);
			}

			/* Check whether genome is different than last one */
			if (strcmp(lastg, org.genome)) {
				/* Print the organism */
				printf(
				"Organism:%6ld/%-8s Parent:%6ld PC:%8.8lX ",
				org.number, org.genome, org.parent, org.initpc);
				printf("b:%8ld d:%8ld a:%3ld\n",
				org.birthtick, org.deathtick, org.deathage);

				/* Copy the new genome to the last genome */
				strcpy(lastg, org.genome);
			}

			/* Get ready to print parent next */
			orgnum = org.parent;
		} while (orgnum != -1L);	/* While organism has parent */
	} else {			/* if "genomes" arg not specified */
		do {
			/* Read the organism's record from the genealogy file */
			fseek(genealogyfile,
				orgnum * (long) sizeof(genealogyrecord), 0);
			if (fread(&org, sizeof(genealogyrecord), 1,
							genealogyfile) == 0) {
				printf("Can't read genealogy file %s.\n",
								GENEALOGYFILE);
			}

			/* Print the organism */
			printf("Organism:%6ld/%-8s Parent:%6ld PC:%8.8lX ",
			org.number, org.genome, org.parent, org.initpc);
			printf("b:%8ld d:%8ld a:%3ld\n",
			org.birthtick, org.deathtick, org.deathage);

			/* Get ready to print parent next */
			orgnum = org.parent;
		} while (orgnum != -1L);	/* While organism has parent */
	}
}
	
void
usage()
{
	printf("Primordial Soup %d.%d%c database query program\n",
					VERSION, REVISION, MdGREV);
	printf("Usage:\n");
	printf("  show arg\n");
	printf("    where arg is either an organism number or a genome\n");
	printf("    specification of the form aaaa.len\n");
	printf("  show organisms\n");
	printf("    show number, genome, parent, initial PC, birthtick,\n");
	printf("    deathtick, and age for every organism, 1 per line.\n");
	printf("  show census [nn]\n");
	printf("    shows each genome with an organism count.  If nn is\n");
	printf("    specified, only genomes with counts >= nn are shown.\n");
	printf("  show lineage nn [genomes]\n");
	printf("    show organism nn and its parent, grandparent, etc. If\n");
	printf("    the genomes argument is specified, only show the genomes\n");
	printf("    from which the organism descended.\n");
}
