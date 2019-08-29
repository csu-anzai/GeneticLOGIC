/* Genealogy database recording code.  Called when a new
 * organism is spawned.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot. All rights reserved.
 *
 * Modification log:
 * 08Feb92 MdG	0.3e	Sped up the genome database search by checking
 *						whether current organism is same as last.
 * 21Jan92 MdG	0.2b	Wrote the bulk of the code.
 * 19Jan92 MdG	0.2		File created.
 */

/*************************************************************************
Genome files have names like genomes.xxx where xxx is the length of
the genomes in bytes.  The genome files contain records consisting
of:
a genomerecord		( Containing the genome name )
the code			( A record of length bytes with the organism's code )

The genealogy file is named family.tre and contains records consisting
of:

a genealogyrecord	( Containing the organism #, the parent #, and the
					  genome name )
*************************************************************************/

#ifdef	GBASE			/* Corresponding endif is on last line of file	*/

#include "soup.h"
#include "gbase.h"
#include "organism.h"
#include "protos.h"
#include <stdio.h>

/* Variables common to routines in this file */
genomerecord	newg;			/* New genome record to write to file	*/
unsigned char	bytes[MAXBYTES]; /* Local copy of the organism's code	*/
genomerecord	g;				/* Genome record from genome file		*/
unsigned char	b[MAXBYTES];	/* Genome's code from genome file		*/
genealogyrecord	glogy;			/* Organism's new genealogy record		*/
FILE			*genomefile;
char			genomefilename[GENOMELEN];

/* Externs */
extern	FILE	*genealogyfile;
extern	ORGANISM	organisms[];
extern	unsigned char	*soup;
extern	long	instructions;
extern	long	lifetime;

/*
 * Initialize the genealogy database code
 */
void
init_gbase()
{

	/* Try to open the genealogy file */
	if ( (genealogyfile = fopen(GENEALOGYFILE, WUMODE)) == NULL ) {
		printf("*** Can't open the genealogy file.\n");
		soup_exit();
	}
}

/*
 * Record the new organism in the database.
 */
void
record_birth(index)
long index;
{
	long len1;
	long len2;
	int len;
	short nosearch;
	short found;
	int codelen;

	/* Make a local copy of the organism's code. */
	if (organisms[index].initpc + organisms[index].length > SOUPLEN) {
		len1 = SOUPLEN - organisms[index].initpc;
		len2 = organisms[index].length - len1;
		memcpy(bytes, &soup[organisms[index].initpc], len1);
		memcpy(&bytes[len1], &soup[organisms[index].initpc + len1], len2);
	} else {
	memcpy(bytes, &soup[organisms[index].initpc],
												(int) organisms[index].length);
	}

	codelen = (int) organisms[index].length;

	/* Open the genome file for this length of organism.  If the
	 * file doesn't exist, create it.
	 */
	nosearch = 0;			/* Initialize flag */
	sprintf(genomefilename, "%s.%d", GENOMEFILESTART, codelen);
	if ( (genomefile = fopen(genomefilename, RUMODE)) == NULL ) {

		/* Set a flag indicating no need to search
		 * file for this genome.
		 */
		nosearch = 1;
		if ( (genomefile = fopen(genomefilename, WMODE)) == NULL ) {
			printf("*** Can't open %s for writing.\n", genomefilename);
			soup_exit();
		}
	}

	found = 0;					/* Initialize flag	*/

	/* If there's stuff in the file, find out whether this
	 * organism's genome is already in there.
	 */
	if (nosearch == 0) {
		/*
		 * First check whether this new organism is the same as
		 * the last organism that was spawned.  Check the genome
		 * name start to be sure that the variables have
		 * been assigned values.
		 */
		if (memcmp( (char *) b, (char *) bytes, codelen ) == 0
		&& memcmp(g.genome, GENOMEFILESTART, strlen(GENOMEFILESTART)) == 0) {
			found = 1;
		} else {
			for(;;) {
				/* Read the genome record from the file */
				len = fread((char *) &g, sizeof(g), 1, genomefile);
				if (len == 0) {			/* If end of file	*/
					break;				/* Exit loop		*/
				}

				/* Read the genome's code from the file */
				len = fread((char *) b, sizeof(unsigned char), codelen,
																genomefile);
				if (len < codelen) {
					printf("*** Unexpected end of file %s\n", genomefilename);
					soup_exit();
				}

				/* If the genome's code matches the organism's code,
				 * we found it in the genome file.
				 */
				if (memcmp( (char *) b, (char *) bytes, codelen) == 0) {
					found = 1;
					break;
				}
			}
		}
	}

	/* If we found it, set the current organism's genome
	 * for recording in the genealogy file.  If not, generate
	 * a new genome name and write the new genome to the file.
	 */
	if (found) {			/* If organism found in genome file */
		/* Copy genome name from genome file to genealogy record
		 * and organism's process structure
		 */
		strcpy(glogy.genome, g.genome);
		strcpy(organisms[index].genome, g.genome);
	} else {				/* If organism not found in genome file */
		/* If new genome file, start with aaaa.len, else
		 * get last genome name and make a new one from it
		 */
		if (nosearch) {		/* If brand new genome file */
			sprintf(newg.genome, "aaaa.%d", codelen);
		} else {			/* If existing genome file */
			strcpy(newg.genome, g.genome);
			nextgenomename(newg.genome);
		}

printf("*** New genome: %-8s  Instructions = %ld.\n",
													newg.genome, instructions);

		/* Copy the new genome name to the genealogy record 
		 * and to the organism's process structure
		 */
		strcpy(glogy.genome, newg.genome);
		strcpy(organisms[index].genome, newg.genome);

		/* Write the genome record to the file */
		if (fwrite(&newg, sizeof(newg), 1, genomefile) == 0) {
			printf("Couldn't write header to %s.\n", genomefilename);
			soup_exit();
		}

		/* Write the organism's code to the file */
		if (fwrite(bytes, sizeof(unsigned char), codelen, genomefile) <
																codelen) {
			printf("Couldn't write code to %s.\n", genomefilename);
		}
	}
	fclose(genomefile);

	/* Set up the new genealogy record and write it
	 * to the genealogy file.
	 */
	glogy.number = organisms[index].number;
	glogy.parent = organisms[index].parent;
	glogy.initpc = organisms[index].initpc;
	glogy.birthtick = organisms[index].birthtick;
	fseek(genealogyfile, 0L, 2);	/* Seek to end of file	*/
	if (fwrite(&glogy, sizeof(glogy), 1, genealogyfile) == 0) {
		printf("Couldn't write to genealogy file.\n");
		soup_exit();
	}
}

/*
 * Record the death of an organism in the genealogy file
 */
void
record_death(ind)
long ind;
{
	genealogyrecord deathg;

	/* Compute position of organism in genealogy file */
	long pos = organisms[ind].number * (long) sizeof (genealogyrecord);

	/* Read the organism into deathg */
	fseek(genealogyfile, pos, 0);
	if (fread(&deathg, sizeof(genealogyrecord), 1, genealogyfile) == 0) {
		printf("record_death: Can't read organism %ld\n",
													organisms[ind].number);
		soup_exit();
	}

	/* Set time at which death occurred from # of instructions
	 * executed so far.
	 */
	deathg.deathtick = instructions;

	/* Set age of organism */
	deathg.deathage = lifetime - organisms[ind].lifetime;

	/* Write the record back to the file */
	fseek(genealogyfile, pos, 0);
	if (fwrite(&deathg, sizeof(genealogyrecord), 1, genealogyfile) == 0) {
		printf("record_death: Can't write organism %ld\n",
													organisms[ind].number);
	}
}

void
nextgenomename(genome)
char *genome;
{
	if (genome[3] == 'z') {
		genome[3] = 'a';
		if (genome[2] == 'z') {
			genome[2] = 'a';
			if (genome[1] == 'z') {
				genome[1] = 'a';
				if (genome[0] == 'z') {
					genome[0] = 'a';
				} else {
					genome[0]++;
				}
			} else {
				genome[1]++;
			}
		} else {
			genome[2]++;
		}
	} else {
		genome[3]++;
	}
}
#endif
