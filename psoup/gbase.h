/* Genealogy database header file for Primordial Soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 21Jan92 MdG	0.02b		File created.
 */

#define	MAXBYTES	(256)				/* Max len of organism's code	*/
#define	GENOMELEN	(10)				/* Max len of genome name		*/

typedef struct {
	long			number;				/* Organism's number			*/
	long			parent;				/* Organism's parent's number	*/
	long			initpc;				/* Organism's initial PC		*/
	long			birthtick;			/* Time when spawned			*/
	long			deathtick;			/* Time when died				*/
	long			deathage;			/* Age when died				*/
	char			genome[GENOMELEN];	/* Name of organism's genome	*/
} genealogyrecord;

typedef struct {
	char			genome[GENOMELEN];	/* Name of organism's genome	*/
} genomerecord;

#define	GENEALOGYFILE	"family.tre"
#define	GENOMEFILESTART	"genomes"

/* Record for show_census */
struct crec {
	char			genome[GENOMELEN];
	long			count;
	struct crec		*cp;
};
typedef struct crec censusrecord;
