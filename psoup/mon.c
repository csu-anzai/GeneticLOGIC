/* Console monitor for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 29Feb92 MdG	0.4c	Changed the o command to take an optional argument.
 * 01Feb92 MdG	0.3d	Cleaned up monitor prompt.
 * 27Jan92 MdG	0.3c	Added error message to B command.
 * 19Jan92 MdG	0.2a	Added gbase stuff.
 * 15Jan92 MdG		Added stuff to stats cmd.
 * 14Jan92 MdG		Added load and save commands.
 * 12Jan92 MdG		File created.
 */

#include "soup.h"
#include "organism.h"
#include "protos.h"
#include <ctype.h>
#include <stdio.h>

/* Externs */
extern	unsigned char	*soup;
extern	long		births;
extern	long		deaths;
extern	long		instructions;
extern	ORGANISM	organisms[];
extern	short		adjflag;
extern	long		heartbeat;
extern	long		lifetime;
extern	long		mutate_limit;
extern	long		notify;
extern	long		watch;
extern	long		noparentflag;
extern	long		noparentlength;
#ifdef	GBASE
extern	short		gbaseflag;
#endif

void
mon()
{
	char answer[100];	/* User's input line			*/
	char choice;		/* User's choice of command		*/
	char *p;		/* Used for parsing cmd line		*/
	long addr;		/* Address in the soup			*/
	int i;			/* Loop counter				*/
	long org;		/* Organism #				*/
	long num;		/* Scratch				*/
	long ind;		/* Index of organism			*/

	printf("\nInstructions = %lu.  ", instructions);
	printf("Type ? at the monitor prompt for help.\n");
	for (;;) {
		printf("---- Monitor > ");
		fflush(stdout);
		gets(answer);

		/* Skip to top of loop if user typed blank line */
		if (*answer == '\0') {
			continue;
		}
		choice = islower(answer[0]) ? toupper(answer[0]) : answer[0];
		switch (choice) {
			case '?':
				monhelp();
				break;
			case 'D':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks	*/
				sscanf(p, "%lx", &addr);
				dump_soup(addr); /* Spill soup on console */
				break;
			case 'O':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				if (*p == '\0') { /* If no argument */
					dump_organisms(); /* Dump proc table */
				} else {	/* If numeric argument */
					org = atol(p);
					dump_one_organism(org);
				}
				break;
			case 'S':
				dump_stats();	/* Print numbers	*/
				break;
			case 'G':
				printf("Resuming the interpreter.\n");
				return;
				break;
			case 'K':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks	*/
				org = atol(p);	/* Get the organism to kill */
				if (reap(org) == 0) {
					printf("*** Error killing organism.\n");
				} else {
					printf("*** Killed.\n");
				}
				break;
			case 'R':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				read_soup(p);
				break;
			case 'W':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				write_soup(p);
				break;
			case 'N':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				addr = atol(p);		/* Get the addr	*/
				addr &= SOUPLEN - 1; /* Modulo soup length */
				while (*p != ' ') p++;	/* Skip first arg */
				while (*p == ' ') p++;	/* Skip blanks */
				noparentlength = atol(p);
				noparentflag = 1;
				if (spawn(addr)) {
					printf("Spawn successful.\n");
				} else {
					printf("Spawn failed.\n");
				}
				noparentflag = 0;
				break;
			case 'A':
				if (adjflag) {
					printf(
					    "Adjust_population turned off.\n");
					adjflag = 0;
				} else {
					printf(
					    "Adjust_population turned on.\n");
					adjflag = 1;
				}
				break;
#ifdef	GBASE
			case 'B':
				if (gbaseflag) {
		printf("*** Genealogy database recording off.\n");
					gbaseflag = 0;
				} else {
					if (births) {
		printf("*** Error.  Recording can only be turned %s",
		"on before organisms are spawned.\n");
					} else {
		printf("*** Genealogy database recording on.\n");
						gbaseflag = 1;
					}
				}
#endif
				break;
			case 'H':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				heartbeat = atol(p);	/* Get the # */
				printf("Heartbeat: %ld\n", heartbeat);
				break;
			case 'L':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				lifetime = atol(p);	/* Get the # */
				printf("Lifetime: %ld\n", lifetime);
				break;
			case 'M':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				mutate_limit = atol(p);	/* Get the #	*/
				printf("Mutate: %ld\n", mutate_limit);
				break;
			case 'T':
				p = &answer[1];	/* Point at 2nd char	*/
				while (*p == ' ') p++;	/* Skip blanks	*/
				notify = atol(p);	/* Get the #	*/
				printf("Notify: %ld\n", notify);
				break;
			case 'U':
				p = &answer[1];	/* Point at 2nd character */
				while (*p == ' ') p++;	/* Skip blanks */
				sscanf(p, "%lx", &addr); /* Get hex addr */
				for (i = 0; i < 22; i++) { /* For 22 lines */
					printf("%8.8lx: ", addr);
					addr += disassemble(&soup[addr], addr,
						(short) 1, (short) 0);
					printf("\n");
				}
				break;
			case 'F':
				p = &answer[1];
				while (*p == ' ') p++;
				num = atol(p);
				if ( (ind = orgindex(num)) == -1) {
		printf("*** Error. Organism not found.\n");
					watch = -1;
				} else {
					printf("*** Following %ld.\n", num);
					watch = ind;
				}
				break;
			case 'C':
				for (i = 0; i < NORGANISMS; i++) {
					kill_organism((long) i);
				}
				births = deaths = 0;
				printf("*** Processes cleared.\n");
				break;
			case 'Q':
				printf("*** Exiting psoup.\n");
				soup_exit();
				break;	
			default:
				printf("Illegal command.  Try again.\n");
				break;
		}
	}
}
void
dump_soup(addr)
long addr;
{
	short i, j;

printf("Dumping from %lX\n", addr);

	for (i = 0; i < 8; i++) {
		printf("%8.8lX:  ", 16 * i + addr);
		for (j = 0; j < 16; j++) {
			if (j != 0 && (j & 3) == 0) {
				printf(" ");
			}
			printf("%2.2X ", soup[i * 16 + j + addr]);
		}
		printf("\n");
	}
}

void
dump_one_organism(orgn)
long orgn;
{
	long ind;		/* Index into process table */

	if ( (ind = orgindex(orgn)) == -1) {
		printf("*** Organism not found.\n");
	} else {
		printf("Organism #     %lu\n", organisms[ind].number);
		printf("Initial PC     %8.8lx\n", organisms[ind].initpc);
#ifdef GBASE
		if (gbaseflag) {
			printf("Genome         %s\n", organisms[ind].genome);
		} else {
			printf("Length         %lu\n", organisms[ind].length);
		}
#else
		printf("Length         %lu\n", organisms[ind].length);
#endif
		printf("Current PC     %8.8lx\n", organisms[ind].pc);
		printf("Lifetime       %lu\n", organisms[ind].heartbeat);
		printf("Heartbeat      %lu\n", organisms[ind].lifetime);
		printf("Parent         %lu\n", organisms[ind].parent);
		printf("Birth tick     %lu\n", organisms[ind].birthtick);
		printf("R1             %8.8lx\n", organisms[ind].r1);
		printf("R2             %8.8lx\n", organisms[ind].r2);
		printf("R3             %8.8lx\n", organisms[ind].r3);
		printf("R4             %8.8lx\n", organisms[ind].r4);
	}
}

/*
 * If o command given without an argument, dump the whole
 * process table.
 */
void
dump_organisms()
{
	long i;
	short nprinted = 0;	/* Number of lines printed */

	for (i = 0; i < NORGANISMS; i++) {
		if (organisms[i].alive) {
			/* Print column headings every twenty lines */
			if (nprinted++ % 20 == 0) {
				printf("Org # /Init PC ");
#ifdef	GBASE
				if (gbaseflag) {
					printf("/ Genome : ");
				} else {
					printf(": ");
				}
#else
				printf(": ");
#endif
				printf("Curr PC  HBt LfT ");
				printf("   R1       R2       R3       R4\n");
			}

#ifdef	GBASE
			if (gbaseflag) {
				printf("%6ld/%8.8lX/%-8s: ",
					organisms[i].number, 
					organisms[i].initpc,
					organisms[i].genome);
			} else {
				printf("%6ld/%8.8lX: ", organisms[i].number,
					organisms[i].initpc);
			}
#else
			printf("%6ld/%8.8lX: ", organisms[i].number,
					organisms[i].initpc);
#endif
			printf("%8.8lX ", organisms[i].pc);
			printf("%3ld ", organisms[i].heartbeat);
			printf("%3ld ", organisms[i].lifetime);
			printf("%8.8lX ", organisms[i].r1);
			printf("%8.8lX ", organisms[i].r2);
			printf("%8.8lX ", organisms[i].r3);
			printf("%8.8lX\n", organisms[i].r4);
		}
	}

	/* If no organisms, print a message */
	if (nprinted == 0) {
		printf("*** There are no organisms in the soup.\n");
	}
}

void
dump_stats()
{
	printf("Instructions interpreted: %lu\n", instructions);
	printf("Population:               %lu\n", births - deaths);
	printf("Births:                   %lu\n", births);
	printf("Deaths:                   %lu\n", deaths);
	printf("Initial Lifetime:         %lu\n", lifetime);
	printf("Initial Heartbeat:        %lu\n", heartbeat);
	printf("Mutation interval:        %lu\n", mutate_limit);
	printf("Tell after age:           %lu\n", notify);
	printf("Adjust_population() is turned ");
	if (adjflag) printf("on.\n"); else printf("off.\n");
#ifdef GBASE
	printf("Genealogy database recording is turned ");
	if (gbaseflag) printf("on.\n"); else printf("off.\n");
#endif
}

void
read_soup(name)
char *name;
{
	FILE *fp;
	int nitems;

	if ( (fp = fopen(name, RMODE)) == NULL ) {
		printf("File open failed\n");
		return;
	} else {
		nitems = fread(soup, 1, SOUPLEN, fp);
		printf("%d bytes read.\n", nitems);
		fclose(fp);
	}
}

void
write_soup(name)
char *name;
{
	FILE *fp;
	int nitems;

	if ( (fp = fopen(name, WMODE)) == NULL ) {
		printf("File open failed\n");
		return;
	} else {
		nitems = fwrite(soup, 1, SOUPLEN, fp);
		printf("%d bytes written.\n", nitems);
	}
}

void
monhelp()
{
printf("?               Print this command list\n");
printf("(A)djust        Toggle on/off adjust_population()\n");

#ifdef GBASE
printf("data(B)ase      Toggle on/off genome database recording\n");
#endif

printf("(C)lear		Remove all organisms (clear process table)\n");
printf("(D)ump addr	Dump the soup starting at addr\n");
printf("(F)ollow org#	Follow execution of organism org#\n");
printf("(G)o            Exit the monitor, restart the interpreter\n");
printf("(H)eartbeat nn  Set organisms' initial heartbeat to nn\n");
printf("(K)ill org#	Kill organism org#\n");
printf("(L)ifetime nn	Set organisms' initial lifetime to nn\n");
printf("(M)utate nn     Set the mutation interval to nn\n");
printf("(N)ew addr len  Start new organism at addr with length len\n");
printf("(O)rganism [nn] If no arg dump process table else dump one organism\n");
printf("(Q)uit		Exit psoup\n");
printf("(R)ead file     Read file into the soup starting at addr 0\n");
printf("(S)tats         Print various interesting numbers\n");
printf("(T)ell nn       Print msg when org >= nn instructions old\n");
printf("(U)nassemble nn Disassemble instructions from addr nn\n");
printf("(W)rite file	Write the entire soup to file\n");
}
