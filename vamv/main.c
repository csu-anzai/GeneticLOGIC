/*
 * Copyright 1990 Rick McGowan
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/signal.h>

#include "prog.h"

char *version = "@(#) VAMV - Version 1.0; Copyright 1990 Rick McGowan";

extern int optind, opterr;
extern char *optarg;
char *prog;

char ebuf[BUFSIZ*2];
int resolution = 50;	/* major cycle length */
int optmute, optasm, optSTAT, optstat, optzero, optx, optcan, optboot;
int optvid, optASM;
int asmthresh = 0;
int optran = 0;
int ranseed = 0;
int majcyc = 0;	/* number of major cycles run */
int inthandle();	/* handle INT signal */
int gotsig;
extern int nalive;

main(argc, argv)
	int argc;
	char **argv;
{
	register int nmoves, c;
	register FILE *fp;

	optmute = opterr = 0;
	prog = *argv;
	while ((c = getopt(argc, argv, "SsA:avzxcbmr:")) != EOF) {
		switch (c) {
			case 'm': optmute = 1;
				  break;
			case 'A': optASM = 1;
				  asmthresh = atoi(optarg);
				  break;
			case 'a': optasm = 1;
				  break;
			case 'S': optSTAT = 1;
				  break;
			case 's': optstat = 1;
				  break;
			case 'z': optzero = 1;
				  break;
			case 'x': optx = 1;
				  break;
			case 'c': optcan = 1;
				  break;
			case 'v': optvid = 1;
				  break;
			case 'b': optboot = 1;
				  break;
			case 'r': optran = 1;
				  ranseed = atoi(optarg);
				  break;
			case '?':
			default:
	usage:
				printf("Usage: %s [-maSszxcbv] [-A n] [-r n] cyclen\n",
					prog);
				exit(1);
		}
	}
	if (optind < argc) {
		resolution = atoi(argv[optind++]);
		if (resolution < 0) {
			printf("Warning cycle length %d out of bounds.\n",
				resolution);
			goto usage;
		}
	}
	printf(version);
	readlim();
	if (optstat || optSTAT) {
		showlim(stderr);
		psuc(stderr);
	}
	setvbuf(stderr, ebuf, _IOFBF, BUFSIZ*2);
	printf(PCLEAR); fflush(stdout);
	init();
	syncurs();
	if (optstat || optSTAT) {
		fprintf(stderr, "Init.  Ran Seed = %d, Maj Cyc = %d\n",
			ranseed, resolution);
	}
	if (fp = fopen(".LastRun", "w")) {
		fprintf(fp, "Init.  Ran Seed = %d, Maj Cyc = %d\n",
			ranseed, resolution);
		showlim(fp);
		psuc(fp);
		fflush(fp);
		fclose(fp);
	}
	signal(SIGINT, inthandle);
	nmoves = 0;
	majcyc = 1;
	printf(PCYCLE, majcyc);
	for ( ; ; ) {
		if (gotsig) {
			printf("\n\nCaught Signal\n\n");
			if (optstat || optSTAT)
				finstat(1);
			exit(1);
		}
		makemoves();
		++nmoves;
		if (nmoves == resolution) {
			printf(PCYCLE, majcyc);
			fflush(stderr);
			pstat();
			if (optzero && ! nalive) {
				if (optstat || optSTAT) {
					fprintf(stderr,
					"\nAborted: Zero Population\n");
					finstat(1);
				}
				printf(PHOME);
				printf("Zero Population\n");
				exit(1);
			}
			cull();
			if (optstat || optSTAT) {
				if ((majcyc % 5) == 0)
					popstats();
			}
			nmoves = 0;
			++majcyc;
			if (optASM) {
				if ((! optstat) && (! optSTAT))
					optASM = 0;
				else {
					if (optasm == 1) {
						if (majcyc == (asmthresh+5))
							optasm = optASM = 0;
					}
					else if (majcyc == asmthresh) {
						orgheader();
						optasm = 1;
					}
				}
			}
		}
	}
}

int
inthandle()

{
	gotsig = 1;
}

extern int mindens, STARVE, MAXFOOD;	/* , overate; */
extern int bestinstr, goodinstr, crowded;
extern int macrowded, maxldiv, elbow, culdens;
extern int nseed, nreseed, divstr, synstr, convert;
extern int initfood, ifoodmax, neisave;
extern int sucary[];

readlim()

{
	register FILE *fp;
	char buf[256];
	register int i;

	if (! (fp = fopen("Lim.File", "r")))
		return(0);

	i = 0;
	while (fgets(buf, 256, fp) && i < 5) {
		if (buf[0] == '#') {
			continue;
		}
		else {
if (i == 0)
	sscanf(buf, "%d %d %d\n", &nseed, &nreseed, &mindens);
if (i == 1)
	sscanf(buf, "%d %d %d %d\n",&STARVE, &MAXFOOD, &bestinstr, &goodinstr);
if (i == 2)
	sscanf(buf, "%d %d %d %d\n",&crowded, &macrowded, &maxldiv, &elbow);
if (i == 3)
	sscanf(buf, "%d %d %d %d\n",&culdens, &divstr, &synstr, &convert);
if (i == 4)
	sscanf(buf, "%d %d %d\n",&initfood, &ifoodmax, &neisave);
			++i;
		}
	}
/* Now, read 16 instruction scores */
	i = 0;
	while (fgets(buf, 256, fp) && (i < 16)) {
		if (buf[0] == '#') {
			continue;
		}
		else {
			sscanf(buf, "%d %d %d %d\n",
				&(sucary[i]),
				&(sucary[i+1]),
				&(sucary[i+2]),
				&(sucary[i+3]));
			i += 4;
		}
	}
	if (i > 0 && (i != NINSTR)) {
printf("Warn: only got %d instr point values from Lim.File!\n", i);
fprintf(stderr, "Warn: only got %d instr point values from Lim.File!\n", i);
	}
	fclose(fp);
	return(1);
}

showlim(fp)
	FILE *fp;
{
fprintf(fp, "Lim.File Settings:\n");
fprintf(fp, "\tnseed %d, nreseed %d, mindens %d\n",
	nseed, nreseed, mindens);
fprintf(fp, "\tSTARVE %d, MAXFOOOD %d, bestinstr %d, goodinstr %d\n",
	STARVE, MAXFOOD, bestinstr, goodinstr);
fprintf(fp, "\tcrowded %d, macrowded %d, maxldiv %d, elbow %d\n",
	crowded, macrowded, maxldiv, elbow);
fprintf(fp, "\tculdens %d, divstr %d, synstr %d, convert %d\n",
	culdens, divstr, synstr, convert);
fprintf(fp, "\tinitfood %d, ifoodmax %d, neisave %d\n",
	initfood, ifoodmax, neisave);
}

psuc(fp)
	FILE *fp;
{
	register int i;

fprintf(fp, "\tScore Array:\n");
	for (i = 0; i < NINSTR; i += 4)
		fprintf(fp, "\t%5d %5d %5d %5d\n",
			sucary[i], sucary[i+1],
			sucary[i+2], sucary[i+3]);
}
