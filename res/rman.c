#include "rman.h"

/*  
 * This is a trivial remote executor for commnads that take a long
 * time...
 * it uses
 * the sun-rpc services:
 * rstat and rexec to achieve its ends...
 * 
 * It reads a .rman file in yer home directory
 * and implements a trivial lowest load first placement
 * with periodic load gathering algorith
 */

/*
 * Globular data
 */
ResourceRecord *RR;
/*
 * Should do all this *per command* type (e.g. cc, troff, tex, ...)
 */
int 	NR;
User 	U;

/*
 * Use this to assess user's win from current weights
 * (plus central GA will use view of load etc to asses global
 * consequences 
 */
int timing = 0; /* The essence of true comedy */
struct timeval now, then, elapse;

#define prompt() printf("> ")

/*
 * Initial weights for a first go
 */
#define UW 100
#define L1 15
#define L5 5
#define L15 1

int weight[4] =  { UW, L1, L5, L15 };
/*
 * This is where the real work goes on -
 * given the list of machines...
 *
 * and load averages over 1, 5 15 mins
 * and number of users
 *
 * Would be nice to add distance in the network
 * 	+ size of binary to run
 *	+ size of data files it accesses
 * To be continued when we have 'Initial File Placement' (ie whole
 * file caching + process migration...
 *
 * which to use??? 
 * it is a linear combination
 * of these, io stats and network traffic
 * (see bezerkeley papers which say so)
 *
 * Mr Hickling says - could combine heuristics rather than/as well as
 * weights... a more subtle approach...
 *
 * Weights are currently based on intuition that users do a lot, and 
 * recent history is more important than distant history:
 *
 * A REALLY funky thing to do here would be to ffed the weights into
 * A GENETIC Algorithm
 * which i have one here that i prepared earlier...
 *
 * the load should really be characterised by type as well as number
 * jobs in the queue ... we would need to log this to reward
 * (==payoff) a GA
 *
 * Unfortunately, under load, the system may ant to DYNAMICALLY adjust
 * these weights...what dynamic function population... how to represent the 
 * dynamic function, etc
 * i.e. as Alastair says - function = heuristic in some way...
 * 	could use a taylor series or fourier and combine coefficients
 *		on basis of GA or whatever...
 *
 *
 * Another angle of attack is STRATEGY a la Hoffstadter's iterated
 * prisoner's dilemma - run a range of strategies on the different
 * systems (as a population) - allow succesful strategies to
 * propagate and breed more than unsuccesful ones... this should cut down
 * the evolution time - i.e. really run the evolution in parallel (PDP)
 * instead of running one strategy for n steps (e.g. 3 days per
 * generation )
 * 
 * All this and more could be a good PhD
 */
/*
 * hosekeeping
 */
struct servent *svc;


/*

/*
 * Should do a GA rpc to send these to the central GA
 * Response should be the ratio of elapsed time to system time
 *
 * i.e. normalised response... note this doesnt take account of
 * different i/o characteristics - but may be a reasonable guesstimate
 */
recordweightsandresponse(RR,NR,weight,elapse,cmd)
ResourceRecord RR[];
int weight[4];
struct timeval elapse;
char *cmd;
{
/*
 * Here we cache result to be cast a bit later if a new cmd is typed,
 * or on timeout
 */
	dump(stderr, RR, NR);
	fprintf(stderr, "%s: %d %d %d %d => %d.%d secs\n", cmd,
		weight[0], weight[1], weight[2], weight[3],
			elapse.tv_sec, (elapse.tv_usec*1000));
}

/* 
 * Ditto here - we dont really want to do File IO
 * So we cast our own values, collect resposes, and
 * Apply Learning
 */
calcnewweights()
{
}

/*
 * A payoff function for a GA or something to operate on...
 */
payoff(response, weights)
{
}

/*
 * Init a UDP socket for later broadcast usage...
 */
employcast()
{
}

/*
 * Do a cast of thousands...
 *
 * Should record up to N answers (timing each one to gwet some notion
 * of network 'distance' which could be used as yet another metric
 * (YAM)
 */
performcast()
{
}

/*
 * Hear, if we had a characteristic of process/file mixture,
 * we might adjust weights accordingly ...
 *
 * this might be based on a linear combination of past job/weights
 * or might by a step function or...
 *
 * It should be noted that optimising TWO things at once ain't really
 * meaningful...blah de blah...
 * 
 */
linear(load1, load5, load15, users)
{
	return  weight[0] * users +	
		weight[1] * load1 +
		weight[2] * load5 +
		weight[3] * load15;
}

cmp(R1, R2)
ResourceRecord *R1, *R2;	
{
	int l1, l2;

	l1 = linear(R1->statp.avenrun[0],
			R1->statp.avenrun[1],
			R1->statp.avenrun[2],
			R1->nusers);

	l2 = linear(R2->statp.avenrun[0],
			R2->statp.avenrun[1],
			R2->statp.avenrun[2],
			R2->nusers);

	if (l1 < l2)
		return -1;
	else if (l1 > l2)
		return 1;
	else
		return 0;
}

/*
 * main line
 */
main (argc, argv)
char *argv[];
{
	int i;
	char *usage = "%s: [-t]\n";

	FindUser(&U);

	NR = FindHosts(&U, &RR); 
/* 
 * Note we could apply GA to host list itself...
 */
	svc = getservbyname("exec", "tcp");
	if (svc == (struct servent *)0) {
		printf("No rexec svc known\n");
		exit(-4);
	}

	if (argc > 2) {
		printf(usage, argv[0]);
		exit(-1);
	}
	if (argc > 1) 
		timing++;	

	glove();

	employcast();

/*
 * The art of the matter
 */
	for (;;) {
/* 
 * Here should not just choose 0t, but ith with exponentillay
 * decreasing prob of choice with i...
 */
		if (getanddocmd(&U, &RR[0]) == 0) {
			calcnewweights();
			dostat(RR, NR);
#ifdef DEBUG
			dump(stdout, RR, NR);
			sleep(5);
#endif
		}
	}
}

/*
 * A much better algorith is like ARP
 *
 * each time a user types a command
 *	for each machine we havnt heard from in the last CACHE time
 * 	ask it it's load and users etc, AND
 *	tell it OURs!!
 *
 * This ensures that the worst case is a lot of busy machines is
 * 1 msg per machine per CACHE time.
 *
 *
 * Here we get new weights if we need...
 * so that qsort/choice algy works from latest global view...
 *
 * at this popint we want everyone elses weights so we can calculate
 * a new generation for us...
 */
dostat(RR, NR)
ResourceRecord RR[];
{
	int i;

/*
 * This should be multicast if we had it
 *
 * but f'now we use sun-rpc rstat
 */
	for(i=0; i<NR; i++) {
		rstat (RR[i].hostname, &(RR[i].statp));
		RR[i].nusers = rnusers(RR[i].hostname);
	}
/*
 * Evolve....
 */
	qsort(RR, NR, SR, cmp);
}

/*
 * FYI :
 */
dump(fi, RR, NR)
FILE *fi;
ResourceRecord RR[];
{
	int i;

	for(i=0; i<NR; i++) {
		fprintf(fi, "On %s: %d users ", RR[i].hostname, RR[i].nusers);
#ifdef DEBUG
		fprintf(fi, "%d in %d out ", 
			RR[i].statp.if_ipackets, 
			RR[i].statp.if_opackets); 
#endif
		fprintf(fi, "%4.2f, %4.2f, %4.2f\n", 
			(double)RR[i].statp.avenrun[0]/FSCALE,
			(double)RR[i].statp.avenrun[1]/FSCALE,
			(double)RR[i].statp.avenrun[2]/FSCALE);
	}
}

/*
 * Login to the resource manager
 * rather simple minded for now
 */
FindUser(U)
User *U;
{
	struct sgttyb modes, new;
	setbuf(stdout, NULL);

	U->pwd = getpwuid(getuid());
	if (U->pwd == (struct passwd *)0) {
		printf("Dont know who we are\n");
		exit(-3);
	}	
	printf("\nLogin: %s\npasswd: ", U->pwd->pw_name);
	gtty(0, &modes);
	new = modes;
        new.sg_flags &= ~ECHO;
	stty(0, &new);
	gets(U->pass);
	stty(0, &modes);
	printf("\n");
	prompt();
}

/*
 * get this users preferred host list - ideally derived from
 * a poassword/account management system
 */
FindHosts(U, Rp)
User *U;
ResourceRecord **Rp;
{
	FILE *fp;
	ResourceRecord *R;
	int NR = 0;
	char rc[120], h[120];

	strcpy(rc, U->pwd->pw_dir);
	strcat(rc, "/.rman");

	fp = fopen(rc, "r");
	if (fp == (FILE *)0) {
		printf("No .rman file\n");
		exit(-1);
	}

	R = (ResourceRecord *)calloc(NR, SR);
	for(NR=1;;NR++) {
		R = (ResourceRecord *)realloc(R, NR*SR);
		if (R == (ResourceRecord *)0) {
			printf("rman: out of mem\n");
			exit(-1);
		}
		if (fgets(h, sizeof(h), fp) == NULL) {
			NR--;
			break;
		}
		h[strlen(h)-1] = '\0';
		R[NR-1].hostname = (char *)malloc(strlen(h)+1);
		if (R[NR-1].hostname == (char *)0) {
			printf("rman: out of mem\n");
			exit(-1);
		}
/*
 * XXX shouldcheck host exists
 */
		strcpy(R[NR-1].hostname, h);
	}
	fclose(fp);
	*Rp = R;
	return NR;
}
 
/*
 * As it says ...
 */
getanddocmd(U, R)
User *U;
ResourceRecord *R;
{
	char cmd[120];
	int r;
	if ((r = getcmd(cmd)) != 0) {
		docmd(U, R->hostname, cmd);
		prompt();
	}
	return r;
}

getcmd(b)
char *b;
{
	int r;
	int rfd;
	struct timeval to;

	rfd = (1 << 0);
/*
 * Here lies a polling policy for howoften we get the
 * stat information - not a good place you may say
 * and you would be right to think of putting this up near the 
 * actual stat info gathering code...
 */
	to.tv_sec = 5;
	to.tv_usec = 0;
	r = select(32, &rfd, (int *)0, (int *)0, &to);
	switch(r) {
	case 0: 
		return 0;
		break;
	case -1:
		if (errno == EINTR)
			return 0;
		perror("rman");
		printf("in getcmd\n");
		return 0;
		break;
	default:
		fgets(b, 120, stdin);
		return 1;
		break;
	}
}

/*
 * Use rexec to run cmd on remote host given by all the above
 */
docmd(u, h, b)
User *u;
char *h, *b;
{
	char host[120], ch;
	char *hp = host, **hpp = &hp;
	FILE *fp;
	int sd;

	startwatch();

	strcpy(host, h);

#ifdef DEBUG
	printf("h %s\nsvc %d\nnm %s\npwd %s\ncmd %s\n\n",
		host, svc->s_port,
		u->pwd->pw_name, u->pwd->pw_passwd, b);
#endif
/*
 * XXX here we should set the fd2p to a global so signals to rman can
 * be propagated to the program, and stderr can get back
 *
 * note rexex is a sun-rpc service too - should be something else if
 * we want to get back resource in the call too usage 
 */
	sd = rexec(hpp, svc->s_port, 
		u->pwd->pw_name, u->pass, b, 0);
	if (sd < 0) {
		perror("rman: rexec");
		printf("rexec failed\n");
		return;
	}
	fp = fdopen(sd, "r");
	if (fp == (FILE *)0) {
		printf("rediculous error in fdopen\n");
		exit(-99);
	}
	while ((ch = getc(fp)) != EOF)
		putchar(ch);

	stopwatch();
	if (timing)
		whattime();
	recordweightsandresponse(RR,NR,weight,elapse,b);
}

/*
 * SIG handlers for sintegrity
 */
dint()
{
	dump(stdout, RR,NR);
	signal(SIGINT, dint);
}

dont()
{
	dump(stdout, RR,NR);
	exit(0);
}

glove()
{
	signal(SIGINT, dint);
	signal(SIGQUIT, dont);
}


/*
 * Timing
 */
startwatch()
{
	gettimeofday(&then, (struct timezone *)0);
}

stopwatch()
{
	gettimeofday(&now, (struct timezone *)0);
	elapse.tv_sec = now.tv_sec - then.tv_sec;

	if ((elapse.tv_usec = now.tv_usec - then.tv_usec) < 0) {
		elapse.tv_sec--;
		elapse.tv_usec += 1000000;
	}
}

whattime()
{
	printf("\ntook: %d.%d\n", elapse.tv_sec, elapse.tv_usec);
}
