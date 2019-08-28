#include "ru.h"

/*
 * usage
 * 
 * ru user [machine]+ [-c cmd]
 */
 char *usage = "user [machine]+ [-c cmd]";

#define RCVALERT "/usr/ucl/lib/mmdf/rcvmail/rcvalert"


main(argc, argv)
char *argv[];
{
	char *user;
	int i;
	unsigned idle;
	unsigned bestidle = 0;
	char besthost[120];
	char *cmd = RCVALERT;
	u_short inport();

	if (argc < 3) {
		(void)fprintf(stderr, "%s %s\n", argv[0], usage);
		exit(-1);
	}

	user = argv[1];
	for(i=2; i < argc; i++) {
		if (strcmp(argv[i], "-c") == 0) {
			if (++i >= argc) {
				(void)fprintf(stderr, "Bad cmd arg\n");
				exit(-1);
			}
			cmd = argv[i];
			++i;
			continue;
		}
		if (rfind(user, argv[i], &idle)) {
			if (bestidle == 0) {
				bestidle = idle;
				(void)strcpy(besthost, argv[i]);
			}
			else if (idle < bestidle) {
				bestidle = idle;
				strcpy(besthost, argv[i]);
			}
		}
	}
	if (bestidle != -1) 
		doit(besthost, user, cmd);
	else
		fprintf(stderr, "No host found\n");
}

/*
 * rexec command
 *
 * Read back anything from command -
 * conversely could right to it if that what's wanted...
 */

doit(h, u, c)
char *h, *u, *c;
{
	int rem ;
	char ch;
	FILE *fp;

	rem = rexec(&h, inport(), u, NULL, c, (int *) 0);
	if (rem < 0) {
		(void)fprintf(stderr, "rexec failed\n");
		exit(-1);
	}
	if ((fp = fdopen(rem, "r")) == (FILE *)0) {
		(void)fprintf(stderr, "cannot open to remote cmd\n");
		exit(-1);
	}
	while ((ch = getc(fp)) != EOF)
		putchar(ch);
}


/*
 * Do the horrid things to  find users there
 * and filter for this user
 */
rfind(user, host, idle)
char *user, *host;
unsigned *idle;
{
	int i;
	struct utmpidlearr ut;
	struct utmpidle **up;

	up = (struct utmpidle **)calloc(MAXUSERS, sizeof(struct utmpidle));
	

	ut.uia_arr = up;
	ut.uia_cnt = MAXUSERS;

	if (rusers(host, &ut) != 0) {
		free(up);
		return 0;
	}

	for(i=0; i<ut.uia_cnt; i++) {
		if (!nonuser(ut.uia_arr[i]->ui_utmp) &&
			(strcmp(user, ut.uia_arr[i]->ui_utmp.ut_name) == 0)) {
			 	*idle = ut.uia_arr[i]->ui_idle;
				free(up);
				return -1;
			}
	}
	free(up);
	return 0;	
}

/*
 * Find the rexec port
 */
u_short
inport()
{
	struct servent *svc;
	svc = getservbyname("exec", "tcp");
        if (svc == (struct servent *)0) {
                (void)fprintf(stderr, "No rexec svc known\n");
                exit(-1);
	}
	return svc->s_port;
}
