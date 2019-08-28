
#include "extern.h"

LogStart()
{
	time_t clock;
	char msg[80];
	FILE *fp;

	Trace("LogStart entered");

	Log = (LOGSTRUCT *) calloc((unsigned) Processes, sizeof (LOGSTRUCT));
	if (Log == NULL)
		Error ("Input: memory allocation failed for Log");

	/* log this activation */
	if ((fp = fopen (Logfile, "a")) == NULL) {
		sprintf (msg, "Input: can't open %s", Logfile);
		IOError (msg);
	}
	if (Restartflag)
		fprintf (fp, "Experiment Restarted ");
	else 
		fprintf (fp, "Experiment Started   ");

	time (&clock);
	fprintf (fp, "%s", ctime (&clock));
	fclose (fp);

	Trace("LogStart completed");
}	

/*
LogRegister(clock, system)
time_t clock;
char *system;
{
*/