
/*  Classifier system:
	as found in Appendix D of "Genetic Algorithms in Search, Optimization,
	and Machine Learning" by David E. Goldberg.  Rewritten in C language by
	Erik Mayer   emayer@uoft02.utoledo.edu
*/


/* include files */
#include "declare.scs"
#include "io.scs"
#include "utility.scs"
#include "environ.scs"
#include "detector.scs"
#include "perform.scs"
#include "aoc.scs"
#include "effector.scs"
#include "reinforc.scs"
#include "timekeep.scs"
#include "advance.scs"
#include "ga.scs"
#include "report.scs"
#include "initial.scs"

main ()
{
    int iter;

    printf ("Enter random number seed (0-728): ");
    scanf ("%d", &seed);
    initialization ();
    detectors ();
    report ();
    for (iter = 0; iter <= timekeeprec.iterationsbrigade; ++iter) {
	timekeeper ();
	environment ();
	detectors ();
	matchclassifiers (envmessage);
	aoc ();
	effector ();
	reinforcement ();
	if (timekeeprec.reportflag)
	    report ();
	if (timekeeprec.consolereportflag)
	    consolereport ();
	if (timekeeprec.plotreportflag)
	    plotreport ();
	advance ();
	if (timekeeprec.gaflag) {
	    ga ();
	    if (timekeeprec.reportflag)
		reportga ();
	}
    }
    report ();
    fclose (pfile);
}
