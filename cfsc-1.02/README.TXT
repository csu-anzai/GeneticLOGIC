                           CFS-C  /  LETSEQ1 / FSW1  

                       Version 1.02  /  March 1989
						(some notes added in 1991)

This file contains a description the CFS-C/LETSEQ1 and FSW1 distribution files.
The documentation for these systems include:

0. This file.

1. "CFS-C: A Package of Domain Independent Subroutines for Implementing Classifier
    Systems in Arbitrary, User-Defined Environments," January, 1986
    (hereafter called "the CFS-C manual").
    This is the original description. I have not yet updated it.

2. "LETSEQ: An Implementation of the CFS-C Classifier System in a Task-Domain
    that Involves Learning to Predict Letter Sequences," January, 1986
    (hereafter called "the LETSEQ manual").
    This is the original description. I have not yet updated it.

3. "CFS-C/FSW1: An Implementation of the CFS-C Classifier System in a Task-Domain
   that Involves Learning to Traverse a Finite State World,"  August , 1987
    (hereafter called "the FSW1 manual").
    This is labeled "DRAFT" but it is complete.

4.  The file INTRO.TXT on this disk, which:

    A. Briefly describes how to run the system;

    B. Describes changes made to the system since the "official"
       documentation was last updated; and

    C. Describes how to compile the CFS-C system and link it to
       a domain like LETSEQ1 or one of your own.

5.  The file VARS.TXT, on this disk, which briefly describes
    the most important CFS-C parameters.

6.  The file CHANGES.TXT, on this disk, which lists in detail all
    the changes made to the CFS-C system since the documentation
    was last updated. Compare the Version numbers listed in this
    file to the one in your distribution (in CFSC.C and in the
    startup message) to see if you have changes to make.

For general references I highly recommend the following, especially the latest
 ICGA conference proceedings as good places to start and look for references
 to follow up on:

 "Genetic Algorithms and Their Applications:
 Proceedings of the Second International Conference on Genetic Algorithms,"
 John Grefenstette (ed.), Lawrence Erlbaum Associates: Hillsdale NJ (1987).

 "Genetic Algorithms and Their Applications:
 Proceedings of the Third International Conference on Genetic Algorithms,"
 J.David Schaffer (ed.), Morgan Kaufmann (1989).

 "Genetic Algorithms and Their Applications:
 Proceedings of the Third International Conference on Genetic Algorithms,"
 R.Belew and L.Booker (eds.), Morgan Kaufmann (1991).

 "Induction" by John H. Holland, Keith J. Holyoak, Richard E. Nisbett,
 and Paul R. Thagard (MIT Press, 1986).  There are several chapters in
 that book that deal with classifier systems, and most of the rest of the
 book is relevant to learning classifier systems.

 "Genetic Algorithms in Search, Optimization and Machine Learning"
 by David E. Goldberg (Addison-Wesley, 1988).

 "Machine Learning" 1988 had two special issues with several articles
 on genetic algorithms and classifier systems. 
 There was also a special issue in 1990, and some other articles in 1991,
 and another special issue due out in 1992.

**************************

NOTE for folks who got this off the net:

The basic structure of the directories in the tar file are the same as
described below for the dos disk distribution.
The tar file also contains dvi files for the documentation
that I would normally send hardcopies of (the main CFSC description and the docuements
for letseq1 and fsw1 domains). You don't need a ps printer for this
documentation, but dvi->ps should work fine too.

There is a Makefile in the letseq1 directory, which makes the "core" classifier
system files (the *.c files in cfsc1.hc in the tar archive) and then
compiles the letseq1.c file and links them all together. 
You can use this to set up a make on your own system.

That Makefile also shows how to make getperf.c, one of the utitlies
for extracting information from the log files that letseq1 (or other
cfsc-systems) produce.  You can use that as a template for making
the other utilities (getstr1,c, getvars.c, getvc.c for the fsw1.c domain, etc.)

**************************

===> NOTE ====>

For a description of the classifier system I am working on now (1991),
see "Lookahead Planning and Latent Learning in a Classifier System", in
the book Simulated Adaptive Behavior, eds J.-A. Meyer and S. Wilson, MIT Press (1990).
That article describes what I consider to be the future of classifier systems,
at least those that work on tasks that are "sequential decision problems" or
some variant thereof (ie, not just Stimulus-Response classification problems).



**** NOTE WELL ************************************************************
*
* I have finally decided to just call this 1.0x and be done with it.
* There are still things that code be done to speed it up, clean up the
* code and comments, and so on.  In particular, the 
* "discovery" code (in the DSCLEARN.* and DSCOPS?.* files) is constantly
* being changed, to try various learning algorithms.  The code is not
* only unstable, it is somewhat uncommented and "kludgy." 
* I had hoped to clean it up soon and install some "official" algorithms.
* For example, I probably will install an SUS algorithm to select parents
* (see Baker's paper in the 1987 Genetic Algorithms Conf. Proceedings),
* and in general I want to clean up the Parent selection mechanism
* to make it clearer and more unified (to the extent that is possible,
* given that for different operators different selection algorithms are used).
* I may also implement a "uniform crossover" if the early results using it
* continue to look good.
*
* I would recommend you read the chapters in the "Induction" book on
* rule-generationg algorithms: there are lots of ideas there for more
* specific, triggered operators than I have so far implemented in the CFS-C
* system.  I think the more sophisticated rule-generating mechanisms will
* be required to get really interesting learning done in most domains.
* 
* Thus...I recommend you NOT take the "discovery" code in this package as
* written in stone or as "best" or even "good" algorithms.
* The genetic algorithm code in CFSC 1.0 has, frankly, become obsolete
* with all that has been learned about GA's in the few years since it
* CFSC 1.0 was mainly written (eg pre 1988).		
* Instead, you should experiment with your own implementations
* of these and other "rule discovery" operators and algorithms.
*
* I really do appreciate hearing about bugs, suggestions for improvements
* or additions to CFS-C.  I am quite willing to strongly consider
* inserting #if-tested code specifying modifications or additions that
* help support your particular needs, e.g., new operators, new ways to
* allocate strength, or even nice graphics and/or window-based interfaces
* (e.g., XWindows, NEWS, Mac Interface, etc.).
*
* If you do use the code please let me know--I like to hear
* about people who are interested in classifier systems and what
* domains they are working with. I will also be able to send you updates.
* And of course any news about experimental results (or problems)
* you get in your own task domains is VERY welcome.
* I am always glad to discuss issues and problems of
* interest to people working in this area---I usually learn
* as much as anyone from these interactions!
* Finally, I am open to invitations to visit your
* site for a few days for more intense discussions
* if that would be of interest or help to you;
* I must admit that if you live in Italy or France or some other
* interesting place I am more likely to accept such an
* invitation than if you live in New Jersey or Texas!
*
* Please send any bug reports, suggestions, requests for changes or
* special code for particular compilers/machines to:
*
*     Rick Riolo
*     905 Miller Ave.
*     Ann Arbor MI 48103
*
*     EMail: Rick_Riolo%um.cc.umich.edu
*            USERW280%umichum  (Bitnet)
*     Phone: (313) 769-0425 (at reasonable EST hours!)
*
***************************************************************************

The distribution disk should have the following files:

Root directory:
---------------
README.TXT
INTRO.TXT
VARS.TXT
CHANGES.TXT

CFSC Directory 
---------------
*.C          The source code for the CFS-C package.

*.BAT        Some batch files to compile/re-compile the CFS-C system
             (using Microsoft 4.0 compiler).

FILES.TXT    Summary of contents (and includes) for all CFS-C *.C files.

INCLUDE      A directory with the *.DEF, *.EXT, and *.H  source
             files required for compiling the CFS-C source files.


FSW1 Directory
--------------
FSW1.H      Source code files for the FSW1 domain. H contains constants,
FSW1.DEF    DEF contains global variable definitions, EXT contains 'extern'
FSW1.EXT    versions of the variables in the DEF file,
FSW1.C
FSW1A.C

GREF1.ENV   An environment-defining file for the FSW1 domain.
            This happens to define a world just like the one described by
            Grefenstette in his paper on hierarchical credit allocation
            in the Proceedings of the 2nd Intern. Conf. on GA  (1987).
GREF1.LD    A file of commands to load the GREF1.ENV by entering
                < gref1.ld
            at the FSW1/CFS-C prompt.  

X001.CM     Command files to the FSW1 system in the GREF1.ENV world.
X001.CM1,.. A typical way to run the FSW1/CFS-C system is:
                 fsw1 c:\cfsc\ <x001.cm1
            This starts a log file, sets the random number generator seed,
            and then reads commands from the file X001.CM to do a run.
            This assumes the INIT.CFS file (see INTRO.TXT) is in the
            directory c:\cfsc\ .

GETVC1.C    Two "glp" to extract performance counts (in terms of state
GETVC.C     "visit counts" from a log file created when running FSW1/CFS-C.
            Enter GETVC1 or GETVC to get futher information.
CONCNT-1.C  A "glp" to extract "macro state descriptions" of classifiers
            listed out into a log file during a FSW1/CFS-C run.
            Enter CONCNT-1 to get futher information.


LETSEQ1 Directory
-----------------
LETSEQ1.H   Source code files for the LETSEQ domain. H contains constants,
LETSEQ1.DEF DEF contains global variable definitions, and C contains 
LETSEQ1.C   the functions and subroutines.
LETSEQ1.EXE The compiled system, ready to run on an AT (80x86,x>=2) style machine
            under DOS 3.0+, without a math coprocessor.

MAKELET.BAT Some MS-DOS "batch" files that can be used to compile
            LETSEQ.C and link it to the CFS-C object files to produce an
            executable program (using Microsoft 5.0 and assuming the files 
            are in the appropriate directories).
MAKELET.LNK Commands to the MS linker, used by MAKELET.BAT .

LETSEQ1     MAKE command file for the MPW C make facility,
            which is similar to many others.

GETPERF.C   A "glp" that can be used to extract the
            performance data from a log file produced by the
            LETSEQ1/CFS-C program.  Enter GETPERF to get futher information.
GETPERF.EXE Compiled version of GETPERF.C, for DOS 3.0+, 80286 machines.

CCLS1.C     A "glp" that can be used to  "macro state descriptions"
            of classifiers listed out into a log file during a 
            LETSEQ1/CFS-C run. Enter CCLS1 to get futher information.

GETVARS.C   Three "glp's" to extract information about classifier
GETDVARS.C  strengths (GETSTR1) and activities (GETVARS) and about
GETSTR1.C   discovery operator activity (GETDVARS).
            To use GETSTR1, classifiers must be periodically displayed
            in a CFS-C log, and to use the other two, variables must
            be displayed in the log. Enter GETVARS, GETDVARS, or GETSTR1
            to get help about each.  Note that these are domain independent.

AB-BB.CM1   A sample command file for a simple non-discovery experiment.
AB-BB.CF    A set of 24 classifiers used by AB-BB.CM1.  
AB-BB.ENV   A problem for the 24 classifiers to "solve."
AB-BB.LG1   The log file produced by running the system
            on an ALR AT (with a 80287 math chip) as follows:

                letseq1 c:\cfsc\  <ab-bb.cm1

            This simple experiment should show how the bucket brigade
            works to allocate strength to classifiers that make correct
            guesses and from classifiers that make mistakes (or don't guess).

            (Note that the system will always look for the files INIT.MSG,
            INIT.CF, and INIT.ENV when it is started. If those files are not
            in the current directory, the system will print warning messages.
            This is not something to worry about---typically the environment
            and any initial classifiers are loaded by commands to the system,
            e.g., the LE and LC commands in the AB-BB.CM1 file.
            Also note again that this assumes the INIT.CFS file 
            (see INTRO.TXT) is in the directory c:\cfsc\ .)

ABEF.ENV    Defines a letter sequence environment (just abef...).
ABEF.CM     A sample "command" file for a simple experiment, invokable
ABEF.CM0,CM1,... by using these files, as described below, i.e.,
ABEF.LG0,LG1,... by running the system as follows:

                letseq1 c:\cfsc\ <abef.cm0
                letseq1 c:\cfsc\ <abef.cm1
                    ...
            (ABEF.LG? files not on disk---no room!)

ABEF.R      The results I get when I enter the above followed by:

                getperf abef.lg,5 g >abef.r

            Compare your results to these.  They may be a bit different
            (different number representations, etc.) but you should
            get similar results (especially averaged over a few runs).
