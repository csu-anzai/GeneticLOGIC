/* main.c - driver for parsing and producing L-systems.
 *
 * Copyright (C) 1990, Jonathan P. Leech
 *
 * This software may be freely copied, modified, and redistributed,
 * provided that this copyright notice is preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is". Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 *
 * $Log:	main.c,v $
 * Revision 1.3  91/03/20  10:34:46  leech
 * Support for new generator. Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:24  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: main.c,v 1.3 91/03/20 10:34:46 leech Exp Locker: leech $";

#include <osfcn.h>
#ifndef __GNUG__
#include <libc.h>
#else
#include <std.h>
#endif /*__GNUG__*/
#include <new.h>
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>

#include "global_headers.h"
#include "vector.h"
#include "parser.h"
#include "interpret.h"
#include "PSGenerator.h"
#include "GenericGenerator.h"
#include "PPHIGSGenerator.h"
#include "BLFGenerator.h"

Symtab(Value)	 IgnoreTable,	    // modules to ignore in context matching
		 SymbolTable;	    // bound variables for expressions
List(Production) Rules;		    // productions
List(Module)	*Start;		    // initial string

int ParseDebug = 0;
extern int yyparse();

void out_of_memory() {
    cerr << "FATAL: no free memory!" << endl;
    exit(1);
}

static char *memstart = NULL;

// Accumulate running memory statistics (a debugging aid)
static void memstats(boolean do_stats, boolean first_call, char *label) {

    if (do_stats == false)
	return;

    if (label == NULL)
	label = "";

    if (first_call) {
	memstart = sbrk(0);
	cerr << "break value " << label << ": " << (void *)memstart << endl;
    } else {
	char *p = sbrk(0);
	cerr << "break value " << label << ": " << (void *) p
	     << " memory used: " << p - memstart << endl;
    }
}

// Return current memory use (assumes memstart has been set)
static size_t memuse() {
    char *p = sbrk(0);

    return p - memstart;
}

// Return a copy of a string stripped of its trailing
//  extension (.[^.]*), if any.
char *basename(char *s) {
    char *c = strdup(s),
	 *p = strrchr(c, '.');

    if (p != NULL)
	*p = '\0';

    return c;
}

// Concatenate two strings, returning the result in a dynamically
//  allocated buffer
char *concat(const char *p, const char *q) {
    int len = strlen(p) + strlen(q);
    char *s = new char[len+1];

    strcpy(s,p);
    strcat(s,q);

    return s;
}

main(int argc, char *argv[])
{
#ifdef M_MXFAST
    // Enable fast small block allocation
    mallopt(M_MXFAST, 32);
#endif

    // Enable memory allocator logging, if compiled in
    memlog_enable(true);

    int   maxgen = -1;		    // Default # of generations to produce
    float width  = -1;		    // Default line width
    float delta  = -90;		    // Default turn angle

    boolean autoscale = true;	    // Automatic output scaling?
    boolean display = false;	    // Display module list at each generation?
    boolean generic = false;	    // Generic output format?
    boolean pphigs = false;	    // PPHIGS output format?
    boolean stats = false;	    // Compute statistics
    enum FileType {
	PostScript,
	Generic,
	PPHIGS,
	BLF
    }	    filetype = PostScript;  // Output format
    static
    char   *extension[] = {	    // Default file extensions for output formats
	".ps",
	".generic",
	".pphigs",
	".blf"
    };
    char   *ofile = "output";	    // Default output file name

    // Camera parameters
    Vector eye(0,0,1),
	   lookat(0,0,0),
	   up(0,1,0);
    float  fov = 0;		    // fov out of bounds -> orthographic projection

    // Initialize random number generator

    srand48(time(NULL));

    for (int i = 1; i < argc; i++) {
	if (!strncmp(argv[i], "-maxgen", 2)) {
	    maxgen = atoi(argv[++i]);
	} else if (!strcmp(argv[i], "-stats")) {
	    stats = true;
	} else if (!strncmp(argv[i], "-width", 2)) {
	    width = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-display")) {
	    display = true;
	} else if (!strncmp(argv[i], "-delta", 2)) {
	    // Note this must follow -display
	    delta = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-eye")) {
	    eye[0] = atof(argv[++i]);
	    eye[1] = atof(argv[++i]);
	    eye[2] = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-lookat")) {
	    lookat[0] = atof(argv[++i]);
	    lookat[1] = atof(argv[++i]);
	    lookat[2] = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-up")) {
	    up[0] = atof(argv[++i]);
	    up[1] = atof(argv[++i]);
	    up[2] = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-fov")) {
	    fov = atof(argv[++i]);

	    if (fov <= 0 || fov >= 180) {
		cerr << "Illegal field of view " << fov << " specified (must be between 0 and 180)\n";
		exit(1);
	    }
	} else if (!strcmp(argv[i], "-noscale")) {
	    autoscale = false;
	} else if (!strcmp(argv[i], "-D")) {
	    ParseDebug = 0;
	    for (char *p = argv[++i]; *p; p++) {
		switch (*p) {
		    case 'A': ParseDebug = -1;
		    case 'E': ParseDebug |= PD_EXPRESSION; break;
		    case 'G': ParseDebug |= PD_PARSER;	   break;
		    case 'I': ParseDebug |= PD_INTERPRET;  break;
		    case 'L': ParseDebug |= PD_LEXER;	   break;
		    case 'M': ParseDebug |= PD_MODULE;	   break;
		    case 'N': ParseDebug |= PD_NAME;	   break;
		    case 'P': ParseDebug |= PD_PRODUCTION; break;
		    case 'S': ParseDebug |= PD_SYMBOL;	   break;
		    case 'l': ParseDebug |= PD_MEMLEAK;    break;
		    case 'm': ParseDebug |= PD_MAIN;	   break;
		    default:  break;
		}
	    }
	} else if (!strcmp(argv[i], "-generic")) {
	    filetype = Generic;
	} else if (!strcmp(argv[i], "-pphigs")) {
	    filetype = PPHIGS;
	} else if (!strcmp(argv[i], "-blf")) {
	    filetype = BLF;
	} else if (!strcmp(argv[i], "-o")) {
	    ofile = argv[++i];
	} else {
	    if (strcmp(argv[i], "-help"))
		cerr << "Unrecognized option " << argv[i] << endl;;

	    cerr << "Usage: " << argv[0] << " [-maxgen n] [-display]] [-delta angle] [-width w]\n"
		 << "\t\t[-eye x y z] [-lookat x y z] [-up x y z] [-fov theta]\n"
		 << "\t\t[-generic] [-pphigs] [-blf] [-noscale] [-o file] [-D [lmEGILMNPSA]\n"
		 << "-maxgen  sets number of generations to produce\n"
		 << "-display displays L-systems produced at each generation\n"
		 << "-delta   sets default turn angle\n"
		 << "-width   sets default line width\n"
		 << "-eye     sets the eye position, default (0,0,1)\n"
		 << "-lookat  sets the look-at position, default (0,0,0)\n"
		 << "-up      sets the up vector, default (0,1,0)\n"
		 << "-fov     sets the field of view (default orthographic projection)\n"
		 << "-generic generates a generic database format\n"
		 << "-pphigs  generates a PPHIGS database\n"
		 << "-blf     generates a .BLF (Blinn-Like Format) database\n"
		 << "-noscale suppresses automatic output scaling\n"
		 << "-o       specifies output file (default 'output.ps')\n"
		 << "-D enables debugging statements for any of:\n"
		 << "    l - memory allocation\n"
		 << "    m - main program loop\n"
		 << "    E - Expressions\n"
		 << "    G - Parsing\n"
		 << "    I - Interpretation\n"
		 << "    L - Lexical scanning\n"
		 << "    M - Modules\n"
		 << "    N - Hashed names\n"
		 << "    P - Productions\n"
		 << "    S - Symbols\n"
		 << "    A - all of the above" << endl;
	    exit(1);
	}
    }

    set_new_handler(out_of_memory);

    // Build the output file name; if the specified name has no extension,
    //	add one based on the output format
    if (strchr(ofile, '.') == NULL)
	ofile = concat(ofile, extension[filetype]);

    // Open output file before applying (potentially lengthy) productions
    ofstream *f = new ofstream(ofile);
    if (f->bad()) {
	cerr << "Error opening output file " << ofile << ", aborting" << endl;
	exit(1);
    }

    memstats(stats, true, "before parse");

    // Parse standard input
    yyparse();
    memstats(stats, false, "after parse");

    if (Start) {
	PDEBUG(PD_MAIN, cerr << "Starting module list: " << *Start << endl);
    } else {
	PDEBUG(PD_MAIN, cerr << "No starting module list" << endl);
    }

    PDEBUG(PD_SYMBOL, cerr << "\nSymbol Table:\n" << SymbolTable);

    PDEBUG(PD_PRODUCTION, cerr << "\nProductions:\n" << Rules << endl);

    if (!Start) {
	cerr << "No starting point found!" << endl;
	exit(1);
    }

    // Look up defaults in the symbol table, if not overridden by command
    //	line arguments. The values looked up are:
    // maxgen	- number of generations to produce (default 0)
    // width	- line aspect ratio (unit line is width units wide, default 1/100)
    // delta	- turn angle (default 90 degrees == right angle)
    Value v;
    if (maxgen < 0) {
	if (SymbolTable.lookup("maxgen", v)) {
	    if (v.value(i))
		maxgen = i;
	    else {
		cerr << "Invalid value specified for maxgen: " << v << endl;
		exit(1);
	    }
	} else
	    maxgen = 0; // Default: just do sanity checking
    }
    if (delta < 0) {
	if (SymbolTable.lookup("delta", v)) {
	    if (!v.value(delta)) {
		cerr << "Invalid value specified for delta: " << v << endl;
		exit(1);
	    }
	} else
	    delta = 90; // Default: turn at right angles
    }
    if (width < 0) {
	if (SymbolTable.lookup("width", v)) {
	    if (!v.value(width)) {
		cerr << "Invalid value specified for width: " << v << endl;
		exit(1);
	    }
	} else
	    width = 1;	// Default line aspect ratio (1/100)
    }

    // Temporary horrible kludge to enable interpretation of AIKL modules by
    //	'#define plant <anything>'
    boolean plant = false;
    if (SymbolTable.lookup("plant", v))
	plant = true;

    Camera view(view_matrix(eye, lookat, up), fov);
    PDEBUG(PD_MAIN, cerr << "Viewing transform is:\n" << view);

    if (display)
	cout << "gen0 : " << *Start << endl;

    // For each generation, apply appropriate productions
    //	in parallel to all modules.
    ListIterator(Production) pi(Rules);

    List(Module) *old_ml = Start;
    for (int gen = 1; gen <= maxgen; gen++) {
	ListIterator(Module) mi(*old_ml);
	Module		    *m;
	List(Module)	    *new_ml = new List(Module);

	PDEBUG(PD_MEMLEAK, cerr << "\tnew ModuleList @ " << (void *)new_ml << '\n');
	// For each input module
	for (m = mi.first(); m; m = mi.next()) {
	    PDEBUG(PD_PRODUCTION, cerr << "Searching for matching production to " << *m << endl);

	    // Find a matching production
	    // This could be optimized a bunch.
	    Production *p;

	    for (p = pi.first(); p; p = pi.next()) {
		if (p->matches(mi, m, SymbolTable)) {
		    PDEBUG(PD_PRODUCTION, cerr << "\tmatched by: " << *p << endl);
		    break;
		}
	    }

	    // If we found one, replace the module by its successor
	    if (p) {
		List(Module) *result = p->produce(m, SymbolTable);
		PDEBUG(PD_MEMLEAK, cerr << "\t@ " << (void *)result << ':');
		PDEBUG(PD_PRODUCTION, cerr << "\tapplied production yielding: " << *result << endl);

		new_ml->append(result);
		PDEBUG(PD_MEMLEAK, cerr << "deleting production result <- " << (void *)result << endl);
		delete result;
	    } else {
		PDEBUG(PD_PRODUCTION, cerr << "\tno match found, passing production unchanged\n");
		new_ml->append(new Module(*m));
		m->empty();
	    }
	}

	if (display)
	    cout << "gen" << gen << ": " << *new_ml << endl;

	if (stats) {
	    cerr << "Gen" << setw(3) << gen << ": # modules = "
		 << setw(5) << new_ml->size() << ' ';
	    memstats(stats, false, NULL);
	}

	PDEBUG(PD_MEMLEAK, cerr << "deleting old module list <- " << (void *)old_ml << endl);
	delete old_ml;
	old_ml = new_ml;
    }

    // Construct an output generator and apply it to the final module
    //	list to build a database.
    DBGenerator *g;
    switch (filetype) {
	case PPHIGS:
	    cerr << "Generating PPHIGS in " << ofile << endl;
	    g = new PPHIGSGenerator(f, view);
	    break;
	case Generic:
	    cerr << "Generating generic database in " << ofile << endl;
	    g = new GenericGenerator(f, view);
	    break;
	case BLF:
	    cerr << "Generating BLF database in " << ofile << endl;
	    g = new BLFGenerator(f, view);
	    break;
	default:
	    cerr << "Generating PostScript in " << ofile << endl;
	    g = new PSGenerator(f, view, autoscale);
	    break;
    }

    g->set_name(basename(ofile));

    interpret(*old_ml, *g, plant, delta, width);
    delete f;
    delete g;

    // Generate final memory and time statistics
    memstats(stats, false, "after interpretation");

    if (stats) {
	struct tms t;

	if (times(&t) < 0)
	    cerr << "Cannot get process times\n";
	else {
#ifndef HZ
#define HZ 60
#endif
	    float seconds = (t.tms_utime + t.tms_stime) / (float)HZ;
	    seconds = ((int)(seconds * 100)) / 100.0;
	    cerr << "CPU time: " << setw(8) << setiosflags(ios::fixed)
		 << setprecision(2) << seconds
		 << " memory: " << setw(8) << memuse() << '\n';
	}
    }

    memlog_enable(false);
    exit(0);
}
