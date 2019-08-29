#include "makefile.h"

EXECUTABLES = pw

PWBINARY = pw.o

#ifdef ATT_IRIX3.2
MISCBINARIES = dumpload.o misc.o error.o indexlist.o debug.o kluge.o
#else
MISCBINARIES = dumpload.o misc.o error.o indexlist.o debug.o
#endif

GBINARIES = gmisc.o gobject.o gcamera.o glight.o gstage.o gscene.o gwindow.o

SBINARIES = critter.o interact.o food.o barrier.o

MYLIBS = $(MISCBINARIES) $(GBINARIES) $(SBINARIES)
LIBS = -lgl_s -lbsd -lm -lmalloc -lc

CFILES = pw.C

MISCFILES = dumpload.C misc.C error.C indexlist.C debug.C

GFILES = gmisc.C gobject.C gcamera.C glight.C gstage.C gscene.C gwindow.C	

SFILES = critter.C interact.C food.C barrier.C

.PRECIOUS:  $(CFILES) $(MISCFILES) $(GFILES) $(SFILES)\
 $(EXECUTABLES) $(PWBINARY) $(MISCBINARIES) $(GBINARIES) $(SBINARIES)

default:	pw

include dependfile

careful:
	@ echo "subsequent makes will use the full list of dependencies"
	@ cp dependencies dependfile

normal:
	@ echo "subsequent makes will use only the dependencies in the makefile"
	@ echo > dependfile

yucko:	yucko.o
	$(CC) -g $(CFLAGS) yucko.o -o yucko $(MYLIBS) $(LIBS)

#ifdef ATT_IRIX3.2
kluge.o:  $(KLUGEFILE) $(GLFILE)
	cc -c $(KLUGEFILE)

kluge:	kluge.o
#endif

all:		$(EXECUTABLES) $(GBINARIES) $(MISCBINARIES) $(SBINARIES)

misc:		$(MISCBINARIES)

graphics:	$(GBINARIES)

simulation:	$(SBINARIES)

pw.o:		newdelete.C

debug:		debug.o
debug.o:	debug.h

dumpload:	dumpload.o

error:		error.o
error.o:	error.h

gmisc:		gmisc.o
gmisc.o:	gmisc.h

gobject:	gobject.o
gobject.o:	gobject.h

gcamera:	gcamera.o
gcamera.o:	gcamera.h

glight:		glight.o
glight.o:	glight.h

gstage:		gstage.o
gstage.o:	gstage.h

gscene:		gscene.o
gscene.o:	gscene.h

gwindow:	gwindow.o
gwindow.o:	gwindow.h

critter:	critter.o
critter.o:	critter.h

food:		food.o
food.o:		food.h

barrier:	barrier.o
barrier.o:	barrier.h

interact:	interact.o

pw:	$(PWBINARY) $(MISCBINARIES) $(GBINARIES) $(SBINARIES)
	$(CC) $(LFLAGS) -o pw pw.o $(MYLIBS) $(LIBS)

DEPENDFILES = $(CFILES) $(GFILES) $(MISCFILES) $(SFILES) $(LFILES) $(YFILES)
depend:
	@ echo making dependencies
	@ cc -M $(cFLAGS) $(DEPENDFILES) > dependencies
	cp dependencies dependfile
