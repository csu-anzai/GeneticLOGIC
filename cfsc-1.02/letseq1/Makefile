#
# Makefile for letseq1 program
#
# You will have to change some paths here.
# I have made all the compiles explicit and just made everything
# depend on all the H,DEF,EXT files, just to be safe.
#
# Check $(INCLUDE)/compiler.h for the correct compiler #define,
# and $(INCLUDE)/core.h for message length, message and classifier list sizes, etc.
#
# NOTE WELL: if you are using the same domain independent modules (from $(INCLUDE)/*.c)
# and you change any of those and make this file, then to get those same "core"
# changes to take affect in some other domain (eg fsw1), be sure to re-make that too.
#
#
# This also includes commands to make getperf.c into the utility to process 
# letseq1 logs into performance tables (see distribution *.TXT files).
#

CFLAGS = -c -O2

CFSC = /usr/users/rlr/cfsc1.hc
INCLUDE = $(CFSC)/include

DEFS = $(INCLUDE)/core.def $(INCLUDE)/cfops.def $(INCLUDE)/dsclearn.def \
		$(INCLUDE)/cfsio.def $(INCLUDE)/usercmd.def

EXTS = $(INCLUDE)/core.ext $(INCLUDE)/cfops.ext $(INCLUDE)/dsclearn.ext \
		$(INCLUDE)/cfsio.ext $(INCLUDE)/usercmd.ext

HS = $(INCLUDE)/core.h $(INCLUDE)/cfops.h $(INCLUDE)/dsclearn.h \
		$(INCLUDE)/cfsio.h $(INCLUDE)/usercmd.h \
		$(INCLUDE)/utility.h $(INCLUDE)/compiler.h

CFSCOBJS = $(CFSC)/bblearn.o $(CFSC)/cfsc.o $(CFSC)/core.o $(CFSC)/dsclearn.o $(CFSC)/utility.o \
		$(CFSC)/cfmsgio.o $(CFSC)/cfsio.o $(CFSC)/display.o $(CFSC)/dscops2.o $(CFSC)/usercmd.o \
		$(CFSC)/cfops.o $(CFSC)/cfsutil.o $(CFSC)/dscops.o $(CFSC)/grandcfs.o $(CFSC)/cfsrand.o
 
#
# the stuff specific to letseq1, and then the linking.
#

letseq1: letseq1.o $(CFSCOBJS) Makefile
	cc -o letseq1 letseq1.o $(CFSCOBJS) -lm

letseq1.o:	letseq1.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o letseq1.o letseq1.c

#
# for getperf.c -> getperf
#

getperf: getperf.c $(CFSC)/cfsio.o $(CFSC)/utility.o
	cc -I$(INCLUDE) $(CFLAGS) -o getperf.o getperf.c
	cc -o getperf getperf.o  $(CFSC)/cfsio.o $(CFSC)/utility.o -lm
	rm getperf.o

#
#  all the domain independent files; you need these for all systems.
#

$(CFSC)/bblearn.o:	$(CFSC)/bblearn.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/bblearn.o $(CFSC)/bblearn.c

$(CFSC)/cfmsgio.o:	$(CFSC)/cfmsgio.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/cfmsgio.o $(CFSC)/cfmsgio.c

$(CFSC)/cfops.o:	$(CFSC)/cfops.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/cfops.o $(CFSC)/cfops.c

$(CFSC)/cfsio.o:	$(CFSC)/cfsio.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/cfsio.o $(CFSC)/cfsio.c

$(CFSC)/cfsutil.o:	$(CFSC)/cfsutil.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/cfsutil.o $(CFSC)/cfsutil.c

$(CFSC)/core.o:	$(CFSC)/core.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/core.o $(CFSC)/core.c

$(CFSC)/display.o:	$(CFSC)/display.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/display.o $(CFSC)/display.c

$(CFSC)/grandcfs.o:	$(CFSC)/grandcfs.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/grandcfs.o $(CFSC)/grandcfs.c

$(CFSC)/cfsrand.o:	$(CFSC)/cfsrand.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/cfsrand.o $(CFSC)/cfsrand.c

$(CFSC)/usercmd.o:	$(CFSC)/usercmd.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/usercmd.o $(CFSC)/usercmd.c

$(CFSC)/utility.o:	$(CFSC)/utility.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/utility.o $(CFSC)/utility.c

$(CFSC)/cfsc.o:	$(CFSC)/cfsc.c $(DEFS) $(EXTS) $(HS)
	cc -I$(INCLUDE) $(CFLAGS) -o $(CFSC)/cfsc.o $(CFSC)/cfsc.c

$(CFSC)/dsclearn.o:	$(CFSC)/dsclearn.c $(DEFS) $(EXTS) $(HS)
	cc $(CFLAGS) -I$(INCLUDE) -o $(CFSC)/dsclearn.o $(CFSC)/dsclearn.c

$(CFSC)/dscops.o:	$(CFSC)/dscops.c $(DEFS) $(EXTS) $(HS)
	cc $(CFLAGS) -I$(INCLUDE) -o $(CFSC)/dscops.o $(CFSC)/dscops.c

$(CFSC)/dscops2.o:	$(CFSC)/dscops2.c $(DEFS) $(EXTS) $(HS)
	cc $(CFLAGS) -I$(INCLUDE) -o $(CFSC)/dscops2.o $(CFSC)/dscops2.c
