# $Id: Makefile,v 1.21 1994/05/01 10:38:48 holger Exp holger $

# PARATSP 1.0 - Linux

# -DUSE_PROTO
# -DTRACE_1 -DTRACE_2 -DTRACE_3
# -DUSE_CURSES
# -DDEBUG
# -DLINUX -DSUN_UNIX -DAIX -DPARIX -DPARIX_HOST
DEFINES=-DUSE_PROTO -DTRACE_1 -DUSE_CURSES -DLINUX

# macros for C-compiler
CC=gcc
CFLAGS=-O -Wall -fwritable-strings $(DEFINES) #-g
CPP=$(CC) -E $(DEFINES)
LIBS=-lm -lcurses -ltermcap -lfl
SLIBS=
RLIBS=-lcurses
XLIBS=-lX11
SALIBS=-lm -lcurses -ltermcap -lfl
MFLAGS=#-s
MAKE=make $(MFLAGS)

# executable programs
EXECFLAG=-o
PARATSP=paratsp
SETUP=setup
REPORT=report
XTSP=xtsp
SATSP=satsp

# commands
RM=rm -f
CP=cp
MV=mv
STRIP=strip

# directories
SRCDIR=.
BAKDIR=./BAK

# lex & yacc
YACC=bison
LEX=flex
YFLAGS=-d -l -y
LFLAGS=-L
YACC_OUT=y.tab
LEX_OUT=lex.yy

# object modules
OBJS=	best.o\
	bit.o\
	boltzman.o\
	brselect.o\
	chkpoint.o\
	chrom.o\
	commpop.o\
	control.o\
	converge.o\
	cross.o\
	dbuff.o\
	done.o\
	dump.o\
	elite.o\
	error.o\
	eval.o\
	filtwins.o\
	fitscale.o\
	fopen.o\
	generate.o\
	getopt.o\
	global.o\
	graphics.o\
	init.o\
	input.o\
	interfac.o\
	logit.o\
	maselect.o\
	mate.o\
	measure.o\
	mutation.o\
	normal.o\
	optlocal.o\
	other.o\
	parallel.o\
	popinit.o\
	random.o\
	readfile.o\
	readpars.o\
	readscan.o\
	replace.o\
	schema.o\
	trace.o

POBJS=	main.o\
	$(OBJS)

SOBJS=	setup.o\
	fopen.o\
	getopt.o\
	global.o\
	interfac.o\
	trace.o

ROBJS=	report.o\
	error.o\
	fopen.o\
	getopt.o\
	global.o\
	interfac.o\
	trace.o

XOBJS=	xtsp.o\
	fopen.o\
	global.o\
	graphics.o\
	trace.o

SAOBJS= satsp.o\
	best.o\
	error.o\
	eval.o\
	fitscale.o\
	fopen.o\
	getopt.o\
	global.o\
	interfac.o\
	logit.o\
	other.o\
	parallel.o\
	random.o\
	readfile.o\
	readpars.o\
	readscan.o\
	trace.o

# actions
all: parser progs

progs: paratsp setup report xtsp satsp

paratsp: $(POBJS)
	$(CC) $(CFLAGS) $(POBJS) $(EXECFLAG) $(PARATSP) $(LIBS)

setup: $(SOBJS)
	$(CC) $(CFLAGS) $(SOBJS) $(EXECFLAG) $(SETUP) $(SLIBS)

report: $(ROBJS)
	$(CC) $(CFLAGS) $(ROBJS) $(EXECFLAG) $(REPORT) $(RLIBS)

xtsp: $(XOBJS)
	$(CC) $(CFLAGS) $(XOBJS) $(EXECFLAG) $(XTSP) $(XLIBS)

satsp: $(SAOBJS)
	$(CC) $(CFLAGS) $(SAOBJS) $(EXECFLAG) $(SATSP) $(SALIBS)

yacc: $(YACC_OUT).c

lex: $(LEX_OUT).c

parser: yacc lex

super:
	$(MAKE) parser
	$(MAKE) depend
	$(MAKE) progs

clean:
	$(RM) $(POBJS) $(SOBJS) $(ROBJS) $(XOBJS) $(SAOBJS)
	$(RM) $(PARATSP) $(SETUP) $(REPORT) $(XTSP) $(SATSP)

cleanall: clean
	$(RM) $(YACC_OUT).c $(YACC_OUT).h $(LEX_OUT).c

cleanpars:
	$(RM) readpars.[ch] readscan.c

cleansuper: cleanall cleanpars

strip:
	$(STRIP) $(PARATSP)
	$(STRIP) $(SETUP)
	$(STRIP) $(REPORT)
	$(STRIP) $(XTSP)
	$(STRIP) $(SATSP)

new: clean progs

newall: cleanall all

newsuper: cleansuper super

rcs:
	ci -l -f -mParaTSP_Version *.[chly] [Mm]akefile*

links: cleanlinks
	ln -s error.c h_error.c
	ln -s fopen.c h_fopen.c
	ln -s getopt.c h_getopt.c
	ln -s global.c h_global.c
	ln -s graphics.c h_graphics.c
	ln -s interfac.c h_interfac.c
	ln -s trace.c h_trace.c

cleanlinks:
	$(RM) h_*.c

bak: cleanbak
	$(CP) $(SRCDIR)/*.[chly] $(BAKDIR)
	$(CP) $(SRCDIR)/*.new $(BAKDIR)
	$(CP) $(SRCDIR)/[Mm]akefile* $(BAKDIR)
	$(CP) $(SRCDIR)/.depend $(BAKDIR)
	cd $(BAKDIR); $(MAKE) taz

taz:
	tar cvf paratsp.tar * .depend
	compress paratsp.tar
	$(MV) paratsp.tar.Z paratsp.taz

cleanbak:
	$(RM) $(BAKDIR)/*
	$(RM) $(BAKDIR)/.depend

depend dep:
	for i in *.c;do echo -n "";$(CPP) -MM $$i;done > .depend~
	$(MV) .depend~ .depend

# depends
$(YACC_OUT).c $(YACC_OUT).h: readpars.y
	$(YACC) $(YFLAGS) readpars.y
	$(CP) $(YACC_OUT).c readpars.c
	$(CP) $(YACC_OUT).h readpars.h

$(LEX_OUT).c: readscan.l
	$(LEX) $(LFLAGS) readscan.l
	$(CP) $(LEX_OUT).c readscan.c

include .depend

