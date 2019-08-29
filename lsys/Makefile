# Makefile for L-system generator and interpreter.
#
# $Id: Makefile,v 1.3 91/03/20 21:37:18 leech Exp Locker: leech $
#
# Copyright (C) 1990, Jonathan P. Leech
#
# This software may be freely copied, modified, and redistributed,
# provided that this copyright notice is preserved on all copies.
#
# There is no warranty or other guarantee of fitness for this software,
# it is provided solely "as is". Bug reports or fixes may be sent
# to the author, who may or may not act on them as he desires.
#
# You may not include this software in a program or other software product
# without supplying the source, or without informing the end-user that the
# source is available for no extra charge.
#
# If you modify this software, you should include a notice giving the
# name of the person performing the modification, the date of modification,
# and the reason for such modification.
#

###############################################################################
# Compilation tools, flags to tools, and libraries

# Compiler and parsing tools; choose one set, either standard Unix
#   or GNU tools. The following symbols must be defined:
# CC OLDC YACC LEX PARSELIBS INCLUDEDIR

# Unix tools
#CC     = CC
#OLDC   = cc
#YACC   = yacc
#LEX    = lex
#PARSELIBS = -ly -ll
# Headers in /usr/include/CC for AT&T C++, /usr/CC/incl for Sun C++.
#INCLUDEDIR = /usr/include/CC
#INCLUDEDIR = /usr/CC/incl

# GNU tools
# No lex/yacc libraries are needed.
CC      = g++
OLDC    = gcc
YACC    = bison -y
LEX     = flex
PARSELIBS =
# Headers for g++ will undoubtedly be somewhere else on your system
INCLUDEDIR = /usr/softlab/contrib/lib/mips_ultrix/g++-1.39-include

# Other libraries
# Ultrix should use -lmalloc to get a version of malloc supporting mallopt()
#MEMLIB = -lmalloc
MEMLIB  =

# All libraries to link.
LIBS = $(PARSELIBS) $(MEMLIB) -lm

# Compilation flags.
# Normally DB=-O to enable optimization.
# Set DB=-g for debugging, PROF=-pg for profiling
DB = -O
CFLAGS = $(INCLUDE) $(DEFS) $(DB) $(PROF)

# Minor speed enhancements will result from removing this line, but
#  builtin debugging printout will be compiled out.
DEFS    = -DPDEBUG_ENABLED

# Diff flags
DFLAGS  = -wt

###############################################################################
# Lists of programs, source files, object files, headers, etc.

PROGS = lsys sizes bug

# Sources not generated automagically
SRCS =  debug.c main.c \
	List.c Symtab.c \
	Expression.c Module.c Production.c Value.c \
	actions.c interpret.c \
	Turtle.c vector.c \
	DBGenerator.c PSGenerator.c GenericGenerator.c Polygon.c \
	Name.c PPHIGSGenerator.c BLFGenerator.c \
	sizes.c bug.c

# Headers not generated automagically
HEADERS = \
	DBGenerator.h Expression.h GenericGenerator.h List.h Module.h \
	PSGenerator.h Polygon.h Production.h Symtab.h Turtle.h Value.h \
	actions.h boolean.h debug.h global_headers.h interpret.h \
	lexdefs.h parser.h vector.h \
	Name.h PPHIGSGenerator.h BLFGenerator.h streamio.h

# Files in RCS directory to run diff on
DIFFSRCS = lsys.y lex.l $(SRCS) $(HEADERS) lsys.l Makefile

# Source files to 'make depend' on
DEPENDSRCS = parser.c lexer.c $(SRCS)

OBJS =  parser.o lexer.o debug.o \
	List.o Symtab.o \
	Expression.o Module.o Production.o Value.o \
	actions.o interpret.o \
	Turtle.o vector.o \
	DBGenerator.o PSGenerator.o GenericGenerator.o Polygon.o \
	Name.o PPHIGSGenerator.o BLFGenerator.o

###############################################################################
# Code targets

# Target for the main program
lsys: main.o $(OBJS)
	$(CC) $(CFLAGS) main.o $(OBJS) $(LIBS) -o lsys

# Test program to help trace memory usage problems with g++
sizes: sizes.o $(OBJS)
	$(CC) $(CFLAGS) sizes.o $(OBJS) $(LIBS) -lm -o $@

# Test program to determine if a bug in g++ operator delete() is present
bug: bug.o
	$(CC) $(CFLAGS) bug.o -o $@

# Headers from automagically generated files don't work properly
#  with 'make depend'
lexer.o Expression.o Value.o: token.h

token.h: parser.o

parser.o: parser.c lsys.y

parser.c: lsys.y
	$(YACC) -d lsys.y
	egrep '#.*define' y.tab.h > token.h
	egrep -v '#.*line' y.tab.c > parser.c
	-rm y.tab.c

# lexer is compiled with old-C due to compatibility problems
lexer.o: lex.l lexdefs.h token.h
	$(OLDC) $(CFLAGS) -c lexer.c

lexer.c: lex.l lexdefs.h
	$(LEX) lex.l
	mv lex.yy.c lexer.c

###############################################################################
# Non-code targets

clean:
	-rm -f $(PROGS) main.o sizes.o $(OBJS) token.h parser.c y.tab.[ch] lexer.c y.output

diffs: $(DIFFSRCS)
	rcsdiff $(DFLAGS) $(DIFFSRCS) > diffs 2>&1

# Target to build a distribution in the subdirectory 'distribution'
# Builds from the RCS files.
DIST = distribution
NONRCS_FILES = MANIFEST README README.EXAMPLES README.OUTPUT make.att make.gnu
dist:
	-mkdir $(DIST) ; rm -f $(DIST)/*
	cp $(NONRCS_FILES) $(DIST)
	cd $(DIST) && co ../RCS/*,v && nroff -man lsys.l > lsys.cat
	-mkdir $(DIST)/Examples
	cp Examples/* $(DIST)/Examples

###############################################################################
# Dependencies are automatically generated by the 'makedepend' program
#  available under Ultrix. This may not be available on some systems.
depend: parser.c lexer.c token.h
	cp Makefile Makefile.bak
	makedepend -- -I$(INCLUDEDIR) -- $(DEPENDSRCS)
	expand < Makefile | \
	/bin/sed -e "s#$(INCLUDEDIR)/[^ ]*\.h##g" \
		 -e "s#//*usr/include/[^ ]*\.h##g" | \
	egrep -v '^[A-Za-z_]+\.o: *$$' | unexpand > Makefile.dep
	mv Makefile.dep Makefile

# DO NOT DELETE THIS LINE -- make depend depends on it.

parser.o: parser.h global_headers.h streamio.h
parser.o: boolean.h debug.h List.h
parser.o: Symtab.h Name.h Value.h Expression.h Module.h Production.h
parser.o: lexdefs.h
lexer.o: lexdefs.h debug.h boolean.h token.h
debug.o: debug.h boolean.h streamio.h
main.o: global_headers.h streamio.h
main.o: boolean.h debug.h List.h
main.o: vector.h parser.h Symtab.h Name.h Value.h Expression.h Module.h
main.o: Production.h interpret.h DBGenerator.h Polygon.h Turtle.h
main.o: PSGenerator.h GenericGenerator.h PPHIGSGenerator.h BLFGenerator.h
List.o: List.h streamio.h
List.o: debug.h boolean.h
Symtab.o: Symtab.h streamio.h
Symtab.o: boolean.h debug.h List.h
Symtab.o: Name.h Value.h
Expression.o: global_headers.h streamio.h
Expression.o: boolean.h debug.h List.h
Expression.o:   vector.h token.h
Expression.o: Symtab.h Name.h Value.h Expression.h
Module.o: global_headers.h streamio.h
Module.o: boolean.h debug.h List.h
Module.o: Symtab.h Name.h Value.h Expression.h Module.h
Production.o:  global_headers.h streamio.h
Production.o: boolean.h debug.h List.h
Production.o: Symtab.h Name.h Value.h Expression.h Module.h Production.h
Value.o: global_headers.h streamio.h
Value.o: boolean.h debug.h List.h
Value.o: Value.h Symtab.h Name.h
actions.o: global_headers.h streamio.h
actions.o: boolean.h debug.h List.h
actions.o: Symtab.h Name.h Value.h Expression.h Module.h Turtle.h vector.h
actions.o: Polygon.h DBGenerator.h actions.h
interpret.o: global_headers.h streamio.h
interpret.o: boolean.h debug.h List.h
interpret.o: Symtab.h Name.h Value.h Expression.h Module.h Turtle.h vector.h
interpret.o: DBGenerator.h Polygon.h interpret.h actions.h
Turtle.o: streamio.h
Turtle.o: Turtle.h vector.h boolean.h
vector.o:  vector.h streamio.h
vector.o: boolean.h
DBGenerator.o:  global_headers.h streamio.h
DBGenerator.o: boolean.h debug.h List.h
DBGenerator.o: vector.h Turtle.h DBGenerator.h Module.h Expression.h Value.h
DBGenerator.o: Symtab.h Name.h Polygon.h
PSGenerator.o: global_headers.h streamio.h
PSGenerator.o: boolean.h debug.h List.h
PSGenerator.o: vector.h Turtle.h PSGenerator.h DBGenerator.h Module.h
PSGenerator.o: Expression.h Value.h Symtab.h Name.h Polygon.h
GenericGenerator.o: global_headers.h streamio.h
GenericGenerator.o: boolean.h debug.h List.h
GenericGenerator.o: vector.h Turtle.h GenericGenerator.h DBGenerator.h
GenericGenerator.o: Module.h Expression.h Value.h Symtab.h Name.h Polygon.h
Polygon.o: global_headers.h streamio.h
Polygon.o: boolean.h debug.h List.h
Polygon.o: vector.h Polygon.h
Name.o: global_headers.h streamio.h
Name.o: boolean.h debug.h List.h
Name.o: Name.h Symtab.h
PPHIGSGenerator.o:  global_headers.h streamio.h
PPHIGSGenerator.o: boolean.h debug.h List.h
PPHIGSGenerator.o: vector.h Turtle.h PPHIGSGenerator.h DBGenerator.h Module.h
PPHIGSGenerator.o: Expression.h Value.h Symtab.h Name.h Polygon.h
BLFGenerator.o:  global_headers.h streamio.h
BLFGenerator.o: boolean.h debug.h List.h
BLFGenerator.o: vector.h Turtle.h BLFGenerator.h DBGenerator.h Module.h
BLFGenerator.o: Expression.h Value.h Symtab.h Name.h Polygon.h
sizes.o: global_headers.h streamio.h
sizes.o: boolean.h debug.h List.h
sizes.o: vector.h parser.h Symtab.h Name.h Value.h Expression.h Module.h
sizes.o: Production.h interpret.h DBGenerator.h Polygon.h Turtle.h
