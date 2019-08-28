#
# Makefile for Simple Genetic Algorithm C code
#
#####################################
# Define implicit compilation rules #
#####################################

# uncomment following lines for UNIX computers
# nominally tested on Sun, Cray UNICOS, and Vax Ultrix systems
CC=cc
CFLAGS= -DUNIX
LDLIBS= -lm

# uncomment following 5 lines for nCUBE 2 parallel supercomputers
# .SUFFIXES: .on
# CC=ncc
# CFLAGS= -g -DNCUBE -Nheap 700000
# LDLIBS=
# .c.on:;	$(CC) $(CFLAGS) -c $<

# uncomment three of the following lines to determine selection routine
# roulette wheel, stochastic remainder, and tournament selection
# are distributed with SGA-C
SELECTCODE = rselect.c
SELECTOBJ = rselect.o
SELECTON = rselect.on
#SELECTCODE = srselect.c
#SELECTOBJ = srselect.o
#SELECTON = srselect.on
#SELECTCODE = tselect.c
#SELECTOBJ = tselect.o
#SELECTON = tselect.on

SRCCODE=main.c app.c exchange.c generate.c initial.c memory.c\
	operators.c random.c report.c $(SELECTCODE)\
	statistic.c utility.c

#--- for UNIX systems, uncomment the following lines
OBJECTS=main.o app.o exchange.o generate.o initial.o\
 	memory.o operators.o random.o report.o\
 	$(SELECTOBJ) statistic.o utility.o

#--- for ncube, uncomment the following lines
#OBJECTS=main.on app.on exchange.on generate.on initial.on\
#	memory.on operators.on random.on report.on\
#	$(SELECTON) statistic.on utility.on

sga:    $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LDLIBS)
	chmod 755 $@

$(OBJECTS): sga.h external.h

listing:
	cat *.h > sga.list
	cat $(SRCCODE) >> sga.list

clean:
	rm -f sga 
	rm -f *.on sga.sym
	rm -f *.o

