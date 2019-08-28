
# Makefile for UNIX version of GENESIS

# Note: select the appropriate compiler options
CFLAGS = -O
# CFLAGS = -O4 -ffpa	# sun3 with fpa
# CFLAGS = -g		# debugging

GA = display.o error.o done.o serial-convert.o restart.o \
     time.o 

CS = convert.o evaluate.o best.o communication.o measure.o main.o \
     input.o checkpnt.o schema.o elitist.o generate.o \
     init.o select.o cross.o mutate.o uni-cross.o prob-select.o \
     local-select.o

H = define.h extern.h global.h format.h parameters.h

f = f1

ga.$f:  $(GA) $(CS) $(H) $f.o
	cs $(CFLAGS) -o ga.$f $f.o $(GA) $(CS) -lm -lcurses -ltermlib

$(GA) : $(H)

$(CS) : $(H)
	cs $(CFLAGS) -c $(@:.o=.cs)

$f.o : $(H)
	cs $(CFLAGS) -c $f.cs

report : $(H)
	cc -o report report.c error.c

setup : $(H)
	cc -o setup setup.c
