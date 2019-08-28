
# Makefile for UNIX version of GENESIS

# Note: select the appropriate compiler options
CFLAGS = -O 
# CFLAGS = -O4 -ffpa	# sun3 with fpa
# CFLAGS = -g		# debugging

GA = best.o checkpnt.o convert.o cross.o display.o \
    done.o elitist.o error.o evaluate.o \
    generate.o init.o input.o main.o measure.o \
    mutate.o restart.o schema.o select.o

H = define.h extern.h global.h format.h

f = f1

ga.$f:  ga.a $f.o
	cc $(CFLAGS) -o ga.$f $f.o ga.a -lm -lcurses -ltermlib

ga.a : $(GA)
	rm -f ga.a
	ar r ga.a $(GA)
	ranlib ga.a

$(GA) : $(H)

install : ga.a report setup
	chmod ugo+x go

report : report.o error.o
	cc -o report report.o error.o

report.o : $(H)

setup : setup.o
	cc -o setup setup.o
