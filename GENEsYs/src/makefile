#
#	GENESIS 5.0
#				Ba, 05.12.'90
#				Ba, 06.12.'90
#				Ba, 17.05.'91
#
#	Makefile for the new GENESIS GA.
#

#
#	The objective functions are included in files named as follows:
#	
#	f_i.c		
#
#	with i being the only variable part. Here a number must be given,
#	such that objective functions are uniquely numbered. New functions
#	must be added to this list with the o-extension (see instructions 
#	for the installation of a new objective function in the Users Guide).
#

#
# 	fct/Makefile flags
#
MFLAGS=-s

#
# 	the directories
#

FDIR=fct

BINDIR=../bin

#
#	the functions
#
FUNS = 	f_01.o f_02.o f_03.o f_04.o f_05.o f_06.o f_07.o f_08.o f_09.o 	\
	f_10.o f_11.o f_12.o f_13.o f_14.o f_15.o f_16.o f_17.o f_18.o	\
	f_19.o f_20.o f_21.o f_22.o f_23.o f_24.o

#
#	The list of header files
#
HDRS= 	define.h extern.h

#
#	The list of object files
#
FOBJ=	$(FDIR)/*.o

GA=     best.o checkpoint.o converge.o cross.o done.o   		\
	dmpind.o elitist.o error.o evaluate.o fOpen.o gap.o 		\
	generate.o code.o ifc.o init.o input.o main.o measure.o 	\
	mlsel.o mutate.o proportional.o ranking.o 			\
	schema.o select.o setflag.o dbuff.o whitley.o boltzmann.o 	\
	$(FOBJ)

#
#	the C-compiler
#
CC=cc

#
#	flags for the C-compiler
#
CFLAGS=-g
CFLAGS=-O
CDEFS=

#
#	the loader
#
LDFLAGS= -lm

#
#	dependencies
#
default:
			cd $(FDIR); 					\
			make $(MFLAGS) FUNS="$(FUNS)" CFLAGS=$(CFLAGS);	\
			cd .. ; make all

debug :				
			cd $(FDIR); 					\
			make FUNS="$(FUNS)" CFLAGS=-g; 			\
			cd ..; make CFLAGS=-g all

all :			ga report setup


report : 		report.o code.o dbuff.o error.o init.o ifc.o 	\
			fOpen.o	mutate.o $(FOBJ) $(HDRS)
			cd $(FDIR); 					\
			make $(MFLAGS) FUNS="$(FUNS)" CFLAGS=$(CFLAGS);	\
			cd ..
			$(CC) $(CFLAGS) report.o dbuff.o code.o error.o \
			ifc.o fOpen.o init.o mutate.o 			\
			$(FOBJ) $(LDFLAGS) -o report
			mv report $(BINDIR)
 
setup : 		setup.o code.o  dbuff.o error.o init.o ifc.o 	\
			fOpen.o mutate.o $(FOBJ) $(HDRS)
			cd $(FDIR); 					\
			make $(MFLAGS) FUNS="$(FUNS)" CFLAGS=$(CFLAGS);	\
			cd ..
			$(CC) $(CFLAGS) setup.o code.o dbuff.o error.o 	\
			ifc.o init.o fOpen.o mutate.o 			\
			$(FOBJ) $(LDFLAGS) -o setup 
			mv setup $(BINDIR)

setup.o :		setup.c  $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c setup.c

report.o :		report.c  $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c report.c

ga :			$(GA) 
			$(CC) $(CFLAGS) $(GA) $(LDFLAGS) -o ga
			mv ga $(BINDIR)

xmon: 			xmon.c fct/towns100.h
			$(CC) $(CDEFS) $(CFLAGS) -o xmon xmon.c 	\
			-lXw -lXt -lX11 -lm

best.o :		best.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c best.c

checkpoint.o :		checkpoint.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c checkpoint.c

converge.o :		converge.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c converge.c

cross.o :		cross.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c cross.c

done.o :		done.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c done.c

dmpind.o :		dmpind.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c dmpind.c

elitist.o :		elitist.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c elitist.c

error.o :		error.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c error.c

evaluate.o :		evaluate.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c evaluate.c

fOpen.o :		fOpen.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c fOpen.c

gap.o :			gap.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c gap.c

generate.o :		generate.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c generate.c

code.o :		code.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c code.c

ifc.o :			ifc.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c ifc.c

init.o :		init.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c init.c

input.o :		input.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c input.c

main.o :		main.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c main.c

measure.o :		measure.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c measure.c

mlsel.o :		mlsel.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c mlsel.c

mutate.o :		mutate.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c mutate.c

proportional.o :	proportional.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c proportional.c

ranking.o :		ranking.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c ranking.c

whitley.o :		whitley.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c whitley.c

boltzmann.o :		boltzmann.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c boltzmann.c

schema.o :		schema.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c schema.c

select.o :		select.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c select.c

setflag.o :		setflag.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c setflag.c

dbuff.o :		dbuff.c $(HDRS)
			$(CC) $(CDEFS) $(CFLAGS) -c dbuff.c

clean:
			rm -f $(GA) report.o setup.o
			cd $(FDIR) ; make clean ; cd ..
