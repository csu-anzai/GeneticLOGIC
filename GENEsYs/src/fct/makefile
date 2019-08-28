#
#	GENESIS 5.0
#				Ba, 05.12.'90
#				Ba, 06.12.'90
#				Ba, 12.06.'91	getarg, arglst
#
#	Makefile for the new GENESIS GA.
#


#
#	The list of header files
#
HDRS = 	../define.h ../extern.h

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
default:	ftab.o getarg.o getopt.o arglst.o $(FUNS)

arglst.o:	arglst.c arglst.h $(HDRS)
		$(CC) $(CDEFS) $(CFLAGS) $(INFLAGS) $(CFLAGS) -c arglst.c

getarg.o:	getarg.c getarg.h $(HDRS)
		$(CC) $(CDEFS) $(CFLAGS) $(INFLAGS) $(CFLAGS) -c getarg.c

getopt.o:	getopt.c $(HDRS)
		$(CC) $(CDEFS) $(CFLAGS) $(INFLAGS) $(CFLAGS) -c getopt.c

ftab.o :	ftab.c $(HDRS)
		$(CC) $(CDEFS) $(CFLAGS) $(INFLAGS) $(CFLAGS) -c ftab.c

f_%.o:		f_%.c $(HDRS)
		$(CC) $(CDEFS) $(CFLAGS) $(INFLAGS) -c f_$*.c

clean:
		rm -f f_*.o ftab.o getarg.o arglst.o
