#  Makefile for sa2.c "Simple GA #2" by Rick Riolo (-joke)

#  Use one of these, but must be ANSI-C compilers!
CC	= acc
CFLAGS	= -O2

CC	= gcc
CFLAGS	= -O -Wall -ansi

sa2:	sa2.c
	$(CC) -c $(CFLAGS) sa2.c
	$(CC) -o sa2 sa2.o -lm

clean:
	$(RM) a.out cpre *~ *.bak sa2.o

clobber: clean
	$(RM) sa2
