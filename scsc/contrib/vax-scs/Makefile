CC	= gcc -Wall -g -O

scs:	scs.o
	$(CC) $(CFLAGS) -o scs scs.o -lm

clean:
	$(RM) *~ *.out core tags index scs.o

clobber: clean
	$(RM) scs
