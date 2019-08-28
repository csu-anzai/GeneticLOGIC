CC	= gcc -Wall
CFLAGS	= -DREAL #-DINTEGER -DTEST

PRG	= trand
OBJS	= trand.o srand.o rand.o

$(PRG): $(OBJS)
	$(CC) $(CFLAGS) -o trand $(OBJS)

clean:
	$(RM) *~ a.out core a b c $(OBJS)

clobber: clean
	$(RM) $(PRG)
