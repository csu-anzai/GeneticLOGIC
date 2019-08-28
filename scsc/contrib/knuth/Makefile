# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#  $Id: Makefile,v 1.1 1993/02/15 09:43:09 joke Exp $
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
F77	= f77
F2C	= f2c

F2CLIB	= /home/ls11soft/lib
F2CINC = .		# /home/ls11soft/include

P2CLIB	= /home/ls11soft/lib
P2CINC	= .		# /home/ls11soft/include
	
all:	rand crand scs-rand sga-rand

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
rand: rand.o
	$(F77) -o rand rand.o

rand.o: rand.f
	$(F77) -c rand.f


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
crand: crand.c crand.o
	$(CC) $(CFLAGS) -o crand crand.o -L$(F2CLIB) -lf2cF77 -lf2cI77 -lm -lc


crand.c:
	cp rand.f crand.f
	$(F2C) crand.f
	$(RM) crand.f

crand.o: crand.c
	$(CC) $(CFLAGS) -I$(F2CINC)  -c crand.c

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#  Goldberg's implementations of Knuth generator: sga-rand is buggy (sic!)
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
scs-rand:
	$(CC) $(CFLAGS) -DTEST -o scs-rand scs-rand.c

sga-rand:
	$(CC) $(CFLAGS) -DTEST -o sga-rand sga-rand.c

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#  Proof: Goldberg's sga-rand is buggy (sic!)
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
simple:
	$(CC) $(CFLAGS) -I$(P2CINC) -c simple3a.c
	$(CC) $(CFLAGS) -c sga-rand.c
	$(CC) $(CFLAGS) -o simple3a sga-rand.o simple3a.o -L$(P2CLIB) -lp2c

simple2:
	$(CC) $(CFLAGS) -I$(P2CINC) -c simple3a.c
	$(CC) $(CFLAGS) -c scs-rand.c
	$(CC) $(CFLAGS) -o simple3a-2 scs-rand.o simple3a.o -L$(P2CLIB) -lp2c

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
pretty:
	tgrind crand.c
	tgrind -lf77 rand.f

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
test:	all
	rand		>a
	crand		>b
	scs-rand	>c
	sga-rand	>d
	gnuplot compare.gnu

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
clean:
	$(RM) *~ *.o a.out core a b c d

clobber: clean
	$(RM) rand crand scs-rand sga-rand simple3a simple3a-2
