# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
SHELL	= /bin/sh

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
CC	= gcc
CFLAGS	= -g -O -Wall -I../include
LFLAGS	= ../src/cfsio.o ../src/utility.o -lm

PRGS	= ccls1 getdvars getperf getstr1 getvars \
	getvc getvc1 concnt-1

OBJS	= ccls1.o getdvars.o getperf.o getstr1.o getvars.o \
	getvc.o getvc1.o concnt-1.o

INSTALL	= install -s
BINDIR	= ../bin

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
all: $(PRGS)

install:
	$(INSTALL) $(PRGS) $(BINDIR)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#   LETSEQ1 analysing tools
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
ccls1: ccls1.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)
getdvars: getdvars.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)
getperf: getperf.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)
getstr1: getstr1.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)
getvars: getvars.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#   FSW1 analysing tools
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
concnt-1: concnt-1.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)
getvc: getvc.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)
getvc1: getvc1.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LFLAGS)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
tags:
	ctags *.c

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
clean:
	$(RM) $(OBJS) tags

clobber: clean
	$(RM) $(PRGS)
