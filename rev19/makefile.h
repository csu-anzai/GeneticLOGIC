#ifdef NEVERDEFINED
// Place flags here to turn them off.
// It's okay to keep a list of possible flags here,
// and just copy them below to turn them on.
#define DEBUG
#define ATT_IRIX3.2
#endif

#ifdef ATT_IRIX3.2
CC=CC
CCINCLUDE = /usr/local/include/CC
GLFILE = /usr1/u/larryy/CC/gl.h
KLUGEFILE = /usr1/u/larryy/CC/kluge.c
#else
CC=CC
CCINCLUDE = /usr/include/CC
GLFILE = /usr/include/gl/gl.h
#endif

#ifdef DEBUG
LFLAGS = -g
OLIMIT =
#else
LFLAGS = -O2
OLIMIT = -Olimit 1200
#endif

IFLAGS = -I$(CCINCLUDE) -DGLFILE=\"$(GLFILE)\" $(LFLAGS)

#ifdef ATT_IRIX3.2
cFLAGS = $(IFLAGS) $(OLIMIT) -DATT_IRIX3.2
#else
cFLAGS = $(IFLAGS) $(OLIMIT) -cckr
#endif

CFLAGS = $(cFLAGS) +i

.SUFFIXES:	.C ..c .o
.C.o:
	$(CC) -c $(CFLAGS) $<
.C..c:
	$(CC) -Fc -..c $(CFLAGS) $<
