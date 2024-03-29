#
# Makefile for cluster/pca
# $Header: /n/icsib30/da/stolcke/project/utils/cluster/RCS/Makefile,v 1.11 1992/02/12 07:14:50 stolcke Exp stolcke $
#

RELEASE = -2.3

# for private installation
BINDIR = $$HOME/bin.$(ARCH)
MANDIR = $$HOME/man/man$(MANEXT)
MANEXT = 1

# for public installation
#BINDIR = /usr/local/bin
#MANDIR = /usr/local/man/man$(MANEXT)
#MANEXT = l

# the following constants could be redefined in DEFINES
#
# SCALE -- the string used to label the scaling vector
#	default is -DSCALE=\"_SCALE_\"
#
# DONTCARE -- the string used to identify don't care values
#	default is -DDONTCARE=\"D/C\"
#
# NO_DONTCARES -- define this with -DNO_DONTCARES if you don't want don't care
#	values at all.  The implementation of this depends on the
#	math library having support for IEEE infinity values (the constant
#	HUGE and the function finite()).  If you can't get those to work on
#	your system, you might have to take out don't care support altogether
#	to compile the program.
# FLOAT -- define this to `float' (default) or `double' depending on
#	what precision you want for float point computation.  On some machines
#	float will be faster that double, on others the situation is reversed.
#	On machines where memory is tight you'll run out of space a lot
#	sooner with `double'.
# HAVE_CURSES -- enables support for terminal graphics using the curses(3)
#	library.  This requires a System V compatible curses library --
#	the Berkeley version won't do.
#

DEFINES	= -DFLOAT=double

#
# choose best compiler for given OS and hardware
# (pmake does this automatically by virtue of the #ifdef's below)
#
#ifdef mc68000
#
# GCC compiler
#
#ifdef sun
CC	= /usr/5bin/cc -pipe
CFLAGS	= -O4 -fsingle -f68881 /usr/lib/f68881/libm.il $(DEFINES) -DHAVE_CURSES
#else
CC	= gcc
CFLAGS	= -O -fstrength-reduce -finline-functions $(DEFINES)
#endif
#elifdef sparc
#
# best optimization on SPARC Suns
# (use /usr/5bin/cc to get the right curses library)
#
CC	= /usr/5bin/cc -pipe
CFLAGS	= -O4 -dalign -fsingle /usr/lib/libm.il $(DEFINES) -DHAVE_CURSES
#elifdef mips
#
# best optimization on MIPS machines
#
# RISC/os machines can use /sysv/bin/cc to take advantage of System V curses
#if exists(/sysv/bin/cc)
CC	= /sysv/bin/cc
CFLAGS	= -O2 -f $(DEFINES) -DHAVE_CURSES
#else
CC	= cc
CFLAGS	= -O2 -f $(DEFINES)
#endif
#else
#
# generic optimizing compiler
#
CC	= cc
CFLAGS	= -O $(DEFINES)
#endif

FILES	= $(HDRS) $(SRCS) $(OTHERS)

PROG	= cluster
ALIAS	= pca
HDRS	= alloc.h error.h cluster.h
SRCS	= alloc.c cluster.c pca.c graph.c
OBJS	= alloc.o cluster.o pca.o graph.o
LIBS	= -lm -lcurses

OTHERS	= ANNOUNCE README Makefile vectors names $(PROG).man $(ALIAS).man

$(PROG):	$(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(LIBS)

$(ALIAS):	$(PROG)
	ln -s $(PROG) $(ALIAS)

$(OBJS):	$(HDRS)

clean:
	rm -f $(PROG) $(ALIAS) $(OBJS)

install:	$(PROG)
	install -c -s $(PROG) $(BINDIR)
	-ln -s $(PROG) $(BINDIR)/$(ALIAS)
	install -c $(PROG).man $(MANDIR)/$(PROG).$(MANEXT)
	install -c $(ALIAS).man $(MANDIR)/$(ALIAS).$(MANEXT)
	
manroff:	$(PROG).man
	nroff -man $(PROG).man | $${PAGER-more}

#
# Packing and ftp
#
FTPDIR	= /usr/local/ftp/ai

tar:	$(PROG)$(RELEASE).tar.Z

$(PROG)$(RELEASE).tar.Z:	$(FILES)
	tar cf - $(FILES) | compress > $(PROG)$(RELEASE).tar.Z

shar:	$(PROG)$(RELEASE).shar

$(PROG)$(RELEASE).shar:	$(FILES)
	shar  $(FILES) > $(PROG)$(RELEASE).shar

ftp:	tar shar
	cp $(PROG)$(RELEASE).tar.Z $(FTPDIR)
	cp $(PROG)$(RELEASE).shar $(FTPDIR)
	
#
# some help in code cleanup
#
IFLAGS = -bap -nce -di8 -nfc1 -i4 -lp

.SUFFIXES:	.c.indent .h.indent

.c.c.indent:
	indent $< $@ $(IFLAGS) 

.h.h.indent:
	indent $< $@ $(IFLAGS) 
