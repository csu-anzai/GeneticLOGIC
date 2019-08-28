# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# 	Makefile for scs: Goldberg's Simple Classifier System
# 	$Id: Makefile,v 1.1 1993/10/04 12:00:09 joke Exp $
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
SHELL	= /bin/sh

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	C compiler & tools
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# define:
#  -DLOCAL_MAINTAINER  if you want to support SCS-C locally (see version.h)
#CC	= cc
#CFLAGS	= -g -w

CC	= gcc
CFLAGS	= -g -O

#CC	= acc
#CFLAGS	= -g -w


MKPS	= "dvips -tletter -r -f"
MKDEP	= makedepend
MKPRO	= mkproto
MKTAGS	= ctags -t
MKPRETTY= tgrind

LATEX	= latex
BIBTEX	= bibtex

DITROFF	= groff -Tps
NROFF	= nroff

COMPRESS= gzip -9 -v
ZSUFF	= gz

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Logical structure
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
TOP	= .

BLURB	= $(TOP)/Makefile	\
	$(TOP)/README		\
	$(TOP)/CONFIGME		\
	$(TOP)/COPYLITE		\
	$(TOP)/INSTALL		\
	$(TOP)/OVERVIEW		\
	$(TOP)/LICENSE


HDR	= $(TOP)/src
HDRS	= $(HDR)/advance.h	\
	$(HDR)/aoc.h		\
	$(HDR)/declare.h	\
	$(HDR)/detector.h	\
	$(HDR)/environ.h	\
	$(HDR)/effector.h	\
	$(HDR)/ga.h		\
	$(HDR)/initial.h	\
	$(HDR)/io.h		\
	$(HDR)/perform.h	\
	$(HDR)/random.h		\
	$(HDR)/srand.h		\
	$(HDR)/rand48.h		\
	$(HDR)/prand.h		\
	$(HDR)/reinfor.h	\
	$(HDR)/report.h		\
	$(HDR)/scs.h		\
	$(HDR)/timekeep.h	\
	$(HDR)/utility.h	\
	$(HDR)/vlcr.h		\
	$(HDR)/version.h

XHDRS	= $(HDR)/getopt.h	\
	$(HDR)/panic.h

SRC	= $(TOP)/src
SRCS	= $(SRC)/advance.c	\
	$(SRC)/aoc.c		\
	$(SRC)/detector.c	\
	$(SRC)/environ.c	\
	$(SRC)/effector.c	\
	$(SRC)/ga.c		\
	$(SRC)/initial.c	\
	$(SRC)/io.c		\
	$(SRC)/scs.c		\
	$(SRC)/perform.c	\
	$(SRC)/random.c		\
	$(SRC)/srand.c		\
	$(SRC)/rand48.c		\
	$(SRC)/prand.c		\
	$(SRC)/reinfor.c	\
	$(SRC)/report.c		\
	$(SRC)/timekeep.c	\
	$(SRC)/utility.c	\
	$(SRC)/vlcr.c

XSRCS	= $(SRC)/getopt.c	\
	$(SRC)/getopt1.c	\
	$(SRC)/panic.c

OBJ	= $(TOP)/src
OBJS	= $(OBJ)/advance.o	\
	$(OBJ)/aoc.o		\
	$(OBJ)/detector.o	\
	$(OBJ)/environ.o	\
	$(OBJ)/effector.o	\
	$(OBJ)/ga.o		\
	$(OBJ)/initial.o	\
	$(OBJ)/io.o		\
	$(OBJ)/scs.o		\
	$(OBJ)/perform.o	\
	$(OBJ)/reinfor.o	\
	$(OBJ)/report.o		\
	$(OBJ)/timekeep.o	\
	$(OBJ)/utility.o

XOBJS	= $(OBJ)/getopt.o	\
	$(OBJ)/getopt1.o	\
	$(OBJ)/panic.o

CONFIG	= $(TOP)/config
CONFIGS	= $(CONFIG)/*

CONTRIB = $(TOP)/contrib
CONTRIBS= $(CONTRIB)/*

DAT	= $(TOP)/data
DATS	= $(DAT)/class10.dta	\
	$(DAT)/class100.dta	\
	$(DAT)/ga.dta		\
	$(DAT)/lessthan.dta	\
	$(DAT)/lessthan2.dta	\
	$(DAT)/perfect.dta	\
	$(DAT)/pop.dta		\
	$(DAT)/time.dta

DOC	= $(TOP)/docs
DOCS	= $(DOC)/*

MAN	= $(TOP)/man
MANS	= $(MAN)/scsc.1		\
	$(MAN)/scsc-man.ps	\
	$(MAN)/scsc.man

FIGS	= $(TOP)/figs/*

PRG	= scsc
VERS	= 0.99j
LIBS	= -lm

JUNK	= *~ $(OBJ)/*.o core errs warns a.out *.bak *.ps *.man *.plt *.log *.$(ZSUFF)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Random number generator setup (uncomment the one you want)
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
RANDOM	= $(SRC)/random.o	# Goldberg's default generator (Knuth 1981)
#RANDOM	= $(SRC)/vlcr.o		# BYTE's very long cycled generator
#RANDOM	= $(SRC)/prand.o	# Plauger's ANSI C random number generator
#RANDOM	= $(SRC)/srand.o	# UN*X simple random number generator
#RANDOM	= $(SRC)/rand48.o	# UN*X sophisticated random number generator

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	General rules
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
.c.o:
	$(CC) $(CFLAGS) -I$(SRC) -o $(TOP)/$@  -c $(TOP)/$<

$(PRG):	$(OBJS) $(RANDOM) $(XOBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(RANDOM) $(XOBJS) $(LIBS)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Special/extra rules
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
XFLAGS	= -DSTRERROR_MISSING ##-DEDDIE
$(OBJ)/panic.o: $(HDR)/panic.h $(SRC)/panic.c
	$(CC) $(CFLAGS) $(XFLAGS) -I$(HDR) -o $(TOP)/$@  -c $(SRC)/panic.c

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Prototypes
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
pro:
	$(RM) $(HDR)/proto.h
	$(MKPRO) $(SRCS) > $(HDR)/proto.h

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Pretty printing
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
pretty:
	$(MKPRETTY) $(HDRS) $(SRCS)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Figures
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
figs:	$(PRG)
	(cd figs; make)

nfigs:	$(PRG)
	(cd figs; make export)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Documentation
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
doc:	ps-doc ps-man

dvi-doc:
	(cd docs; make LATEX=$(LATEX) BIBTEX=$(BIBTEX) dvi)

ps-doc:
	(cd docs; make MKPS=$(MKPS) ps clean)

ps-man:
	$(DITROFF) -man $(MAN)/$(PRG).1 >$(MAN)/$(PRG)-man.ps
	$(NROFF)   -man $(MAN)/$(PRG).1 >$(MAN)/$(PRG).man

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Distribution
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
DIST	= $(BLURB) $(CONFIGS) $(CONTRIBS) $(DOCS) $(MANS) \
	$(HDRS)	$(XHDRS) $(SRCS) $(XSRCS) $(DATAS) $(FIGS)

dist:	ps-doc ps-man
	$(RM) $(PRG)-$(VERS).tar*
	tar cvf $(PRG)-$(VERS).tar $(DIST)
	$(COMPRESS) $(PRG)-$(VERS).tar

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Editor tags
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
tags:
	$(RM) tags
	$(MKTAGS) $(HDRS) $(SRCS) $(XHDRS) $(XSRCS)

index:
	$(RM) index
	$(MKTAGS) -x $(SRCS) $(XSRCS) | sort -f >index

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Version control
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
RCS	= $(TOP)/RCS
RCSS	= $(RCS)/*,v $(SRC)/RCS/*,v $(CONFIG)/RCS/*,v
RCSALL	= Makefile configure $(HDRS) $(SRCS) $(XHDRS) $(XSRCS) $(CONFIGS)
rcs:
	$(RM) $(RCSS)
	rcs -i -U -t-"Initial Version" -q $(RCSALL)

up:
	ci -u -w"joke" -t-"Update" -q $(RCSALL)

out:
	co $(RCSALL)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Home
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
disk:
	mcopy -t -n $(BLURB) $(SRCS) $(HDRS) $(DOCS) a:

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Cleanup
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
clean:
	(cd figs; make clean)
	(cd docs; make clean)
	$(RM) $(JUNK)

clobber: clean
	(cd docs; make clobber)
	$(RM) $(PRG) $(SRC)/config.h tags index $(MAN)/*.ps $(MAN)/*.man

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Dependencies
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
depend:
	$(MKDEP) $(CFLAGS) -I$(GCC_INCLUDE) $(SRCS) $(XSRCS)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# DO NOT DELETE THIS LINE -- make depend depends on it.

advance.o: ./src/scs.h ./src/config.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
advance.o: /usr/include/ctype.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
advance.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
advance.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
advance.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
advance.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
advance.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
advance.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
aoc.o: ./src/scs.h ./src/config.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
aoc.o: /usr/include/ctype.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
aoc.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
aoc.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
aoc.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
aoc.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h ./src/io.h
aoc.o: ./src/perform.h ./src/reinfor.h ./src/report.h ./src/timekeep.h
aoc.o: ./src/utility.h ./src/random.h ./src/panic.h
detector.o: ./src/scs.h ./src/config.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
detector.o: /usr/include/ctype.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
detector.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
detector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
detector.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
detector.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
detector.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
detector.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
environ.o: ./src/scs.h ./src/config.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
environ.o: /usr/include/ctype.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
environ.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
environ.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
environ.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
environ.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
environ.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
environ.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
effector.o: ./src/scs.h ./src/config.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
effector.o: /usr/include/ctype.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
effector.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
effector.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
effector.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
effector.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
effector.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
effector.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
ga.o: ./src/scs.h ./src/config.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
ga.o: /usr/include/ctype.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
ga.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
ga.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
ga.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
ga.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h ./src/io.h
ga.o: ./src/perform.h ./src/reinfor.h ./src/report.h ./src/timekeep.h
ga.o: ./src/utility.h ./src/random.h ./src/panic.h
initial.o: ./src/scs.h ./src/config.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
initial.o: /usr/include/ctype.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
initial.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
initial.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
initial.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
initial.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
initial.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
initial.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
io.o: ./src/scs.h ./src/config.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
io.o: /usr/include/ctype.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
io.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
io.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
io.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
io.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h ./src/io.h
io.o: ./src/perform.h ./src/reinfor.h ./src/report.h ./src/timekeep.h
io.o: ./src/utility.h ./src/random.h ./src/panic.h
scs.o: ./src/scs.h ./src/config.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
scs.o: /usr/include/ctype.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
scs.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
scs.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
scs.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
scs.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h ./src/io.h
scs.o: ./src/perform.h ./src/reinfor.h ./src/report.h ./src/timekeep.h
scs.o: ./src/utility.h ./src/random.h ./src/panic.h ./src/getopt.h
scs.o: ./src/version.h
perform.o: ./src/scs.h ./src/config.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
perform.o: /usr/include/ctype.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
perform.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
perform.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
perform.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
perform.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
perform.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
perform.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
random.o: ./src/scs.h ./src/config.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
random.o: /usr/include/ctype.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
random.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
random.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
random.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
random.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
random.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
random.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
srand.o: ./src/scs.h ./src/config.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
srand.o: /usr/include/ctype.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
srand.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
srand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
srand.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
srand.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
srand.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
srand.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
srand.o: ./src/srand.h
rand48.o: ./src/scs.h ./src/config.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
rand48.o: /usr/include/ctype.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
rand48.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
rand48.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
rand48.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
rand48.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
rand48.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
rand48.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
rand48.o: ./src/rand48.h
prand.o: ./src/scs.h ./src/config.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
prand.o: /usr/include/ctype.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
prand.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
prand.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
prand.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
prand.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
prand.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
prand.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
prand.o: ./src/prand.h
reinfor.o: ./src/scs.h ./src/config.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
reinfor.o: /usr/include/ctype.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
reinfor.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
reinfor.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
reinfor.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
reinfor.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
reinfor.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
reinfor.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
report.o: ./src/scs.h ./src/config.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
report.o: /usr/include/ctype.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
report.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
report.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
report.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
report.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
report.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
report.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
timekeep.o: ./src/scs.h ./src/config.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
timekeep.o: /usr/include/ctype.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
timekeep.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
timekeep.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
timekeep.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
timekeep.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
timekeep.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
timekeep.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
utility.o: ./src/scs.h ./src/config.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
utility.o: /usr/include/ctype.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
utility.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
utility.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
utility.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
utility.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
utility.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
utility.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
vlcr.o: ./src/scs.h ./src/config.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
vlcr.o: /usr/include/ctype.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/math.h
vlcr.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/string.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdlib.h
vlcr.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/unistd.h
vlcr.o: ./src/declare.h ./src/aoc.h ./src/environ.h ./src/effector.h
vlcr.o: ./src/detector.h ./src/advance.h ./src/ga.h ./src/initial.h
vlcr.o: ./src/io.h ./src/perform.h ./src/reinfor.h ./src/report.h
vlcr.o: ./src/timekeep.h ./src/utility.h ./src/random.h ./src/panic.h
vlcr.o: ./src/vlcr.h
getopt.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
getopt.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
getopt.o: ./src/getopt.h
getopt1.o: ./src/getopt.h
panic.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdio.h
panic.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/stdarg.h
panic.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/time.h
panic.o: /usr/local/gnulang-93c/lib/gcc-lib/sun4/2.4.3/include/sys/stdtypes.h
panic.o: ./src/panic.h
