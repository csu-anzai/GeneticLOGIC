# Makefile for Primordial soup.
# by Marc de Groot.
# Copyright (c) 1992 by Marc de Groot. All rights reserved.

###########################################################################
# Set the platform you are compiling on.
#
# Use the following line for BSD systems, including SunOS
PLATFORM= -DUNIX -DBSD
#
# Use the following line for System V systems
# PLATFORM= -DUNIX -DSYSV
#
###########################################################################

###########################################################################
# Define GBASE if you want the genealogy database code to be compiled.
GBASE= -DGBASE
# GBASE=
###########################################################################

###########################################################################
# Define SPAWNCOST if you want the number of bytes copied by SPWN to
# be subtracted from the organism's lifetime and heartbeat down-counters.
SPAWNCOST= -DSPAWNCOST
# SPAWNCOST=
###########################################################################

###########################################################################
# Define PROTOS if your compiler groks ANSI function prototypes.
# PROTOS= -DPROTOTYPES
PROTOS=
###########################################################################

###########################################################################
# The following may need to be changed.
###########################################################################

# Your C compiler
CC= cc
# CC= gcc

# Optimizer flags and other misc stuff
OPTIM= -g
# OPTIM= -g -O    -W -Wreturn-type -Wunused -Wcomment -Wwrite-strings

# Where the compiler should look for include files
INCLUDE= .

# When changing this, also change it in soup.h and README
VER= 1.0

# Where to put the tar and shar files created by make backup
ARC=/home/src/archive

###########################################################################
# The rest shouldn't need changing
###########################################################################

CFLAGS= -I${INCLUDE} ${PLATFORM} ${GBASE} ${SPAWNCOST} ${PROTOS} ${OPTIM}

.c.o:
	${CC} ${CFLAGS} -c $*.c

SRC= main.c rnd.c soup.c dosoup.c organism.c globals.c signals.c \
	mon.c execute.c disassm.c gbase.c exit.c
OBJS= main.o rnd.o soup.o dosoup.o organism.o globals.o signals.o \
	mon.o execute.o disassm.o gbase.o exit.o
HDRS= soup.h protos.h

SHOWSRC= show.c disassm.c
SHOWOBJ= show.o disassm.o
SHOWHDR= soup.h gbase.h organism.h

DIST= *.c *.h Makefile README Log makedepend

all:	psoup mkorg show

psoup:	${SRC} ${OBJS} ${HDRS}
	${CC} ${CFLAGS} -o psoup ${OBJS}

backup:
	tar cvf ${ARC}/pbackup${VER}.tar *

dist:
	shar ${DIST} > ${ARC}/psoup${VER}.shar
	tar cvf ${ARC}/psoup${VER}.tar ${DIST}
	compress ${ARC}/psoup${VER}.tar

show:	${SHOWSRC} ${SHOWOBJ} ${SHOWHDR}
	${CC} ${CFLAGS} -o show ${SHOWOBJ}

tstrnd:	tstrnd.o rnd.o
	${CC} ${CFLAGS} -o tstrnd tstrnd.o rnd.o

mkorg:	mkorg.c
	${CC} ${CFLAGS} -o mkorg mkorg.c

neworg:	neworg.c
	${CC} ${CFLAGS} -o neworg neworg.c

shortorg:	shortorg.c
		${CC} ${CFLAGS} -o shortorg shortorg.c

clean:
	rm -f *.o psoup mkorg neworg shortorg org.out show genomes.* family.tre

depend:
	./makedepend ${PLATFORM} ${GBASE} ${SPAWNCOST} ${PROTOS}

# DO NOT REMOVE THIS LINE - USED BY MAKE DEPEND
disassm.o: disassm.c
disassm.o: /usr/include/stdio.h
disassm.o: ./soup.h
disassm.o: ./opcodes.h
dosoup.o: dosoup.c
dosoup.o: ./soup.h
dosoup.o: ./organism.h
dosoup.o: ./protos.h
dosoup.o: /usr/include/signal.h
dosoup.o: /usr/include/sys/signal.h
dosoup.o: /usr/include/vm/faultcode.h
dosoup.o: /usr/include/sys/stdtypes.h
dosoup.o: /usr/include/stdio.h
execute.o: execute.c
execute.o: ./soup.h
execute.o: ./organism.h
execute.o: ./protos.h
execute.o: /usr/include/signal.h
execute.o: /usr/include/sys/signal.h
execute.o: /usr/include/vm/faultcode.h
execute.o: /usr/include/sys/stdtypes.h
execute.o: ./opcodes.h
exit.o: exit.c
exit.o: /usr/include/stdio.h
exit.o: ./soup.h
exit.o: ./protos.h
exit.o: /usr/include/signal.h
exit.o: /usr/include/sys/signal.h
exit.o: /usr/include/vm/faultcode.h
exit.o: /usr/include/sys/stdtypes.h
gbase.o: gbase.c
gbase.o: ./soup.h
gbase.o: ./gbase.h
gbase.o: ./organism.h
gbase.o: ./protos.h
gbase.o: /usr/include/signal.h
gbase.o: /usr/include/sys/signal.h
gbase.o: /usr/include/vm/faultcode.h
gbase.o: /usr/include/sys/stdtypes.h
gbase.o: /usr/include/stdio.h
globals.o: globals.c
globals.o: ./soup.h
globals.o: ./organism.h
globals.o: /usr/include/stdio.h
main.o: main.c
main.o: ./soup.h
main.o: ./protos.h
main.o: /usr/include/signal.h
main.o: /usr/include/sys/signal.h
main.o: /usr/include/vm/faultcode.h
main.o: /usr/include/sys/stdtypes.h
mkorg.o: mkorg.c
mkorg.o: ./opcodes.h
mkorg.o: ./soup.h
mkorg.o: /usr/include/stdio.h
mon.o: mon.c
mon.o: ./soup.h
mon.o: ./organism.h
mon.o: ./protos.h
mon.o: /usr/include/signal.h
mon.o: /usr/include/sys/signal.h
mon.o: /usr/include/vm/faultcode.h
mon.o: /usr/include/sys/stdtypes.h
mon.o: /usr/include/ctype.h
mon.o: /usr/include/stdio.h
neworg.o: neworg.c
neworg.o: ./opcodes.h
neworg.o: ./soup.h
neworg.o: /usr/include/stdio.h
organism.o: organism.c
organism.o: ./soup.h
organism.o: ./organism.h
organism.o: ./protos.h
organism.o: /usr/include/signal.h
organism.o: /usr/include/sys/signal.h
organism.o: /usr/include/vm/faultcode.h
organism.o: /usr/include/sys/stdtypes.h
rnd.o: rnd.c
rnd.o: ./soup.h
rnd.o: ./protos.h
rnd.o: /usr/include/signal.h
rnd.o: /usr/include/sys/signal.h
rnd.o: /usr/include/vm/faultcode.h
rnd.o: /usr/include/sys/stdtypes.h
shortorg.o: shortorg.c
shortorg.o: ./opcodes.h
shortorg.o: ./soup.h
shortorg.o: /usr/include/stdio.h
show.o: show.c
show.o: /usr/include/ctype.h
show.o: /usr/include/stdio.h
show.o: ./soup.h
show.o: ./gbase.h
show.o: ./organism.h
signals.o: signals.c
signals.o: ./protos.h
signals.o: /usr/include/signal.h
signals.o: /usr/include/sys/signal.h
signals.o: /usr/include/vm/faultcode.h
signals.o: /usr/include/sys/stdtypes.h
signals.o: /usr/include/signal.h
signals.o: /usr/include/stdio.h
soup.o: soup.c
soup.o: ./soup.h
soup.o: ./protos.h
soup.o: /usr/include/signal.h
soup.o: /usr/include/sys/signal.h
soup.o: /usr/include/vm/faultcode.h
soup.o: /usr/include/sys/stdtypes.h
tstrnd.o: tstrnd.c
tstrnd.o: /usr/include/stdio.h
