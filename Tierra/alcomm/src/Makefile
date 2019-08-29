
#
# best if you setenv ARCH `arch` befor running this ...
#
ALC_INC	= ../inc
ALC_LIB	= ${HOME}/lib/${ARCH}
#ALC_LIB		= .

ALC_SRCS = alcomm.c rqueue.c alerror.c gencomm.c memory.c \
	   mlayer.c
ALC_OBJS = alcomm.o rqueue.o alerror.o gencomm.o memory.o \
	   mlayer.o
ALC_HDRS = alcomm.h alcommp.h alreques.h debug.h \
	   mlayer.h mlayerp.h mrequest.h

# CFLAGS	= -g -I$(ALC_INC) -I. 
# CFLAGS	= -g -I$(ALC_INC) -I. -DFRONTEND=BASIC
CFLAGS	= -g -I$(ALC_INC) -I. 
# for sun's
#LDFLAGS	= -L$(ALC_LIB) -L./ -lalcom -lm -lresolv
# LDFLAGS		= -L./ -lalcom -lm -lresolv
# for decs's
LDFLAGS	= -L$(ALC_LIB) -L./ -lalcom -lm 


.c.o:
	cc -c $(CFLAGS) $*.c


default: lib

lib:	libalcom.a

libalcom.a: $(ALC_OBJS) $(ALC_HDRS)
	ar rcv libalcom.a $(ALC_OBJS)
	ranlib libalcom.a
#	cp libalcom.a $(ALC_LIB)/

try:	try.o libalcom.a
	cc -o try try.o $(LDFLAGS)

try2:	try2.o libalcom.a
	cc -o try2 try2.o $(LDFLAGS)

alcomm.c: $(ALC_HDRS)
rqueue.c: $(ALC_HDRS)
alerror.c: $(ALC_HDRS)
gencomm.c: $(ALC_HDRS)
memory.c: $(ALC_HDRS)
mlayer.c: $(ALC_HDRS)

alcomm.h:
alcommp.h:
alreques.h:
debug.h:
mlayer.h:
mlayerp.h:
mrequest.h:

