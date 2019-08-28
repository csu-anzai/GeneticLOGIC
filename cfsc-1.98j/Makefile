# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Makefile for CFS-C 1.98j library, and fsw1 & letseq1 envs
#	$Id: Makefile,v 1.1 1992/12/17 15:55:13 joke Exp $
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
SHELL	= /bin/sh

VERS	= 1.98j

TOP	= .

SRC	= $(TOP)/src
SRCS	= $(SRC)/*.c

TOOL_SRC= $(TOP)/tools
TOOL_SRCS= $(TOOL_SRC)/*.c

HDR	= $(TOP)/include
HDRS	= $(HDR)/*.h $(HDR)/*.def $(HDR)/*.ext

OBJ	= $(TOP)/src
OBJS	= $(OBJ)/bblearn.o $(OBJ)/cfmsgio.o $(OBJ)/cfops.o $(OBJ)/cfsc.o  \
	$(OBJ)/cfsio.o $(OBJ)/cfsutil.o \
	$(OBJ)/core.o $(OBJ)/display.o $(OBJ)/dsclearn.o $(OBJ)/dscops.o \
	$(OBJ)/dscops2.o $(OBJ)/grandcfs.o $(OBJ)/usercmd.o $(OBJ)/utility.o

LIB	= CFSC
LIBDIR	= $(TOP)/lib
CFSCLIB	= $(LIBDIR)/lib$(LIB).a

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
CC	= gcc
CFLAGS	= -g -O -Wall -pedantic -ansi

CC	= acc
CFLAGS	= -g -Xc

CC	= cc
CFLAGS	= -g

AR	= ar
AFLAGS	= rv

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
.c.o:
	$(CC) $(CFLAGS) -I$(HDR) -c -o $*.o $<

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
all:	$(CFSCLIB) tools letseq1 fsw1

$(CFSCLIB):	$(OBJS)
	$(AR) $(AFLAGS) $(CFSCLIB) $(OBJS)
	ranlib $(CFSCLIB)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
FSW1		= $(TOP)/fsw1
FSW1OBJS	= $(FSW1)/fsw1a.o $(FSW1)/fsw1.o
fsw1:		$(FSW1OBJS)
	$(CC) $(CFLAGS) -o $(FSW1)/$@ $(FSW1OBJS) -L$(LIBDIR) -l$(LIB) -lm

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
LETSEQ1		= $(TOP)/letseq1
LETSEQ1OBJS	= $(LETSEQ1)/letseq1.o
letseq1:	$(LETSEQ1OBJS)
	$(CC) $(CFLAGS) -o $(LETSEQ1)/$@ $(LETSEQ1OBJS) -L$(LIBDIR) -l$(LIB) -lm

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
tools:	$(CFSLIB) $(TOOL_SRCS)
	( cd tools; make all CC=$(CC) CFLAGS="$(CFLAGS) -I../include" )

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
dvi:
	tgrind $(HDRS) $(SRCS)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
clean:
	$(RM) errs warns a.out core *~ *.dvi $(OBJS) $(FSW1OBJS) $(LETSEQ1OBJS)
	( cd tools; make clean )

clobber: clean
	$(RM) $(CFSCLIB) $(FSW1)/fsw1 $(LETSEQ1)/letseq1
	( cd tools; make clobber )
