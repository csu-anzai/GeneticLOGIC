#
# DGENESIS 1.0 Makefile
# 
# Erick Cantu-Paz
# ecantu@lamport.rhon.itam.mx
# Instituto Tecnologico Autonomo de Mexico	
# October 1993
#

#
# System dependencies 
#

# CFLAGS are the flags given to the C compiler
CFLAGS = -O	# normal optimization
#CFLAGS = -g	# for debug

#
# file handling commands:
#
RM = rm -f # command to delete a file
MD = mkdir # command to create a new directory
CP = cp    # command to copy a file to a directory

# compilation commands:
CC = cc    # the C compiler
AR = ar r  # the archiver/librarian


#              <<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>
#              <<<<<<<<<<<<<<<<< DO NOT EDIT BELOW HERE >>>>>>>>>>>>>>
#              <<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>

#
# GAeval should be set from command line
#
GAeval = GAeval


OBJS = $(DGENESIS)/src/best.o $(DGENESIS)/src/checkpnt.o \
       $(DGENESIS)/src/convert.o $(DGENESIS)/src/cross.o \
       $(DGENESIS)/src/display.o $(DGENESIS)/src/dist.o \
       $(DGENESIS)/src/done.o $(DGENESIS)/src/elitist.o \
       $(DGENESIS)/src/error.o $(DGENESIS)/src/evaluate.o \
       $(DGENESIS)/src/generate.o $(DGENESIS)/src/inipop.o \
       $(DGENESIS)/src/init.o $(DGENESIS)/src/input.o \
       $(DGENESIS)/src/main.o $(DGENESIS)/src/measure.o \
       $(DGENESIS)/src/migrate.o $(DGENESIS)/src/misc.o \
       $(DGENESIS)/src/mutate.o $(DGENESIS)/src/restart.o \
       $(DGENESIS)/src/schema.o $(DGENESIS)/src/select.o \
       $(DGENESIS)/src/socket.o $(DGENESIS)/src/log.o

HDRS = $(DGENESIS)/src/define.h $(DGENESIS)/src/global.h \
       $(DGENESIS)/src/format.h



#
# dependencies
#
$(GAeval) :  $(DGENESIS)/bin/ga.a $(GAeval).o
	$(CC) $(CFLAGS) -o $(GAeval) $(GAeval).o $(DGENESIS)/bin/ga.a -lm -lcurses -ltermlib

all : install 

install : ga.a report setup rgo links

ga.a: $(DGENESIS)/bin/ga.a

report: $(DGENESIS)/bin/report

setup: $(DGENESIS)/bin/setup

rgo: $(DGENESIS)/bin/rgo

links: $(DGENESIS)/bin/links


$(DGENESIS)/bin/ga.a : $(OBJS)
	-$(RM) $(DGENESIS)/bin/ga.a
	$(AR) $(DGENESIS)/bin/ga.a $(OBJS)
	ranlib $(DGENESIS)/bin/ga.a

$(OBJS) : $(HDRS)
	cd $(DGENESIS)/src ;\
	$(CC) -c $(CFLAGS) $<

$(DGENESIS)/bin/setup : $(DGENESIS)/src/setup.c $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o $(HDRS)
	$(CC) $(CFLAGS) -o $(DGENESIS)/bin/setup $(DGENESIS)/src/setup.c $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o -lm


$(DGENESIS)/bin/links : $(DGENESIS)/src/links.c $(DGENESIS)/src/convert.o  $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o $(HDRS)
	$(CC) $(CFLAGS) -o $(DGENESIS)/bin/links $(DGENESIS)/src/links.c $(DGENESIS)/src/convert.o $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o

$(DGENESIS)/bin/report : $(DGENESIS)/src/report.c $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o $(HDRS)
	$(CC) $(CFLAGS) -o $(DGENESIS)/bin/report $(DGENESIS)/src/report.c $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o
	
$(DGENESIS)/bin/rgo : $(DGENESIS)/src/rgo.c $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o
	$(CC) $(CFLAGS) -o $(DGENESIS)/bin/rgo $(DGENESIS)/src/rgo.c $(DGENESIS)/src/error.o $(DGENESIS)/src/misc.o


clean :
	-$(RM) *.o
