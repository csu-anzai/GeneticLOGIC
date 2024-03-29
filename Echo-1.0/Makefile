##
## Makefile for Echo-1.0
##
## You will need to change the definitions below for
## X_INCLUDES and X_LIBS. Nothing else should have to 
## change.
##
## The first of these is the directory in which your
## system has the X window system include files.
##
## The second is where you have the X libraries stored.
##

X_INCLUDES = /network/software/packages/X/include
X_LIBS = /network/software/packages/X/lib


##############################################################################
#
# Nothing below should need to be touched.
#
# If you have gcc, you might want to change the next line to use it,
# in which case you can put "$(GCCFLAGS)" into the CFLAGS line.
#
CC       = cc
DEFINES  = 
INCLUDES = -I. -I$(X_INCLUDES) -IWIDGETS/fwf/include -IWIDGETS/At/include \
           -IWIDGETS/Athena3d/include
GCCFLAGS = -Wall
CFLAGS   = -g $(INCLUDES) $(DEFINES)
LDFLAGS  = -L$(X_LIBS) -LWIDGETS/Athena3d/lib -LWIDGETS/fwf/lib \
           -LWIDGETS/At/lib
LIBS     = WIDGETS/Athena3d/lib/libXaw3d.a \
           WIDGETS/Athena3d/lib/libXaw3d.sa.5.0 \
           WIDGETS/At/lib/libAt.a WIDGETS/fwf/lib/libDir.a \
           WIDGETS/fwf/lib/libfwf.a

OBJS     = agents.o chromosomes.o error.o free.o graphics.o hash.o \
           interaction.o main.o misc_graphics.o options.o plotter.o \
           random.o regexp.o regsub.o resources.o running.o schema.o \
           sites.o syscalls.o worlds.o

Echo : $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS) \
        -lfwf -lDir -lAt -lXaw3d -lXmu -lXt -lXext -lX11 -lm

all : libs Echo cluster run-echo-setup

simple-setup : x-setup sh-setup

x-setup:
	@echo "Setting up X resources for Echo... "
	@./echo-x-setup
	@echo Done.

sh-setup:
	@echo "Setting up environment variables for Echo... "
	@./echo-sh-setup $$SHELL
	@echo Done.

run-echo-setup:
	@echo "Making run-echo shell script... "
	@./build-run-echo
	@echo Done.

cluster :
	cd cluster; make
	cp cluster/cluster .

libs :
	for i in $(LIBS); do \
		ranlib $$i ; \
	done

depend:
	makedepend $(INCLUDES) $(DEFINES) $(SRC)

clean :
	rm -f *.o

clobber : clean
	rm -f Echo
