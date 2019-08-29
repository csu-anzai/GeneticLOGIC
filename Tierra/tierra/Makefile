
# Makefile_sccsid: @(#)Makefile	1.1	4/11/92

## edit the following variables to tailor this Makefile to the system
## you want to install tierra on. (hint: if your `make' complains
## about missing values for any variable that is commented out, just
## give it a null value, ie: VAR =<Return>

## NOTICE: special features such as
## Almond, CM5, and soon PHYLO have been moved after the OS specific
## section, most users won't have to worry about them
## to use them, make sure ALL relavant lines have been uncommented

## Destination: set as appropriate for your system. it currently assumes
## you are building the system in a directory called /usera/tierra
#
DEST	      = ./

## Tierra: uncomment the following lines as appropriate to build tierra
#
TIERRADIR     = $(DEST)

## Instruction Set: uncomment your choice of instruction sets
#
## Tom Ray's original instruction set 1
INST	= 1

## OS type: uncomment the set of lines for the operating system
## and/or machine type most closely resembling yours, if you port
## tierra to a system significantly different from any of these or 
## one marked "not tested", please send us any changes you needed to
## make to get it working.
#
## The -ltermcap and -lcurses are libs for the BASIC ( curses) frontend only
#
# Sun Workstations: choose Sun3 or Sun4
# other systems running Berkeley 4.2 or 4.3: choose bsd 4.3
#
# SGI Iris: choose IRIX
# AT&T SysIII: choose Sys3 (not tested)
# AT&T SysV: choose SysV   (not tested)
#
# IBM RS/6000: choose RS6000

## Sun3 -------------------------------------
#
# OSFLAGS	= -fswitch
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM
# LIBS	= -lm -lcurses -ltermcap -lalcom -lresolv
#
## Sun4 -------------------------------------
#
# OSFLAGS
LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM 
# LIBS	= -lm -lcurses -ltermcap -lalcom -lresolv
#
## next -------------------------------------
#
# OSFLAGS	= 
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM
# LIBS	= -lm -lcurses -ltermcap -lalcom -lresolv
#
## bsd 4.3 ----------------------------------
#
# OSFLAGS	=
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM
# LIBS	= -lm -lcurses -ltermcap -lalcom -lresolv
#
## IRIX -------------------------------------
#  only use BSD_SIGNALS for irix < 4.0
# OSFLAGS	= -D_BSD_SIGNALS	
# LIBS	= -lm -lsun -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM -D_BSD_SIGNALS
# LIBS	= -lm -lcurses -ltermcap -lalcom -lsun
#
## Sys3 -------------------------------------
#  (untested)
# OSFLAGS	= -D_BSD_SIGNALS
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM -D_BSD_SIGNALS
# LIBS	= -lm -lcurses -ltermcap -lalcom -lresolv
#
## SysV -------------------------------------
#  (untested)
# OSFLAGS	= -D_BSD_SIGNALS
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM -D_BSD_SIGNALS
# LIBS	= -lm -lcurses -ltermcap -lalcom -lresolv
#
## DEC 5000  -----------------------------------
#
# OSFLAGS	= -D_BSD_SIGNALS 
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -DALCOMM
# LIBS	= -lm -lcurses -ltermcap -lalcom 
#
## RS6000 -----------------------------------
#
# OSFLAGS	= -D_BSD_SIGNALS -D__TRS6000__
# LIBS	= -lm -lcurses -ltermcap
#
# or for almond ...
#
# OSFLAGS	= -D_BSD_SIGNALS -D__TRS6000__ -DALCOMM
# LIBS	= -lm -lcurses -ltermcap -lalcom 
#
##
## Optimization: set as appropriate for your compiler
## for debugging you should (usually) set this to -g
## to perform code optimization this setting is (usually) -O or -O<digit>
#
OPTIMIZ	= -g

## Flags to be passed to the linker (usually cc). Add any special requirements
## for your system. this is hopefully oniform enough that there is no need
## to include all of the possibilities under the OS section. if you are using
## optimization, you may want to add -s to strip symbol tables and thus create 
## a smaller binary.
#
LDFLAGS = $(OPTIMIZ)
#
# or for almond ...
#
# LDFLAGS	= $(OPTIMIZ) -L$(ALCLIBDIR)

## you shouldn't have to change anything below this line.
## (if you are lucky, and the gods of technology aren't angry with you...)
CFLAGS	= -w $(OPTIMIZ) -DINST=$(INST) $(OSFLAGS) 
#
# or for almond ...
#
# CFLAGS	= -w $(OPTIMIZ) -DINST=$(INST) $(OSFLAGS) -I$(ALCOMMDIR)/inc

## ALmond: uncomment the following lines if you want ALmond support
# and uncomment the -DALCOMM lines for you specific machine below
#
# ALCOMMDIR	= /net/user/cocteau/al/alcomm
# ALC_INCS	= $(ALCOMMDIR)/mlayer.h
# ALC_TOBJS	= tmonitor.o
# ALC_TSRCS	= tmonitor.c
# ALC_TINCS	= tmonitor.h
# ALCLIBDIR	= $(ALCOMMDIR)/src
# or (better )
# ALCLIBDIR	= $(HOME)/lib/$(ARCH)
# ALCLIB	= $(ALCLIBDIR)/libalcom.a

# the sim.pn.[oc] files are needed only for the CM5
# its entire contents are ifdef'd out if CM5 is not defined
# un comment the next lines for CM5
# CM5_OBJ	= sim.pn.o 
# CM5_SRC	= sim.pn.o 

EXTHDRS	      = /usr/include/arpa/inet.h \
		/usr/include/ctype.h \
		/usr/include/errno.h \
		/usr/include/fcntl.h \
		/usr/include/limits.h \
		/usr/include/malloc.h \
		/usr/include/math.h \
		/usr/include/memory.h \
		/usr/include/netdb.h \
		/usr/include/netinet/in.h \
		/usr/include/rpcsvc/ypclnt.h \
		/usr/include/stdio.h \
		/usr/include/stdlib.h \
		/usr/include/string.h \
		/usr/include/sys/errno.h \
		/usr/include/sys/fcntlcom.h \
		/usr/include/sys/param.h \
		/usr/include/sys/signal.h \
		/usr/include/sys/socket.h \
		/usr/include/sys/stat.h \
		/usr/include/sys/sysmacros.h \
		/usr/include/sys/time.h \
		/usr/include/sys/types.h \
		/usr/include/time.h

HDRS	      = declare.h extern.h portable.h tierra.h debug.h	\
		trequest.h phylo.h $(ALC_INCS)

CC            = cc
LINKER	      = cc 

MAKEFILE      = Makefile

PROGRAM	      = tierra 

OBJS      = bookeep.o \
		diskbank.o \
		rambank.o \
		genio.o \
		parse.o \
                frontend.o \
		instruct.o \
		memalloc.o \
		memtree.o \
		portable.o \
		queues.o \
		slicers.o \
		tierra.o \
		trand.o \
		tsetup.o \
		ttools.o \
		$(CM5_OBJ)\
		$(ALC_TOBJS)


SRCS      = bookeep.c \
		diskbank.c \
		rambank.c \
		genio.c \
		parse.c \
		frontend.c \
		instruct.c \
		memalloc.c \
		memtree.c \
		portable.c \
		queues.c \
		slicers.c \
		tierra.c \
		trand.c \
		tsetup.c \
		ttools.c \
		$(CM5_SRC)\
		$(ALC_TSRCS)


ARGOBJ	  = arg.o arg_inc.o

all:;
	@echo "   "
	@echo " Tierra Artificial Life system "
	@echo "   "
	@echo " Please edit this Makefile, "
	@echo " un-commenting the line(s) for your machine type / OS. "
	@echo " and choice of options. "
	@echo "   "
	@echo " Then say:  " 
	@echo " make programs  " 
	@echo "   " 


## for almond flavored tierra use the following target
# tierra: $(OBJS) $(ALCLIB)
## else
tierra: $(OBJS) 
	$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o tierra$(INST)

## for almond flavored tierra uncomment the following 2 lines
## make suer that there is a TAB before the second line
#$(ALCLIB):
#	(cd $(ALCOMMDIR)/src; make lib)

arg:      $(ARGOBJ) 
		$(LINKER) $(LDFLAGS) $(ARGOBJ) $(LIBS) -o arg

probe:;		cc -g probe.c -lm -o probe
clean:;		rm -f *.o  
#		rm -f $(ALCOMMDIR)/*.o

depend:;	mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		install -s $(PROGRAM) $(DEST)

programs:       tierra arg gene$(INST)

gene:;echo NO INST DEFINED!
gene1:;	(cd gb1; \
	../arg cv 0073gen.vir 73 0073aaa.tie ; \
	cp 0073gen.vir 0073.gen ; \
	../arg cv 0080gen.vir 80 0080aaa.tie ; \
	cp 0080gen.vir 0080.gen ; )

gene2:;	(cd gb2; \
	../arg cv 0095gen.vir 95 0095aaa.tie ; \
	cp 0095gen.vir 0095.gen ; )

gene3:;	(cd gb3; \
	../arg cv 0093gen.vir 93 0093aaa.tie ; \
	cp 0093gen.vir 0093.gen ; )

gene4:;	(cd gb4; \
	../arg cv 0082gen.vir 82 0082aaa.tie ; \
	cp 0082gen.vir 0082.gen ; )

tags:           $(HDRS) $(SRCS); ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

#distrib:	$(SRCS) $(HDRS) $(DOCS) arg.c Makefile
#		cp $(SRCS) $(HDRS) $(DOCS) arg.c Makefile $(DEST)/src/distrib
#		cd $(DEST)/src/distrib; 

$(DEST)/$(PROGRAM): $(PROGRAM)
		@make -f $(MAKEFILE) DEST=$(DEST) install

