HOME =/uac/gds/tang028/TOLKIEN
#
# directory in which you put TOLKIEN include file
#
INCLUDEPATH = $(HOME)/include
#
# the following two variables, MYGNUINCPATH and GNUSRCPATH, define
# the paths in which the modified gnu files supplied by
# TOLKIEN is stored
#
MYGNUINCPATH = $(HOME)/gnu/include
GNUSRCPATH = $(HOME)/gnu/src
#
# directory for TOLKIEN's source files
#
TKSRCPATH = $(HOME)/src
#
#
LDFLAGS = -o
CXX     = g++
AR = ar
ARFLAGS = rv
#
#
CCFLAGS = -I$(INCLUDEPATH) -I$(MYGNUINCPATH) -DUSE_LIBGXX_INLINES -O
#
#
LIBPATH = /public/anthony
TKLIB = $(LIBPATH)/tolkien.a
GNULIB = $(LIBPATH)/gnulib.a

all : dcga

dcga : dcga.o mmfuncs.o $(TKLIB) $(GNULIB)
	$(CXX) $(LDFLAGS) dcga dcga.o mmfuncs.o $(TKLIB) $(GNULIB) -lm
	strip dcga

mmfuncs.o : mmfuncs.cc
	$(CXX) -c $(CCFLAGS) mmfuncs.cc

dcga.o : dcga.cc
	$(CXX) -c $(CCFLAGS) dcga.cc

