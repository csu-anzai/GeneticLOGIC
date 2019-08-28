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

all : GATSP genitor

GATSP : main.o gatsp.o tour.o edge.o tsp.o SIntVec.o
	$(CXX) $(LDFLAGS) GATSP main.o gatsp.o edge.o tour.o tsp.o SIntVec.o $(TKLIB) $(GNULIB) -lm
	strip GATSP

genitor : genitor.o tour.o edge.o tsp.o SIntVec.o
	$(CXX) $(LDFLAGS) genitor genitor.o edge.o tour.o tsp.o SIntVec.o $(TKLIB) $(GNULIB) -lm
	strip genitor

main.o : main.cc 
	$(CXX) -c $(CCFLAGS) main.cc
                                         
gatsp.o : gatsp.cc 
	$(CXX) -c $(CCFLAGS) gatsp.cc
                                         
genitor.o : genitor.cc 
	$(CXX) -c $(CCFLAGS) genitor.cc

tour.o : tour.cc tour.h
	$(CXX) -c $(CCFLAGS) tour.cc

edge.o : edge.cc edge.h
	$(CXX) -c $(CCFLAGS) edge.cc

tsp.o : tsp.cc tsp.h
	$(CXX) -c $(CCFLAGS) tsp.cc

SIntVec.o : SIntVec.cc SIntVec.h
	$(CXX) -c $(CCFLAGS) SIntVec.cc

