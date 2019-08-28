# your home directory
HOME =/uac/gds/tang028/TOLKIEN
#
# directory in which you put TOLKIEN include file
#
TKINCLUDEPATH = $(HOME)/include
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
CCFLAGS = -I$(TKINCLUDEPATH) -I$(MYGNUINCPATH) -DUSE_LIBGXX_INLINES -O
#
#

GNUOBJS = errhndl.o SmplStat.o IntVec.o ACG.o MLCG.o Normal.o DblVec.o 
GNULIB = gnulib.a

all : $(GNULIB)

$(GNULIB) : $(GNUOBJS)
	$(AR) $(ARFLAGS) gnulib.a $(GNUOBJS)
	$(AR) t gnulib.a 
	ranlib gnulib.a

ACG.o : $(GNUSRCPATH)/ACG.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/ACG.cc
 
MLCG.o : $(GNUSRCPATH)/MLCG.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/MLCG.cc
 
RNG.o : $(GNUSRCPATH)/RNG.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/RNG.cc
 
IntVec.o : $(GNUSRCPATH)/IntVec.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/IntVec.cc

DblVec.o : $(GNUSRCPATH)/DblVec.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/DblVec.cc

SmplStat.o : $(GNUSRCPATH)/SmplStat.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/SmplStat.cc

Normal.o : $(GNUSRCPATH)/Normal.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/Normal.cc

errhndl.o : $(GNUSRCPATH)/errhndl.cc
	$(CXX) -c $(CCFLAGS) $(GNUSRCPATH)/errhndl.cc
 
