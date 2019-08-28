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

rrmf: rrmf.o $(TKLIB) $(GNULIB)
	$(CXX) $(LDFLAGS) rrmf rrmf.o $(TKLIB) $(GNULIB) -lm   
	strip rrmf

rrjh: rrjh.o $(TKLIB) $(GNULIB)
	$(CXX) $(LDFLAGS) rrjh rrjh.o $(TKLIB) $(GNULIB) -lm   
	strip rrjh

rrmf.o : rrmf.cc 
	$(CXX) -c $(CCFLAGS) rrmf.cc

rrjh.o : rrjh.cc 
	$(CXX) -c $(CCFLAGS) rrjh.cc
