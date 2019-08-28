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

Maze: Maze.o $(TKLIB) $(GNULIB)
	$(CXX) $(LDFLAGS) Maze Maze.o $(TKLIB) $(GNULIB) -lm
	strip Maze

mpcls: mpcls.o $(TKLIB) $(GNULIB)
	$(CXX) $(LDFLAGS) mpcls mpcls.o $(TKLIB) $(GNULIB) -lm
	strip mpcls

dejong: dejong.o $(TKLIB) $(GNULIB) 
	$(CXX) $(LDFLAGS) dejong dejong.o $(TKLIB) $(GNULIB) -lm 
	strip dejong

xovertst: xovertst.o $(TKLIB) $(GNULIB) 
	$(CXX) $(LDFLAGS) xovertst xovertst.o $(TKLIB) $(GNULIB) -lm 
	strip xovertst

Maze.o : Maze.cc 
	$(CXX) -c $(CCFLAGS) Maze.cc

mpcls.o : mpcls.cc 
	$(CXX) -c $(CCFLAGS) mpcls.cc
                                         
dejong.o : dejong.cc 
	$(CXX) -c $(CCFLAGS) dejong.cc

xovertst.o : xovertst.cc 
	$(CXX) -c $(CCFLAGS) xovertst.cc
