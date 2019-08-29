#
# VAMV Makefile
# Copyright 1990 Rick McGowan
#
MISC  = README Vamv.Doc Makefile Lim.File Time.Stamp .LastRun
HDRS  = prog.h
SRC   = doscr.c main.c move.c mute.c util.c

# The following 2 lines are used when compiling normally:
CFLAGS= -O
STR   = -s

# The following 2 lines are used when compiling for profiling:
# CFLAGS= -O -g -DMARK -p
# STR   =

#
# The following LIBS is for SVR3 (shared library):
#
LIBS  = -lc_s

# LIBS  =

OBJ   = doscr.o main.o move.o mute.o util.o

vamv:	$(OBJ)
	cc $(CFLAGS) $(STR) -o vamv $(OBJ) $(LIBS)

$(OBJ): prog.h

cpio:	$(SRC) $(HDRS) $(MISC)
	/bin/ls $(SRC) $(HDRS) $(MISC) | cpio -ovc > VAMV.cpio

shar:	$(SRC) $(HDRS) $(MISC)
	shar $(SRC) $(HDRS) $(MISC) > VAMV.shar

Time.Stamp:
	echo "CPIO/SHAR Built on \c" > Time.Stamp
	date >> Time.Stamp

clean:
	/bin/rm $(OBJ)
