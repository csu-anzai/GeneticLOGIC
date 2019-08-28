
###############################################################
#                                                             #
#   Copyright (c) 1992 Nicol N. Schraudolph                   #
#   Computer Science & Engineering, C-014                     #
#   University of California, San Diego                       #
#   La Jolla, CA 92093-0114                                   #
#                                                             #
#   Permission is hereby granted to copy all or any part of   #
#   this program for free distribution.   The author's name   #
#   and this copyright notice must be included in any copy.   #
#                                                             #
###############################################################

# Local object files you wish to include:
#
LOBJS =

############################
# CUSTOMIZE SECTION BELOW  #
# FOR EACH TARGET PLATFORM #
############################

# MACH must uniquely identify target platform:
#
MACH = `mach`

# CFLAGS are the flags given to the C compiler
# (add -DNOGAGX if you're not using ga and gx):
#
CFLAGS = -O

# DQ should be set to a double quote that will
# survive the command shell, i.e. it should be
# escaped if it is a special shell character:
#
DQ = \"

# DS should be set to your directory separator:
#
DS = /

# file naming conventions:
#
OBJ = .o   # suffix for object files
LIB = .a   # suffix for libraries
EXE =      # suffix for executables

# file handling commands:
#
RM = rm -f # command to delete a file
MD = mkdir # command to create a new directory
CP = cp    # command to copy a file to a directory

# compilation commands:
#
CC = cc    # the C compiler
AR = ar r  # the archiver/librarian


##########################
# DO NOT EDIT BELOW HERE #
##########################

# GAeval should be set from command line
#
GAeval = GAeval

$(GAeval)$(EXE): $(GAeval)_ga$(OBJ)
	$(CC) $(CFLAGS) -o $(GAeval)$(EXE) $(GAeval)_ga$(OBJ) $(LOBJS) $(GAUCSD)$(DS)bin$(DS)$(MACH)$(DS)ga$(LIB) -lm

$(GAeval)_ga.c: $(GAeval).c $(GAUCSD)$(DS)etc$(DS)wrapper
	awk -f $(GAUCSD)$(DS)etc$(DS)wrapper $(GAeval).c > $(GAeval)_ga.c


OBJS = best$(OBJ) checkpt$(OBJ) converge$(OBJ) cross$(OBJ) decode$(OBJ) \
	done$(OBJ) dpe$(OBJ) elitist$(OBJ) encode$(OBJ) error$(OBJ) \
	evaluate$(OBJ) gap$(OBJ) generate$(OBJ) init$(OBJ) input$(OBJ) \
	main$(OBJ) measure$(OBJ) mutate$(OBJ) random$(OBJ) restart$(OBJ) \
	schema$(OBJ) select$(OBJ) setflag$(OBJ)

HDRS = Makefile define.h global.h format.h

DFLAGS = -DDS=$(DQ)$(DS)$(DQ) \
	-DMF=$(DQ)$(GAUCSD)$(DS)bin$(DS)$(MACH)$(DS)Makefile$(DQ)

all : ga$(LIB) inset$(EXE) report$(EXE)

ga$(LIB) : $(OBJS)
	-$(RM) ga$(LIB)
	$(AR) ga$(LIB) $(OBJS)

$(OBJS) : $(HDRS)

main$(OBJ) : main.c
	$(CC) $(CFLAGS) $(DFLAGS) -c main.c

inset$(EXE) : inset.c random$(OBJ) $(HDRS)
	$(CC) $(CFLAGS) $(DFLAGS) -o inset$(EXE) inset.c random$(OBJ) -lm

report$(EXE) : report.c error$(OBJ) $(HDRS)
	$(CC) $(CFLAGS) $(DFLAGS) -o report$(EXE) report.c error$(OBJ)

install : ga$(LIB) report$(EXE) inset$(EXE)
	-$(MD) $(GAUCSD)$(DS)bin
	-$(MD) $(GAUCSD)$(DS)bin$(DS)$(MACH)
	$(CP) ga$(LIB) $(GAUCSD)$(DS)bin$(DS)$(MACH)
	-ranlib $(GAUCSD)$(DS)bin$(DS)$(MACH)$(DS)ga$(LIB)
	$(CP) inset$(EXE) $(GAUCSD)$(DS)bin$(DS)$(MACH)
	$(CP) report$(EXE) $(GAUCSD)$(DS)bin$(DS)$(MACH)
	$(CP) Makefile $(GAUCSD)$(DS)bin$(DS)$(MACH)

clean :
	-$(RM) *$(OBJ)
	-$(RM) ga$(LIB)
	-$(RM) inset$(EXE)
	-$(RM) report$(EXE)

.c$(OBJ) :
	$(CC) $(CFLAGS) -c $<

