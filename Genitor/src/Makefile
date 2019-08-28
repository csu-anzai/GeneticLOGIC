# ***************************************************************************
# 
# This makefile allows you to create libraries of fundamental Genitor 
# functions (files containing these functions begin with `ga_'.  
# You should make three libraries, one for each data type 
# (binary, integer, or floating point).
#
# make DATA=BIN
# make DATA=INT
# make DATA=FLOAT
#
# Although your applications must link with the appropriate library for the
# data type to be used, the function calls themselves should be identical
# regardless of the data type.  See the GENITOR CODE STRUCTURE document
# for details.
#
# ***************************************************************************


#**************************
# where are things located?
# *************************

# GaRoot is the only thing that you should want to alter.
GaRoot  = /mntf/fac/whitley/NEW_GENITOR/Genitor/

GaSrc   = $(GaRoot)src/ga/
GaInc   = $(GaRoot)include/ga/
GaLib   = $(GaRoot)lib/ga/


# *************************************
# One of the following must be defined:
#
# DATA=BIN
# DATA=INT
# DATA=FLOAT
#
# This can be done on the command line:
#   ` make library DATA=INT '
# *************************************


# ********************************************************
# application independent, data type independent functions
# ********************************************************
GaObjects = $(GaSrc)ga_xdr.o    $(GaSrc)ga_params.o $(GaSrc)ga_signals.o \
			$(GaSrc)ga_status.o $(GaSrc)ga_pool.o   $(GaSrc)ga_selection.o \
			$(GaSrc)ga_copy.o   $(GaSrc)ga_random.o

# ************************
# library of ga_ functions
# ************************

LibName = $(GaLib)libgacsu$(DATA)

library : $(LibName).a

$(LibName).a: $(GaObjects)
	ar rv $(LibName).a $(GaObjects)
	ranlib $(LibName).a
	rm $(GaSrc)*.o


# ********************************************************
# application independent, data type independent functions
# ********************************************************
$(GaSrc)ga_xdr.o : $(GaSrc)ga_xdr.c $(GaInc)ga_xdr.h $(GaInc)gene.h
	cc -c -O $(GaSrc)ga_xdr.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_xdr.o

$(GaSrc)ga_params.o : $(GaSrc)ga_params.c $(GaInc)ga_params.h \
					  $(GaInc)gene.h $(GaInc)ga_global_extern.h \
					  $(GaInc)ga_status.h 
	cc -c -O $(GaSrc)ga_params.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_params.o

$(GaSrc)ga_signals.o : $(GaSrc)ga_signals.c $(GaInc)ga_status.h
	cc -c -O $(GaSrc)ga_signals.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_signals.o

$(GaSrc)ga_status.o : $(GaSrc)ga_status.c $(GaInc)ga_status.h \
					  $(GaInc)gene.h $(GaInc)ga_global_extern.h \
					  $(GaInc)ga_pool.h
	cc -c -O $(GaSrc)ga_status.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_status.o

$(GaSrc)ga_pool.o : $(GaSrc)ga_pool.c $(GaInc)ga_status.h $(GaInc)gene.h \
					$(GaInc)ga_random.h $(GaInc)ga_pool.h $(GaInc)ga_copy.h
	cc -c -O $(GaSrc)ga_pool.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_pool.o

$(GaSrc)ga_selection.o : $(GaSrc)ga_selection.c $(GaInc)gene.h \
						 $(GaInc)ga_random.h $(GaInc)ga_copy.h
	cc -c -O $(GaSrc)ga_selection.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_selection.o

$(GaSrc)ga_copy.o : $(GaSrc)ga_copy.c $(GaInc)ga_copy.h $(GaInc)gene.h
	cc -c -O $(GaSrc)ga_copy.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_copy.o

$(GaSrc)ga_random.o : $(GaSrc)ga_random.c $(GaInc)ga_random.h $(GaInc)gene.h
	cc -c -O $(GaSrc)ga_random.c -D$(DATA) \
	   -I$(GaInc) \
	   -o $(GaSrc)ga_random.o
