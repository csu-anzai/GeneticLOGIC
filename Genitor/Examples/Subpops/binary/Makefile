# *************************
# where are things located?
# *************************

# Genitor root directory

# this is the only thing you should want to alter
  GaRoot = /mntf/fac/whitley/NEW_GENITOR/Genitor/

# include file directories

  GaInc = $(GaRoot)include/ga/
  OpInc = $(GaRoot)include/op/

# library

  GaLib = $(GaRoot)lib/ga/

# operator source code directory
	 
  OpSrc = $(GaRoot)src/op/


# *****************************************
# application specific, data type dependent 
# *****************************************

    DataType = BIN
 MainProgram = main_bin
EvalFunction = eval_bin
     OpName1 = op_red_surrog
	 OpName2 = op_adapt_mutate
   Operator1 = $(OpSrc)$(OpName1)
   Operator2 = $(OpSrc)$(OpName2)
    ProgName = Genitor$(DataType)


# ********************************
# let's make an executable program
# ********************************

all: $(ProgName)

$(ProgName) : $(MainProgram).o $(EvalFunction).o  $(Operator1).o $(Operator2).o
	cc -o $(ProgName) \
		  $(MainProgram).o $(EvalFunction).o  $(Operator1).o  $(Operator2).o \
		  -L$(GaLib) -lgacsu$(DataType) \
		  -lm
	rm $(Operator1).o
	rm $(Operator2).o

# *****************
# genetic operators 
# *****************

$(Operator1).o: $(Operator1).c $(OpInc)$(OpName1).h \
				$(GaInc)ga_random.h $(GaInc)gene.h
	cc -c -g $(Operator1).c -I$(GaInc) -I$(OpInc) -D$(DataType) \
	-o $(Operator1).o


$(Operator2).o: $(Operator2).c $(OpInc)$(OpName2).h \
				$(GaInc)ga_random.h $(GaInc)gene.h
	cc -c -g $(Operator2).c -I$(GaInc) -I$(OpInc) -D$(DataType) \
	-o $(Operator2).o


# *******************************************************
# application independent, data type independent function
# information to include in main() driver file
# *******************************************************

GaHeaders = $(GaInc)ga_random.h  $(GaInc)gene.h      $(GaInc)ga_status.h \
			$(GaInc)ga_params.h  $(GaInc)ga_pool.h   $(GaInc)ga_selection.h \
			$(GaInc)ga_signals.h $(GaInc)ga_global.h $(GaInc)ga_xdr.h


# *****************************************
# application specific main driver function
# *****************************************
$(MainProgram).o : $(MainProgram).c \
                   $(GaHeaders) \
				   $(EvalFunction).h \
				   $(OpInc)$(OpName1).h \
				   $(OpInc)$(OpName2).h
	cc -c -g $(MainProgram).c -I$(GaInc) -I$(OpInc) -D$(DataType) 


# ****************************************
# application specific evaluation function
# ****************************************
$(EvalFunction).o : $(EvalFunction).c $(GaInc)gene.h $(OpInc)$(OpName1).h
	cc -c -g $(EvalFunction).c -I$(GaInc) -I$(OpInc) -D$(DataType) 
