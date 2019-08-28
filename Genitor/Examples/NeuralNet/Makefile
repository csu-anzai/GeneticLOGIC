# *************************
# where are things located?
# *************************

# Genitor root directory

# this is the only thing you should want to alter
  GaRoot = /home/bach1/chrisw/personal/thesis/

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

    DataType = FLOAT
 MainProgram = main_float
EvalFunction = eval_float
     OpName1 = op_red_surrog
   Operator1 = $(OpSrc)$(OpName1)
    ProgName = Genitor$(DataType)


# ********************************
# let's make an executable program
# ********************************

all: $(ProgName)

$(ProgName) : $(MainProgram).o $(EvalFunction).o  $(Operator1).o
	cc -o $(ProgName) \
		  $(MainProgram).o $(EvalFunction).o  $(Operator1).o  \
		  -L$(GaLib) -lgacsu$(DataType) \
		  -lm
	rm $(Operator1).o

# *****************
# genetic operators 
# *****************

$(Operator1).o: $(Operator1).c $(OpInc)$(OpName1).h \
				$(GaInc)ga_random.h $(GaInc)gene.h
	cc -c -g $(Operator1).c -I$(GaInc) -I$(OpInc) -D$(DataType) \
	-o $(Operator1).o


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
				   $(OpInc)$(OpName1).h 
	cc -c -g $(MainProgram).c -I$(GaInc) -I$(OpInc) -D$(DataType) 


# ****************************************
# application specific evaluation function
# ****************************************
$(EvalFunction).o : $(EvalFunction).c $(GaInc)gene.h $(OpInc)$(OpName1).h
	cc -c -g $(EvalFunction).c -I$(GaInc) -I$(OpInc) -D$(DataType) 
