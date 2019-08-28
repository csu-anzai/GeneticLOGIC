
OBJS = main.o io.o error.o storage.o pop.o opers.o timing.o mergeSorted.o roulette.o rand.o

# NOTE that algorithm can be compiled independently of representation
#   and problem
algorithm.o : $(OBJS)
	$(LD) -r -o algorithm.o $(OBJS)

$(OBJS) : main.h 
