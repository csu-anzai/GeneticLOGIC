CFLAGS = -O
PROBLEMDIR = noLibProblems
PROBLEMNAME = problem
EXENAME=$(PROBLEMNAME)

library : alg.o repOper.o pLib.o
	($(CC) $(CFLAGS) -o $(REPDIR)/$(EXENAME).run $(REPDIR)/REP/repAndOpers.o problem.o ALG/algorithm.o -lm; rm problem.o)

noLibrary : alg.o pNoLib.o
	($(CC) $(CFLAGS) -o $(PROBLEMDIR)/$(EXENAME).run problem.o ALG/algorithm.o -lm; rm problem.o)

alg.o :
	(cd ALG; make algorithm.o CC=$(CC) LD=$(LD) CFLAGS=$(CFLAGS))


repOper.o :
	(cd $(REPDIR)/REP; make repAndOpers.o CC=$(CC) LD=$(LD) CFLAGS=$(CFLAGS); cd ../..)

pLib.o :
	$(CC) $(CFLAGS) -I$(REPDIR)/REP -IALG -c -o problem.o $(REPDIR)/$(PROBLEMNAME).c

pNoLib.o :
	$(CC) $(CFLAGS) -IALG -c -o problem.o $(PROBLEMDIR)/$(PROBLEMNAME).c


