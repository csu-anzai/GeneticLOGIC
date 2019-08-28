#
# Makefile for MGA-C
#
CC=cc
#CFLAGS= -O -w -p -lm -g
CFLAGS= -lm -O

mga: main.o generate.o initial.o report.o operators.o objfunc.o functions.o \
	stats.o supports.o utility.o random.o 
	$(CC) $(CFLAGS) -o mga main.o generate.o initial.o report.o \
			operators.o objfunc.o functions.o stats.o  \
			supports.o utility.o random.o 

main.o: main.c mga.h mga.ext mga.def 
	$(CC) $(CFLAGS) -c main.c

generate.o: generate.c mga.ext mga.def
	$(CC) $(CFLAGS) -c generate.c

initial.o: initial.c mga.ext mga.def 
	$(CC) $(CFLAGS) -c initial.c

report.o: report.c mga.ext mga.def 
	$(CC) $(CFLAGS) -c report.c

operators.o: operators.c mga.ext mga.def
	$(CC) $(CFLAGS) -c operators.c
	
objfunc.o: objfunc.c mga.ext mga.def
	$(CC) $(CFLAGS) -c objfunc.c

functions.o: functions.c mga.ext mga.def
	$(CC) $(CFLAGS) -c functions.c

stats.o: stats.c mga.ext mga.def
	$(CC) $(CFLAGS) -c stats.c

supports.o: supports.c mga.ext mga.def
	$(CC) $(CFLAGS) -c supports.c

utility.o: utility.c
	$(CC) $(CFLAGS) -c utility.c

random.o: random.c mga.ext
	$(CC) $(CFLAGS) -c random.c

mga.h: mga.def

mga.ext: mga.def mga.h


clean:
	rm -f *.o mga
