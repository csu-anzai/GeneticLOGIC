# -==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-
# $Id: Makefile,v 1.1 1993/11/24 17:56:26 joke Exp $
# -==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-

F77	= f77
FFLAGS	= -g -C -u

PRG	= sgatest
LIB	= libSGA.a

AR	= ar
ARFLAGS	= rv
RANLIB	= ranlib

SRCS	= sgatest.f crossover.f decodeparm.f flip.f gdecode.f generation.f \
	initialize.f initpop.f initreport.f movepop.f \
	mutation.f objfunc.f ran2.f random.f randomize.f \
	report.f rnd.f select.f sga.f statistics.f \
	storegen.f runti.f frist.f scalepop.f prescale.f

OBJS	= crossover.o decodeparm.o flip.o gdecode.o generation.o \
	initialize.o initpop.o initreport.o movepop.o \
	mutation.o objfunc.o ran2.o random.o randomize.o \
	report.o rnd.o select.o sga.o statistics.o \
	storegen.o runti.o frist.o scalepop.o prescale.o

JUNK	= *.trace

# -==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-
$(PRG):	$(LIB) $(PRG).o
	$(F77) $(FFLAGS) -o $(PRG) $(PRG).o -L. -lSGA

$(LIB):	$(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

# -==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-
crossover.o:	crossover.f
flip.o:		flip.f
gdecode.o:	gdecode.f
decodeparm.o:	decodeparm.f
generation.o:	generation.f
initialize.o:	initialize.f
initpop.o:	initpop.f
initreport.o:	initreport.f
movepop.o:	movepop.f
mutation.o:	mutation.f
objfunc.o:	objfunc.f
ran2.o:		ran2.f
random.o:	random.f
randomize.o:	randomize.f
report.o:	report.f
rnd.o:		rnd.f
select.o:	select.f
sga.o:		sga.f
sgatest.o:	sgatest.f
statistics.o:	statistics.f
storegen.o:	storegen.f
runti.o:	runti.f
frist.o:	frist.f
scalepop.o:	scalepop.f
prescale.o:	prescale.f

# -==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-
pretty:
	tgrind -lf77 $(SRCS)

# -==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-
clean:
	$(RM) $(OBJS) $(PRG).o $(JUNK)

clobber: clean
	$(RM) $(PRG) $(LIB)
