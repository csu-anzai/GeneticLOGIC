#
#	Makefile for SES
#
#		Joachim Sprave, 09-12-92
#

### commands & options

# Linux
#CC      = gcc -m486 -ansi -Wall -DPOSIX_SOURCE
#LNS     = ln -sf

# SunOs
CC      = gcc -ansi
LNS     = ln -sf

CFLAGS  = -finline-functions -fomit-frame-pointer -O6 -I$(INCDIR)  $(DEFS)
# CFLAGS  = -ggdb -I$(INCDIR)  $(DEFS)
LFLAGS  = -lm



### paths

SRCDIR  = source
INCDIR  = source
OBJDIR  = source
BINDIR  = .



### targets

SESOBJ  = $(OBJDIR)/cross.o \
          $(OBJDIR)/cycle.o \
          $(OBJDIR)/dump.o \
          $(OBJDIR)/generate.o \
	  $(OBJDIR)/getparam.o\
          $(OBJDIR)/init.o \
          $(OBJDIR)/main.o \
          $(OBJDIR)/measure.o \
          $(OBJDIR)/mutate.o \
          $(OBJDIR)/rand.o

f       = default

FUNC    = $(f).dir/$(f)


### rules


all: $(FUNC) tkjoe

clean:
	rm -f core $(FUNC).o $(FUNC) $(SESOBJ)
	(cd $(BINDIR); rm -f core)

veryclean:
	rm -f core $(FUNC).o $(FUNC) $(SESOBJ)
	(cd $(BINDIR); rm -f core)
	rm -f \
		*.ps \
		*.err \
		*.log \
		*.bst \
		*.dat \
		*.dir/core \
		*.dir/*.o \
		*.dir/*.ps \
		*.dir/*.dat \
		*.dir/*.dmp \
		*.dir/*.log \
		*.dir/*.err \
		*.dir/*.bst
	for i in *.dir; do \
		rm -f $$i/`basename $$i .dir` $$i/`basename $$i .dir`.o; \
	done 
	rm -f tkjoe

$(SESOBJ): $(INCDIR)/es.h

$(OBJDIR)/main.o $(OBJDIR)/getparam.o: $(INCDIR)/getparam.h

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(INCDIR)/lice.h
	$(CC) -o $@ -c $(CFLAGS) $< 

dump.o : $(PIXHDR)

$(FUNC).o: $(FUNC).c $(FUNC).h
	$(CC) -o $@ -c -I$(f).dir $(CFLAGS) $< 
	
$(FUNC): $(FUNC).o $(SESOBJ)
	$(CC) $(CFLAGS) -o $(FUNC) $(FUNC).o $(SESOBJ) $(LFLAGS)


#
# If 
#	make tkjoe 
# doesn't work, replace `which wish` by the absolute path to Tk/Tcl's 
# window shell, e.g.
#
# tkjoe: tkjoe.tcl
#	sed -e s@WISH@/usr/local/bin/wish@ <tkjoe.tcl >tkjoe
#	chmod u+x tkjoe
#

tkjoe: tkjoe.tcl
	sed -e s@WISH@`which wish`@ <tkjoe.tcl >tkjoe
	chmod u+x tkjoe
