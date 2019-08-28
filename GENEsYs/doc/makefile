#
#	makefile	Ba, 21.Jun'91				SunOS 4.0.3
#
#	makefile for the GENEsYs-documentation

DOC		= GENEsYs
FIG		= fig

DVI		= $(DOC).dvi
BBL		= $(DOC).bbl
MBR		= $(DOC).tex 	 	\
		  $(DOC)-abs.tex 	\
		  $(DOC)-int.tex 	\
		  $(DOC)-pro.tex 	\
		  $(DOC)-ist.tex 	\
		  $(DOC)-opt.tex 	\
		  $(DOC)-rep.tex 	\
		  $(DOC)-exm.tex 	\
		  $(DOC)-mod.tex 	\
		  $(DOC)-f01c.tex 	\
		  $(DOC)-f12c.tex 	\
		  $(DOC)-ftabc.tex 	\
		  $(DOC)-ack.tex 	\
		  $(DOC)-fct.tex

GFX		= $(FIG)-mut1.ps	\
		  $(FIG)-mut2.ps	\
		  $(FIG)-cross.ps

LATEX		= latex
BIBTEX		= bibtex
GNUPLUS		= gnuplus
DVI2PS		= dvi2ps -o a4
VEC2PS		= vecps -eps
LP		= prt -c ls11lw

SUNDVI		= texsun
XDVI		= xdvi

.SUFFIXES:	.dvi .ps .vec .bbl .tex .bib .gp
.tex.dvi:
		$(LATEX)	$*
.bib.bbl:
		$(BIBTEX)	$(DOC)

.vec.ps:
		$(VEC2PS)	-outfile $@ $<
### 2.0		$(VEC2PS)	$< | pubps2eps >$@

$(DOC)-fct.tex:		../src/fct/f_*.c
			cd ../src/fct; mkfd; 

$(DVI) dvi:	$(MBR) $(GFX) $(BBL) $(PSZ)
		$(LATEX)	$(DOC)

final:		$(GFX) $(BBL) $(DVI)
		$(LATEX)	$(DOC)
		$(LATEX)	$(DOC)

$(BBL) bbl:	$(BIB)
		$(BIBTEX)	$(DOC)


print:		$(DVI)
		$(DVI2PS)	$(DOC)	| $(LP)

preview:	$(DVI)
		$(SUNDVI)	$(DOC) &
xpreview:	$(DVI)
		$(XDVI)		$(DOC) &

clean:
		-rm	*.aux *.log *.blg *.dlog
		-rm	PDclipboard.vec


chmod:
	chmod	660	*.aux *.tex
