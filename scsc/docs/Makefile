SHELL	= /bin/sh

MKPS	= dvips -f -r0
LATEX	= latex
BIBTEX	= bibtex

REPORT	= scsc
REPRO	= repro

JUNK	= *.*log *.aux *.dvi *.blg *.bbl

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Documenatation
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
dvi:	report repro

report:
	$(LATEX) $(REPORT)
	-$(BIBTEX) $(REPORT)
	$(LATEX) $(REPORT)
	$(LATEX) $(REPORT)

repro:
	$(LATEX) $(REPRO)
	-$(BIBTEX) $(REPRO)
	$(LATEX) $(REPRO)
	$(LATEX) $(REPRO)

ps:	dvi $(REPORT).ps $(REPRO).ps

$(REPORT).ps: $(REPORT).dvi
	$(MKPS) $(REPORT).dvi >$(REPORT).ps
$(REPRO).ps: $(REPRO).dvi
	$(MKPS) $(REPRO).dvi >$(REPRO).ps

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#	Cleanup
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
clean:
	$(RM) $(JUNK)

clobber: clean
	$(RM) $(PRG) $(REPORT).ps $(REPRO).ps
