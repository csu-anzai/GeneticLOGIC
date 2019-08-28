SCS	= ../scs

FIGS	= fig-6.18	\
	fig-6.19	\
	fig-6.20	\
	fig-6.21	\
	fig-6.22

JUNK	= *.log *.plt *.ps


figs:	$(FIGS)

export: $(FIGS)
	cp *.ps ../docs

fig-6.18:	fig-6.18.gnu
	@echo "Creating figure 6.18..."
	$(SCS) -b \
	-c data/fig-6.18/perfect.dta \
	-d data/fig-6.18/detec.dta \
	-e data/fig-6.18/environ.dta \
	-g data/fig-6.18/ga.dta \
	-r data/fig-6.18/reinf.dta \
	-t data/fig-6.18/time.dta \
	-l fig-6.18.log \
	-p fig-6.18.plt \
	-s 0.3333 \
	-T 2000
	gnuplot fig-6.18.gnu
	@echo "*** done."
	@echo

fig-6.19:	fig-6.19.gnu
	@echo "Creating figure 6.19..."
	$(SCS) -b \
	-c data/fig-6.19/lessthan.dta \
	-d data/fig-6.19/detec.dta \
	-e data/fig-6.19/environ.dta \
	-g data/fig-6.19/ga.dta \
	-r data/fig-6.19/reinf.dta \
	-t data/fig-6.19/time.dta \
	-l fig-6.19.log \
	-p fig-6.19.plt \
	-s 0.3333 \
	-T 2000
	gnuplot fig-6.19.gnu
	@echo "*** done."
	@echo

fig-6.20:	fig-6.20.gnu
	@echo "Creating figure 6.20..."
	$(SCS) -b \
	-c data/fig-6.20/lessthan.dta \
	-d data/fig-6.20/detec.dta \
	-e data/fig-6.20/environ.dta \
	-g data/fig-6.20/ga.dta \
	-r data/fig-6.20/reinf.dta \
	-t data/fig-6.20/time.dta \
	-l fig-6.20.log \
	-p fig-6.20.plt \
	-s 0.3333 \
	-T 2000
	gnuplot fig-6.20.gnu
	@echo "*** done."
	@echo

fig-6.21:	fig-6.21.gnu
	@echo "Creating figure 6.21..."
	$(SCS) -b \
	-c data/fig-6.21/class100.dta \
	-d data/fig-6.21/detec.dta \
	-e data/fig-6.21/environ.dta \
	-g data/fig-6.21/ga.dta \
	-r data/fig-6.21/reinf.dta \
	-t data/fig-6.21/time.dta \
	-l fig-6.21.log \
	-p fig-6.21.plt \
	-s 0.3333 \
	-T 45000
	gnuplot fig-6.21.gnu
	@echo "*** done."
	@echo

fig-6.22:	fig-6.22.gnu
	@echo "Creating figure 6.22..."
	$(SCS) -b \
	-c data/fig-6.22/class100.dta \
	-d data/fig-6.22/detec.dta \
	-e data/fig-6.22/environ.dta \
	-g data/fig-6.22/ga.dta \
	-r data/fig-6.22/reinf.dta \
	-t data/fig-6.22/time.dta \
	-l fig-6.22.log \
	-p fig-6.22.plt \
	-s 0.3333 \
	-T 45000
	gnuplot fig-6.22.gnu
	@echo "*** done."
	@echo

clean:
	$(RM) $(JUNK)
