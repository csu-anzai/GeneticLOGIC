life: life.o blit.o readin.o
	ln -o life life.o blit.o readin.o -lc

structures.b: structures.h
	cc -b -a +hstructures.b structures.h

blit.o: blit.c structures.b
	cc -b +istructures.b blit.c

life.o: life.c structures.b
	cc -b +istructures.b life.c

readin.o: readin.c structures.b
	cc -b +istructures.b readin.c
