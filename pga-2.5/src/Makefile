CC = gcc
SRCS = cross.c eval.c help.c main.c mutate.c reprod.c select.c
OBJS = cross.o eval.o help.o main.o mutate.o reprod.o select.o

CFLAGS= -g
LDFLAGS= -lcurses -ltermcap -lm

pga:  $(OBJS) pga.h
	$(CC) -o pga $(OBJS) $(LDFLAGS)

clean:
	rm -f *.o pga


cross.o: cross.c pga.h
	$(CC) -c $(CFLAGS) cross.c

eval.o: eval.c pga.h
	$(CC) -c $(CFLAGS) eval.c

help.o: help.c pga.h
	$(CC) -c $(CFLAGS) help.c

main.o: main.c pga.h screen.h
	$(CC) -c $(CFLAGS) main.c

mutate.o: mutate.c pga.h
	$(CC) -c $(CFLAGS) mutate.c

reprod.o: reprod.c pga.h
	$(CC) -c $(CFLAGS) reprod.c

select.o: select.c pga.h
	$(CC) -c $(CFLAGS) select.c

