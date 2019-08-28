# /*
# 
#         *********************** MATING **************************
#         *       Mating and Replacement Routine                  *
#         *********************************************************
# 
# * Copyright
# * Author: Joseph Culberson
# * Date: April 24, 1992
# * Permission is hereby granted to copy all or any part of
# * this program for free distribution.  The author's name
# * and this copyright notice must be included in any copy.
# * This program is provided ``as is'' without any express or implied
# * warranties.
# 
# * Comments and suggestions may be emailed to joe@cs.ualberta.ca
# * but this implies no obligation on the part of the author.
# 
# * Users are encouraged to modify and extend this program,
# * and such modifications should be commented with appropriate attribution.
# * This program should not be considered a final product, but rather it is hoped
# * that it will serve as a stimulus for further experimentation and development.
# 
# * Initial Version Created April 12, 1992.
# 
# 
# */

CC = cc
CFLAGS = -O

GIGA = cross.o global.o mate.o select.o evaluate.o giga.o init.o pool.o io.o\
	summary.o\
	f00.o f01.o f02.o f03.o f04.o f05.o f06.o fdj1.o fdj2.o fdj3.o fdj4.o\
	fdj5.o fbin.o fdgb.o fdl.o f07.o f08.o

giga: $(GIGA)
	$(CC) $(CFLAGS) -o giga $(GIGA) -lm

$(GIGA): giga.h

test: 
	lint cross.c global.c mate.c select.c evaluate.c giga.c init.c pool.c\
 io.c summary.c f00.c f01.c f02.c f03.c f04.c f05.c f06.c fdj1.c fdj2.c fdj3.c fdj4.c fdj5.c fbin.c fdgb.c fdl.c f07.c f08.c
