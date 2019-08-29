#/****************************************************************/
#/*															  	 */
#/* makefile for breeder										 */
#/*																 */
#/****************************************************************/

INC			= -I/usr/include/xview
SLIB		= -lsuntool -lsunwindow -lpixrect -lm
XLIB		= -lxview -lX11 -lm

breeder : Breed.o Make_Cntrl.o Make_Draw.o Make_Buttons.o Grow.o Noise.o
	cc -g  Breed.o Make_Cntrl.o Make_Draw.o Make_Buttons.o Grow.o Noise.o -o breeder $(XLIB)


Breed.o : Breed.c Control.h GA.h Curves.h
	cc -c -g  $(INC) Breed.c -o Breed.o

Make_Cntrl.o : Make_Cntrl.c Control.h
	cc -c -g  $(INC) Make_Cntrl.c -o Make_Cntrl.o

Make_Draw.o : Make_Draw.c Control.h GA.h
	cc -c -g  $(INC) Make_Draw.c -o Make_Draw.o

Make_Buttons.o : Make_Buttons.c Control.h Text_Tokens.h GA.h
	cc -c -g  $(INC) Make_Buttons.c -o Make_Buttons.o

Grow.o : Grow.c Control.h GA.h Curves.h
	cc -c -g  $(INC) Grow.c -o Grow.o

Noise.o : Noise.c
	cc -c -g  $(INC) Noise.c -o Noise.o
