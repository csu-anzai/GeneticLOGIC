#/****************************************************************/
#/*															  	 */
#/* makefile for breeder										 */
#/*																 */
#/****************************************************************/

breeder : Breed.o Make_Cntrl.o Make_Draw.o Make_Buttons.o Grow.o Noise.o
	cc -g Breed.o Make_Cntrl.o Make_Draw.o Make_Buttons.o Grow.o Noise.o -o breeder -lsuntool -lsunwindow -lpixrect -lm

Breed.o : Breed.c Control.h GA.h Curves.h
	cc -c -g Breed.c -o Breed.o

Make_Cntrl.o : Make_Cntrl.c Control.h
	cc -c -g Make_Cntrl.c -o Make_Cntrl.o

Make_Draw.o : Make_Draw.c Control.h GA.h
	cc -c -g Make_Draw.c -o Make_Draw.o

Make_Buttons.o : Make_Buttons.c Control.h Text_Tokens.h GA.h
	cc -c -g Make_Buttons.c -o Make_Buttons.o

Grow.o : Grow.c Control.h GA.h Curves.h
	cc -c -g Grow.c -o Grow.o

Noise.o : Noise.c
	cc -c -g Noise.c -o Noise.o
