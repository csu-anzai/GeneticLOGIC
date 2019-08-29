/*
This code is MINE MINE MINE!
If you would like to use it, then AT LEAST leave this header on it!
A donation for whatever it's worth to you would be greatly appreciated.
NOT for resale unless otherwise arranged for with myself.
Private use only.

	Richard H. Clark
	14550 Triadelphia Rd.
	Glenelg, MD 21737

*/

/*
A rudimentary curses type package that actually works....
Note that there is only ONE window, the WHOLE SCREEN.
*/

#include <stdio.h>
#include <stdlib.h>
#include <graph.h>
#include <conio.h>
#include <time.h>
#include "devo.h"

struct videoconfig myscreen;
time_t *ptime;
time_t time1;
time_t time2;
time_t time3;

 
static char inscreen[WYSIZE*WXSIZE*3];
static char outscreen[WYSIZE*WXSIZE*3];
static int initialed = 0;
static int deffore = 7;
static int defback = 0;
static int curx,cury = 0;
static int minx[WYSIZE];
static int miny = (WYSIZE-1);
static int maxx[WYSIZE];
static int maxy = 0;
static int lastfore = 7;
static int lastback = 0;
static int lastrow;
static int lastcol;
static char obuf[500];
static int ocount;
static int clearit = 0;

/* WINDOW *************************************************************/
w_initscreen()
{

    initialed = 1;
    deffore = 7;
    defback = 0;
    lastfore = 7;
    lastback = 0;
    ocount = 0;
    do_clear();
}

/* WINDOW *************************************************************/
w_normal()
{
}

/* WINDOW *************************************************************/
w_raw()
{
}

/* WINDOW *************************************************************/
w_setcolor(fg,bg)
int fg,bg;
{

    if(initialed == 0){
         my_initscreen();
    }

    if((fg < 0)||(bg < 0))
	 return;
    if((fg > 15)||(bg > 15))
         return;
    if(fg == (bg & 0x07)){
         return;
    }

    deffore = fg;
    defback = bg;
}

/* WINDOW *************************************************************/
w_clear()
{

    if(initialed == 0){
         my_initscreen();
    }else{
         do_clear();
    }
    clearit = 1;
}

/* WINDOW *************************************************************/
w_move(row,col)
int row,col;
{

    if(initialed == 0){
         my_initscreen();
    }

    if((row<0)||(col<0))
         return;
    if((row>(WYSIZE-1))||(col>(WXSIZE-1)))
         return;

    cury = row;
    curx = col;
}

/* WINDOW *************************************************************/
w_addstr(carp)
char *carp;
{
int i;
char *cp;

    cp = carp;

    for(;*cp != 0x00;){
         w_addchar(*cp);
         cp++;
    }
}

/* WINDOW *************************************************************/
w_addchar(car)
char car;
{
register int indx;

    if(car == '\n'){
         curx = 0;
         cury++;
         if(cury > (WYSIZE-1))
              cury = 0;
         return;
    }
    indx = (cury*WXSIZE*3)+(curx*3);
    inscreen[indx] = car;
    inscreen[indx+1] = deffore;
    inscreen[indx+2] = defback;

    if(miny > cury)
         miny = cury;
    if(maxy < cury)
         maxy = cury;
    if(minx[cury] > curx)
         minx[cury] = curx;
    if(maxx[cury] < curx)
         maxx[cury] = curx;

    curx++;
    if(curx > (WXSIZE-1)){
         curx = 0;
    }
}

/* WINDOW *************************************************************/
w_default()
{
	_setvideomode(_DEFAULTMODE);
	my_setcolor(deffore,defback);
	my_clear();
	my_setcolor(deffore,defback);
}

/* WINDOW *************************************************************/
w_refresh()
{
int i,j,ix;
register int idx;

    if(initialed == 0){
         my_initscreen();
         my_clear();
         return;
    }

    if(clearit != 0){
         my_clear();
         clearit = 0;
    }

    if(miny > maxy)
         return;  /* No Changes have been made */

    lastrow = -2;
    lastcol = -2;
    lastfore = -2;
    lastback = -2;

    for(i=miny;i<(maxy+1);i++){ /* Check each line for mods */
         if(minx[i] <= maxx[i]){
              ix = (i*WXSIZE*3);
              for(j=minx[i];j<(maxx[i]+1);j++){
                   idx = ix + (j*3);
                   if((inscreen[idx]!=outscreen[idx])
                     ||(inscreen[idx+1]!=outscreen[idx+1])
                     ||(inscreen[idx+2]!=outscreen[idx+2])){
/*
Can do alot of processing here...
Especially since i/o is slow, and we want to avoid it.
*/
                        my_add_to_buffer(inscreen[idx],inscreen[idx+1],inscreen[idx+2],i,j);
                        outscreen[idx]=inscreen[idx];
                        outscreen[idx+1]=inscreen[idx+1];
                        outscreen[idx+2]=inscreen[idx+2];
                   }
              }
              maxx[i] = 0;
              minx[i] = (WXSIZE-1);
         }
    }
    my_flush();
    my_move(cury,curx); /*Put cursor at end of last known output*/
    maxy = 0;
    miny = (WYSIZE-1);
}

/* IMMEDIATE ****************************/
my_setcolor(fg,bg)
int fg,bg;
{
long bgg;
int fgg;

    if(initialed == 0){
         my_initscreen();
    }

    bgg = (bg & 0x07);
    fgg = (fg & 0x0f);

    if((fg < 0)||(bg < 0))
         return;
    if((fg > 15)||(bg > 15))
         return;
    if(fg == (bg & 0x07)){
         return;
    }
    if(bg >= 8)
	fgg = (fgg | 0x010);

    _setbkcolor(bgg);
    _settextcolor(fgg);
    _setcolor(fgg);

}

/* IMMEDIATE ****************************/
my_clear()
{

    if(initialed == 0){
         my_initscreen();
    }

    my_setcolor(7,0);

    _clearscreen(_GCLEARSCREEN);
    _settextposition(1,1);

}

/* IMMEDIATE ****************************/
my_move(row,col)
int row,col;
{

    if(initialed == 0){
         my_initscreen();
    }

    if((row<0)||(col<0))
         return;
    if((row>(WYSIZE-1))||(col>(WXSIZE-1)))
         return;


    _settextposition(row+1,col+1);

}


/* LOCAL - Do not use ! */
my_initscreen()
{

    w_initscreen();
    _getvideoconfig(&myscreen);
    _setvideomode(SCREEN_MODE);
    my_setcolor(deffore,defback);
    my_clear();
    my_setcolor(deffore,defback);
}

/* LOCAL - Do not use ! */
do_clear()
{
int i,j;

    curx = 0;
    cury = 0;
    miny = (WYSIZE-1);
    maxy = 0;

    for(i=0;i<WYSIZE;i++){
         maxx[i] = 0;
         minx[i] = (WXSIZE-1);
         for(j=0;j<WXSIZE;j++){
              inscreen[(i*WXSIZE*3)+(j*3)] = 0x00;
              inscreen[(i*WXSIZE*3)+(j*3)+1] = 7;
              inscreen[(i*WXSIZE*3)+(j*3)+2] = 0;
              outscreen[(i*WXSIZE*3)+(j*3)] = 0x00;
              outscreen[(i*WXSIZE*3)+(j*3)+1] = 7;
              outscreen[(i*WXSIZE*3)+(j*3)+2] = 0;
         }
    }
}

/* LOCAL - Do not use ! */
my_flush()
{

    if(ocount <= 0)
         return;

    _outtext(obuf);

    ocount = 0;
    lastrow = -2;
    lastcol = -2;
    lastfore = -2;
    lastback = -2;
}

/* LOCAL - Do not use ! */
my_add_to_buffer(inchar,infore,inback,inrow,incol)
unsigned char inchar,infore,inback;
int inrow,incol;
{


    if(ocount > 45)
         my_flush();

    if((inrow != lastrow)||(incol != (lastcol+1))){
	my_flush();
	_settextposition(inrow+1,incol+1);
    }

    if((infore!=lastfore)||(inback!=lastback)){
	 my_flush();
	 my_setcolor(infore,inback);
         lastfore = infore;
         lastback = inback;
    }
        obuf[ocount] = inchar;
        ocount++;
        obuf[ocount] = 0x00;
        lastcol = incol;
        lastrow = inrow;

}

/*
for DOS compatibility...
*/

char getachar()
{
	return(getch());
}

rdchk(dumm)
int dumm;
{
	return(kbhit());
}

sleep(nsec)
int nsec;
{
int idone;
int icheck;

	idone = 0;
	ptime = &time1;
	time(ptime);
	for(;idone == 0;){
		ptime = &time2;
		time(ptime);
		time3 = difftime(time2,time1);
		icheck = (time3 & 0x0fff);
		if(icheck > nsec)
			idone = 1;
	}
}


