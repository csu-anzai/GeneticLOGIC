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



#include <stdio.h>

#include "devospac.h"

#ifdef THING_CHECK
	struct thing *thing_check();
	#define THINGS(p,s)	thing_check((p),s)
#else
	#define THINGS(p,s)	things[(p)]
#endif
/*
----------------------------------------------------------------------
*/
block_it()
{
int i;
struct thing *ttp;

	i = frame[yoff+cur_y][xoff+cur_x];
	if(i >= 0){
		for(;i >= 0;){
			ttp = (struct thing*)THINGS(i,"302");
			i = ttp->next;
		}
		if(i <= -3){
			ttp->next = -1;
			print_block(cur_x*8,cur_y*CHARY,i);
		}else if(i == -1){
			ttp->next = v_btype;
			print_block(cur_x*8,cur_y*CHARY,v_btype);
		}

	}else{
		if(i <= -3){
			frame[yoff+cur_y][xoff+cur_x] = -1;
			print_block(cur_x*8,cur_y*CHARY,i);
		}else if(i == -1){
			frame[yoff+cur_y][xoff+cur_x] = v_btype;
			print_block(cur_x*8,cur_y*CHARY,v_btype);
		}
	}
}

/*
----------------------------------------------------------------------
*/
block_type()
{

	if(v_btype == -3){
		v_btype = -4;
		w_move((WYSIZE-1),20);
		w_addstr("Death Block");
		w_refresh();
		sleep(1);
		w_move((WYSIZE-1),20);
		w_addstr("           ");
		w_refresh();
	}else{
		v_btype = -3;
		w_move((WYSIZE-1),20);
		w_addstr("Bounce Block");
		w_refresh();
		sleep(1);
		w_move((WYSIZE-1),20);
		w_addstr("            ");
		w_refresh();
	}
}

/*
----------------------------------------------------------------------
*/
bounce_block(tp)
struct thing *tp;
{
register int iy,ix;
int m1,m2,ysp,xsp;
register int xp,yp,dir;
char c;

/*
A couple quickies...
*/
	ysp = tp->yspeed;
	if(ysp == 0)
		goto side;
	xsp = tp->xspeed;
	if(xsp == 0)
		goto top;
/*
Gotta grunt and grind from here...
*/
	xp = tp->xpos+8;
	yp = tp->ypos+8;
	m1 = (ysp * 16) / xsp;
	ix = xp & 0xfff0;
	iy = yp & 0xfff0;
	dir = 0;
	if(xsp > 0){
		if(ysp < 0){
			iy += 15;
			dir++;
		}
	}else{
		if (ysp > 0) {
			ix += 15;
			dir++;
		}else{
			ix += 15;
			iy += 15;
		}
	}	
	yp -= iy;
	if(!yp)
		goto top_chk;
	xp -= ix;
	if(!xp)
		goto side_chk;
	m2 = (yp * 16) / xp;	

	if(!dir){
		if (m1 < m2)
			goto side_chk;
	}else{
		if (m1 > m2)
			goto side_chk;
	}
/*
See if it sort of slid on in.. by passing through the one next to me...
*/
top_chk:
	if(ysp > 0){
		ix = ix >> 4;
		if(ix > (XSIZE -1))
			goto side;
		iy -= 1;
		if(iy < 0)
			goto top;
		iy = iy >> 4;
		if(frame[iy][ix] <= -3)
			goto side;
		goto top;
	}else{
		ix = ix >> 4;
		if(ix > (XSIZE -1))
			goto side;
		iy += 16;
		iy = iy >> 4;
		if(iy > (YSIZE-1))
			goto top;
		if(frame[iy][ix] <= -3)
			goto side;
		goto top;
	}


side_chk:
	if(xsp > 0){
		iy = iy >> 4;
		if(iy > (YSIZE -1))
			goto side;
		ix -= 1;
		if(ix < 0)
			goto top;
		ix = ix >> 4;
		if(frame[iy][ix] <= -3)
			goto top;
		goto side;
	}else{
		iy = iy >> 4;
		if(iy > (YSIZE -1))
			goto side;
		ix += 16;
		ix = ix >> 4;
		if(ix > (XSIZE-1))
			goto top;
		if(frame[iy][ix] <= -3)
			goto top;
		goto side;
	}

/*
Almost definately...
*/
top:
	tp->yspeed = -tp->yspeed;
	return;
side:
	tp->xspeed = -tp->xspeed;
}

/*
----------------------------------------------------------------------
*/
block_die(tp)
struct thing *tp;
{
int i,inst,iold;
struct thing *ttp;

	inst = tp->stnum;
	if(inst == 0){
		tp->lifespan = 0;
		return;
	}
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i,"301");
		if(ttp->prev == -2)
			continue;
		if(ttp->stnum == inst){
			ttp->lifespan = 0;
		}
	}
}
