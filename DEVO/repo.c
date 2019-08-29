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
repo_man(tp)
struct thing *tp;
{
int i,stn,icount;
int ixmin,ixmax,iymin,iymax;
int i_xoff,i_yoff;
int nstn,j;
int ud,lr,k,iold;
struct thing *ttp;
struct thing *ntp;

	stn = tp->stnum;
	if(stn == 0)
		return;
	if(free_num < tp->numst)	/* not enough room */
		return;

	repos++;
/*
Size 'em up...
*/
	ixmin = XSIZE<<4;
	ixmax = 0;
	iymin = YSIZE<<4;
	iymax = 0;
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i,"100");
		if(ttp->prev == -2)
			continue;
		if(stn == ttp->stnum){
			ud = ttp->xpos;
			lr = ttp->ypos;
			if(ud > ixmax){
				ixmax = ud;
			}
			if(ud < ixmin){
				ixmin = ud;
			}
			if(lr > iymax){
				iymax = lr;
			}
			if(lr < iymin){
				iymin = lr;
			}
		}
	}
/*
We don't really care about type... these can change to balance out
with the majority of our structures...
BUT, now we know that we have plenty enough things to reproduce with,
and we also have the min and max x and y positions.
*/
/*
So, Let's get started...
*/
	nstn = new_stnum();
	i_xoff = (ixmax - ixmin);
	i_yoff = (iymax - iymin);
	i_xoff = (((XSIZE-1)<<4) - i_xoff)/2;
	i_yoff = (((YSIZE-1)<<4) - i_yoff)/2;
	ud = rand() & 0x03;
	switch(ud){
	case 0:	{ud=1;lr=1;break;}
	case 1:	{ud=1;lr=-1;break;}
	case 2:	{ud=-1;lr=1;break;}
	case 3:	{ud=-1;lr=-1;break;}
	default: {ud=0;lr=0;break;}
	}

	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i,"101");
		if(tp->prev == -2)
			continue;
		if(stn == ttp->stnum){
#ifdef DEBUG
	if((free_list < 0)||(free_list >= MAX_THINGS)){
		w_move(2,2);
		w_addstr("BAD FREE LIST\n");
		w_refresh();
	}
	if(free_num <= 0)
		printf("FREE LIST EXHAUSTED!\n");
#endif
			ntp = (struct thing *)THINGS(free_list,"102");
			free_list = ntp->next;
			free_num--;
			num_things++;
			births++;
			ntp->type = ttp->type;
			ntp->xpos = i_xoff + (ttp->xpos - ixmin);
			ntp->ypos = i_yoff + (ttp->ypos - iymin);
#ifdef FRAME_CHECK
			if(frame_check(ntp->ypos>>4,ntp->xpos>>4,"repo")){
				printf("i_x,i_y,mnx,mny,mxx,mxy %d %d %d %d %d %d\n",
				i_xoff,i_yoff,ixmin,iymin,ixmax,iymax);
				printf("nty,ntx,tty,ttx= %d %d %d %d\n",
				ntp->ypos,ntp->xpos,ttp->ypos,ttp->xpos);
				j = getachar();
			}
#endif
			ntp->lifespan = ttp->lifespan / 2;
			ttp->lifespan = ntp->lifespan;
			iold = ttp->energy;
			ntp->energy = ttp->energy / 2;
			if(ttp->energy & 0x01)
				ttp->energy = ntp->energy + 1;
			else
				ttp->energy = ntp->energy;
			ntp->stnum = nstn;
			ntp->numst = ttp->numst;
			ntp->num_links = ttp->num_links;
/*
JUST to help with links... Record which cell I correspond to...
*/
			ntp->prev = ttp->myself;
/* */
			for(j=0;j<MAX_LINKS;j++)
				ntp->links[j] = ttp->links[j];
			ntp->next = -1;
			ntp->xspeed = ttp->xspeed+lr;
			ntp->yspeed = ttp->yspeed+ud;
			if(tp->myself != ttp->myself)
				re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,iold,ttp);
		}
	}
/*
OK, nothing left now but to fix up the proper links...
and place them on the virtual screen...
For each new cell, grab the cell number it matches to, then loop through
them again looking through the links to replace with the new cell number.
Then, schtick them on the screen.
*/
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i,"103");
		if(ttp->prev == -2)
			continue;
		if(nstn == ttp->stnum){
			ud = ttp->prev;
			lr = ttp->myself;
			ttp->prev = -1;
			for(j=0;j<MAX_THINGS;j++){
				ntp = (struct thing *)THINGS(j,"104");
				if(nstn == ntp->stnum){
					if(ntp->num_links > 0){
						for(k=0;k<ntp->num_links;k++){
							if(ntp->links[k] == ud){
								ntp->links[k] = lr;
							}
						}
#ifndef DEBUG
					}
#else
					}else{
						w_move(4,4);
						w_addstr("YEEK! num_links = 0\n");
						w_refresh();
					}
#endif
				}
			}
/*
Place into framework...
*/
			ud = ttp->xpos>>4;
			lr = ttp->ypos>>4;
#ifdef FRAME_CHECK
			if(frame_check(lr,ud,"repo(2)")){
				j = getachar();
			}
#endif
			k = frame[lr][ud];
			frame[lr][ud] = i;
			if( k >= 0 ){
				ntp = (struct thing *)THINGS(k,"105");
				ntp->prev = i;
			}
			ttp->next = k;
/*
Draw self...
*/
			re_printme(ttp->xpos,ttp->ypos,-1,-1,ttp->energy,ttp);
		}
	}
/*
Done So Soon... ?
*/
}

