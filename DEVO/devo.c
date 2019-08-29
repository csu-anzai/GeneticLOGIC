/*
#define FRAME_CHECK
#define DEBUG
#define THING_CHECK
#define INTEGRITY_CHECK
#define UNLINK_DEBUG
*/
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

#ifdef THING_CHECK
	struct thing *thing_check();
	#define THINGS(p,s)	thing_check((p),s)
#else
	#define THINGS(p,s)	things[(p)]
#endif
/*

Main Routine for devo stuffs...

Some code in here may get pretty ugly... but believe me, it's not
even close to some of the stuff I've done before... try getting
about 15 deep in "if"s and "do"s sometime... ( in a one-page routine !).


*/
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <graph.h>

#include "devospac.h"

static char tmpbuf[100];
static char c;
extern int do_energy();

main(argv,argc)
char *argv[];
int argc;
{
int i;
struct thing *tp;

	printf("Initializing...\n");
	init_devo();
	printf("Starting...\n");
	w_clear();
	w_refresh();
	w_raw();

	for(;;){
		c = 0x00;
		i = rdchk(0);
		if((i > 0)||(ssmode)){
			if((lastact == 1)&&(ssmode)){
				w_clear();
				w_setcolor(7,0);
				if(status_line){
					w_move((WYSIZE-1),0);
					sprintf(tmpbuf,"Window @ %d, %d.    ",yoff,xoff);
					w_addstr(tmpbuf);
				}
				w_refresh();
				re_printit();
				if(cursor_mode)
					dr_cursor((cur_x*8),(cur_y*CHARY));
				lastact = 0;
			}
			if((ssmode)&&(status_line)){
				w_move((WYSIZE-1),25);
				w_setcolor(7,0);
				w_addstr("Paused...");
				w_refresh();
			}
			c=getachar();
		}

		switch(c){
		case 'U':
		case 'D':
		case 'L':
		case 'R':
		case 'u':
		case 'd':
		case 'l':
		case 'r':
			{
			move_wc();
			break;
			}
		case 's':
		case 'S':
			{
			if(status_line){
				status_line = 0;
				w_move((WYSIZE-1),0);
				w_addstr("                                       ");
				w_move((WYSIZE-1),39);
				w_addstr("                                       ");
				w_refresh();
			}else{
				status_line = 2;
			}
			break;
			}
		case 'g':
		case 'G':
			{
			if(cursor_mode){
				get_debug();
				lastact = 1;
			}
			break;
			}
		case 'b':
			{
			block_it();
			break;
			}
		case 'B':
			{
			block_type();
			break;
			}
		case 'c':
		case 'C':
			{
			if(cursor_mode){
				if(lastact == 0){
					dr_cursor((cur_x*8),(cur_y*CHARY));
				}
				cursor_mode = 0;
			}else{
				if(lastact == 0){
					dr_cursor((cur_x*8),(cur_y*CHARY));
				}
				cursor_mode = 1;
			}
			break;
			}
		case 0x0d:
		case 0x0a:
			{
			w_move((WYSIZE-1),25);
			w_setcolor(7,0);
			w_addstr("         ");
			w_refresh();
			ssmode = 0;
			break;
			}
		case 0x1b:
		case '?':
		case 'h':
		case 'H':
			{
			w_clear();
			w_refresh();
			show_help();
			c = getachar();
			lastact = 1;
			break;
			}
		case 'v':
		case 'V':
			{
			w_clear();
			w_refresh();
			new_variables();
			c = getachar();
			lastact = 1;
			break;
			}
		case 'w':
		case 'W':
			{
			w_clear();
			w_refresh();
			show_stats();
			c = getachar();
			lastact = 1;
			break;
			}
		case 'f':
		case 'F':
			{
			w_clear();
			w_refresh();
			file_stuff();
			c = getachar();
			lastact = 1;
			break;
			}
		case 'q':
		case 'Q':
			{
			w_clear();
			w_refresh();
			w_normal();
			w_default();
			printf("Bye.\n");
			exit(0);
			}
		case ' ':
			{
			ssmode = 1;
			}
		default:
			{
			if((lastact == 1)||(status_line == 2)){
				status_line = 1;
				w_clear();
				w_setcolor(7,0);
				if(status_line){
					w_move((WYSIZE-1),0);
					sprintf(tmpbuf,"Window @ %d, %d.    ",yoff,xoff);
					w_addstr(tmpbuf);
				}
				w_refresh();
				re_printit();
				if(cursor_mode)
					dr_cursor((cur_x*8),(cur_y*CHARY));
			}
			if((((loopcount/10)*10) == loopcount)&&(status_line)){
				w_setcolor(7,0);
				w_move((WYSIZE-1),40);
				sprintf(tmpbuf,"Loopcount x10 = %ld           ",loopcount/10);
				w_addstr(tmpbuf);
				w_refresh();
			}
			do_something();
#ifdef INTEGRITY_CHECK
			check_all_frames();
#endif
			loopcount++;
			lastact = 0;
			break;
			}
		}
	}
}

show_help()
{
	w_setcolor(10,0);
	w_addstr("Help...\n\n");
	w_addstr("        U,u          Move Cursor/Window UP.\n");
	w_addstr("        D,d          Move Cursor/Window DOWN.\n");
	w_addstr("        L,l          Move Cursor/Window LEFT.\n");
	w_addstr("        R,r          Move Cursor/Window RIGHT.\n");
	w_addstr("        C,c          Cursor/Window Toggle.\n");
	w_addstr("        B            Block Type Toggle.\n");
	w_addstr("        b            Block On/Off Toggle.\n");
	w_addstr("        G,g          Get info on Thing(s) Cursor is on.\n");
	w_addstr("        S,s          Status Line On/Off Toggle.\n");
	w_addstr("        V,v          Modify Variables.\n");
	w_addstr("        W,w          Show Global Stats.\n");
	w_addstr("        F,f          File Stuff Menu.\n");
	w_addstr("        <space>      Single Step Mode.\n");
	w_addstr("        <Enter>      Free Running Mode.\n");
	w_addstr("        Q,q          Quit.\n");
	w_addstr("\n\n");
	w_addstr("        H,h,?,<esc>  This Help Menu...\n");
	w_setcolor(7,0);
	w_refresh();
}

/*
----------------------------------------------------------------------------
*/
init_devo()
{
int i,j;
int irx,iry,inxt;
time_t t_tmp;
time_t *tp_tmp;
struct thing *tp;

	if(MAX_THINGS <= START_THINGS){
		printf("MAX_THINGS must be greater than START_THINGS !\n");
		exit(1);
	}
/*
Allocate and zero the framework array...
*/
	for(i=0;i<YSIZE;i++){
		frame[i] = (int*)malloc(XSIZE*sizeof(int));
		for(j=0;j<XSIZE;j++){
			frame[i][j] = -1;
		}
	}
/*
Allocate space for MAX_THINGS things...
*/
	for(i=0;i<MAX_THINGS;i++){
		things[i] = (char*)malloc(THING_SIZE);
/*
Initialize the thing...
*/
		tp = (struct thing *)THINGS(i,"000");
		tp->prev = -1;
		tp->next = -1;
		tp->myself = i;
		tp->xpos = 0;
		tp->ypos = 0;
		tp->xspeed = 0;
		tp->yspeed = 0;
		tp->stnum = 0;
		tp->numst = 1;
		tp->energy = 0;
		tp->lifespan = 0;
		for(j=0;j<MAX_LINKS;j++)
			tp->links[j] = -1;
		tp->num_links = 0;
		tp->type = 0;
	}
/*
Put in some numbers to start with...
*/
	tp_tmp = &t_tmp;
	time(tp_tmp);
	srand(t_tmp);
	inxt = 0;

	for(i=0;i<START_THINGS;i++){
		tp = (struct thing *)THINGS(i,"001");
		tp->type = inxt;
/*
Double-up on the energies...
*/
		if(inxt == 0)
			tp->type = T_ENERGY;
		if(tp->type != T_ENERGY){
			irx = (rand()&0x0fff) % (XSIZE<<4);
			if(irx >= (XSIZE<<4))
				irx = (XSIZE<<4)-1;
			iry = (rand()&0x0fff) % (YSIZE<<4);
			if(iry >= (YSIZE<<4))
				iry = (YSIZE<<4)-1;
			tp->lifespan = (rand() & 0xfff);
		}else{
			irx = (rand()&0x0fff) % (XSIZE>>1);
			iry = (rand()&0x0fff) % (YSIZE>>1);
			tp->lifespan = 10000;
		}
		tp->xpos = irx;
		tp->ypos = iry;
		tp->xspeed = (rand()&0x0f);
		tp->yspeed = (rand()&0x0f);
		if(tp->type != T_ENERGY){
			if((rand() & 0x04))
				tp->xspeed = -tp->xspeed;
			if((rand() & 0x04))
				tp->yspeed = -tp->yspeed;
		}
		inxt++;
		if(inxt > MAXTYPE)
			inxt = 0;
		iry = iry>>4;
		irx = irx>>4;
#ifdef FRAME_CHECK
		frame_check(iry,irx,"spot 1");
#endif
		j = frame[iry][irx];
		frame[iry][irx] = i;
		tp->next = j;
		if( j >= 0 ){
			tp = (struct thing *)THINGS(j,"002");
			tp->prev = i;
		}

	}

/*
Set up the free list...
*/
	free_list = START_THINGS;
	free_num = MAX_THINGS - START_THINGS;
	num_things = START_THINGS;
	for(i=START_THINGS;i<MAX_THINGS;i++){
		tp = (struct thing *)THINGS(i,"003");
		tp->prev = -2;
		tp->next = i+1;
	}
	tp = (struct thing *)THINGS(MAX_THINGS-1,"004");
	tp->next = -2;
/*
Some other miscellaneous variables...
*/
	c = 0;
	lastact = 1;
	xoff = 0;
	yoff = 0;
	loopcount = 0;
	ssmode = 0;
	cursor_mode = 0;
	cur_x = 0;
	cur_y = 0;
	stn_next = 1;
	status_line = 1;
	births = 0;
	deaths = 0;
	repos = 0;
/*
Some variable variables...
*/
	v_total_off = 32;
	v_diff = 16;
	v_k = 3;
	v_smx = 16;
	v_total_equal = 48;
	v_slowdown = 12;
	v_life = 3000;
	repo_rate = 9000;
	v_btype = -3;
	max_size = 20;
	v_steal = 500;
	v_fatactive = 1000;
	v_fatlife = 500;
}

/*
----------------------------------------------------------------------------
*/
re_printit()
{
register int i,j,k;
register struct thing *tp;

	for(i=0;i<(WYSIZE-1);i++){
		for(j=0;j<WXSIZE;j++){
#ifdef FRAME_CHECK
			frame_check(i+yoff,j+xoff,"spot 3");
#endif
			k = frame[i+yoff][j+xoff];
next_thing:
			if(k >= 0){
				tp = (struct thing *)THINGS(k,"005");
				re_printme(tp->xpos,tp->ypos,-1,-1,tp->energy,tp);
				k = tp->next;
				goto next_thing;
			}else if(k <= -3)
				print_block(j*8,i*CHARY,k);
		}
	}
}

/*
----------------------------------------------------------------------------
*/
do_something()
{
int i,j;
int ix,iy,ixold,iyold,ienergy_old;
int ixx,iyy,txo,tyo;
register struct thing *tp;
struct thing *ttp;
#ifdef DEBUG
	int	meself;
#endif

/*
First, we move...
*/
	for(i=0;i<MAX_THINGS;i++){
		tp = (struct thing *)THINGS(i,"006");
/*
Ignore if on free list...
*/
		if(tp->prev == -2)
			continue;

		txo = tp->xpos;
		ixold = txo>>4;
		tyo = tp->ypos;
		iyold = tyo>>4;
		ienergy_old = tp->energy;
#ifdef DEBUG
		meself = tp->myself;
		if(i != meself)
			printf("Whoa.... i,meself == %d, %d.\n",i,meself);
#endif

		i_gotta_move(tp);

/*
Now put self in new position in frame...
*/
		ix = tp->xpos>>4;
		iy = tp->ypos>>4;
/*
Extract self from list in frame...
*/
		if(tp->next >= 0){
			ttp = (struct thing *)THINGS(tp->next,"007");
			ttp->prev = tp->prev;
		}
		if(tp->prev >= 0){
			ttp = (struct thing *)THINGS(tp->prev,"008");
			ttp->next = tp->next;
		}else{
#ifdef FRAME_CHECK
			frame_check(iyold,ixold,"spot 4");
#endif
			frame[iyold][ixold] = tp->next;
		}
		tp->next = -1;
		tp->prev = -1;
/*
Schtick self on head of new frame...
*/
#ifdef FRAME_CHECK
		frame_check(iy,ix,"spot 5");
#endif
		j = frame[iy][ix];
		if(j >= 0){
			ttp = (struct thing *)THINGS(j,"009");
			ttp->prev = i;
		}
		tp->next = j;
		frame[iy][ix] = i;
/*
Now stick to something... maybe...
*/
		if(j >= 0)
			get_stuck(tp);

/*
Check for to reproduce...
*/
		if(tp->type == T_REPO)
			if(tp->lifespan > repo_rate)
				repo_man(tp);
#ifdef DEBUG
		if(tp->myself != meself)
			printf("Whoa shit.... tp->myself,meself == %d, %d.\n",i,meself);
		meself = tp->myself;
		if(i != meself)
			printf("Whoa dude.... i,meself == %d, %d.\n",i,meself);
#endif
/*
Check for too much energy... must pass along...
*/
		if((tp->type == T_SINGLE)||(tp->type == T_DOUBLE)||(tp->type == T_FAT))
			pass_energy(tp);
/*
Check for dead (MUST be LAST routine in loop)...
*/
		if(tp->lifespan <= 0)
			die(tp);
		else
			tp->lifespan--;

/*
Redraw if something significant happened...
*/
		ix = tp->xpos;
		iy = tp->ypos;
		if((ix != txo)||(iy != tyo)||(ienergy_old != tp->energy)){
			re_printme(ix,iy,txo,tyo,ienergy_old,tp);
		}
	}
}


/*
----------------------------------------------------------------------------
*/
re_printme(ix,iy,ixold,iyold,ienergy_old,tp)
int ix,iy,ixold,iyold,ienergy_old;
struct thing *tp;
{

	xx = (ixold>>1) - (xoff*8);
	yy = iyold - (yoff*CHARY);
	ee = ienergy_old;
	if((xx >= 0)&&(xx < (WXSIZE*8)-7)&&(yy >= 0)&&(yy < ((WYSIZE-2)*CHARY+1))&&(ixold >= 0)){
		switch(tp->type){
		case T_SINGLE:
			{
			dr_single();
			break;
			}
		case T_DOUBLE:
			{
			dr_double();
			break;
			}
		case T_MOVE:
			{
			dr_move();
			break;
			}
		case T_ENERGY:
			{
			dr_energy();
			break;
			}
		case T_REPO:
			{
			dr_repo();
			break;
			}
		case T_FAT:
			{
			dr_fat();
			break;
			}
		default:
			break;
		}
	}

	xx = (ix>>1) - (xoff*8);
	yy = iy - (yoff*CHARY);
	ee = tp->energy;
	if((xx >= 0)&&(xx < (WXSIZE*8)-7)&&(yy >= 0)&&(yy < ((WYSIZE-2)*CHARY+1))&&(ix >= 0)){
		switch(tp->type){
		case T_SINGLE:
			{
			dr_single();
			break;
			}
		case T_DOUBLE:
			{
			dr_double();
			break;
			}
		case T_MOVE:
			{
			dr_move();
			break;
			}
		case T_ENERGY:
			{
			dr_energy();
			break;
			}
		case T_REPO:
			{
			dr_repo();
			break;
			}
		case T_FAT:
			{
			dr_fat();
			break;
			}
		default:
			break;
		}
	}
}

/*
----------------------------------------------------------------------
*/
i_gotta_move(tp)
struct thing *tp;
{
register struct thing *ttp;
int	xdiff,ydiff,ix,iy;
int	k,total_off;
int	sxdiff,sydiff;
int	axdiff,aydiff;
int	xsign,ysign;
int	idx,idy;
register int i,j,jj;
/*
Figure position based on last speeds...
*/
	tp->xpos = tp->xpos + tp->xspeed;
	if(tp->xpos > ((XSIZE-1)<<4)){
		tp->xpos = ((XSIZE-1)<<4);
		tp->xspeed = -tp->xspeed;
	}
	if(tp->xpos < 0){
		tp->xpos = 0;
		tp->xspeed = -tp->xspeed;
	}
	tp->ypos = tp->ypos + tp->yspeed;
	if(tp->ypos > ((YSIZE-1)<<4)){
		tp->ypos = ((YSIZE-1)<<4);
		tp->yspeed = -tp->yspeed;
	}
	if(tp->ypos < 0){
		tp->ypos = 0;
		tp->yspeed = -tp->yspeed;
	}

	ix = (tp->xpos+8)>>4;
	iy = (tp->ypos+8)>>4;
	if(ix > (XSIZE-1))
		ix = (XSIZE-1);
	if(iy > (YSIZE-1))
		iy = (YSIZE-1);
	jj = frame[iy][ix];
	if(jj == -3){
		bounce_block(tp);
	}else if(jj == -4){
		block_die(tp);
		return;
	}

	if(tp->type == T_ENERGY)
		return;		/* I'm free ! */

/*
Now I'm where I WANT to be... but I might be attached to something...
So don't stray more than +- 2.0 from any attached thingy... and
then share our x and y speeds...
AND, try to get about .5 away...
*/

	if(tp->num_links > 0){
		for(i=0;i<tp->num_links;i++){
#ifdef DEBUG
if((tp->links[i] < 0)||(tp->links[i] >= MAX_THINGS)||(tp->num_links > max_links[tp->type])){
	printf("Whoops! Thing %d, num_links=%d\n",tp->myself,tp->num_links);
	printf("Links = %d %d %d %d.\n",tp->links[0],tp->links[1],tp->links[2],tp->links[3]);
	break;
}
#endif
#ifdef THING_CHECK
			if((tp->links[i] < 0)||(tp->links[i]>=MAX_THINGS)){
				printf("ZOWIE ! ZOWIE !\n");
				printf("ZOWIE ! ZOWIE !\n");
			}
#endif
			ttp = (struct thing *)THINGS(tp->links[i],"010");
/*
Fetch relative position to an attached thing...
*/
			xdiff = tp->xpos - ttp->xpos;
			ydiff = tp->ypos - ttp->ypos;
			if(xdiff < 0){
				total_off = -xdiff;
				axdiff = -xdiff;
				xsign = -1;
			}else{
				total_off = xdiff;
				axdiff = xdiff;
				xsign = 1;
			}
			if(ydiff < 0){
				total_off = total_off - ydiff;
				aydiff = -ydiff;
				ysign = -1;
			}else{
				total_off = total_off + ydiff;
				aydiff = ydiff;
				ysign = 1;
			}
/*
If far away...
*/
			if(total_off > v_total_off){
/*	TUNE
Make some rotation if necessary... but at any rate, we gotta
equalize the speeds to get the things to stay together somewhat...
Now.. How's this go? ... This gets tough afer a few beers...
It's a good thing I didn't flunk geometry in High School...
Well, Here goes...
Oh, well... if at first you don't succeed...
???
*/
				axdiff -= v_diff;
				aydiff -= v_diff;
				if(axdiff <= 0)
					axdiff = 1;
				if(aydiff <= 0)
					aydiff = 1;

				k = aydiff/2*aydiff/v_k/total_off;
				tp->xspeed -= k*xsign;
				ttp->xspeed += k*xsign;
				tp->yspeed -= k*ysign;
				ttp->yspeed += k*ysign;

				k = axdiff/2*axdiff/v_k/total_off;
				tp->yspeed -= k*ysign;
				ttp->yspeed += k*ysign;
				tp->xspeed -= k*xsign;
				ttp->xspeed += k*xsign;
/*
Now equalize a little at a time...
*/
				if(tp->xspeed > 0)
					tp->xspeed++;
				else
					tp->xspeed--;
				if(tp->yspeed > 0)
					tp->yspeed++;
				else
					tp->yspeed--;
				tp->xspeed = tp->xspeed * (v_smx - 1) / v_smx;
				tp->yspeed = tp->yspeed * (v_smx - 1) / v_smx;

				if(total_off > v_total_equal){
					if(tp->xspeed > ttp->xspeed){
						tp->xspeed--;
						ttp->xspeed++;
					}else if(tp->xspeed < ttp->xspeed){
						tp->xspeed++;
						ttp->xspeed--;
					}
					if(tp->yspeed > ttp->yspeed){
						tp->yspeed--;
						ttp->yspeed++;
					}else if(tp->yspeed < ttp->yspeed){
						tp->yspeed++;
						ttp->yspeed--;
					}
				}
			}
		}
	}
	ix = tp->xpos>>4;
	iy = tp->ypos>>4;
	for(i=-1;i<2;i++){
		idy = iy + i;
		if(idy < 0)
			idy = 1;
		if(idy >= YSIZE)
			idy = YSIZE - 2;
		for(j=-1;j<2;j++){
			idx = ix + j;
			if(idx < 0)
				idx = 1;
			if(idx >= XSIZE)
				idx = XSIZE - 2;
			jj = frame[idy][idx];
			if(jj >= 0){
				for(ttp = (struct thing *)THINGS(jj,"011");ttp->next >= 0;ttp = (struct thing *)THINGS(ttp->next,"012")){
					if((i == 0)&&(j == 0)){
						if(tp->ypos > ttp->ypos){
							tp->yspeed++;
							ttp->yspeed--;
						}else if(tp->ypos < ttp->ypos){
							tp->yspeed--;
							ttp->yspeed++;
						}
						if(tp->xpos > ttp->xpos){
							tp->xspeed++;
							ttp->xspeed--;
						}else if(tp->xpos < ttp->xpos){
							tp->xspeed--;
							ttp->xspeed++;
						}
					}else{
						tp->yspeed -= i;
						tp->xspeed -= j;
						ttp->yspeed += i;
						ttp->xspeed += j;
					}
				}
			}
		}
	}
}
/*
------------------------------------------------------------------------
*/
get_stuck(tp)
register struct thing *tp;
{
register struct thing *ttp;
register int i;
int j,k,idone,ienergy_old;
int ixold,iyold;

	if(tp->next < 0)
		return;
	idone = 0;
	for(ttp = (struct thing *)THINGS(tp->next,"013");!idone;ttp = (struct thing *)THINGS(ttp->next,"014")){
		if(tp->myself != ttp->myself){	/* can't stick to myself... */
			if((tp->type != T_ENERGY)&&(ttp->type != T_ENERGY)){
/*
Can I link ?
*/
				if(tp->num_links < max_links[tp->type]){
/*
Can it link?
*/
					if(ttp->num_links < max_links[ttp->type]){
/*
Only one stick per thingy...
*/
						k = 0;
						for(i=0;i<tp->num_links;i++){
							if(tp->links[i] == ttp->myself)
								k = 1;
						}
						if(k == 0){
/*
Yes! we have a linkage... MAYBE...
REPO's only stick to other REPO's and DOUBLE's...
*/
							if(tp->type == T_REPO){
								if((ttp->type != T_REPO)&&(ttp->type != T_DOUBLE))
									goto endloop;
							}else if(ttp->type == T_REPO){
								if((ttp->type != T_REPO)&&(ttp->type != T_DOUBLE))
									goto endloop;
							}
/*
Check against MAX in struct number...
*/
							if(tp->stnum != ttp->stnum)
								if((tp->numst + ttp->numst) > max_size)
									goto endloop;
/*
Begin Joining...
*/
							for(i=0;((i<max_links[tp->type])&&(tp->links[i] >= 0));i++) ;
							tp->links[i] = ttp->myself;

							for(j=0;((j<max_links[ttp->type])&&(ttp->links[j] >= 0));j++) ;
							ttp->links[j] = tp->myself;

							tp->num_links++;
							ttp->num_links++;
/*
Now Join structures...
*/
							if(ttp->stnum != 0){
								if(tp->stnum != 0){
									if(tp->stnum != ttp->stnum)
										change_all(tp,ttp);
								}else{
									tp->stnum = ttp->stnum;
									tp->lifespan = ttp->lifespan;
									adj_all(ttp,1);
								}
							}else{
								if(tp->stnum != 0){
									ttp->stnum = tp->stnum;
									ttp->lifespan = tp->lifespan;
									adj_all(tp,1);
								}else{
									tp->stnum = new_stnum();
									ttp->stnum = tp->stnum;
									ttp->lifespan = tp->lifespan;
									adj_all(tp,1);
								}
							}
#ifdef DEBUG
	if(tp->num_links > max_links[tp->type]){
		printf("MAX_LINKS! (%d,%d)\n",tp->myself,tp->num_links);
	}
	if(ttp->num_links > max_links[ttp->type]){
		printf("MAX_LINKS! (%d,%d)\n",ttp->myself,ttp->num_links);
	}
#endif
/*
Am I full ?
*/
							if(tp->num_links >= max_links[tp->type])
								idone = 1;
						}
					}
				}
			}else{
				if(!((tp->type == T_ENERGY)&&(ttp->type == T_ENERGY))){
/*
Now we have an energy and a non-energy type...
*/
					if(tp->type == T_ENERGY){
						ienergy_old = ttp->energy;
						idone = do_energy(tp,ttp);
						if(idone == 2)
						re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,ienergy_old,ttp);
					}else{
						ixold = ttp->xpos;
						iyold = ttp->ypos;
						idone = do_energy(ttp,tp);
						if(idone)
						re_printme(ttp->xpos,ttp->ypos,ixold,iyold,1,ttp);
					}
				}
				goto endloop;
			}
/*
Is it part of me?
*/
			if(tp->stnum != ttp->stnum){
/*
Can I attack it?
*/
				if(tp->type == T_SINGLE){
					if(tp->energy > 0){
/*
Is it worth attacking ?
*/
						if(ttp->lifespan <= 2)
							goto endloop; /*nope*/
						if(ttp->stnum == 0)
							goto endloop; /*nope*/
/*
Can it defend itself?
*/
						if(ttp->type == T_DOUBLE){
							if(ttp->energy > 0){
								ienergy_old = ttp->energy;
								ttp->energy--;
								re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,ienergy_old,ttp);
								tp->energy--;
								goto endloop;
							}
						}
/*
YES, we have a ferocious attack betwixt structures !
*/
						ienergy_old = v_steal;
						if(ttp->lifespan < (v_steal + 2))
							ienergy_old = ttp->lifespan - 2;
						add_life(tp,ienergy_old);
						add_life(ttp,-ienergy_old);
						tp->energy = tp->energy + ttp->energy;
						ienergy_old = ttp->energy;
						ttp->energy = 0;
						re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,ienergy_old,ttp);
					}
				}
			}
		}
endloop:
		if(ttp->next < 0)
			idone = 1;
		if(idone)
			break;
	}
}

/*
------------------------------------------------------------------------
*/
move_wc()
{
struct thing *tp;
int i;

	if(!cursor_mode){
		if(c > 0x60)
			c = (c - 0x61) + 0x41;
		if((c == 0x4b)||(c == 'L')){
			xoff = xoff - 1;
			if(xoff < 0)
				xoff = 0;
		}else if((c == 0x4d)||(c == 'R')){
			xoff = xoff + 1;
			if(xoff > (XSIZE-WXSIZE))
				xoff = XSIZE - WXSIZE;
		}else if((c == 0x48)||(c == 'U')){
			yoff = yoff - 1;
			if(yoff < 0)
				yoff = 0;
		}else{
			yoff = yoff + 1;
			if(yoff > (YSIZE-WYSIZE+1))
				yoff = YSIZE - WYSIZE + 1;
		}
		lastact = 1;
	}else{
		if(lastact == 0){
			dr_cursor((cur_x*8),(cur_y*CHARY));
		}
		if(c > 0x60)
			c = (c - 0x61) + 0x41;
		if((c == 0x4b)||(c == 'L')){
			cur_x = cur_x - 1;
			if(cur_x < 0)
				cur_x = 0;
		}else if((c == 0x4d)||(c == 'R')){
			cur_x = cur_x + 1;
			if(cur_x >= WXSIZE)
				cur_x = WXSIZE - 1;
		}else if((c == 0x48)||(c == 'U')){
			cur_y = cur_y - 1;
			if(cur_y < 0)
				cur_y = 0;
		}else{
			cur_y = cur_y + 1;
			if(cur_y > (WYSIZE-2))
				cur_y = WYSIZE - 2;
		}
		if(lastact == 0){
			dr_cursor((cur_x*8),(cur_y*CHARY));
		}
	}
}

/*
------------------------------------------------------------------------
*/
get_debug()
{
int i,j,inext;
struct thing *tp;

	w_clear();
	w_refresh();
#ifdef FRAME_CHECK
	frame_check(cur_y+yoff,cur_x+xoff,"spot 8");
#endif
	inext = frame[cur_y+yoff][cur_x+xoff];
	for(;;){
		w_move(0,0);
		w_setcolor(7,0);
		sprintf(tmpbuf,"Absolute position = %d, %d.            ",cur_y+yoff,cur_x+xoff);
		w_addstr(tmpbuf);
		if(inext < 0){
			w_move(1,0);
			w_setcolor(15,0);
			w_addstr("NO More Things Here...");
			inext = frame[cur_y+yoff][cur_x+xoff];
		}else{
			tp = (struct thing *)THINGS(inext,"015");
			w_move(1,0);
			w_addstr("                      ");
			w_move(2,0);
			sprintf(tmpbuf,"Thing Number:    %d    \n",tp->myself);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Type:            %d    \n",tp->type);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Previous:        %d    \n",tp->prev);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Next:            %d    \n",tp->next);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Xspeed:          %d    \n",tp->xspeed);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Yspeed:          %d    \n",tp->yspeed);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Struct. Num.:    %d    \n",tp->stnum);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Num. in Struct:  %d    \n",tp->numst);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Energy:          %d    \n",tp->energy);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Num_Links:       %d    \n",tp->num_links);
			w_addstr(tmpbuf);
			sprintf(tmpbuf,"Lifespan:        %d    \n",tp->lifespan);
			w_addstr(tmpbuf);
			w_addstr("Links:           ");
			for(i=0;i<MAX_LINKS;i++){
				sprintf(tmpbuf,"%d      ",tp->links[i]);
				w_addstr(tmpbuf);
			}
			inext = tp->next;
		}
		w_refresh();
		c = getachar();
		if((c != 'g')&&(c != 'G'))
			break;
	}
}
/*
--------------------------------------------------------------------
*/
new_energy(tp)
struct thing *tp;
{
int i,j,k,l,irx,iry;
int ixold,iyold;
struct thing *ttp;

/*
Unlink self from wherever I be...
*/

	ixold = tp->xpos>>4;
	iyold = tp->ypos>>4;
	if(tp->next >= 0){
		ttp = (struct thing *)THINGS(tp->next,"016");
		ttp->prev = tp->prev;
	}
	if(tp->prev >= 0){
		ttp = (struct thing *)THINGS(tp->prev,"017");
		ttp->next = tp->next;
	}else{
#ifdef FRAME_CHECK
		frame_check(iyold,ixold,"spot 9");
#endif
		frame[iyold][ixold] = tp->next;
	}

/*
Now get some new Values...
*/
	irx = (rand()&0x0fff) % (XSIZE>>1);
	iry = (rand()&0x0fff) % (YSIZE>>1);
	tp->xpos = irx;
	tp->ypos = iry;
	tp->prev = -1;
	tp->next = -1;
	tp->xspeed = (rand()&0x0f);
	tp->yspeed = (rand()&0x0f);
	tp->stnum = 0;
	tp->energy = 1;
	tp->lifespan = 32767;
	iry = iry>>4;
	irx = irx>>4;
#ifdef FRAME_CHECK
	frame_check(iry,irx,"spot 10");
#endif
	j = frame[iry][irx];
	frame[iry][irx] = tp->myself;
	if( j >= 0 ){
		ttp = (struct thing *)THINGS(j,"018");
		ttp->prev = tp->myself;
	}
	tp->next = j;
/*
Now slow some things down a bit... so some things will die...
*/

	i = (rand() & 0xfff) % MAX_THINGS;
	k = 0;
	for(j=0;j<MAX_THINGS;j++){
		ttp = (struct thing *)THINGS(i,"019");
		if(ttp->prev == -2)	/*ignore if free*/
			continue;
		if(ttp->xspeed != 0){
			if(ttp->xspeed > 0)
				ttp->xspeed--;
			else
				ttp->xspeed++;
			k++;
		}
		if(ttp->yspeed != 0){
			if(ttp->yspeed > 0)
				ttp->yspeed--;
			else
				ttp->yspeed++;
			k++;
		}
		i++;
		if(i >= MAX_THINGS)
			i = 0;
		if(k >= v_slowdown)
			break;
	}
}

#ifdef FRAME_CHECK
/***************************DEBUG***********************************/
int frame_check(iry,irx,strg)
int	iry,irx;
char	*strg;
{
int ifound;

	ifound = 0;
	if(iry > (YSIZE-1)){
		printf("IRY>(%s)",strg);
		ifound++;
	}
	if(iry < 0){
		printf("IRY<(%s)",strg);
		ifound++;
	}
	if(irx > (XSIZE-1)){
		printf("IRX>(%s)",strg);
		ifound++;
	}
	if(irx < 0){
		printf("IRX<(%s)",strg);
		ifound++;
	}
	return(ifound);
}
#endif
#ifdef THING_CHECK
struct thing *thing_check(pindex,sstr)
int pindex;
char *sstr;
{
char c;

	if((pindex < 0)||(pindex >= MAX_THINGS)){
		printf("OOPS!!! THING OUT OF RANGE ! (%d) <<%s>>\n",pindex,sstr);
		c = getachar();
		return((struct thing *)things[0]);
	}
	return((struct thing *)things[pindex]);
}
#endif

#ifdef INTEGRITY_CHECK
check_all_frames()
{
int i,j,k,l,m;
int	found[MAX_THINGS];
struct thing *tp;
struct thing *ttp;


	for(i=0;i<MAX_THINGS;i++)
		found[i] = 0;

	for(i=0;i<YSIZE;i++){
		for(j=0;j<XSIZE;j++){
			k = frame[i][j];
			if(k >= 0){
				if(k >= MAX_THINGS)
					printf("BAD thing # !!!\n");
				found[k]++;
				tp = (struct thing *)THINGS(k,"020");
				l = tp->num_links;
				if((l < 0)||(l > MAX_LINKS)){
					printf("BAD num_links !\n");
				}else{
					if(l > 0){
						for(m=0;m<l;m++){
							if((tp->links[m] < 0)||(tp->links[m] >= MAX_THINGS))
								printf("LINK out of RANGE %d !\n",tp->links[m]);
							if(tp->links[m] == tp->myself)
								printf("Self Link!\n");
						}
					}
				}
				if(tp->prev != -1)
					printf("Bad PREV thing %d.\n",tp->prev);
				if(tp->next >= 0){
					for(k=tp->next;k >= 0;k=ttp->next){
						found[k]++;
						ttp = (struct thing *)THINGS(k,"021");
						if(tp->next != ttp->myself)
							printf("BAD NEXT LINK (%d,%d)\n",tp->next,ttp->prev);
						if(tp->myself != ttp->prev)
							printf("BAD PREV LINK (%d,%d)\n",tp->next,ttp->prev);
						tp = ttp;
						l = tp->num_links;
						if((l < 0)||(l > MAX_LINKS)){
							printf("BAD num_links !\n");
						}else{
							if(l > 0){
								for(m=0;m<l;m++){
									if((tp->links[m] < 0)||(tp->links[m] >= MAX_THINGS))
										printf("LINK out of RANGE %d !\n",tp->links[m]);
									if(tp->links[m] == tp->myself)
										printf("Self Link!\n");
								}
							}
						}
					}
				}
			}
		}
	}
	j = 0;
	i = free_list;
	if(i != -2){
		for(;i != -2;){
			tp = (struct thing *)THINGS(i,"022");
			found[i]++;
			j++;
			i = tp->next;
		}
	}
	if(j != free_num)
		printf("BAD free list and/or count!\n");
	free_num = j;
	for(i=0;i<MAX_THINGS;i++){
		if(found[i] <= 0)
			printf("LOST (%d)\n",i);
		if(found[i] > 1)
			printf("MULTIPLE (%d, %d)\n",i,found[i]);
	}
}
#endif
/*
---------------------------------------------------------------------
*/
change_all(tp_from,tp_to)
struct thing *tp_from;
struct thing *tp_to;
{
int i,ifrom,ito,imax,inst;
struct thing *tp;

	imax = tp_from->lifespan;
	if(imax < tp_to->lifespan)
		imax = tp_to->lifespan;

	ifrom = tp_from->stnum;
	ito = tp_to->stnum;
	inst = tp_from->numst + tp_to->numst;

	for(i=0;i<MAX_THINGS;i++){
		tp = (struct thing *)THINGS(i,"023");
		if(tp->prev == -2)
			continue;
		if(tp->stnum == ifrom){
			tp->stnum = ito;
			tp->lifespan = imax;
			tp->numst = inst;
		}else if(tp->stnum == ito){
			tp->lifespan = imax;
			tp->numst = inst;
		}
	}
}
/*
---------------------------------------------------------------------
*/
adj_all(tp,aaa)
struct thing *tp;
int aaa;
{
int i,inst,ito;
struct thing *ttp;

	ito = tp->stnum;
	inst = tp->numst + aaa;

	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i,"024");
		if(ttp->prev == -2)
			continue;
		if(ttp->stnum == ito){
			ttp->numst = inst;
			if(inst == 1)
				ttp->stnum = 0;
		}
	}
}

/*
---------------------------------------------------------------------
*/
add_life(struct_num,howmuch)
int struct_num;
int howmuch;
{
int i;
long il;
struct thing *tp;

	for(i=0;i<MAX_THINGS;i++){
		tp = (struct thing *)THINGS(i,"025");
		if(tp->prev == -2)
			continue;
		if(tp->stnum == struct_num){
			il = tp->lifespan;
			il += howmuch;
			if(il > 30000)
				il = 30000;
			tp->lifespan = il;
		}
	}
}

/*
---------------------------------------------------------------------
*/
int new_stnum()
{
int i;
struct thing *tp;

	stn_next++;
	if(stn_next > 32760)
		stn_next = 1;

	for(;;){
		for(i=0;i<MAX_THINGS;i++){
			tp = (struct thing *)THINGS(i,"026");
			if(tp->prev == -2)
				continue;
			if(tp->stnum == stn_next)
				break;
		}
		if(i >= MAX_THINGS)
			break;
		stn_next++;
		if(stn_next > 32760)
			stn_next = 1;
	}
	return(stn_next);
}
/*
--------------------------------------------------------------------------
*/
die(tp)
struct thing *tp;
{
int i,j,k,iy,ix;
struct thing *ttp;

	deaths++;
	if(tp->num_links > 0){
/*
Break links...
*/
#ifdef UNLINK_DEBUG
		printf("Unlinking thing %d, num_links = %d.\n",tp->myself,tp->num_links);
#endif
		for(i=0;i<tp->num_links;i++){
#ifdef THING_CHECK
			if((tp->links[i] < 0)||(tp->links[i]>=MAX_THINGS)){
				printf("YOWZA ! YOWZA!\n");
				printf("YOWZA ! YOWZA!\n");
			}
#endif
			ttp = (struct thing *)THINGS(tp->links[i],"027");
#ifdef UNLINK_DEBUG
			printf("Thing %d BEFORE %d links: ",ttp->myself,ttp->num_links);
			for(k=0;k<MAX_LINKS;k++)
				printf("%d ",ttp->links[k]);
			printf("\n");
#endif
			ttp->num_links--;
			for(j=0;j<ttp->num_links;j++){
				if(ttp->links[j] == tp->myself)
					ttp->links[j] = -1;
				if(ttp->links[j] == -1){
					ttp->links[j] = ttp->links[j+1];
					ttp->links[j+1] = -1;
				}
			}
			ttp->links[ttp->num_links] = -1;
#ifdef UNLINK_DEBUG
			printf("Thing %d AFTER %d links: ",ttp->myself,ttp->num_links);
			for(k=0;k<MAX_LINKS;k++)
				printf("%d ",ttp->links[k]);
			printf("\n");
#endif
		}
		tp->num_links = 0;
		for(i=0;i<MAX_LINKS;i++)
			tp->links[i] = -1;
	}
/*
Fix structure counts
*/
	if(tp->numst > 1)
		adj_all(tp,-1);
/*
If less than one max structure size free...
*/
	if((free_num < max_size)&&(tp->type != T_ENERGY)){
/*
Unlink self from wherever I be...
*/

		ix = tp->xpos>>4;
		iy = tp->ypos>>4;
		if(tp->next >= 0){
			ttp = (struct thing *)THINGS(tp->next,"028");
			ttp->prev = tp->prev;
		}
		if(tp->prev >= 0){
			ttp = (struct thing *)THINGS(tp->prev,"029");
			ttp->next = tp->next;
		}else{
#ifdef FRAME_CHECK
			frame_check(iy,ix,"spot 21");
#endif
			frame[iy][ix] = tp->next;
		}
/*
Stick self on free list... Martyrdom lives!
*/
		free_num++;
		tp->prev = -2;
		tp->next = free_list;
		free_list = tp->myself;
		num_things--;
		tp->stnum = 0;
		tp->numst = 1;
		tp->xpos = -1;	/*disappear*/
/*
Else, just let 'em float around... but lose their energy...
*/
	}else{
		births++;
		tp->xspeed = (rand() & 0x0f);
		tp->yspeed = (rand() & 0x0f);
		if((rand() & 0x100))
			tp->xspeed = -tp->xspeed;
		if((rand() & 0x100))
			tp->yspeed = -tp->yspeed;
		if(tp->type != T_ENERGY)
			tp->energy = 0;
		tp->stnum = 0;
		tp->numst = 1;
		tp->lifespan = (rand() & 0xfff);
	}
}

