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
int do_energy(etp,ctp)
struct thing *etp;
struct thing *ctp;
{
int	i,idone,isumm;

#ifdef DEBUG
if(etp->myself >= MAX_THINGS)
	printf("ERROR etp >= MAX_THINGS!!! (%d.)\n",etp->myself);
if(ctp->myself >= MAX_THINGS)
	printf("ERROR ctp >= MAX_THINGS!!! (%d.)\n",ctp->myself);
#endif

	idone = 0;
	switch(ctp->type){
	case	T_MOVE:{
			move_it(ctp,etp);
			idone = 1;
			break;
			}
	case	T_REPO:{
			ctp->energy++;
			if(ctp->energy > 12){
				ctp->energy = 0;
				if(ctp->lifespan > 2) /* if NOT dying... */
					add_life(ctp->stnum,v_life);
			}
			idone = 2;
			break;
			}
	default:	{
			ctp->energy++;
			idone = 2;
			break;
			}
	}
	if(idone)
		new_energy(etp);
	return(idone);
}

move_it(ctp,etp)
struct thing *ctp;
struct thing *etp;
{
register int isumm;
/*
Figure what side we hit from to determine direction...
*/
	isumm = abs(etp->xspeed)+abs(etp->yspeed);
	if(isumm==0){
		isumm = abs(etp->xpos - ctp->xpos)+abs(etp->ypos - ctp->ypos);
		if(isumm == 0){
			ctp->xspeed -= 8;
			etp->yspeed -= 8;
		}else{
			ctp->xspeed = ctp->xspeed + ((16*(etp->xpos - ctp->xpos))/isumm);
			ctp->yspeed = ctp->yspeed + ((16*(etp->ypos - ctp->ypos))/isumm);
		}
	}else{
		ctp->xspeed = ctp->xspeed - ((16*etp->xspeed)/isumm);
		ctp->yspeed = ctp->yspeed - ((16*etp->yspeed)/isumm);
	}
/*
Place a few limits on the speeds...
NO more than two frames per cycle...
*/
	if(ctp->xspeed > 32)
		ctp->xspeed = 30;
	else if(ctp->xspeed < -32)
		ctp->xspeed = -30;
	if(ctp->yspeed > 32)
		ctp->yspeed = 30;
	else if(ctp->yspeed < -32)
		ctp->yspeed = -30;
}

pass_energy(ctp)
struct thing *ctp;
{
register struct thing *ttp;
int i,igone,iold,igone_old,ignored;

	if(ctp->stnum == 0)
		return;
	if(ctp->type == T_SINGLE){
		igone = ctp->energy - 1;
		if(igone <= 0)
			return;
	}else if(ctp->type == T_DOUBLE){
		igone = ctp->energy - 2;
		if(igone <= 0)
			return;
	}else if(ctp->type == T_FAT){
		if(ctp->lifespan > v_fatactive)
			return;
		igone = ctp->energy;
		if(igone <= 0)
			return;
	}else
		return;

	if(ctp->num_links == 0)
		return;

	igone_old = igone;
	ignored = 0;

/*
Pass energy off to other cells
*/
	for(i=0;(i<ctp->num_links)&&(igone);i++){
		ttp = (struct thing *)THINGS(ctp->links[i],"401");
		switch(ttp->type){
		case	T_SINGLE:{
				iold = ttp->energy;
				if(iold >= 1)
					break;
				igone--;
				ttp->energy++;
				ctp->energy--;
				if(iold < 1)
					re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,iold,ttp);
				break;
				}
		case	T_DOUBLE:{
				iold = ttp->energy;
				if(iold >= 2)
					break;
				igone--;
				ttp->energy++;
				ctp->energy--;
				if(iold < 2)
					re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,iold,ttp);
				break;
				}
		case	T_MOVE:{
				igone--;
				ctp->energy--;
				ctp->xspeed = -ctp->xspeed;
				ctp->yspeed = -ctp->yspeed;
				move_it(ttp,ctp);
				ctp->xspeed = -ctp->xspeed;
				ctp->yspeed = -ctp->yspeed;
				break;
				}
		case	T_REPO:{
				igone--;
				ctp->energy--;
				iold = ttp->energy;
				if(ttp->energy > 12){
					ttp->energy = 0;
					if(ttp->lifespan > 2) /* if NOT dying... */
						add_life(ttp->stnum,v_life/ttp->numst);
				}
				re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,iold,ttp);
				break;
				}
		case	T_FAT:{
				iold = ttp->energy;
				if((ctp->energy-iold) < 2) /*no transfer unless I have at least two more than he does*/
					break;
				igone--;
				ttp->energy++;
				ctp->energy--;
				ignored++;
				re_printme(ttp->xpos,ttp->ypos,ttp->xpos,ttp->ypos,iold,ttp);
				break;
				}
		default:
				break;
		}
	}

/*
Fat cells add life to structure if they have passed energy...
but, we don't count transfers between FAT cells...
*/
	if(ctp->type == T_FAT){
		if(((igone_old - igone) - ignored) > 0){
			if(ctp->lifespan > 2) /* if NOT dying... */
				add_life(ctp->stnum,(v_fatlife/ctp->numst)*((igone_old - igone) - ignored));
		}
	}
}

