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
#include <time.h>
#include <stdlib.h>
#include <graph.h>

#include "devospac.h"

#ifdef THING_CHECK
	struct thing *thing_check();
	#define THINGS(p)	thing_check((p))
#else
	#define THINGS(p)	things[(p)]
#endif


new_variables()
{
	w_setcolor(7,0);
	new_backdrop();
	w_refresh();
	show_values();
	w_refresh();
	new_getval(3,&v_total_off);
	new_getval(4,&v_diff);
	new_getval(5,&v_k);
	new_getval(6,&v_smx);
	new_getval(7,&v_total_equal);
	new_getval(8,&v_slowdown);
	new_getval(9,&v_life);
	new_getval(10,&repo_rate);
	new_getval(11,&max_size);
	new_getval(12,&v_steal);
	new_getval(13,&v_fatactive);
	new_getval(14,&v_fatlife);
	w_move(20,0);
	w_addstr("Press return to return:");
	w_refresh();
}

show_values()
{
char	tstr[10];

	sprintf(tstr,"%6.6d",v_total_off);
	w_move(3,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_diff);
	w_move(4,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_k);
	w_move(5,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_smx);
	w_move(6,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_total_equal);
	w_move(7,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_slowdown);
	w_move(8,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_life);
	w_move(9,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",repo_rate);
	w_move(10,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",max_size);
	w_move(11,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_steal);
	w_move(12,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_fatactive);
	w_move(13,0);
	w_addstr(tstr);
	sprintf(tstr,"%6.6d",v_fatlife);
	w_move(14,0);
	w_addstr(tstr);
	w_refresh();
}

new_getval(where,point_what)
int where;
int *point_what;
{
int newval;
int i;
char tstr[10];
char inchar;

	w_move(where,0);
	sprintf(tstr,"%6.6d",*point_what);
	w_setcolor(13,0);
	w_addstr(tstr);
	w_setcolor(7,0);
	w_move(20,0);
	w_addstr("Enter New Value: ");
	w_refresh();

	i = 0;
	for(;;){
		if(i >= 6){
			i = 0;
			break;
		}
		tstr[i] = 0x00;
		inchar = getachar() & 0xff;
		if((inchar == 0x08)||(inchar == 0xff)||(inchar == 0x7f)){
			if(i > 0){
				w_move(20,17+i-1);
				w_addchar(0x20);
				w_refresh();
				i--;
			}
			continue;
		}
		if((inchar == 0x0a)||(inchar == 0x0d))
			break;
		if((inchar < 0x30)||(inchar > 0x39)){
			i = 0;
			break;
		}
		tstr[i] = inchar;
		w_move(20,17+i);
		w_addchar(inchar);
		w_refresh();
		i++;
	}
	w_move(20,17);
	w_addstr("          ");
	if(i <= 0){
		w_move(where,0);
		sprintf(tstr,"%6.6d",*point_what);
		w_addstr(tstr);
		w_refresh();
		return;
	}
	sscanf(tstr,"%d",&newval);
	*point_what = newval;
	w_move(where,0);
	sprintf(tstr,"%6.6d",*point_what);
	w_addstr(tstr);
	w_refresh();
}

new_backdrop()
{
	w_clear();
	w_move(1,0);
	w_addstr("Variable Modification Menu");
	w_move(3,10);
	w_addstr("v_total_off           - Low bound of bounce checking");
	w_move(4,10);
	w_addstr("v_diff                - adjustment to bounce distances");
	w_move(5,10);
	w_addstr("v_k                   - division factor for bounce amplitude");
	w_move(6,10);
	w_addstr("v_smx                 - minimum speed to begin slowing");
	w_move(7,10);
	w_addstr("v_total_equal         - Low bound speed equalization");
	w_move(8,10);
	w_addstr("v_slowdown            - Units of motion adjustment");
	w_move(9,10);
	w_addstr("v_life                - Units of life increment");
	w_move(10,10);
	w_addstr("repo_rate             - Energy level for Reproduction");
	w_move(11,10);
	w_addstr("max_size              - Max size of structure");
	w_move(12,10);
	w_addstr("v_steal               - Life stolen during attacks");
	w_move(13,10);
	w_addstr("v_fatactive           - Fat cell activation level");
	w_move(14,10);
	w_addstr("v_fatlife             - Fat cell life increment");
}

show_stats()
{
int 	i,icount;
struct thing *ttp;
char	tstr[80];

	sprintf(tstr,"Num_things = %6.6d",num_things);
	w_move(2,0);
	w_addstr(tstr);
	sprintf(tstr,"Free_num = %6.6d",free_num);
	w_move(3,0);
	w_addstr(tstr);
	icount = 0;
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i);
		if(ttp->prev == -2)
			continue;
		if(ttp->type == T_SINGLE)
			icount++;
	}
	sprintf(tstr,"SINGLE count = %6.6d",icount);
	w_move(4,0);
	w_addstr(tstr);
	icount = 0;
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i);
		if(ttp->prev == -2)
			continue;
		if(ttp->type == T_DOUBLE)
			icount++;
	}
	sprintf(tstr,"DOUBLE count = %6.6d",icount);
	w_move(5,0);
	w_addstr(tstr);
	icount = 0;
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i);
		if(ttp->prev == -2)
			continue;
		if(ttp->type == T_MOVE)
			icount++;
	}
	sprintf(tstr,"MOVE count = %6.6d",icount);
	w_move(6,0);
	w_addstr(tstr);
	icount = 0;
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i);
		if(ttp->prev == -2)
			continue;
		if(ttp->type == T_REPO)
			icount++;
	}
	sprintf(tstr,"REPO count = %6.6d",icount);
	w_move(7,0);
	w_addstr(tstr);
	icount = 0;
	for(i=0;i<MAX_THINGS;i++){
		ttp = (struct thing *)THINGS(i);
		if(ttp->prev == -2)
			continue;
		if(ttp->type == T_FAT)
			icount++;
	}
	sprintf(tstr,"FAT count = %6.6d",icount);
	w_move(8,0);
	w_addstr(tstr);
	sprintf(tstr,"Births = %10.10ld",births);
	w_move(9,0);
	w_addstr(tstr);
	sprintf(tstr,"Deaths = %10.10ld",deaths);
	w_move(10,0);
	w_addstr(tstr);
	sprintf(tstr,"Reproductions = %6.6d",repos);
	w_move(11,0);
	w_addstr(tstr);
	w_refresh();
}
