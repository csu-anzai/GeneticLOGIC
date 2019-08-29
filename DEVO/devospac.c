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

Some global type variables...

*/
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#include "devo.h"

int xoff,yoff;

int *frame[YSIZE];
char *things[MAX_THINGS];
int free_list;
int free_num;
int num_things;
int stn_next;
int v_total_off;
int v_diff;
int v_k;
int v_smx;
int v_total_equal;
int v_slowdown;
int v_life;
int repo_rate;
int v_btype;
int v_fatactive;
int v_fatlife;
long loopcount;
int lastact;
int ssmode;
int cursor_mode;
int cur_x,cur_y;
int status_line;
long births;
long deaths;
int repos;
int max_size;
int v_steal;
int xx,yy,ee;
