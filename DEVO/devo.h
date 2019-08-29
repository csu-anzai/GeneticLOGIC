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
Some constants and defines...
*/
#define VERS		6

#define YSIZE		100
#define XSIZE		200
#define START_THINGS	200
#define MAX_THINGS	400
/*
#define YSIZE		100
#define XSIZE		400
#define START_THINGS	500
#define MAX_THINGS	1500
*/
/*
#define SCREEN_MODE	_ERESCOLOR
*/
#define SCREEN_MODE	_VRES16COLOR

#if SCREEN_MODE == _VRES16COLOR
	#define WXSIZE		80
	#define WYSIZE		26
	#define CHARY		16
#else
	#define WXSIZE		80
	#define WYSIZE		25
	#define CHARY		14
#endif

/*
Types...
*/
#define MAXTYPE		6
#define	T_SINGLE	1
#define T_DOUBLE	2
#define T_MOVE		3
#define T_ENERGY	4
#define T_REPO		5
#define T_FAT		6

#define MAX_LINKS	4
static max_links[MAXTYPE+1] = {0,1,2,2,0,4,4};


struct	thing {
		int	next;
		int	prev;
		int	myself;
		int	xpos;
		int	ypos;
		int	xspeed;
		int	yspeed;
		int	stnum;
		int	numst;
		int	links[MAX_LINKS];
		int	num_links;
		int	energy;
		int	type;
		int	lifespan;
	};

#define THING_SIZE (sizeof(struct thing))
/*
By the way... I actually HATE structures in "C" because of the
pain in the ass they are to declare and create... Once debugged,
however, they are convenient ways of keeping arrays nicely
partitioned into manageable little nameable pieces...
*/

