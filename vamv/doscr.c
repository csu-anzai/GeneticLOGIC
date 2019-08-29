/*
 * Copyright 1990 Rick McGowan
 */
#include <stdio.h>
#include "prog.h"
/*
 * scoring routines.
 * attempting an instruction:
 */

doscr(n)
	int n;
{
	return 2;
#if 0
	switch (n) {
	case PICKUP:	return 3;
	case FOOD:	return 5;
	case EAT:	return 6;
	case DEP:	return 5;
	case MOVE:	return 1;
	case JOIN:	return 2;
	case TRADE:	return 3;
	case DIVI:	return 5;
	default:	return 0;
	}
#endif
}

/*
 * Successful instruction completion (achieved goal of instruction).
 * Table arrangement:

			NOP	PICKUP	EAT	DEP
			BRANCH	JUMP	MOVE	MOVQ
			JOIN	DIVI	MOVI	BRNNE
			BRINE	DIVQ	TRADE	FOOD
			SYNTH	NOP17	NOP18	NOP19
 */

int sucary[NINSTR] = {	0,	6,	12,	7,
			0,	0,	1,	1,
			3,	11,	1,	0,
			0,	11,	8,	5,
/*			1,	0,	0,	0, */
};

succ(n)
	register int n;
{
	if (n < NINSTR)
		return(sucary[n]);
	else
		return(0);
}

reset(o)
	struct org *o;
{
	extern int STARVE;

	if (o->ohold > 1) o->ohold = 1;
	else o->ohold = 0;
	if (o->ostrength > 10) o->ostrength /= 2;
	o->odep = o->oate = o->ofork = 0;
	o->opick = o->ojoin = o->badmove = o->nmove = o->score = 0;
}


comp(op, pp)
	struct org **op, **pp;
{
	register struct org *o, *p;

	o = *op;
	p = *pp;
	/* better scores go at low end of list (ss[0]) */
	if (o->score > p->score)
		return (-1);
	if (o->score < p->score)
		return (1);
	return 0;
}
