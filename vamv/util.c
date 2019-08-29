/*
 * Copyright 1990 Rick McGowan
 */
#include <stdio.h>
#include "prog.h"

int uniqid = 0;
int nalive = 0;

static struct org *
oalloc()
{
	register struct org *o;
	char *calloc();

	o = (struct org *) calloc(1, sizeof(struct org));
	o->id = uniqid++;
	++nalive;
	return(o);
}

extern int ranseed, optran;

initrand()
{
	if (! optran)
		ranseed = (int) time((long *) 0);
	srand(ranseed);
}

/*
 * Return number in range 0 to i-1.
 */
range(i)
	register int i;
{
	return(rand() % i);
}

/*
 * Given a probability "N%" of a thing happening, return 1 if it
 * happened, otherwise 0.
 */

probability(n)
	int n;
{
	register int i;

	i = rand() % 100;
	if (i <= n)
		return 1;
	else
		return 0;
}

/*
 * create a wee beastie.
 */
struct org *
new()

{
	register struct org *o;
	register int i;

	o = oalloc();	/* adjusts nalive! */
	for (i = 0; i < NGENE; i++) {	/* get a few chromosomes */
		o->prog[i] = range(256);
	}
	for (i = 0; i < NPREF; i++) {
		o->pref[i] = range(15)+1;
	}
	return(o);	/* voila: a "hopeful monster". */
}

extern int optboot;

struct org *
copy(o, x, y)
	register struct org *o;
	int x, y;
{
	register struct org *p;
	register int i;

	p = oalloc();	/* adjusts nalive */
	for (i = 0; i < NGENE; i++) {
		p->prog[i] = o->prog[i];
	}
	for (i = 0; i < NPREF; i++) {
		p->pref[i] = o->pref[i];
	}
	++o->ofork;
	p->ostrength = o->ostrength/2;
	o->ostrength = p->ostrength;
	o->oate /= 2;
	p->ohold = p->opick = p->oate = p->odep = p->ofork = 0;
	p->score = p->ojoin = p->badmove = p->everate = 0;
	p->xpo = x; p->ypo = y;
	if (optboot)
		p->pc = 0;
	else
		p->pc = o->pc;	/* literal fork */
	mutate(p);	/* of course */
	return(p);	/* voila: a "hopeful monster". */
}

#if 0
/*
 * This is really a merge!
 */
join(o, p)
	register struct org *o, *p;
{
	register int i;

	for (i = 0; i < NGENE; i++)
		o->prog[i] = (o->prog[i] + p->prog[i]) % 256;
	for (i = 0; i < NPREF; i++)
		o->pref[i] = (o->pref[i] + p->pref[i]) % 16;
	o->ohold += p->ohold;
	o->opick += p->opick;
	o->oate += p->oate;
	o->odep += p->odep;
	o->ofork += p->ofork;
	++o->ojoin;
	o->pc = (o->pc + p->pc) % (NGENE-1);
}
#endif

/*
 * This is a crossing program.  O takes genes
 * before its PC & gives to P; then O takes everything
 * from its PC to end from P.
 */

join(o, p)
	register struct org *o, *p;
{
	register int i, lim;

	lim = o->pc;
	for (i = 0; i < lim; i++)
		p->prog[i] = o->prog[i];
	for (i = lim; i < NGENE; i++)
		o->prog[i] = p->prog[i];
#if 0
	/*
	 * If this is used, must redraw them afterwards
	 */
	for (i = 0; i < NPREF; i++) {
		o->pref[i] = (o->pref[i] + p->pref[i]) % 16;
		p->pref[i] = o->pref[i];
	}
#endif
}

syncurs()
{
	printf(PHOME); fflush(stdout);
}

/*
 * global pop density in squares/organism
 */

extern int globdens;

calgdens()
{
	if (nalive)
		return(globdens = ((BX*BY)/nalive));
	return(globdens = (BX*BY));
}

calinst(o)
	register struct org *o;
{
	register int i, r;

	r = 0;
	for (i = 0; i < NIN; i++)
		if (o->inst[i])
			++r;
	return(r * 2);
}

max(a, b)
	register int a, b;
{
	if (a > b)
		return a;
	return b;
}
