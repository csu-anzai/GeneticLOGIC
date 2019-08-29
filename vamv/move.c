/*
 * Copyright 1990 Rick McGowan
 */
#include <stdio.h>
#include "prog.h"

#define undraw(x, y) printf(PDRAWC, x+XOFF, y+YOFF, board[x][y].item? '.': ' ')
#define draw(o,x,y) printf(PDRAWC, x+XOFF, y+YOFF, o->pref[P_JOIN]+'A')

extern int nalive, majcyc, optvid;
int best = 0;
struct org *oldest;	/* record oldest organism that ever existed */
int oldage = 0;		/* age of oldest recorded */
int otypes[16];	/* for keeping pop stats by type */

/*
 * These are only defaults if a Lim.File can't be found
 */
int nseed = 100;	/* number of orgs to start with */
int mindens = 60;	/* minimum density for reseeding */
int nreseed = 50;	/* number to reseed up to */
int STARVE = 200;	/* max age before starving if oate == 0 */
int MAXFOOD = 50;	/* max food per loc */
int bestinstr = 9;	/* types of instr executed to add to score */
int goodinstr = 7;	/* types of instr exec to add 1/2 to score */
int crowded = 5;	/* loc density considered too crowded to divide */
int macrowded = 18;	/* global density after which prob of expire on div */
int maxldiv = 6;	/* max local density at which div gives succ score */
int elbow = 16;		/* min glob dens (sq/org) at which div succ score */
int culdens = 10;	/* min global dens without cull operations */
int rotprob = 15;	/* percent of food which rots every 1/2 cycle */
int globdens;		/* last calc'd global density */
int divstr = 2;		/* strength needed to divide */
int synstr = 2;		/* strength gained from food synthesis */
int convert = 10;	/* holds to convert to food */
int initfood = 15;	/* percentage of locations initially w/ food */
int ifoodmax = 16;	/* max "depth" of food initialization */
int neisave = 0;	/* neighbor save when moving (1 or 0) */

extern int optSTAT, optstat, optasm, optcan, uniqid;
int didone = 0;	/* TRUE if printed org header for org 0 */
	
struct bo board[BX][BY];

struct st {
	int nmove, nmovi, nmovq, nbr, nbrnne, nbrine, njump;
	int nfood, neat, nop, npick, ndep, nbad;
	int njoin, ndivi, ndivq, ntrade, nsynth;
};
struct st S, T;

/*
 * Move a single organism
 */
move(o)
	register struct org *o;
{
	register int sign, instr, dx, dy, flag;
	register struct org *p;
	int data;
	int ox, oy;

	if ((o->id == best) && optasm) {
		if ((best == 0) && ! didone) {
			didone = 1;
			orgheader();
		}
		flag = 1;
	}
	else
		flag = 0;
	++o->oage;
	if (flag) {
		fprintf(stderr, "%4d\t", o->pc);
	}
	instr = (o->prog[o->pc++]) & 0x0F;

	++o->inst[instr];
		/* preparation of arguments */
		/* data is *pc; dx,dy are 4-bit signed qtys */
		data = o->prog[o->pc++];
		sign = (data & 0x80) ? (-1) : 1;
		dx = ((data & 0x30) >> 4) * sign;
		sign = (data & 0x08) ? (-1) : 1;
		dy = (data & 0x03) * sign;
		ox = dx; oy = dy;
		dx += o->xpo;
		dy += o->ypo;
		if (dx > BXm1) dx -= BX;
		if (dy > BYm1) dy -= BY;
		if (dx < 0) dx += BX;
		if (dy < 0) dy += BY;
/*	} */
	/* now, PC points to next instr; data is arg; dx/dy set */
	switch (instr) {
#if 0
	case NOP:	++S.nop;
if (flag) fprintf(stderr, "NOP");
			++o->badmove;
			break;
#endif

	case PICKUP:
			++S.npick;
if (flag) fprintf(stderr, "PICK\t%d,%d", ox, oy);
			if (! o->pref[P_PICK]) {
				++o->badmove;
				break;
			}
			if (o->pref[P_PICK] == o->pref[P_DEP]) {
				++o->badmove;
				break;
			}
			o->score += doscr(PICKUP);
			if (board[dx][dy].item) {
				undraw(dx, dy);
				draw(o, dx, dy);
				++o->ohold;
				board[dx][dy].item -= o->pref[P_PICK];
if (flag) fprintf(stderr, "\t\t; P %d (%d) lv %d", o->id, o->pref[P_PICK], board[dx][dy].item);
				undraw(dx, dy);
				o->score += succ(PICKUP);
/*
 * If holding lots, 4 pickups same as one bit of food...
 */
				if (o->ohold >= convert) {
					o->ohold -= convert;
					++o->oate;
					o->score += succ(FOOD);
					o->ostrength += succ(FOOD);
				}
			}
			break;
	case FOOD:
			++S.nfood;
if (flag) fprintf(stderr, "FOOD\t%d,%d", ox, oy);
			if ((! o->pref[P_EAT1]) && (! o->pref[P_EAT2])) {
				++o->badmove;
				break;
			}
			if (! board[dx][dy].item)
				break;
			o->score += doscr(FOOD);
			if ((board[dx][dy].item >= o->pref[P_EAT1]) ||
				(board[dx][dy].item >= o->pref[P_EAT2])) {
				++o->oate; o->everate = o->oage;
				o->ostrength += succ(FOOD);
				o->score += succ(FOOD);
if (flag) fprintf(stderr, "\t\t; F %d lv %d",
	max(o->pref[P_EAT1], o->pref[P_EAT2]), board[dx][dy].item);
				board[dx][dy].item -= max(o->pref[P_EAT1],
							o->pref[P_EAT2]);
				undraw(dx, dy);
			}
			break;

	case EAT:	/* eat org at x:y reg */
			++S.neat;
if (flag) fprintf(stderr, "EAT\t%d,%d", ox, oy);
			if ((! o->pref[P_EAT1]) || (! o->pref[P_EAT2])) {
				++o->badmove;
				break;
			}
			if (board[dx][dy].org == o) {
				++o->badmove;
				break;
			}
			o->score += doscr(EAT);
			if (! board[dx][dy].org) {
				break;
			}
			/*
			 * option to allow cannibalism by suppressing
			 * check for organism of same type.
			 */
			if (optcan)
				goto cannibals_ok;
			if (board[dx][dy].org->pref[P_JOIN] != o->pref[P_JOIN]) {
		cannibals_ok:
				++o->oate; o->everate = o->oage;
				o->ostrength += succ(EAT);
				o->score += succ(EAT);
if (flag) fprintf(stderr, "\t\t; E %d org %d", o->id, board[dx][dy].org->id);
				expire(board[dx][dy].org,
					"Eaten by %d\n", o->id);
			}
			break;

	case DEP:
			++S.ndep;
if (flag) fprintf(stderr, "DEP\t%d,%d", ox, oy);
			if (! o->pref[P_DEP])
				break;
			o->score += doscr(DEP);
			if (o->ohold) {
				board[dx][dy].item += o->pref[P_DEP];
if (flag) fprintf(stderr, "\t\t; D %d it %d (%d)", o->id, o->pref[P_DEP],
board[dx][dy].item);
				undraw(dx, dy);
				++o->odep;
				--o->ohold;
				o->score += succ(DEP);
			}
			else {
				++o->badmove;
			}
			break;

	case BRNNE:
			++S.nbrnne;
if (flag) fprintf(stderr, "BRNNE\t%d,%d ", ox, oy);
			if (board[dx][dy].org &&
				(board[dx][dy].org->pref[P_JOIN] == o->pref[P_JOIN])) {
				data = o->prog[o->pc++]; /* extra arg */
				goto dobranch;
			}
			else {
				data = o->prog[o->pc++]; /* pass 2nd arg! */
				if (data & 0x20) sign = (-1);
				else sign = 1;
				data = (data & 0x1F) * sign;
if (flag) fprintf(stderr, "%d\t; fallthru", data);
				break;	/* inedible */
			}
	case BRINE:
			++S.nbrine;
if (flag) fprintf(stderr, "BRINE\t%d,%d ", ox, oy);
			if (board[dx][dy].item &&
				((board[dx][dy].item >= o->pref[P_EAT1]) ||
				(board[dx][dy].item >= o->pref[P_EAT2]))) {

				data = o->prog[o->pc++]; /* pass 2nd arg! */
				if (data & 0x20) sign = (-1);
				else sign = 1;
				data = (data & 0x1F) * sign;
if (flag) fprintf(stderr, "%d\t; fallthru", data);
				break;	/* edible */
			}
			else {
				data = o->prog[o->pc++]; /* extra arg */
				goto dobranch;
			}

	case BRANCH:
			++S.nbr;
if (flag) fprintf(stderr, "BR\t");
dobranch:
			if (data & 0x20) sign = (-1);
			else sign = 1;
			data = (data & 0x1F) * sign;
if (flag) fprintf(stderr, "%d", data);
			if (data == 0)
				++o->badmove;
			o->pc += data;
			if (o->pc >= NGENE || o->pc < 0) {
				expire(o, "Illegal branch %d\n", o->pc);
					/* WOOPS! suicided! */
				return;
			}
			break;

	case JUMP:	++S.njump;
if (flag) fprintf(stderr, "JUMP\t%d", data);
			if (o->pc > 63) {
				++o->badmove; break;
			}
			o->pc = data;
			if (o->pc >= NGENE || o->pc < 0) {
				expire(o, "Illegal jump %d\n", o->pc);
					/* WOOPS! suicided! */
				return;
			}
			break;

	case MOVQ:
			++S.nmovq;
if (flag) fprintf(stderr, "MOVQ\t%d,%d", ox, oy);
			if (board[dx][dy].org) {
				if (board[dx][dy].org == o) {
					++o->badmove;
					break;
				}
if (flag) fprintf(stderr, "\t\t; occ");
				break;
			}
			else goto domove;
	case MOVI:
			++S.nmovi;
if (flag) fprintf(stderr, "MOVI\t%d,%d", ox, oy);
			if (board[dx][dy].item) {
if (flag) fprintf(stderr, "\t\t; occ");
				break;
			}
			else goto domove;
	case MOVE:
			++S.nmove;
if (flag) fprintf(stderr, "MOVE\t%d,%d", ox, oy);
domove:
			if (o->pc >= NGENE) {
				++o->badmove; break;
			}
			++o->nmove;
			o->score += succ(MOVE);
			dirmove(o, dx, dy); /* Move in dir, squash others */
if (flag) fprintf(stderr, "\t\t; to (%d,%d)", dx, dy);
			break;
	/*
	 * Either JOIN or destroy a neighboring organism.
	 */
	case JOIN:
			++S.njoin;
if (flag) fprintf(stderr, "JOIN\t%d,%d", ox, oy);
			if (o->pc >= NGENE) {
				++o->badmove; break;
			}
			o->score += doscr(JOIN);
			if ((p = board[dx][dy].org) &&
			    (p->pref[P_JOIN] == o->pref[P_JOIN])) {
				if (p == o) {
					++o->badmove;
					break;
				}
				join(o, p);
				o->score += succ(JOIN);
if (flag) fprintf(stderr, "\t\t; join suc");
			}
			break;
			
	case TRADE:
			++S.ntrade;
if (flag) fprintf(stderr, "TRADE\t%d,%d", ox, oy);
			o->score += doscr(TRADE);
#if 0
/* preparation for "trade type" in progress */
			p = board[dx][dy].org;
			if (!p || (p == o) {
				if (p == o)
					++o->badmove;
				break;
			}
			switch (tradetype) {
			case 1:	/* own type only */
				if (p->pref[P_JOIN] != o->pref[P_JOIN])

			case 2:
			case 3:
			default: break;
			}
#endif
			if ((p = board[dx][dy].org) && (p != o)) {
				/* early versions allowed organisms to */
				/* trade with self for BIG PROFITS! */
				/* re-use of sign */
				if (p->ostrength < o->ostrength) {
if (flag) fprintf(stderr, "\t\t; trade dif %d",o->ostrength - p->ostrength);
					data = o->ostrength + p->ostrength;
					if (data > 1) {
						o->ostrength = data/2;
						p->ostrength = o->ostrength;
					}
					else if (data == 1) {
						p->ostrength = 1;
						o->ostrength = 0;
					}
				}
				o->ohold += p->ohold;
if (flag) fprintf(stderr, "\t\t; hld res %d (prf %d)", o->ohold, p->ohold);
				/* he he he! profit by trade */
				o->score += succ(TRADE);
			}
			break;

	case DIVI:	/* regulated "query divide" */
/* fprintf(stderr, "DIV: locdens = %d\n", locdens(dx, dy)); */
			++S.ndivi;
if (flag) fprintf(stderr, "DIVI\t%d,%d", ox, oy);
			if ((board[dx][dy].org) || (locdens(dx, dy) >= crowded)) {
if (flag) fprintf(stderr, "\t\t; fail (occ or too dense)");
				if ((board[dx][dy].org == o) ||
					(o->ostrength < divstr)) {
					++o->badmove;
					break;
				}
				o->ostrength -= divstr;
				o->score += doscr(DIVI);
				o->score += succ(DIVI);
				break;
			}
			else goto dodivide;
	case DIVQ:
/* fprintf(stderr, "DIV: locdens = %d\n", locdens(dx, dy)); */
			++S.ndivq;
if (flag) fprintf(stderr, "DIVQ\t%d,%d", ox, oy);
dodivide:
			p = board[dx][dy].org;
			if (p == o) {
				++o->badmove;
				break;
			}
			o->score += doscr(DIVI);
			if (p && (p->pref[P_JOIN] == o->pref[P_JOIN]))
				break; /* can't div over own type */
			if (p) {
				expire(p, "Dest\n"); p = (struct org *) 0;
			}
			if (globdens <= macrowded) {
				if (probability(50)) {
					expire(o, "Too dense for div");
					return;
				}
			}
			if (o->ostrength >= divstr) {
				if ((locdens(dx, dy) <= maxldiv) &&
					(globdens >= elbow)) {
					board[dx][dy].org = p =
							   copy(o, dx, dy);
					draw(p, dx, dy);
if (flag) fprintf(stderr, "\t\t; %d -> %d", o->id, p->id);
					++o->ofork;
					o->score += succ(DIVI);
				}
				else {
if (flag) fprintf(stderr, "\t\t; fail, too dense");
				}
				o->ostrength -= divstr;
			}
			else {
if (flag) fprintf(stderr, "\t\t; fail (too weak)");
				++o->badmove;
			}
			break;

	case SYNTH:	/* synthesize food */
			++S.nsynth;
if (flag) fprintf(stderr, "SYNTH\t%d,%d", ox, oy);
			o->score += doscr(SYNTH);
/*
 * takes strength to start, but gives back more strength.
 */
			if (o->ohold && o->ostrength) {
				board[dx][dy].item += o->pref[P_DEP];
				undraw(dx, dy);
				--o->ohold;
				o->ostrength += synstr;
if (flag) fprintf(stderr, "\t\t; (%d)", o->pref[P_DEP]);
				o->score += succ(SYNTH);
				o->everate = o->oage;
			}
			else
				++o->badmove;
			break;
	default:
if (flag) fprintf(stderr, "<bad %d>", instr);
			++S.nbad; o->badmove += 2;
			break;
	}
	if ((o->everate + STARVE) < o->oage) {
		expire(o, "Starved!\n", 0);
		return;
	}

	if (o->pc >= NGENE || o->pc < 0) {
		if (o->badmove <= 2) {
			++o->badmove; ++o->pcreset;
			o->pc = 0;
			if (flag) fprintf(stderr, "PC rst %d", o->id);
		}
		else {
			expire(o, "PC ovf (%x)", o);
			return;
		}
	}
if (flag) fprintf(stderr, "\n");
}

makemoves()

{
	register int i, j, n;
	register struct org *o;
	register struct bo *b;

	n = 0;
	calgdens();	/* calculate current pop density */
	for (i = 0; i < BX; i++) {
		for (j = 0; j < BY; j++) {
			b = &(board[i][j]);
			if (o = b->org) {
				++n;
				move(o);
			}
			if (b->item > MAXFOOD) {
				b->item = 0;
				if (! b->org)
					undraw(i, j);
			}
		}
		fflush(stdout);
	}
#if 0
	if (n != nalive) {	/* just in case */
		nalive = n;
	}
#endif
}

dirmove(o, dx, dy)
	register struct org *o;
	register int dx, dy;
{
	register int ox, oy;
	register struct org *other;

	ox = o->xpo; oy = o->ypo;
	if (dx == ox && dy == oy) {
		++o->badmove;
		return;
	}
	undraw(ox, oy);	/* redraw 'item' if any */
	board[ox][oy].org = (struct org *) 0;	/* remove from here */
	if (other = board[dx][dy].org) {
		if (neisave && (other->pref[P_JOIN] == o->pref[P_JOIN])) {
			undraw(other->xpo, other->ypo);
			other->xpo = ox;	/* swap with this one */
			other->ypo = oy;
			board[ox][oy].org = other;
			draw(other, ox, oy);
		}
		else {
			expire(other, "Sqsh by %d\n", o->id);
		}
	}
	o->xpo = dx;
	o->ypo = dy;
	board[dx][dy].org = o;	/* put in new pos */
	draw(o, dx, dy);	/* draw organism */
}

#if 0
/*
 * Replaced draw() and undraw() with MACROS (then, only flush stdout on
 * every "line") to speed things up a bit.
 */
undraw(x, y)
	register int x, y;
{
	register char c;

	c = board[x][y].item ? '.' : ' ';
	printf(PDRAWC, x+XOFF, y+YOFF, c); fflush(stdout);
}

draw(o, x, y)	/* draw an organism o at x, y */
	/* register */ struct org *o;
	/* register */ int x, y;
{
	printf(PDRAWC, x+XOFF, y+YOFF, o->pref[P_JOIN] + 'A');
	fflush(stdout);
}
#endif

expire(o, s, i)
	register struct org *o;
	register char *s;
	register int i;
{
	register int x;

	if (! o)
		return;
	undraw(o->xpo, o->ypo);
	if (optasm) {
		if (s && (o->id == best)) {
			fprintf(stderr, "\t\t\t\t; Xp %d: ", o->id);
			fprintf(stderr, s, i);
		}
	}
	board[o->xpo][o->ypo].org = (struct org *) 0;	/* ! */
	++board[o->xpo][o->ypo].item;	/*  += o->pref[P_DEP]; */
	if (o->id == best)
		best = 0;
	--nalive;
	free(o);
}

init()

{
	register int i, j;
	register int x, y;
	register struct org *o;

	initrand();
	for (i = 0; i < BX; i++) {
		for (j = 0; j < BY; j++) {
			if (probability(initfood))
				board[i][j].item = range(ifoodmax-1) + 1;
			else
				board[i][j].item = 0;
		}
	}
	for (i = 0; i < BX; i++) {
		printf(PUTCUR, i+XOFF, YOFF);
		for (j = 0; j < BY; j++) {
			putchar(board[i][j].item ? '.' : ' ');
		}
	}
	nalive = 0;
	reseed(nseed);	/* initial run */
}

ncyc = 0;

reseed(dens)
	register int dens;
{
	register int x, y, tmp;
	register struct org *o;

	if (nalive >= dens) {
		++ncyc;
		return;
	}
	while (nalive < nreseed) {
		x = range(BXm1);
		y = range(BYm1);
		if (! board[x][y].org) {
			o = new();	/* adjusts nalive */
			if (tmp = board[x][y].item) {
				board[x][y].item = 0;
				o->pref[P_JOIN] = (tmp % 16);
			}
			board[x][y].org = o;
			o->xpo = x;
			o->ypo = y;
			draw(o, o->xpo, o->ypo);
		}
	}
	ncyc = 0;
}

addstat()

{
	T.nmove += S.nmove;
	T.nbr += S.nbr;
	T.nbrnne += S.nbrnne;
	T.nbrine += S.nbrine;
	T.njump += S.njump;
	T.neat += S.neat;
	T.ntrade += S.ntrade;
	T.nfood += S.nfood;
	T.nop += S.nop;
	T.npick += S.npick;
	T.ndep += S.ndep;
	T.nbad += S.nbad;
	T.nmovi += S.nmovi;
	T.nsynth += S.nsynth;
	T.njoin += S.njoin;
	T.ndivi += S.ndivi;
	T.ndivq += S.ndivq;
	T.nmove += S.nmove;
	T.nmovq += S.nmovq;
}

finstat(flag)	/* final statistics */
	int flag;	/* if 1, is FINAL */
{
	register int i, j;
	register struct org *o;
	float U, Tot;

	if (flag) {
		fprintf(stderr, "\nFinal Statistics:\n");
		fprintf(stderr, "Created: %d\n", uniqid);
	}

	U = (float) uniqid;
	if (flag) {
	fprintf(stderr, "C%5d  N=%4d  SQ/O=%4d (%4d) STB=%4d\n", majcyc,
		nalive, globdens, nalive? (BX*BY)/nalive : (BX*BY), ncyc);
	fprintf(stderr, "MOVE  %5d  MOVQ  %5d  MOVI  %5d  BR    %5d\n",
		T.nmove, T.nmovq, T.nmovi, T.nbr);
	fprintf(stderr, "BRNNE %5d  BRINE %5d  JMP   %5d  SYNTH %5d\n",
		T.nbrnne, T.nbrine, T.njump, T.nsynth);
	fprintf(stderr, "EAT   %5d  FOOD  %5d  NPCK  %5d  NDEP  %5d\n",
		T.neat, T.nfood, T.npick, T.ndep);
	fprintf(stderr, "NJON  %5d  DIVI  %5d  DIVQ  %5d  NTRD  %5d\n",
		T.njoin, T.ndivi, T.ndivq, T.ntrade);
	}

	Tot =	T.nmove + T.nmovq + T.nmovi + T.nbr
		+ T.nmove + T.nmovq + T.nmovi + T.nbr
		+ T.neat + T.nfood + T.npick + T.ndep
		+ T.njoin + T.ndivi + T.ndivq + T.ntrade;
if (flag) {
	fprintf(stderr, "\nAvg %1.1f instr per org.  Breakdown:\n", Tot/U);
	fprintf(stderr, "MOVE  %5.1f  MOVQ  %5.1f  MOVI  %5.1f  BR    %5.1f\n",
(float) T.nmove/U, (float) T.nmovq/U, (float) T.nmovi/U, (float) T.nbr/U);
	fprintf(stderr, "BRNNE %5.1f  BRINE %5.1f  JMP   %5.1f  SYNTH %5.1f\n",
(float) T.nbrnne/U, (float) T.nbrine/U, (float) T.njump/U, (float) T.nsynth/U);
	fprintf(stderr, "EAT   %5.1f  FOOD  %5.1f  NPCK  %5.1f  NDEP  %5.1f\n",
(float) T.neat/U, (float) T.nfood/U, (float) T.npick/U, (float) T.ndep/U);
	fprintf(stderr, "NJON  %5.1f  DIVI  %5.1f  DIVQ  %5.1f  NTRD  %5.1f\n",
(float) T.njoin/U, (float) T.ndivi/U, (float) T.ndivq/U, (float) T.ntrade/U);
}
if (flag)
fprintf(stderr, "\nTotal instr = %1.0f.  Instr as %% of Total:\n", Tot);
else
fprintf(stderr, "\nRunning tot = %1.0f.  Instr as %% of Total:\n", Tot);

	fprintf(stderr, "MOVE  %5.1f  MOVQ  %5.1f  MOVI  %5.1f  BR    %5.1f\n",
(float) ((float) T.nmove/Tot)*100.0, (float) ((float) T.nmovq/Tot)*100.0,
(float) ((float) T.nmovi/Tot)*100.0, (float) ((float) T.nbr/Tot)*100.0);
	fprintf(stderr, "BRNNE %5.1f  BRINE %5.1f  JMP   %5.1f  SYNTH %5.1f\n",
(float) ((float) T.nbrnne/Tot)*100.0, (float) ((float) T.nbrine/Tot)*100.0,
(float) ((float) T.njump/Tot)*100.0, (float) ((float) T.nsynth/Tot)*100.0);
	fprintf(stderr, "EAT   %5.1f  FOOD  %5.1f  NPCK  %5.1f  NDEP  %5.1f\n",
(float) ((float) T.neat/Tot)*100.0, (float) ((float) T.nfood/Tot)*100.0,
(float) ((float) T.npick/Tot)*100.0, (float) ((float) T.ndep/Tot)*100.0);
	fprintf(stderr, "NJON  %5.1f  DIVI  %5.1f  DIVQ  %5.1f  NTRD  %5.1f\n",
(float) ((float) T.njoin/Tot)*100.0, (float) ((float) T.ndivi/Tot)*100.0,
(float) ((float) T.ndivq/Tot)*100.0, (float) ((float) T.ntrade/Tot)*100.0);

	if (! flag)
		return;

	fflush(stderr);
	/*
	 * free everybody to get consistent counts in profile
	 */
	for (i = 0; i < BX; i++) {
		for (j = 0; j < BY; j++) {
			if (o = board[i][j].org) {
				free(o);
			}
		}
	}
}

pstat()

{
	register int totdone;

	totdone = S.nmove + S.nbr +S.nbrnne + S.nbrine + S.njump
			+ S.neat + S.ntrade + S.nfood
			+ S.npick + S.ndep
			+ S.nmovi + S.nsynth
			+ S.njoin + S.ndivi + S.ndivq + S.nmovq;

if (optstat) {
	addstat();
	if (optasm)
		fprintf(stderr, "\n");
	fprintf(stderr, "C%5d  N=%4d  SQ/O=%4d (%4d) STB=%4d\n", majcyc,
		nalive, globdens, nalive? (BX*BY)/nalive : (BX*BY), ncyc);
	if (optasm)
		fprintf(stderr, "\n");
	fflush(stderr);
}
else if (optSTAT) {
	addstat();
	fprintf(stderr, "\nC%5d  N=%4d  SQ/O=%4d (%4d) STB=%4d\n", majcyc,
		nalive, globdens, nalive? (BX*BY)/nalive : (BX*BY), ncyc);
		fprintf(stderr, "MOVE  %5d  MOVQ  %5d  MOVI  %5d  BR    %5d\n",
			S.nmove, S.nmovq, S.nmovi, S.nbr);
		fprintf(stderr, "BRNNE %5d  BRINE %5d  JMP   %5d  SYNTH %5d\n",
			S.nbrnne, S.nbrine, S.njump, S.nsynth);
		fprintf(stderr, "EAT   %5d  FOOD  %5d  NPCK  %5d  NDEP  %5d\n",
			S.neat, S.nfood, S.npick, S.ndep);
		fprintf(stderr, "NJON  %5d  DIVI  %5d  DIVQ  %5d  NTRD  %5d\n",
			S.njoin, S.ndivi, S.ndivq, S.ntrade);
	finstat(0);	/* not final */
	fprintf(stderr, "\n");	/* extra LF */
	fflush(stderr);
}

	S.nmovi = S.nmovq = S.nmove = S.nbr = S.njump = 0;
	S.nbrine = S.nbrnne = 0;
	S.neat = S.nop = S.npick = S.ndep = S.nbad = S.nsynth = 0;
	S.njoin = S.ndivi = S.ndivq = S.ntrade = S.nfood = 0;
	return totdone;
}

struct org *ss[BX*BY];

cull()

{
	register int i, j, n, tmp;
	register struct org *o;
	extern int optmute;

	int comp();

	n = 0;
	for (i = 0; i < BX; i++) {
		for (j = 0; j < BY; j++) {
			if (o = board[i][j].org) {
				ss[n++] = o;
				o->score -= o->badmove;
				o->score += o->ostrength;
				tmp = calinst(o);
				if (tmp > bestinstr)
					o->score += tmp;
				else if (tmp > goodinstr)
					o->score += tmp/2;
				else
					o->score /= 3;
				if (o->oage > oldage)
					oldage = o->oage;
			}
		}
	}
	/*
	 * On 5th maj cycle, check population densities
	 */
	if ((majcyc % 5) == 0) {
		for (i = 0; i < 16; i++)
			otypes[i] = 0;
		for (i = 0; i < n; i++)
			++otypes[ss[i]->pref[P_JOIN]];
	}

	if (n < 3) {
		reseed(mindens);
		return;
	}
	qsort(&(ss[0]), n, sizeof(struct org *), comp);

	i = 2 * (n / 3);

	if (optSTAT)
		fprintf(stderr, "SCORE: Best = %d, Worst = %d",
			ss[0]->score, ss[n-1]->score);
	if (globdens < culdens) {
		if (optSTAT)
			fprintf(stderr, ", Cull %d", n - i);
		while (i < n) {
			expire(ss[i], (char *) 0, 0);
			ss[i] = (struct org *) 0;
			++i;
		}
	}
	n = 2 * (n / 3);
	i = n / 3;	/* 1/3 of population */
	if (optSTAT) {
		fprintf(stderr, ", Mute %d\n", n - i);
#if 0
	if (oldest) {
		fprintf(stderr,
		"SCORE: old %d age %d pref 1:%d 2:%d D:%d J:%d P:%d (Type %c)\n",
		oldest->id, oldest->oage,
		oldest->pref[P_EAT1], oldest->pref[P_EAT2],
		oldest->pref[P_DEP], oldest->pref[P_JOIN],
		oldest->pref[P_PICK], oldest->pref[P_JOIN] + 'A');

	}
#endif
}
	best = ss[0]->id;
if (optSTAT) {
	fprintf(stderr,
		"SCORE: best ate %d, div %d, dep %d, hold %d strn %d\n",
		ss[0]->oate, ss[0]->ofork, ss[0]->odep, ss[0]->ohold,
		ss[0]->ostrength);
	fflush(stderr);
}
if (optasm) {
	orgheader();
}	
	/*
	 * if specified, mutate the "average" 1/3 of population
	 */
	if (optmute) {
		while (i < n) {
			if (i < (n-1))
				join(ss[i], ss[i+1]);
			mutate(ss[i]);
			draw(ss[i], ss[i]->xpo, ss[i]->ypo);
			++i;
		}
	}
	for (i = 0; i < BX; i++) {
		for (j = 0; j < BY; j++) {
			if (o = board[i][j].org) {
				reset(o);
			}
		}
	}
	reseed(mindens);
}

popstats()	/* called every 5th major cycle when optstat or optSTAT */
		/* can only be called immediately after cull() */
{
	register int i, j;

	fprintf(stderr, "POP: ");
	for (i = 0; i < 16; i++) {
		if (otypes[i])
			fprintf(stderr, "%c:%d ", i+'A', otypes[i]);
	}
	fprintf(stderr, "\n");
	if (optvid) {
		putc('+', stderr);
		for (i = 0; i < BY; i++)	/* woops, X/Y reversal */
			putc('-', stderr);
		putc('+', stderr);
		putc('\n', stderr);
		for (i = 0; i < BX; i++) {
			putc('|', stderr);
			for (j = 0; j < BY; j++) {
				if (board[i][j].org)
					putc(board[i][j].org->pref[P_JOIN]
						+ 'A', stderr);
				else {
					if (board[i][j].item)
						putc('.', stderr);
					else
						putc(' ', stderr);
				}
			}
			putc('|', stderr);
			putc('\n', stderr);
		}
		putc('+', stderr);
		for (i = 0; i < BY; i++)	/* woops, X/Y reversal */
			putc('-', stderr);
		putc('+', stderr);
		putc('\n', stderr);
	}
	fflush(stderr);
}

/*
 * local density in vicinity of organism
 */

locdens(x, y)
	register int x, y;
{
	register int i, j, dx, dy, dens;

	x -= 1; y -= 1;
	dens = 0;
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 3; j++) {
			dx = x + i;
			dy = y + j;
			if (dx > BXm1) dx -= BX;
			if (dy > BYm1) dy -= BY;
			if (dx < 0) dx += BX;
			if (dy < 0) dy += BY;
			if (board[dx][dy].org)
				++dens;
		}
	}
	return(dens);
}

orgheader()

{
	fprintf(stderr,
		";\n;\tORG\t%d\tpref 1:%d 2:%d D:%d J:%d P:%d (Type %c)\n;\n",
		best,
		ss[0]->pref[P_EAT1], ss[0]->pref[P_EAT2],
		ss[0]->pref[P_DEP], ss[0]->pref[P_JOIN],
		ss[0]->pref[P_PICK], ss[0]->pref[P_JOIN] + 'A');
	fflush(stderr);
}
