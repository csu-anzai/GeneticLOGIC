/* instruct.c  9-9-92  instruction set for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)instruct.c    1.22     9/19/91";
#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"

#ifdef ALCOMM
#include "tmonitor.h"
#include "trequest.h"
#endif /* ALCOMM */

#ifdef MEM_CHK
#include <memcheck.h>
#endif /* MEM_CHK */

/* takes no arguments, returns no values (except clears flag) */
void nop(ce) /* no operation */
Pcells  ce;
{   ce->c.fl = 0; }

#if INST == 2

/* void regorder(ce) adjust register order
 * is.sval  = the number of the register that will go to the top
 */
void regorder(ce) /* adjust register order */
Pcells  ce;
{   pushst(ce);
    is.sval += flaw(ce);
    ce->c.re[NUMREG] = mo(is.sval, NUMREG);
    ce->c.fl = 0;
}

void pushst(ce) /* called by regorder() */
Pcells  ce;
{   I16s  i;

    for (i = (2 * NUMREG) - 1; i > NUMREG; i--)
        ce->c.re[i] = ce->c.re[i - 1];
/*
    ce->c.re[7] = ce->c.re[6];
    ce->c.re[6] = ce->c.re[5];
    ce->c.re[5] = ce->c.re[4];
*/
}

#endif /* INST == 2 */

#if INST == 3

/* takes no arguments, returns no values (except clears flag) */
void rollu(ce)
Pcells  ce;
{   I32s  tvar;
    I16s  i;

    tvar = ce->c.re[i = NUMREG - 1] + flaw(ce);
    for (; i > 0; i--)
        ce->c.re[i] = ce->c.re[i - 1] + flaw(ce);
    ce->c.re[0] = tvar;
    ce->c.fl = 0;
}

/* takes no arguments, returns no values (except clears flag) */
void rolld(ce)
Pcells  ce;
{   I32s  tvar;
    I16s  i;

    tvar = ce->c.re[0] + flaw(ce);
    for (i = 0; i < NUMREG - 1; i++)
        ce->c.re[i] = ce->c.re[i + 1] + flaw(ce);
    ce->c.re[NUMREG - 1] = tvar;
    ce->c.fl = 0;
}

/* takes no arguments, returns no values (except clears flag) */
void enter(ce)
Pcells  ce;
{   I32s  tvar;
    I16s  i;

    for (i = NUMREG - 1; i > 0; i--)
        ce->c.re[i] = ce->c.re[i - 1] + flaw(ce);
    ce->c.re[0] += flaw(ce);
    ce->c.fl = 0;
}

/* takes no arguments, returns no values (except clears flag) */
void exch(ce)
Pcells  ce;
{   I32s  tvar;

    tvar = ce->c.re[0] + flaw(ce);
    ce->c.re[0] = ce->c.re[1] + flaw(ce);
    ce->c.re[1] = tvar;
    ce->c.fl = 0;
}

/* void math3(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void math3(ce) /* add two numbers and store them in a register, shift down */
Pcells  ce;
{   I16s  i;

    math(ce);
    for (i = 1; i < NUMREG - 1; i++)
        ce->c.re[i] = ce->c.re[i + 1] + flaw(ce);
}

void pop3(ce)
Pcells  ce;
{   I16s  i;

    for (i = NUMREG - 1; i > 0; i--)
        ce->c.re[i] = ce->c.re[i - 1] + flaw(ce);
    pop(ce);
}

/* void movdd3(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void movdd3(ce) /* inter-register mov, and push up stack */
Pcells  ce;
{   I16s  i;

    for (i = NUMREG - 1; i > 0; i--)
        ce->c.re[i] = ce->c.re[i - 1] + flaw(ce);
    movdd(ce);
}

void adr3(ce) /* find address of a template */
Pcells  ce;
{   I32s  tval;

    if (!is.sval2)
    {   *(is.dreg) = is.sval;   /* source template missing */
/*      SetFlag(ce); */
        return;
    }
    tval = 3 + flaw(ce);
    ce->c.re[mo(tval, NUMREG)] = ce->c.re[0] + flaw(ce);
    adr(ce);
}

void malchm3(ce)
Pcells  ce;
{   I16s  i;

    for (i = NUMREG - 1; i > 0; i--)
        ce->c.re[i] = ce->c.re[i - 1] + flaw(ce);
    malchm(ce);
}

#endif /* INST == 3 */

#if INST != 1

/* void not(ce) *(is.dreg) = ~(is.sval) + flaw(ce);
 * is.dreg = destination register
 * is.sval = value whose bits will be flipped and put in dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void not(ce) /* flip all bits of destination register */
Pcells  ce;
{   *(is.dreg) = ~(is.sval) + flaw(ce);
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
    ce->c.fl = 0;
}

/* void put(ce) write a value to the output buffer
 * ce->c.pb[PUTBUFSIZ]     == pointer to next output value to be written
 * ce->c.pb[PUTBUFSIZ + 1] == pointer to next output value to be read
 * ce->c.pb[PUTBUFSIZ + 2] == number of unread output values
 *
 * is.dcel  = destination cell, in whose buffer the value will be put
 * is.dreg  = destination "register" in the put buffer
 * is.dval3 = destination for address returned by adr()
 * is.mode3 = #ifdef ICC:  0 = broadcast to other cells' get buffer
 *                         1 = write to other cell's get buffer
 *            #ifndef ICC: write to own put buffer (prayer)
 *
 * #ifndef ICC specify the values used by movdd():
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 *
 * #ifdef ICC specify the values used by adr():
 * void adr(ce) find address of a template
 * is.mode  = search mode: 1 = forward, 2 = backward, 0 = outward
 * is.mode2 =  preference: 1 = forward, 2 = backward, and return for
 *        direction found: 1 = forward, 2 = backward, 3 = both, 0 = none
 * is.dval  = starting address for forward search
 * is.dval2 = starting address for backward search
 * is.dreg  = destination register where target address will be stored
 * is.dreg2 = destination register where template size will be stored
 * is.dreg3 = destination register where offset of target will be stored
 * is.sval  = return address if template size = 0
 * is.sval2 = template size, 0 = no template
 * is.sval3 = search limit, and return for distance actually searched
 * is.dmod  = modulus value for is.dreg
 * is.dmod2 = modulus value for is.dreg2
 * is.dmod3 = modulus value for is.dreg3
 * is.dran  = range to maintain for is.dreg
 * is.dran2 = range to maintain for is.dreg2
 * is.dran3 = range to maintain for is.dreg3
 */
void put(ce) /* write a value to the output buffer */
Pcells  ce;
{   Pcells  dc = is.dcel;
    I32s    ta, lpl = Put_limit;
    I8s     tmode2 = is.mode2, md;

    if (is.dreg == &BitBucket)
    {   SetFlag(ce);
        return;
    }
#ifdef ICC

    if (is.mode3) /* write to other cell's get buffer */
        Write2Get(dc, is.sval);
    else
    {   adr(ce);
        if (!ce->c.fl) do /* broadcast to other cells' get buffer */
        {   if (is.mode2 == 1 || is.mode2 == 3)
            {   ta = is.dval - 1;
                if (is.dcel = FindPutCell(ad(ta)))
                {   Write2Get(is.dcel, is.sval);
                }
            }
            if (is.mode2 == 2 || is.mode2 == 3)
            {   ta = is.dval2 - 1;
                if (is.dcel = FindPutCell(ad(ta)))
                {   Write2Get(is.dcel, is.sval);
                }
            }
            is.mode2 = tmode2;
            lpl -= is.sval3;
            is.sval3 = lpl;
            is.dval++; is.dval2--;
            adr(ce);
        } while (!ce->c.fl);
    }

#else  /* ICC */         /* write to own put buffer */

    movdd(ce);
    ce->c.pb[PUTBUFSIZ] = ++(ce->c.pb[PUTBUFSIZ]) % PUTBUFSIZ;
    ce->c.pb[PUTBUFSIZ + 2] =
        1 + ((++(ce->c.pb[PUTBUFSIZ + 2]) - 1) % PUTBUFSIZ);

#endif /* ICC */
    ce->c.fl = 0;
}

Pcells FindPutCell(adre)
I32s  adre;
{   Pcells  ce;
    I8s     md;

#if PLOIDY == 1
    if(strcmp(id[soup[adre].inst].mn, "get")
#else /* PLOIDY > 1 */
    if(strcmp(id[soup[adre][ce->d.tr].inst].mn, "get")
#endif /* PLOIDY > 1 */
        || adre < 0 || adre >= SoupSize || IsFree(adre))
        ce = NULL;
    else
    {   WhichCell(adre, &ce, &md);
/*      if (md == 'd')
            ce = NULL;
*/
    }
    return ce;
}

I8s ReadFPut(ce, value) /* for god to read the data in the output buffer */
Pcells  ce;
I32s    *value;
{   if (ce->c.pb[PUTBUFSIZ + 2])
    {   *value = ce->c.pb[ce->c.pb[PUTBUFSIZ + 1]];
        --(ce->c.pb[PUTBUFSIZ + 2]);
        ce->c.pb[PUTBUFSIZ + 1] = ++(ce->c.pb[PUTBUFSIZ + 1]) % PUTBUFSIZ;
        return 1;
    }
    return 0;
}

/* void get(ce) read a value from the input buffer
 * ce->c.gb[GETBUFSIZ]     == pointer to next input value to be read
 * ce->c.gb[GETBUFSIZ + 1] == pointer to next input value to be written
 * ce->c.gb[GETBUFSIZ + 2] == number of unread input values
 *
 * also specify the values used by movdd():
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void get(ce) /* read a value from the input buffer */
Pcells  ce;
{   if (ce->c.gb[GETBUFSIZ + 2])
    {   movdd(ce);
        --(ce->c.gb[GETBUFSIZ + 2]);
        ce->c.gb[GETBUFSIZ] = ++(ce->c.gb[GETBUFSIZ]) % GETBUFSIZ;
    }
    ce->c.fl = 0;
}

void Write2Get(ce, value) /* place value in input buffer of cell ce */
Pcells  ce;
I32s    value;
{   ce->c.gb[ce->c.gb[GETBUFSIZ + 1]] = value;
    ce->c.gb[GETBUFSIZ + 1] = ++(ce->c.gb[GETBUFSIZ + 1]) % GETBUFSIZ;
    ce->c.gb[GETBUFSIZ + 2] =
        1 + ((++(ce->c.gb[GETBUFSIZ + 2]) - 1) % GETBUFSIZ);
}

void Broad2Get(value) /* broadcast value to input buffer of all cells */
I32s  value;
{   Pcells  ce = ThisSlice;

    do
    {   Write2Get(ce, value);
        ce = &cells[ce->q.n_time.a][ce->q.n_time.i];
    } while (ce != ThisSlice);
}
    
#endif /* INST != 1 */

/* void not0(ce) *(is.dreg) ^= (1 + flaw(ce));
 * is.dreg = destination register, whose bit will be flipped
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void not0(ce) /* flip low order bit of destination register */
Pcells  ce;
{   *(is.dreg) ^= (1 + flaw(ce));
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
    ce->c.fl = 0;
}

/* void shl(ce) *(is.dreg) <<= (Reg) (1 + flaw(ce));
 * is.dreg = destination register, whose bits will be shifted left
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void shl(ce) /* shift left all bits in register */
Pcells  ce;
{   *(is.dreg) <<= (Reg) (1 + flaw(ce));
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
    ce->c.fl = 0;
}

/* void ifz(ce) if (is.sval + flaw(ce)) is.iip = is.sval2;
 * is.sval  = value to test for zero
 * is.sval2 = amount to increment IP if is.sval == 0
 * is.iip   = amount to increment IP if is.sval != 0
 */
void ifz(ce) /* execute or skip next instruction, if is.sval == 0 */
Pcells  ce;
{   if (is.sval + flaw(ce)) /* is.sval2 = 2 to skip next instruction */
        is.iip = is.sval2;  /* is.sval2 = 1 to execute next instruction */
    ce->c.fl = 0;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void math(ce) /* add two numbers and store them in a register */
Pcells  ce;
{   *(is.dreg) = is.sval + is.sval2 + flaw(ce);
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
    ce->c.fl = 0;
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void push(ce) /* push a value onto the stack */
Pcells  ce;
{   ce->c.sp = ++ce->c.sp % STACK_SIZE;
    ce->c.st[ce->c.sp] = is.sval + flaw(ce);
    ce->c.fl = 0;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pop(ce) /* pop a value from the stack into a register */
Pcells  ce;
{   *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
    if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; /* decrement stack pointer */
    else --ce->c.sp;
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
    ce->c.fl = 0;
}

/* void adr(ce) find address of a template
 * is.mode  = search mode: 1 = forward, 2 = backward, 0 = outward
 * is.mode2 =  preference: 1 = forward, 2 = backward, and return for
 *        direction found: 1 = forward, 2 = backward, 3 = both, 0 = none
 * is.dval  = starting address for forward search
 * is.dval2 = starting address for backward search
 * is.dreg  = destination register where target address will be stored
 * is.dreg2 = destination register where template size will be stored
 * is.dreg3 = destination register where offset of target will be stored
 * is.sval  = return address if template size = 0
 * is.sval2 = template size, 0 = no template
 * is.sval3 = search limit, and return for distance actually searched
 * is.dmod  = modulus value for is.dreg
 * is.dmod2 = modulus value for is.dreg2
 * is.dmod3 = modulus value for is.dreg3
 * is.dran  = range to maintain for is.dreg
 * is.dran2 = range to maintain for is.dreg2
 * is.dran3 = range to maintain for is.dreg3
 */
/* void push(ce)
 * is.sval = value to be pushed onto the stack
 */
void tcall(ce) /* call template */
Pcells  ce;
{   adr(ce);
    push(ce);
}

/* specify the values used by movdd():
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
/* void push(ce)
 * is.sval = value to be pushed onto the stack
 */
void call(ce) /* call address */
Pcells  ce;
{   movdd(ce);
    push(ce);
}

/* void mov(ce) move some data
 * is.mode = form of mov to use
 * see specific movs below for other passed values
 */
void mov(ce) /* move some data */
Pcells  ce;
{   switch (is.mode)
    {   case 0: movdd(ce); break; /*   direct destination,   direct source */
        case 1: movdi(ce); break; /*   direct destination, indirect source */
        case 2: movid(ce); break; /* indirect destination,   direct source */
        case 3: movii(ce); break; /* indirect destination, indirect source */
    }
}

/* void movdd(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void movdd(ce) /* inter-register mov */
Pcells  ce;
{   *(is.dreg) = is.sval + flaw(ce);
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
    ce->c.fl = 0;
}

/* void movdi(ce) is.dins->inst = is.sval + flaw(ce);
 * is.dval  = address of destination instruction
 * is.dins  = pointer to destination instruction
 * is.sval  = value to be moved to destination instruction
 * is.sval2 = original value of destination instruction
 */
void movdi(ce) /* move from register to soup, e.g.: soup [R0] = R1 */
Pcells  ce;
{   if ((0 <= is.dval && is.dval < SoupSize)
#ifdef WRITEPROT
        && (!is.dins->write || IsInsideCell(ce, is.dval))
#endif /* WRITEPROT */
        )
    {   is.dins->inst = is.sval + flaw(ce);
        ce->c.fl = 0;
        if (is.dval >= ce->md.p && is.dval < ce->md.p + ce->md.s)
            ce->d.mov_daught++;
        else if (is.dins->inst != is.sval2)
            MutBookeep(is.dval);
    }
    else SetFlag(ce);
}

/* void movid(ce) *(is.dreg) = is.sins->inst + flaw(ce);
 * is.sins = pointer to source instruction
 * is.sval = address of source instruction
 * is.dreg = destination register, where moved value will be placed
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void movid(ce) /* move from soup to register, e.g.: R0 = soup [R1] */
Pcells  ce;
{
#ifdef READPROT
    if (!is.sins->read || IsInsideCell(ce, is.sval))
#endif /* READPROT */
    {   *(is.dreg) = is.sins->inst + flaw(ce);
        ce->c.fl = 0;
    }
#ifdef READPROT
    else
    {   SetFlag(ce);
        return;
    }
#endif /* READPROT */
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }
}

/* void movii(ce) is.dins->inst = is.sins->inst;
 * is.dval  = address of destination instruction
 * is.dins  = pointer to destination instruction
 * is.sval  = address of source instruction
 * is.sins  = pointer to source instruction
 * is.dtra  = track of destination instruction
 * is.sval2 = original value of destination instruction
 */
void movii(ce) /* move data from soup to soup, e.g.: soup [R0] = soup [R1] */
Pcells  ce;
{   if ((is.dval != is.sval)
#ifdef WRITEPROT
        && (!is.dins->write || IsInsideCell(ce, is.dval))
#endif /* WRITEPROT */
#ifdef READPROT
        && (!is.sins->read  || IsInsideCell(ce, is.sval))
#endif /* READPROT */
        && (0 <= is.dval && is.dval < SoupSize)
        && (0 <= is.sval && is.sval < SoupSize))
    {   is.dins->inst = is.sins->inst;
        if (RateMovMut && ++CountMovMut >= RateMovMut)
        {   mut_site(soup + ad(is.dval), is.dtra);
            CountMovMut = tlrand() % RateMovMut;
            TotMovMut++;
        }
        if (WatchMov) GenExMov(ce, is.dval, is.sval);
        if (is.dval >= ce->md.p && is.dval < ce->md.p + ce->md.s)
            ce->d.mov_daught++;
        else if (is.dins->inst != is.sval2)
            MutBookeep(is.dval);
        ce->c.fl = 0;
#ifdef ALCOMM
        if ( MIsDFEnabled( TrtMVEvent ) )
            TMoveD( ce->mm.p,is.sval,is.dval);
#endif /* ALCOMM */
    }
    else SetFlag(ce);
}

/* void adr(ce) find address of a template
 * is.mode  = search mode: 1 = forward, 2 = backward, 0 = outward
 * is.mode2 =  preference: 1 = forward, 2 = backward, and return for
 *        direction found: 1 = forward, 2 = backward, 3 = both, 0 = none
 * is.dval  = starting address for forward search
 *                and return for finish address
 * is.dval2 = starting address for backward search
 *                and return for finish address
 * is.dreg  = destination register where target address will be stored
 * is.dreg2 = destination register where template size will be stored
 * is.dreg3 = destination register where offset of target will be stored
 * is.sval  = return address if template size = 0
 * is.sval2 = template size, 0 = no template
 * is.sval3 = search limit, and return for distance actually searched
 * is.dmod  = modulus value for is.dreg
 * is.dmod2 = modulus value for is.dreg2
 * is.dmod3 = modulus value for is.dreg3
 * is.dran  = range to maintain for is.dreg
 * is.dran2 = range to maintain for is.dreg2
 * is.dran3 = range to maintain for is.dreg3
 */
void adr(ce) /* find address of a template */
Pcells  ce;
{   I32s adrt;

    if (!is.sval2)
    {   *(is.dreg) = is.sval;   /* source template missing */
/*      SetFlag(ce); */
        return;
    }
    if (!is.mode)                 /* outward search */
        adrt = ctemplate(&is.dval, &is.dval2, &is.sval3, &is.mode2, is.sval2,
            'o', ce);
    else if (is.mode == 1)        /* forward search */
        adrt = ctemplate(&is.dval, &is.dval2, &is.sval3, &is.mode2, is.sval2,
            'f', ce);
    else if (is.mode == 2)        /* backward search */
        adrt = ctemplate(&is.dval, &is.dval2, &is.sval3, &is.mode2, is.sval2,
            'b', ce);
    if (adrt < 0)                 /* target template not found */
    {   is.iip = is.sval2 + 1;    /* skip IP over source template */
        SetFlag(ce);
        return;
    }

    *(is.dreg3) = is.sval3;
    if (is.dmod3)
    {   *(is.dreg3) = mo(*(is.dreg3), is.dmod3);
        is.dmod3 = 0;
    }
    else if (is.dran3 && (*(is.dreg3) > is.dran3 || *(is.dreg3) < -is.dran3))
    {   *(is.dreg3) = 0;
        is.dran3 = 0;
    }

    *(is.dreg2) = is.sval2;
    if (is.dmod2)
    {   *(is.dreg2) = mo(*(is.dreg2), is.dmod2);
        is.dmod2 = 0;
    }
    else if (is.dran2 && (*(is.dreg2) > is.dran2 || *(is.dreg2) < -is.dran2))
    {   *(is.dreg2) = 0;
        is.dran2 = 0;
    }

    *(is.dreg) = adrt;
    if (is.dmod)
    {   *(is.dreg) = mo(*(is.dreg), is.dmod);
        is.dmod = 0;
    }
    else if (is.dran && (*(is.dreg) > is.dran || *(is.dreg) < -is.dran))
    {   *(is.dreg) = 0;
        is.dran = 0;
    }

    ce->c.fl = 0;
    return;
}

/* void malchm(ce) allocate space and protect it
 * is.dreg  = destination register where allocated address is stored
 * is.sval  = requested size of block for mal()
 * is.sval2 = flawed size of block
 * is.sval3 = suggested address, and allocated address
 * is.mode  = memory protection mode (rwx), probably MemModeProt
 * is.mode2 = memory allocation mode for mal()
 */
void malchm(ce) /* allocate space and protect it */
Pcells  ce;
{   /* is.sval = requested size of block, is.sval2 = flawed size of block */
    /* is.sval3 = suggested address, & returned address */
    /* is.dreg = location of block, is.mode2 = allocation style */

    if (!(is.sval2 = mal(ce,&is.sval3,is.sval,is.mode2)))
    {   SetFlag(ce);
        return ;
    }
    *(is.dreg) = is.sval3;

    /* is.sval3 = location of chmoded block */
    /* is.sval2 = size of chmoded block */
    /* is.mode  = chmod mode, unix notation, e.g. 7 = full protection */

    if (chmode(ce,is.sval3,is.sval2,is.mode)) /* could be MemModeProt */
        SetFlag(ce);
    else ce->c.fl = 0;
#ifdef HSEX
    if (DoMate())
       SetXover(ce);
    else
       ce->d.x_over_addr = ce->d.mate_addr = 0;
#endif /* ifdef HSEX */
}

/* void divide(ce) cell division
 * is.sval  = offset of IP into daughter's genome
 * is.sval2 = eject genome from soup = 0, 1 = leave in soup
 * is.mode  = divide mode (3 steps)
 */
void divide(ce) /* cell division */
Pcells  ce;
{   Pcells nc;           /* pointer to the new cell */
    I32s  i, j, found = 0;
    CellInd  ni;

    if (ce->md.s < MinCellSize ||
        ce->d.mov_daught < (I32s) (ce->md.s * MovPropThrDiv))
    {   SetFlag(ce);
        return;
    }
    if (DivSameSiz)
    {   if (ce->mm.s != ce->md.s)
        {   SetFlag(ce);
            return;
        }
        if (DivSameGen &&
            !IsSameGen(ce->mm.s, soup + ce->md.p, soup + ce->mm.p))
        {   SetFlag(ce);
            return;
        }
    }
    if (is.sval2) switch (is.mode) {
    case 0:           /* create cpu */
    {   if ((ce->d.ne.a == ce->q.this.a) && (ce->d.ne.i == ce->q.this.i))
            /* if there is no cpu (first call to div 0) */
        {   nc = GetFreeCell();
            nc->ld = 1;
            nc->mm = ce->md;
            nc->d.dm = 1;
            nc->d.genome = soup + nc->mm.p;
            ce->d.ne = nc->q.this;
            nc->c.ip = nc->mm.p;
#if INST == 2
            nc->c.ip += (is.sval % nc->mm.s);
            for (i = 0; i < NUMREG; i++)
                nc->c.re[i] = ce->c.re[i];
#endif /* INST == 2 */
        }
        else      /* if there is a cpu (second call to div 0) */
        {   nc = &cells[ce->d.ne.a][ce->d.ne.i];
            if (nc->d.is)   /* call to div 0 after call to div 1 */
            {   RmvFrmSlicer(nc);
                nc->d.is = 0;
            }
            else     /* two sequential calls to div 0, error */
            {   SetFlag(ce);
                 return;
            }
        }
        break;
    }
    case 1:           /* start cpu */
    {   if ((ce->d.ne.a == ce->q.this.a) && (ce->d.ne.i == ce->q.this.i))
            /* if there is no cpu, div 1 before div 0 */
        {   nc = GetFreeCell();
            nc->ld = 1;
            nc->mm = ce->md;
            nc->d.dm = 1;
            nc->d.genome = soup + nc->mm.p;
            ce->d.ne = nc->q.this;
            nc->c.ip = nc->mm.p;
#if INST == 2
            nc->c.ip += (is.sval % nc->mm.s);
            for (i = 0; i < NUMREG; i++)
                nc->c.re[i] = ce->c.re[i];
#endif /* INST == 2 */
        }
        else     /* if there is already a cpu, make pointers to it */
            nc = &cells[ce->d.ne.a][ce->d.ne.i];
        if (nc->d.is) /* 2nd call to div 1, cpu is already started */
        {   RmvFrmSlicer(nc);
            nc->d.is = 0;
        }
        else    /* not 2nd call to div 1, cpu is not already started */
        {   EntBotSlicer(nc);
            nc->d.is = 1;
        }
        break;
    }
    case 2: /* split cells */
    {   if ((ce->d.ne.a == ce->q.this.a) && (ce->d.ne.i == ce->q.this.i))
            /* if there is no cpu, div 2 before div 0 */
        {   nc = GetFreeCell();
            nc->ld = 1;
            nc->mm = ce->md;
            nc->d.genome = soup + nc->mm.p;
            nc->c.ip = nc->mm.p;
#if INST == 2
            nc->c.ip += (is.sval % nc->mm.s);
            for (i = 0; i < NUMREG; i++)
                nc->c.re[i] = ce->c.re[i];
#endif /* INST == 2 */
        }
        else
            nc = &cells[ce->d.ne.a][ce->d.ne.i];
        if (!nc->d.is)   /* no slicer, div 2 before div 1 */
        {   EntBotSlicer(nc);
            nc->d.is = 1;
        }
        ce->md.s = ce->md.p = 0;
        ce->d.ne = ce->q.this; /* clean up if div 0 or 1 before 2 */
        nc->d.dm = 0;
        EntBotReaper(nc);
        DownReperIf(ce);
        DivideBookeep(ce, nc);
    }
    } /* switch */
#if INST != 1
    else
        Emigrate(ce);
#endif /* INST != 1 */
    ce->c.fl = 0;
}

#if INST != 1

void Emigrate(ce)
Pcells  ce;
{   I32s    i;
    FpInst  p = soup + ce->md.p;

/* write genome to ejection function here */

/* the following code erases the genome from the soup */
#ifdef ERROR
    if (!ce->md.s)
        FEError(-613,EXIT,WRITE, "Tierra Emigrate() error: ce->md.s = 0");
#endif /* ERROR */
    for (i = 0; i < ce->md.s; i++)
        p[i].inst = 0;
#ifdef ERROR
    if (ce->md.p < 0 || ce->md.p >= SoupSize)
    FEError(-613,EXIT,WRITE, "Tierra Emigrate() error: ce->md.p not in soup");
#endif /* ERROR */
    chmode(ce, ce->md.p, ce->md.s, MemModeFree); 
    MemDealloc(ce->md.p, ce->md.s);
    ce->d.mov_daught = 0;
    ce->d.fecundity++;
    ce->md.s = 0;
}

#endif /* INST != 1 */

Pcells GetFreeCell()
{   Pcells fc;
    I32s       i, j, found = 0;

    if (++NumCells > CellsSize - 2)
        CheckCells();
    for (i = 0; i < NumCelAr; i++)  /* find unoccupied cell struct */
    {   for (j = 0; j < CelArSiz; j++)
        {
#ifdef ERROR
            if (i * j >= CellsSize)
                FEError(-502,EXIT,WRITE, 
                    "Tierra GetFreeCell() error, exiting");
#endif
            if(!cells[i][j].ld)
            {   found = 1;
                fc = &cells[i][j];
                break;
            }
        }
        if (found)
            break;
    }
    InitCell(i, j, fc);
    return fc;
}

void CheckCells() /* check and adjust memory allocation if necessary */
{   I32s j, oCellsSize = CellsSize;
    Pcells Fp  tcells;

#ifdef ERROR
    sprintf(mes[0], "in_div CheckCells: recalloc, NumCells = %ld", NumCells);
    sprintf(Buff, "    old CellsSize = %ld  ", CellsSize);
#endif
    NumCelAr++;
    tcells = (Pcells Fp) trecalloc((I8s Fp) cells,
        (I32u) sizeof(Pcells) * (I32u) NumCelAr,
        (I32u) sizeof(Pcells) * (I32u) (NumCelAr - 1));
    if (tcells)
        cells = tcells;
    else if (cells)
    {   tfree(cells);
        cells = NULL;
        FEError(-503,EXIT,WRITE, 
        "Tierra CheckCells() cells trecalloc error, out of memory, exiting");
    }
    CellsSize = NumCelAr * CelArSiz;
    cells[NumCelAr - 1] = (Pcells) tcalloc(CelArSiz, sizeof(Cell));
    if (cells[NumCelAr - 1] == NULL)
    {   FEError(-504,EXIT,WRITE, 
         "Tierra CheckCells() cells[] tcalloc error, out of memory, exiting");
    }
#ifdef ERROR
    sprintf(mes[1], "%s new CellsSize = %ld", Buff, CellsSize);
    FEMessage(2,mes);
#ifdef __TURBOC__
    sprintf(mes[0], "coreleft = %lu  divide (cells)", coreleft());
    FEMessage(1,mes);
#endif
#endif
    for (j = 0; j < CelArSiz; j++)
        InitCell(NumCelAr - 1, j, &cells[NumCelAr - 1][j]);
}

I32s flaw(ce)
Pcells  ce;
{   CountFlaw++;
    if (RateFlaw && CountFlaw >= RateFlaw)
    {   CountFlaw = tlrand() % RateFlaw;
        TotFlaw++;
        ce->d.flaw++;
        if (tcrand() % 2) return 1;
        return -1;
    }
    return 0;
}

/* search in specified direction for
 * nop template return address, returns address of instruction following
 * target template, i.e., target + tz
 * NOTE: ce->c.ip must point to the instruction (agent) being executed
 */
I32s ctemplate(f, b, slim, mode, tz, dir, ce)
I32s *f;    /* starting address for forward search */
I32s *b;    /* starting address for backward search */
I32s *slim; /* search limit, and return for distance searched */
I8s  *mode; /* preference for forward (1) or backward (2) target */
I32s tz;    /* template size */
I32s dir;   /* direction of search, f = forward, b = backward, o = out */
Pcells  ce; /* which cell */
{   I32s  o, l = 1, adrt;
    I32s  i = 0, fmatch = 0, bmatch = 0;
    I8s   df, db;
    Pgl   tgl;

    if ((tz < MinTemplSize) || (tz > SoupSize))
    {   adrt = -1;
        *mode = 0;
        goto finish;
    }
    if ((I8s) dir == 'o')      /* both directions */
        df = db = 1;
    else if ((I8s) dir == 'f') /* forward only */
    {   df = 1;
        db = 0;
    }
    else if ((I8s) dir == 'b') /* backwards only */
    {   df = 0;
        db = 1;
    }
    o = ad(ce->c.ip + 1);
    while (1) {
    while (1) /* this skips sections of codes that are not templates (NOPs) */
    {
#if PLOIDY == 1
        if ((df &&               /* forward */
            (soup[*f].inst == Nop0 || soup[*f].inst == Nop1)
#ifdef READPROT
            && (!soup[*f].read || IsInsideCell(ce,*f))
#endif /* READPROT */
            )
         || (db &&           /* backward */
            (soup[*b].inst == Nop0 || soup[*b].inst == Nop1)
#ifdef READPROT
            && (!soup[*b].read || IsInsideCell(ce,*b))
#endif /* READPROT */
            ))
#else /* PLOIDY > 1 */
        if ((df &&     /* forward */
            (soup[*f][ce->d.tr].inst == Nop0 || soup[*f][ce->d.tr].inst==Nop1)
#ifdef READPROT
            && (!soup[*f][ce->d.tr].read || IsInsideCell(ce,*f))
#endif /* READPROT */
            )
            || (db &&  /* backward */
            (soup[*b][ce->d.tr].inst == Nop0 || soup[*b][ce->d.tr].inst==Nop1)
#ifdef READPROT
            && (!soup[*b][ce->d.tr].read || IsInsideCell(ce,*b))
#endif /* READPROT */
            ))
#endif /* PLOIDY > 1 */
            break;
        if (df)
        {   (*f)++;
            *f = ad(*f);
        }
        if (db)
        {   (*b)--;
            *b = ad(*b);
        }
        l++;
        if (l > *slim)   /* if we exceed the search limit abort */
        {   adrt = -1;
            *mode = 0;
            goto finish;
        }
    }

    /* forward */
#if PLOIDY == 1
    if (df && (soup[*f].inst == Nop0    /* if NOPs */
           ||  soup[*f].inst == Nop1))
#else /* PLOIDY > 1 */
    if (df && (soup[*f][ce->d.tr].inst == Nop0    /* if NOPs */
           ||  soup[*f][ce->d.tr].inst == Nop1))
#endif /* PLOIDY > 1 */
    {   fmatch = 1;
        for (i = 0; i < tz; i++)   /* over the full template size */
        {   
#if PLOIDY == 1
            if (soup[ad(o + i)].inst +
                soup[ad(*f + i)].inst - NopS
#ifdef READPROT
                || (soup[ad(*f + i)].read
                && !IsInsideCell(ce,ad(*f + i)))
#endif /* READPROT */
#else /* PLOIDY > 1 */
            if (soup[ad(o + i)][ce->d.tr].inst +
                soup[ad(*f + i)][ce->d.tr].inst - NopS
#ifdef READPROT
                || (soup[ad(*f + i)][ce->d.tr].read
                && !IsInsideCell(ce,ad(*f + i)))
#endif /* READPROT */
#endif /* PLOIDY > 1 */
                )
            {   fmatch = 0;
                break;
            }
        }
    }
    else fmatch = 0;

    /* backward */
#if PLOIDY == 1
    if (db && (soup[*b].inst == Nop0    /* if NOPs */
           ||  soup[*b].inst == Nop1))
#else /* PLOIDY > 1 */
    if (db && (soup[*b][ce->d.tr].inst == Nop0    /* if NOPs */
           ||  soup[*b][ce->d.tr].inst == Nop1))
#endif /* PLOIDY > 1 */
    {   bmatch = 1;
        for (i = 0; i < tz; i++)  /* over the full template size */
        {
#if PLOIDY == 1
            if (soup[ad(o + i)].inst +
                soup[ad(*b + i)].inst - NopS
#ifdef READPROT
                || (soup[ad(*b + i)].read
                && !IsInsideCell(ce,ad(*b + i)))
#endif /* READPROT */
#else /* PLOIDY > 1 */
            if (soup[ad(o + i)][ce->d.tr].inst +
                soup[ad(*b + i)][ce->d.tr].inst - NopS
#ifdef READPROT
                || (soup[ad(*b + i)][ce->d.tr].read
                && !IsInsideCell(ce,ad(*b + i)))
#endif /* READPROT */
#endif /* PLOIDY > 1 */
                )
            {   bmatch = 0;
                break;
            }
        }
    }
    else bmatch = 0;

    if (fmatch && bmatch)
    {   if (*mode == 1)
        {   *f += flaw(ce);
            adrt = ad(*f + tz);
            *mode = 3;
            goto finish;
        }
        else if (*mode == 2)
        {   *b += flaw(ce);
            adrt = ad(*b + tz);
            *mode = 3;
            goto finish;
        }
    }
    else if (fmatch)
    {   *f += flaw(ce);
        adrt = ad(*f + tz);
        *mode = 1;
        goto finish;
    }
    else if (bmatch)
    {   *b += flaw(ce);
        adrt = ad(*b + tz);
        *mode = 2;
        goto finish;
    }

    if (db)      /* increment search pointers, backward and forward */
    {   (*b)--;
        *b = ad(*b);
    }
    if (df)
    {   (*f)++;
        *f = ad(*f);
    }
    l++;
    if (l > *slim)   /* if we exceed the search limit abort */
    {   adrt = -1;
        *mode = 0;
        goto finish;
    }
    }  /* outermost while(1) */
finish:
    *slim = l;
    if (1 && WatchTem)
    tgl = sl[ce->d.gen.size]->g[ce->d.gi];
    if (1 && WatchTem && adrt >= 0 && !ce->d.flaw && !ce->d.mut &&
        ce->mm.p <= ce->c.ip && ce->c.ip < (ce->mm.p + ce->mm.s) &&
        IsBit(tgl->bits, 0))
    GenExTemp(ad(adrt - tz), ce, tz);
    return adrt;  /* address of instruction following target template */
}

#ifdef FUTURE

I32s template(f, b, slim, tz, dir, mode, ce)
 /* search in specified direction for */
 /* nop template return address, returns address of instruction following */
 /* target template, i.e., target + tz */
 /* NOTE: ce->c.ip must point to the instruction (agent) being executed */
    I32s f;    /* starting address for forward search */
    I32s b;    /* starting address for backward search */
    I32s *slim;
    I32s tz;  /* template size */
    I32s dir;  /* direction of search, f = forward, b = backward, o = out */
    I8s mode; /* match mode: 0 = complement, 1 = direct */
    Pcells  ce; /* which cell */
{
    I32s o, l = 1, adrt;
    I32s i = 0, match;
    I8s df, db;
    Pgl tgl;

    if ((tz < MinTemplSize) || (tz > SoupSize))
    {   adrt = -1;
        goto finish;
    }
    if ((I8s) dir == 'o') df = db = 1;       /* both directions */
    if ((I8s) dir == 'f') /* forward only */
    {   df = 1;
        db = 0;
    }
    if ((I8s) dir == 'b') /* backwards only */
    {   df = 0;
        db = 1;
    }
    o = ad(ce->c.ip + 1);
    while (1) {
    while (1) /* this skips sections of codes that are not templates (NOPs) */
    {   
        if (df && (soup[f][ce->d.tr].inst == Nop0 /* forward */
               ||  soup[f][ce->d.tr].inst == Nop1))
        break;
        else
        {   f++;
            f = ad(f);
        }
        if (db && (soup[b][ce->d.tr].inst == Nop0 /* backward */
               ||  soup[b][ce->d.tr].inst == Nop1))
        break;
        else
        {   b--;
            b = ad(b);
        }
    }
    match = 1;       /* forward */
    if (df && (soup[f][ce->d.tr].inst == Nop0
           ||  soup[f][ce->d.tr].inst == Nop1))  /* if NOPs */
    {   if (!mode)  /* compliment match mode */
        {   for (i = 0; i < tz; i++) /* over the full template size */
            {   if (!(soup[ad(o + i)][ce->d.tr].inst   /* if not compl */
                    - soup[ad(f + i)][ce->d.tr].inst ))
                {   match = 0;
                    break;
                }
            }
        }
        else  /* direct match mode */
        {   for (i = 0; i < tz; i++)  /* over the full template size */
            {   if (soup[ad(o + i)][ce->d.tr].inst         /* if compl */
                    - soup[ad(f + i)][ce->d.tr].inst )
                {   match = 0;
                    break;
                }
            }
        }
        if (match)
        {   f += flaw(ce);
            adrt = ad(f + tz);
            goto finish;
        }
    }
    match = 1;  /* backward */
    if (db && (soup[b][ce->d.tr].inst == Nop0
           ||  soup[b][ce->d.tr].inst == Nop1))  /* if NOPs */
    {   if (!mode)  /* compliment match mode */
        {   for (i = 0; i < tz; i++)  /* over the full template size */
            {   if (!(soup[ad(o + i)][ce->d.tr].inst
                    - soup[ad(b + i)][ce->d.tr].inst ))
                {   match = 0;
                    break;
                }
            }
        }
        else  /* direct match mode */
        {   for (i = 0; i < tz; i++) /* over the full template size */
            {   if (soup[ad(o + i)][ce->d.tr].inst
                    - soup[ad(b + i)][ce->d.tr].inst)
                {   match = 0;
                    break;
                }
            }
        }
        if (match)
        {   b += flaw(ce);
            adrt = ad(b + tz);
            goto finish;
        }
    }   /* increment search pointers, backward and forward */
    if (db)
    {   b--;
        b = ad(b);
    }
    if (df)
    {   f++;
        f = ad(f);
    }
    l++;
    if (l > *slim)  /* if we exceed the search limit abort */
    {   adrt = -1;
        goto finish;
    }
    }
finish:
    *slim = l;
    if (1 && WatchTem)
    tgl = sl[ce->d.gen.size]->g[ce->d.gi];
    if (1 && WatchTem && adrt >= 0 && !ce->d.flaw && !ce->d.mut &&
        ce->mm.p <= ce->c.ip && ce->c.ip < (ce->mm.p + ce->mm.s) &&
        IsBit(tgl->bits, 0))
    GenExTemp(ad(adrt - tz), ce, tz);
    return adrt; /* address of instruction following target template */
}

I32s btemplate(f, b, slim, tz, dir, mode, ce)
 /* search in specified direction for */
 /* binary template return address, returns address of instruction */
 /* following target template, i.e., target + tz */
 /* NOTE: ce->c.ip must point to the instruction (agent) being executed */
    I32s f;    /* starting address for forward search */
    I32s b;    /* starting address for backward search */
    I32s *slim;
    I32s tz;  /* template size */
    I8s dir;  /* direction of search, f = forward, b = backward, o = out */
    I8s mode; /* match mode: 0 = complement, 1 = direct */
    Pcells  ce; /* which cell */
{   I32s o, l = 1, adrt;
    I32s i = 0, match;
    I32s  fd = 0, bd = 0; /* search distance, forward and backward */
    I8s df, db;
    Pgl tgl;

    if ((tz < MinTemplSize) || (tz > SoupSize))
    {   adrt = -1;
        goto finish;
    }
    if (dir == 'o') df = db = 1; /* both directions */
    if (dir == 'f') /* forward only */
    {   df = 1;
        db = 0;
    }
    if (dir == 'b') /* backwards only */
    {   df = 0;
        db = 1;
    }
    o = ad(ce->c.ip + 1);
    while (1) {
    match = 1;
    if (df)  /* if direction forwards */
    {   if (!mode) /* compliment match mode */
        {   for (i = 0; i < tz; i++) /* for full template size */
            {   if ((soup[ad(o + i)][ce->d.tr].inst
                ^ soup[ad(f + i)][ce->d.tr].inst) == 31)
                {   match = 0;
                    break;
                }
            }
        }
        else  /* direct match mode */
        {   for (i = 0; i < tz; i++) /* for full template size */
            {   if (soup[ad(o + i)][ce->d.tr].inst
                    == soup[ad(f + i)][ce->d.tr].inst)
                {   match = 0;
                    break;
                }
            }
        }
        if (match)
        {   f += flaw(ce);
            adrt = ad(f + tz);
            goto finish;
        }
    }
    if (db) /* if direction backwards */
    {   match = 1;
        if (!mode) /* compliment match mode */
        {   for (i = 0; i < tz; i++) /* for full template size */
            {   if ((soup[ad(o + i)][ce->d.tr].inst
                ^ soup[ad(b + i)][ce->d.tr].inst) == 31)
                {   match = 0;
                    break;
                }
            }
        }
        else  /* direct match mode */
        {   for (i = 0; i < tz; i++) /* for full template size */
            {   if (soup[ad(o + i)][ce->d.tr].inst
                    == soup[ad(b + i)][ce->d.tr].inst)
                {   match = 0;
                    break;
                }
            }
        }
        if (match)
        {   b += flaw(ce);
            adrt = ad(b + tz);
            goto finish;
        }
    }   /* increment search pointers, backward and forward */
    if (db)
    {   b--;
        b = ad(b);
    }
    if (df)
    {   f++;
        f = ad(f);
    }
    l++;
    if (l > *slim)
    {   adrt = -1;
        goto finish;
    }
    }
finish:
    *slim = l;
    if (1 && WatchTem)
    tgl = sl[ce->d.gen.size]->g[ce->d.gi];
    if (1 && WatchTem && adrt >= 0 && !ce->d.flaw && !ce->d.mut &&
        ce->mm.p <= ce->c.ip && ce->c.ip < (ce->mm.p + ce->mm.s) &&
        IsBit(tgl->bits, 0))
    GenExTemp(ad(adrt - tz), ce, tz);
    return adrt; /* address of instruction following target template */
}

#endif /* FUTURE */

/* ------------------------------------------------------------------- */
#ifdef HSEX
   /* put this at movii at point of  x over  & at mal */

I8s FindMate(ce)
Pcells ce;
{
/* search out FORWARD (sorry ) to find cell of roughly the close size
 * but diff genotype
 * insert addr into ce->d.mate_addr
 * if (ce->d.x_over_addr > 0)
 *    ce->d.mate_addr = me->mm.p;
 * else
 *    ce->d.mate_addr = me->mm.p + ce->d.x_over_addr;
 * if one can't be found, return 0
 *
 */

Pcells me;
I32s fi,fmasl,ll;
I8s tmd, fi_not_free=0;

#ifdef ERROR
if ((!ce))
   {
   FEError(-12666,NOEXIT,NOWRITE,"FindMate - Bad cell passed !\n");
   return 0;
   }
#endif
/* bi = ad(ce->mm.p -1);   byte before cell */
fi = ad(ce->mm.p +ce->mm.s+1);  /* byte after cell */
fmasl = ad( (I32s) ( MateSearchL * AverageSize));

   while(1)
      {
      /* forward */
      if (!(IsFree(fi)))
         {
         fi_not_free =1;
         WhichCell(fi,&me,&tmd);
         if ((!MateSearchL) && (me == ce)) return 0;
         if ((tmd == 'm') &&
             (me->mm.s >= (ce->mm.s - MateSizeEp)) &&
             (me->mm.s <= (ce->mm.s + MateSizeEp)))
             {
             if (!IsSameGen(ce->mm.s,&soup[ce->mm.p],&soup[me->mm.p]))
                {
                if(ce->d.x_over_addr < 0)
                  {
                  ce->d.mate_addr = me->mm.p;
                  }
                else
                  {
                  ce->d.mate_addr = ad(me->mm.p + ce->d.x_over_addr);
                  }
                return 1;
                }

             }
         }

      /* not found so inc soup indexs */
      if(fi_not_free)
        {
        fi_not_free=0;
        fi = ad(me->mm.s+me->mm.p +1);
        }
      else fi = ad(fi+MinCellSize -1);

      /* check to see if we are done */
      if( fi < ce->mm.p) /* we have rolled of the top */
        {
        ll = SoupSize - ce->mm.p + fi;
        }
      else
        {
        ll = fi - ce->mm.p;
        }
      if ((!MateSearchL) && (ll > fmasl)) return 0;
      }
}       /* end of FindMate */

/* have func that chooses mate or not in mal */
I8s DoMate()
{
if ((NumCells) && (MateProb > 0.0 ) && ((tlrand() %101) < (100*MateProb))) 
   return 1;
return 0;
}
/* then have func that chooses x_over */
I16s SetXover(ce)
Pcells ce;
{
return (((tlrand() %2) ? 1: -1) *
        (tlrand() % ((I32s) (MateXoverProp * ce->mm.s)+1)));
}

void UseMate(ce)
Pcells ce;
{
if ((( ce->d.x_over_addr > 0) &&                /* second half, ! > x_over */
     (ce->d.mov_daught < ce->d.x_over_addr)) ||
    (( ce->d.x_over_addr < 0) &&                /* first half, > x_over */
     (ce->d.mov_daught > (-1*ce->d.x_over_addr))))
   {
   is.sval = ad((is.sval - ce->mm.p) + ce->d.mate_addr);
#if PLOIDY == 1
   is.dins = &soup[is.sval];
#else /* PLOIDY > 1 */
   is.dins = &soup[is.sval][ce-c.tr];
#endif /* PLOIDY > 1 */
   }
}

#endif /* ifdef HSEX */

/* ------------------------------------------------------------------- */
/* ------------------------------------------------------------------- */
