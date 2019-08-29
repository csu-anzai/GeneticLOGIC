/* parse1.c   9-9-92  parser functions for instruction set 1 */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#if INST == 1

/* in INST == 1, the array of registers maps into the registers ax, bx, cx, dx
   as follows:  c.re[0] = ax, c.re[1] = bx, c.re[2] = cx, c.re[3] = dx

    {0x00, "nop0", nop, pnop},
    {0x01, "nop1", nop, pnop},
    {0x02, "not0", not0, pnot0},
    {0x03, "shl", shl, pshl},
    {0x04, "zero", movdd, pzero},
    {0x05, "ifz", ifz, pifz},
    {0x06, "sub_ab", math, psub_ab},
    {0x07, "sub_ac", math, psub_ac},
    {0x08, "inc_a", math, pinc_a},
    {0x09, "inc_b", math, pinc_b},
    {0x0a, "dec_c", math, pdec_c},
    {0x0b, "inc_c", math, pinc_c},
    {0x0c, "pushax", push, ppushax},
    {0x0d, "pushbx", push, ppushbx},
    {0x0e, "pushcx", push, ppushcx},
    {0x0f, "pushdx", push, ppushdx},
    {0x10, "popax", pop, ppopax},
    {0x11, "popbx", pop, ppopbx},
    {0x12, "popcx", pop, ppopcx},
    {0x13, "popdx", pop, ppopdx},
    {0x14, "jmp", adr, ptjmp},
    {0x15, "jmpb", adr, ptjmpb},
    {0x16, "call", tcall, ptcall},
    {0x17, "ret", pop, pret},
    {0x18, "movcd", movdd, pmovdc},
    {0x19, "movab", movdd, pmovba},
    {0x1a, "movii", movii, pmovii},
    {0x1b, "adr", adr, padr},
    {0x1c, "adrb", adr, padrb},
    {0x1d, "adrf", adr, padrf},
    {0x1e, "mal", malchm, pmal},
    {0x1f, "divide", divide, pdivide}
*/

void pnop(ce) /* do nothing */
Pcells  ce;
{   is.iip = is.dib = 1;
}


/* void not0(ce) *(is.dreg) ^= (1 + flaw(ce));
 * is.dreg = destination register, whose bit will be flipped
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pnot0(ce) /* flip low order bit of cx */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.dran = SoupSize;
}


/* void shl(ce) *(is.dreg) <<= (Reg) (1 + flaw(ce));
 * is.dreg = destination register, whose bits will be shifted left
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pshl(ce) /* shift left all register of cx */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.dran = SoupSize;
}

/* void movdd(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pzero(ce) /* cx = 0 */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.sval = 0;
}

/* void ifz(ce) if (is.sval + flaw(ce)) is.iip = is.sval2;
 * is.sval  = value to test for zero
 * is.sval2 = amount to increment IP if is.sval == 0
 * is.iip   = amount to increment IP if is.sval != 0
 */
void pifz(ce) /* execute next instruction, if is.sval == 0 */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.sval = ce->c.re[2];
    is.sval2 = 2;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void psub_ab(ce) /* cx = ax - bx */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.sval = ce->c.re[0];
    is.sval2 = -ce->c.re[1];
    is.dran = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void psub_ac(ce) /* ax = ax - cx */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[0]);
    is.sval = ce->c.re[0];
    is.sval2 = -ce->c.re[2];
    is.dmod = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void pinc_a(ce) /* ax++ */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[0]);
    is.sval = ce->c.re[0];
    is.sval2 = 1;
    is.dmod = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void pinc_b(ce) /* bx++ */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[1]);
    is.sval = ce->c.re[1];
    is.sval2 = 1;
    is.dmod = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void pdec_c(ce) /* cx-- */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.sval = ce->c.re[2];
    is.sval2 = -1;
    is.dran = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void pinc_c(ce) /* cx++ */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.sval = ce->c.re[2];
    is.sval2 = 1;
    is.dran = SoupSize;
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushax(ce) /* push ax onto stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.sval = ce->c.re[0];
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushbx(ce) /* push bx onto stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.sval = ce->c.re[1];
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushcx(ce) /* push cx onto stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.sval = ce->c.re[2];
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushdx(ce) /* push dx onto stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.sval = ce->c.re[3];
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopax(ce) /* pop ax off of stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[0]);
    is.dmod = SoupSize;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopbx(ce) /* pop bx off of stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[1]);
    is.dmod = SoupSize;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopcx(ce) /* pop cx off of stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[2]);
    is.dran = SoupSize;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopdx(ce) /* pop dx off of stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.re[3]);
    is.dran = SoupSize;
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
void ptjmp(ce) /* outward template jump */
Pcells  ce;
{   I32s    a, s = 0;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {   
#if PLOIDY == 1
	if(soup[ad(a + s)].inst != Nop0 &&
           soup[ad(a + s)].inst != Nop1)
#else /* PLOIDY > 1 */
	if(soup[ad(a + s)][ce->d.tr].inst != Nop0 &&
           soup[ad(a + s)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        s++;
    }
    is.dreg  = &(ce->c.ip); /* destination register for address */
    is.dreg2 = &(ce->c.re[3]); /* destination register for template size */
    is.dreg3 = &BitBucket;
    is.sval  = a;  /* if template size == 0, increment IP */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.mode  = 0; /* outward jump */
    is.mode2 = 1;
    is.dib = 1;
    is.iip = 0;
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
void ptjmpb(ce) /* backward template jump */
Pcells  ce;
{   I32s    a, s = 0;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {   
#if PLOIDY == 1
	if(soup[ad(a + s)].inst != Nop0 &&
           soup[ad(a + s)].inst != Nop1)
#else /* PLOIDY > 1 */
	if(soup[ad(a + s)][ce->d.tr].inst != Nop0 &&
           soup[ad(a + s)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        s++;
    }
    is.dreg  = &(ce->c.ip); /* destination register for address */
    is.dreg2 = &(ce->c.re[3]); /* destination register for template size */
    is.dreg3 = &BitBucket;
    is.sval  = a;  /* if template size == 0, increment IP */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
    is.mode  = 2; /* backward jump */
    is.mode2 = 2;
    is.dib = 1;
    is.iip = 0;
}

/* void tcall(ce) adr(ce); push(ce); */
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
/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ptcall(ce) /* push ip to stack, outward template jump */
Pcells  ce;     /* this maps to adr followed by push */
{   I32s    a, s = 0;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {
#if PLOIDY == 1
	if(soup[ad(a + s)].inst != Nop0 &&
           soup[ad(a + s)].inst != Nop1)
#else /* PLOIDY > 1 */
	if(soup[ad(a + s)][ce->d.tr].inst != Nop0 &&
           soup[ad(a + s)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        s++;
    }
    is.dreg  = &(ce->c.ip); /* destination register for address */
    is.dreg2 = &(ce->c.re[3]); /* destination register for template size */
    is.dreg3 = &BitBucket;
    is.sval  = ad(ce->c.ip + s + 1);    /* address to be pushed onto stack */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
    is.mode  = 0; /* outward jump */
    is.mode2 = 1;
    is.dib = 1;
    is.iip = 0;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pret(ce) /* pop ip from stack */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg = &(ce->c.ip);
    is.dmod = SoupSize;
    is.iip = 0; is.dib = 1;
}

/* void movdd(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pmovdc(ce) /* dx = cx */
Pcells  ce;
{   I32s  treg;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    treg = 3 + flaw(ce);
    is.dreg = &(ce->c.re[mo(treg, NUMREG)]);
    treg = 2 + flaw(ce);
    is.sval = ce->c.re[mo(treg, NUMREG)];
    is.iip = is.dib = 1;
    if (is.dreg == &(ce->c.re[0]) || is.dreg == &(ce->c.re[1]))
        is.dmod = SoupSize;
    else
        is.dran = SoupSize;
}

/* void movdd(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pmovba(ce) /* bx = ax */
Pcells  ce;
{   I32s  treg;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    treg = 1 + flaw(ce);
    is.dreg = &(ce->c.re[mo(treg, NUMREG)]);
    treg = 0 + flaw(ce);
    is.sval = ce->c.re[mo(treg, NUMREG)];
    is.iip = is.dib = 1;
    if (is.dreg == &(ce->c.re[0]) || is.dreg == &(ce->c.re[1]))
        is.dmod = SoupSize;
    else
        is.dran = SoupSize;
}

/* void movii(ce) is.dins->inst = is.sins->inst;
 * is.dval  = address of destination instruction
 * is.dins  = pointer to destination instruction
 * is.sval  = address of source instruction
 * is.sins  = pointer to source instruction
 * is.dtra  = track of destination instruction
 * is.sval2 = original value of destination instruction
 */
void pmovii(ce)
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = ce->c.re[0] + flaw(ce);
    is.dval = ad(tval);
    tval = ce->c.re[1] + flaw(ce);
    is.sval = ad(tval);
#if PLOIDY == 1
    is.dins = &soup[is.dval];
    is.sins = &soup[is.sval];
#else /* PLOIDY > 1 */
    is.dins = &soup[is.dval][ce->d.tr];
    is.sins = &soup[is.sval][ce->d.tr];
#endif /* PLOIDY > 1 */
    is.sval2 = is.dins->inst;
    is.dtra = ce->d.tr;
    is.iip = is.dib = 1;
#ifdef HSEX
    if (ce->d.x_over_addr)
       {
       if ((!ce->d.mov_daught) && (!FindMate(ce)))
          ce->d.x_over_addr = ce->d.mate_addr = 0;
       else UseMate(ce);
       }
#endif
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
void padr(ce) /* search outward for template, return address in ax */
Pcells  ce; /* return template size in cx */
{   I32s    a, s = 0;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {
#if PLOIDY == 1
	if(soup[ad(a + s)].inst != Nop0 &&
           soup[ad(a + s)].inst != Nop1)
#else /* PLOIDY > 1 */
	if(soup[ad(a + s)][ce->d.tr].inst != Nop0 &&
           soup[ad(a + s)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        s++;
    }
    is.dreg  = &(ce->c.re[0]); /* destination register for address */
    is.dreg2 = &(ce->c.re[2]); /* destination register for template size */
    is.dreg3 = &BitBucket; /* throw away offset */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
    is.mode  = 0; /* outward jump */
    is.mode2 = 1;
    is.iip = s + 1; is.dib = 1;
    if (!s)
        is.dreg  = &BitBucket; /* dump return value if template size == 0 */
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
void padrb(ce) /* search backward for template, return address in ax */
Pcells  ce; /* return template size in cx */
{   I32s    a, s = 0;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {
#if PLOIDY == 1
	if(soup[ad(a + s)].inst != Nop0 &&
           soup[ad(a + s)].inst != Nop1)
#else /* PLOIDY > 1 */
	if(soup[ad(a + s)][ce->d.tr].inst != Nop0 &&
           soup[ad(a + s)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        s++;
    }
    is.dreg  = &(ce->c.re[0]); /* destination register for address */
    is.dreg2 = &(ce->c.re[2]); /* destination register for template size */
    is.dreg3 = &BitBucket; /* throw away offset */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
    is.mode  = 2; /* backward jump */
    is.mode2 = 2;
    is.iip = s + 1; is.dib = 1;
    if (!s)
        is.dreg  = &BitBucket; /* dump return value if template size == 0 */
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
void padrf(ce) /* search forward for template, return address in ax */
Pcells  ce; /* return template size in cx */
{   I32s    a, s = 0;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {
#if PLOIDY == 1
	if(soup[ad(a + s)].inst != Nop0 &&
           soup[ad(a + s)].inst != Nop1)
#else /* PLOIDY > 1 */
	if(soup[ad(a + s)][ce->d.tr].inst != Nop0 &&
           soup[ad(a + s)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        s++;
    }
    is.dreg  = &(ce->c.re[0]); /* destination register for address */
    is.dreg2 = &(ce->c.re[2]); /* destination register for template size */
    is.dreg3 = &BitBucket; /* throw away offset */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
    is.mode  = 1; /* forward jump */
    is.mode2 = 1;
    is.iip = s + 1; is.dib = 1;
    if (!s)
        is.dreg  = &BitBucket; /* dump return value if template size == 0 */
}

/* void malchm(ce) is.sval2 = mal(ce,&is.sval3,is.sval,is.mode2);
 *                 *(is.dreg) = is.sval3;
 *                 chmode(ce,is.sval3,is.sval2,is.mode);
 * is.dreg  = destination register where allocated address is stored
 * is.sval  = requested size of block for mal()
 * is.sval2 = flawed size of block
 * is.sval3 = suggested address, and allocated address
 * is.mode  = memory protection mode (rwx), probably MemModeProt
 * is.mode2 = memory allocation mode for mal()
 */
void pmal(ce)  /* allocate space for a new cell */
Pcells  ce;  /* allocate space for a new cell */
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.dreg  = &(ce->c.re[0]);
    is.sval  = ce->c.re[2];
    is.sval3 = -1;
    is.mode  = MemModeProt; /* only write privelages works at the moment */
    is.mode2 = MalMode;
}

/* void divide(ce) cell division
 * is.sval  = offset of IP into daughter's genome
 * is.sval2 = eject genome from soup = 0, 1 = leave in soup
 * is.mode  = divide mode (3 steps)
 */
void pdivide(ce)  /* give life to new cell by puting in queue */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.mode = 2;  /* full division */
}

#endif  /* end of INST 1 */
