/* parse4.c   9-9-92  parser functions for instruction set 4 */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#if INST == 4

/* in INST == 4, the array of registers maps into the registers ax, bx, cx, dx
   as follows:  c.re[0] = ax, c.re[1] = bx, c.re[2] = cx, c.re[3] = dx

    {0x00, "nop0", nop, pnop},
    {0x01, "nop1", nop, pnop},
    {0x02, "movdi", movdi, pmovdi},
    {0x03, "movid", movid, pmovid},
    {0x04, "movii", movii, pmovii},
    {0x05, "pushax", push, ppushax},
    {0x06, "pushbx", push, ppushbx},
    {0x07, "pushcx", push, ppushcx},
    {0x08, "pushdx", push, ppushdx},
    {0x09, "popax", pop, ppopax},
    {0x0a, "popbx", pop, ppopbx},
    {0x0b, "popcx", pop, ppopcx},
    {0x0c, "popdx", pop, ppopdx},
    {0x0d, "put", put, pput},
    {0x0e, "get", get, pget},
    {0x0f, "inc", math, pinc},
    {0x10, "dec", math, pdec},
    {0x11, "add", math, padd},
    {0x12, "sub", math, psub},
    {0x13, "zero", movdd, pzero},
    {0x14, "shl", shl, pshl},
    {0x15, "not0", not0, pnot0},
    {0x16, "ifz", ifz, pifz},
    {0x17, "iffl", ifz, piffl},
    {0x18, "jmp", adr, ptjmp},
    {0x19, "jmpb", adr, ptjmpb},
    {0x1a, "call", tcall, ptcall},
    {0x1b, "adr", adr, padr},
    {0x1c, "adrb", adr, padrb},
    {0x1d, "adrf", adr, padrf},
    {0x1e, "mal", malchm, pmal},
    {0x1f, "divide", divide, pdivide}
*/

void pnop(ce) /* do nothing */
Pcells  ce;
{   is.iip = is.dib = 1; }

/* void movdi(ce) is.dins->inst = is.sval + flaw(ce);
 * is.dval  = address of destination instruction
 * is.dins  = pointer to destination instruction
 * is.sval  = value to be moved to destination instruction
 * is.sval2 = original value of destination instruction
 */
void pmovdi(ce) /* soup [AX + CX] = BX */
Pcells  ce;
{   I32s  tval, tval2;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval  = flaw(ce); /* AX */
    tval  = ce->c.re[mo(tval,NUMREG)] + flaw(ce);
    tval2 = 2 + flaw(ce); /* CX */
    tval2 = ce->c.re[mo(tval2,NUMREG)] + flaw(ce);
    is.dval = ad(tval + tval2);
    tval = 1 + flaw(ce); /* BX */
    is.sval = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
#if PLOIDY == 1
    is.dins = &soup[is.dval];
#else /* PLOIDY > 1 */
    is.dins = &soup[is.dval][ce->d.tr];
#endif /* PLOIDY > 1 */
    is.sval2 = is.dins->inst;
}

/* void movid(ce) *(is.dreg) = is.sins->inst + flaw(ce);
 * is.sins = pointer to source instruction
 * is.sval = address of source instruction
 * is.dreg = destination register, where moved value will be placed
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pmovid(ce) /* AX = soup [BX + CX] */
Pcells  ce;
{   I32s  tval, tval2;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = flaw(ce); /* AX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    tval = 1 + flaw(ce); /* BX */
    tval = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    tval2 = 2 + flaw(ce); /* CX */
    tval2 = ce->c.re[mo(tval2, NUMREG)] + flaw(ce);
    is.sval = ad(tval + tval2);
#if PLOIDY == 1
    is.sins = &soup[is.sval];
#else /* PLOIDY > 1 */
    is.sins = &soup[is.sval][ce->d.tr];
#endif /* PLOIDY > 1 */
    is.dran = SoupSize;
}

/* void movii(ce) is.dins->inst = is.sins->inst;
 * is.dval  = address of destination instruction
 * is.sval  = address of source instruction
 * is.dins  = pointer to destination instruction
 * is.sins  = pointer to source instruction
 * is.dtra  = track of destination instruction
 * is.sval2 = original value of destination instruction
 */
void pmovii(ce) /* soup [AX + CX] = soup [BX + CX] */
Pcells  ce;
{   I32s  tval, tval2;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval2 = 2 + flaw(ce); /* CX */
    tval2 = ce->c.re[mo(tval2, NUMREG)] + flaw(ce);
    tval  = flaw(ce); /* AX */
    tval  = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    is.dval = ad(tval + tval2);
    tval = 1 + flaw(ce); /* BX */
    tval = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    is.sval = ad(tval + tval2);
#if PLOIDY == 1
    is.dins = &soup[is.dval];
    is.sins = &soup[is.sval];
#else /* PLOIDY > 1 */
    is.dins = &soup[is.dval][ce->d.tr];
    is.sins = &soup[is.sval][ce->d.tr];
#endif /* PLOIDY > 1 */
    is.dtra = ce->d.tr;
    is.sval2 = is.dins->inst;
#ifdef HSEX
    if (ce->d.x_over_addr)
    {   if ((!ce->d.mov_daught) && (!FindMate(ce)))
            ce->d.x_over_addr = ce->d.mate_addr = 0;
        else UseMate(ce);
    }
#endif
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushax(ce) /* push AX onto stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = flaw(ce); /* AX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushbx(ce) /* push BX onto stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 1 + flaw(ce); /* BX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushcx(ce) /* push CX onto stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
}

/* void push(ce) ce->c.sp = ++ce->c.sp % STACK_SIZE;
 *               ce->c.st[ce->c.sp] = is.sval + flaw(ce);
 * is.sval = value to be pushed onto the stack
 */
void ppushdx(ce) /* push DX onto stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 3 + flaw(ce); /* DX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopax(ce) /* pop AX off of stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = flaw(ce); /* AX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.dran = SoupSize;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopbx(ce) /* pop BX off of stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 1 + flaw(ce); /* BX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.dran = SoupSize;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopcx(ce) /* pop CX off of stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.dran = SoupSize;
}

/* void pop(ce) *(is.dreg) = ce->c.st[ce->c.sp] + flaw(ce);
 *              if (!ce->c.sp) ce->c.sp = STACK_SIZE - 1; else --ce->c.sp;
 * is.dreg = destination register, where value popped off stack will go
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void ppopdx(ce) /* pop DX off of stack */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 3 + flaw(ce); /* DX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.dran = SoupSize;
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
 * is.sval  = value to be placed in the dest reg
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
void pput(ce) /* write DX to output port; or if put template, put to input */
Pcells  ce;   /* port of creature(s) with complementary get template */
{   I32s    a, s = 0, adre, tval;
    Pcells  dc;
    I8s     md;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
#ifdef ICC
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
    tval = 3 + flaw(ce); /* DX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
    if (s)
    {   is.dreg  = &is.dval3;  /* dest register for address */
        is.dreg2 = &BitBucket; /* dest reg for template size */
        is.dreg3 = &BitBucket; /* dest reg for offset */
        is.sval2 = s;
        is.sval3 = Put_limit;
        is.dval  = ce->mm.p + ce->mm.s;
        is.dval2 = ce->mm.p - 1;
        is.mode  = 0;
        is.mode2 = 1;
        is.mode3 = 0;
        is.iip   = s + 1;
    }
    else
    {   tval = 2 + flaw(ce); /* CX */
        tval = ce->c.re[mo(tval, NUMREG)];
        tval = ad(tval);
        if (!IsFree(tval))
            WhichCell(tval, &is.dcel, &md);
        else
            is.dreg = &BitBucket;
        is.mode3 = 1;
    }
#else  /* ICC */
    tval = 3 + flaw(ce); /* DX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
    is.dreg = &ce->c.pb[ce->c.pb[PUTBUFSIZ]];
#endif /* ICC */
}

/* void get(ce) *(is.dreg) = is.sval + flaw(ce);
 * ce->c.gb[GETBUFSIZ]     == pointer to next input value to be read
 * ce->c.gb[GETBUFSIZ + 1] == pointer to next input value to be written
 * ce->c.gb[GETBUFSIZ + 2] == number of unread input values
 *
 * specify the values used by movdd():
 * void movdd(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pget(ce) /* read from input port into DX */
Pcells  ce;
{   I32s    a, tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 3 + flaw(ce); /* DX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.sval = ce->c.gb[ce->c.gb[GETBUFSIZ]];
    is.dran = SoupSize;
    a = ad(ce->c.ip + 1); /* a = address of start of template */
    while(1) /* find size of template, s = size */
    {
#if PLOIDY == 1
        if(soup[ad(a)].inst != Nop0 &&
           soup[ad(a)].inst != Nop1)
#else /* PLOIDY > 1 */
        if(soup[ad(a)][ce->d.tr].inst != Nop0 &&
           soup[ad(a)][ce->d.tr].inst != Nop1)
#endif /* PLOIDY > 1 */
            break;
        a++; is.iip++;
    }
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void pinc(ce) /* CX++ */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    tval = 2 + flaw(ce); /* CX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
    is.sval2 = 1;
    is.dran = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void pdec(ce) /* CX-- */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    tval = 2 + flaw(ce); /* CX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
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
void padd(ce) /* CX = CX + DX */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg  = &(ce->c.re[mo(tval, NUMREG)]);
    tval = 2 + flaw(ce); /* CX */
    is.sval  = ce->c.re[mo(tval, NUMREG)];
    tval = 3 + flaw(ce); /* DX */
    is.sval2 = ce->c.re[mo(tval, NUMREG)];
    is.dran = SoupSize;
}

/* void math(ce) *(is.dreg) = is.sval + is.sval2 + flaw(ce);
 * is.dreg  = destination register, where calculation will be stored
 * is.sval  = a value that will be added to is.sval2 and placed in dest reg
 * is.sval2 = a value that will be added to is.sval  and placed in dest reg
 * is.dmod  = value by which to modulus destination register
 * is.dran  = range within which to contain destination register
 */
void psub(ce) /* CX = CX - DX */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    tval = 2 + flaw(ce); /* CX */
    is.sval  =  ce->c.re[mo(tval, NUMREG)];
    tval = 3 + flaw(ce); /* DX */
    is.sval2 = -ce->c.re[mo(tval, NUMREG)];
    is.dran = SoupSize;
}

/* void movdd(ce) *(is.dreg) = is.sval + flaw(ce);
 * is.dreg = destination register, where moved value will be placed
 * is.sval = value to be placed in the dest reg
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pzero(ce) /* CX = 0 */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.sval = 0;
}

/* void not0(ce) *(is.dreg) ^= (1 + flaw(ce));
 * is.dreg = destination register, whose bit will be flipped
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pnot0(ce) /* flip low order bit of CX */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.dran = SoupSize;
}

/* void shl(ce) *(is.dreg) <<= (Reg) (1 + flaw(ce));
 * is.dreg = destination register, whose bits will be shifted left
 * is.dmod = value by which to modulus destination register
 * is.dran = range within which to contain destination register
 */
void pshl(ce) /* shift left all bits of CX */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.dreg = &(ce->c.re[mo(tval, NUMREG)]);
    is.dran = SoupSize;
}

/* void ifz(ce) if (is.sval + flaw(ce)) is.iip = is.sval2;
 * is.sval  = value to test for zero
 * is.sval2 = amount to increment IP if is.sval == 0
 * is.iip   = amount to increment IP if is.sval != 0
 */
void pifz(ce) /* execute next instruction, if CX == 0 */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = 2 + flaw(ce); /* CX */
    is.sval = ce->c.re[mo(tval, NUMREG)];
    is.sval2 = 2;
}

/* void ifz(ce) if (is.sval + flaw(ce)) is.iip = is.sval2;
 * is.sval  = value to test for zero
 * is.sval2 = amount to increment IP if is.sval == 0
 * is.iip   = amount to increment IP if is.sval != 0
 */
void piffl(ce) /* skip next instruction, if flag == 0 */
Pcells  ce;
{   is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.sval = ce->c.fl;
    is.sval2 = 1;
    is.iip = 2;
    is.dib = 1;
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
{   I32s    a, s = 0, tval;

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
    is.dreg2 = &BitBucket; /* destination register for template size */
    is.dreg3 = &BitBucket; /* dest reg for offset */
    tval = flaw(ce); /* AX */
    tval = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    is.sval  = ad(tval); /* target for IP if s == 0 */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
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
{   I32s    a, s = 0, tval;

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
    is.dreg2 = &BitBucket; /* destination register for template size */
    is.dreg3 = &BitBucket; /* dest reg for offset */
    tval = flaw(ce); /* AX */
    tval = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    is.sval  = ad(tval); /* target for IP if s == 0 */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
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
    is.dreg2 = &BitBucket; /* destination register for template size */
    is.dreg3 = &BitBucket; /* destination register for offset */
    is.sval  = ad(ce->c.ip + s + 1); /* address to be pushed onto stack */
    is.sval2 = s;  /* size of template */
    is.sval3 = Search_limit;
    is.dmod  = SoupSize;
    is.dval  = ad(a + s + 1); /* start address for forward search */
    is.dval2 = ad(a - s - 1); /* start address for backward search */
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
/* search outward for template, put address in AX, template size in DX,
   and offset in CX, start search at offset +- CX
*/
void padr(ce)
Pcells  ce;
{   I32s    a, s = 0, tval;

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
    if (s)
    {   tval = flaw(ce); /* AX */
        is.dreg  = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for address */
        tval = 3 + flaw(ce); /* DX */
        is.dreg2 = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for templ size */
        tval = 2 + flaw(ce); /* CX */
        is.dreg3 = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for offset */
    }
    else
        is.dreg = is.dreg2 = is.dreg3 = &BitBucket;
    is.sval2 = s;  /* size of template */
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dmod3 = SoupSize;
    tval = 2 + flaw(ce); /* CX */
    tval = ce->c.re[mo(tval, NUMREG)]; /* start at offset */
    is.dval  = ad(a + s + tval + 1); /* start address for forward search */
    is.dval2 = ad(a - s - tval - 1); /* start address for backward search */
    is.sval3 = Search_limit - tval;
    is.mode  = 0; /* outward search */
    is.mode2 = 1;
    is.iip = s + 1; is.dib = 1;
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
/* search backward for template, put address in AX, template size in DX,
   and offset in CX, start search at offset - CX
*/
void padrb(ce)
Pcells  ce;
{   I32s    a, s = 0, tval;

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
    if (s)
    {   tval = flaw(ce); /* AX */
        is.dreg  = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for address */
        tval = 3 + flaw(ce); /* DX */
        is.dreg2 = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for templ size */
        tval = 2 + flaw(ce); /* CX */
        is.dreg3 = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for offset */
    }
    else
        is.dreg = is.dreg2 = is.dreg3 = &BitBucket;
    is.sval2 = s;  /* size of template */
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dmod3 = SoupSize;
    tval = 2 + flaw(ce); /* CX */
    tval = ce->c.re[mo(tval, NUMREG)]; /* start at offset */
    is.dval  = ad(a + s + tval + 1); /* start address for forward search */
    is.dval2 = ad(a - s - tval - 1); /* start address for backward search */
    is.sval3 = Search_limit - tval;
    is.mode  = 2; /* backward search */
    is.mode2 = 2;
    is.iip = s + 1; is.dib = 1;
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
/* search forward for template, put address in AX, template size in DX,
   and offset in CX, start search at offset + CX
 */
void padrf(ce)
Pcells  ce;
{   I32s    a, s = 0, tval;

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
    if (s)
    {   tval = flaw(ce); /* AX */
        is.dreg  = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for address */
        tval = 3 + flaw(ce); /* DX */
        is.dreg2 = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for templ size */
        tval = 2 + flaw(ce); /* CX */
        is.dreg3 = &(ce->c.re[mo(tval, NUMREG)]); /* dest reg for offset */
    }
    else
        is.dreg = is.dreg2 = is.dreg3 = &BitBucket;
    is.sval2 = s;  /* size of template */
    is.dmod  = SoupSize;
    is.dran2 = SoupSize;
    is.dmod3 = SoupSize;
    tval = 2 + flaw(ce); /* CX */
    tval = ce->c.re[mo(tval, NUMREG)]; /* start at offset */
    is.dval  = ad(a + s + tval + 1); /* start address for forward search */
    is.dval2 = ad(a - s - tval - 1); /* start address for backward search */
    is.sval3 = Search_limit - tval;
    is.mode  = 1; /* forward search */
    is.mode2 = 1;
    is.iip = s + 1; is.dib = 1;
}

/* void malchm(ce) is.sval2 = mal(ce,&is.sval3,is.sval,is.mode2);
 *                 *(is.dreg) = is.sval3;
 *                 chmode(ce,is.sval3,is.sval2,is.mode);
 * is.dreg  = destination register where allocated address is stored
 * is.sval  = requested size of block for mal()
 * is.sval2 = flawed size of block (assigned in instruct.c)
 * is.sval3 = suggested address, and allocated address
 * is.mode  = memory protection mode (rwx), probably MemModeProt
 * is.mode2 = memory allocation mode for mal()
 */
void pmal(ce)  /* allocate space for a new cell */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    tval = flaw(ce); /* AX destination register */
    is.dreg  = &(ce->c.re[mo(tval, NUMREG)]);
    tval = 2 + flaw(ce); /* CX requested size */
    is.sval  = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    tval = flaw(ce); /* AX suggested address */
    is.sval3 = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    if (is.sval3 < 0)
        is.mode2 = 1; /* better fit */
    else
        is.mode2 = 6; /* suggester address (is.sval3) */
    is.mode = MemModeProt; /* only write privelage works at the moment */
}

/* void divide(ce) cell division
 * is.mode  = divide mode (3 steps)
 * is.sval  = offset of IP into daughter's genome
 * is.sval2 = eject genome from soup = 0, !0 = leave in soup
 */
void pdivide(ce)  /* give life to new cell by puting in queue */
Pcells  ce;
{   I32s  tval;

    is.iip = is.dib = 1;
    if (is.eins->exec)
        return ;
    is.mode  = 2;  /* full division */
    tval = 2 + flaw(ce); /* CX offset into daughter */
    is.sval  = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
    tval = 3 + flaw(ce); /* DX eject? */
    is.sval2 = ce->c.re[mo(tval, NUMREG)] + flaw(ce);
}

#endif  /* INST == 4 */
