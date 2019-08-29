/* portable.c   9-9-92  conditionally compiled functions for portability */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char     portable_sccsid[] = "@(#)portable.c	1.5        7/21/92";
#endif

/* #define ARG   */

#include "license.h"

#include <stdio.h>
#include "tierra.h"
#include "extern.h"

#ifdef ALCOMM
#include <mlayer.h>
#endif

#ifdef MEM_CHK
#include <memcheck.h>
#endif

#ifdef __TURBOC__
/* I8s Hp  GP = (I8s Hp) 0x9bff0008; */ /* leaves about 16K free */
I8s Hp  GP = (I8s Hp) 0x9efd0008; /* leaves about  4K free */
#endif /* __TURBOC__ */

/* free for arrays not greater than 64K */
void tfree(ptr)
I8s Fp  ptr;
{
#ifdef __TURBOC__
    free(ptr);
#endif /* __TURBOC__ */

#ifdef OS2_MC
    DosFreeSeg((I16u) ptr >> 16);
#endif /* OS2_MC */

#ifdef unix

#ifdef ALCOMM
    (void) ALFree( (char *)ptr );
#else /* ALCOMM */
    free(ptr);
#endif  /* ALCOMM */


#endif /* unix */

#ifdef IBM3090
    free(ptr);
#endif /* IBM3090 */
}

/* free for arrays of greater than 64K */
void thfree(ptr)
I8s Hp  ptr;
{
#ifdef __TURBOC__
    farfree(ptr);
#endif

#ifdef OS2_MC
    DosFreeSeg((I16u) ptr >> 16);
#endif

#ifdef unix

#ifdef ALCOMM
    (void) ALFree( (char *)ptr );
#else
    free(ptr);
#endif  /* ALCOMM */

#endif

#ifdef IBM3090
    free(ptr);
#endif
}

/* recalloc for Unix, or for DOS arrays not greater than 64K */
I8s Fp trecalloc(ptr, nsiz, osiz)
I8s Fp  ptr;
I32u    nsiz; /* new array size */
I32u    osiz; /* old array size */
{
#ifdef __TURBOC__
    I8s  Fp tp, Hp hp, swapped = 0;
    I32u  swapsiz = (osiz < nsiz) ? osiz : nsiz;
    FILE  *iof;

#ifndef ARG 
    hp = tp = (I8s  Fp) calloc(nsiz, 1);
    while ((hp + nsiz) > GP || tp == NULL)
    {   if (tp)
        {   tfree(tp);
            tp = NULL;
        }
        if (!GeneBnker || !Swap || !reaped)
            break;
        if (ptr)
        {   iof = fopen("swap","wb");
            tfwrite(ptr, 1, swapsiz, iof);
            fclose(iof);
            tfree(ptr);
            ptr = NULL;
            swapped = 1;
        }
        if (!gq_swap())
            FEExit(-123);
        hp = tp = (I8s  Fp) calloc(nsiz, 1);
    }
    if (tp)
    {   if (swapped)
        {   iof = fopen("swap","rb");
            tfread(tp, 1, swapsiz, iof);
            fclose(iof);
            unlink("swap");
        }
        else if (ptr)
        {   memcpy(tp, ptr, swapsiz);
            tfree(ptr);
            ptr = NULL;
        }
    }
#else /* ARG */
    tp = (I8s  Fp) realloc(ptr, (size_t) nsiz);
#endif /* ARG */

    return  tp;
#endif /* __TURBOC__ */

#ifdef unix
    I8s *  tp;
    I32u  swapsiz = (osiz < nsiz) ? osiz : nsiz;

#ifdef ALCOMM
    tp = (I8s *) ALCalloc(nsiz, 1);
#else /* ALCOMM */
    tp = (I8s *) calloc(nsiz, 1);
#endif /* ALCOMM */
    if(tp == NULL)
    {
#ifdef ARG 
        fprintf(stderr, "Tierra trecalloc() error: realloc failed");
        exit(errno);
#else /* ARG */
        FEError(-700,EXIT,NOWRITE,
            "Tierra trecalloc() error: realloc failed");
#endif /* ARG */
    }
    else if (ptr)
    {   memcpy(tp, ptr, swapsiz);
        tfree(ptr);
        ptr = NULL;
    }
    return tp;

#endif /* unix */

#ifdef OS2_MC
    I16u     NumSegs, Remain, Selector;
    I8s  Fp  tp;

    NumSegs = (I16u) nsiz / 65536ul;
    Remain  = (I16u) nsiz % 65536ul;
    Selector = ((I16u) ptr >> 16);
    DosReallocHuge(NumSegs,Remain,Selector);
    return (void Fp) ptr;
#endif /* OS2_MC */

#ifdef IBM3090
    return (I8s  *) realloc(ptr,nsiz);
#endif /* IBM3090 */
}

/* huge recalloc for arrays of greater than 64K */
I8s Hp threcalloc(ptr, nsiz, osiz)
I8s Hp  ptr;
I32u    nsiz; /* new array size */
I32u    osiz; /* old array size */
{
#ifdef __TURBOC__
    I8s   Hp tp, Hp dp, Hp sp, Hp hp, swapped = 0;
    I32s  segs, rem, i;
    I32u  swapsiz = (osiz < nsiz) ? osiz : nsiz;
    FILE  *iof;

#ifndef ARG 
    hp = tp = (I8s  Hp) farcalloc(nsiz, 1);
    while ((hp + nsiz) > GP || tp == NULL)
    {   if (tp)
        {   thfree(tp);
            tp = NULL;
        }
        if (!GeneBnker || !Swap || !reaped)
            break;
        if (ptr)
        {   iof = fopen("swap","wb");
            tfwrite(ptr, 1, swapsiz, iof);
            fclose(iof);
            thfree(ptr);
            ptr = NULL;
            swapped = 1;
        }
        if (!gq_swap())
            FEExit(-123);
        hp = tp = (I8s  Hp) farcalloc(nsiz, 1);
    }
    if (tp)
    {   if (swapped)
        {   iof = fopen("swap","rb");
            tfread(tp, 1, swapsiz, iof);
            fclose(iof);
            unlink("swap");
        }
        else if (ptr)
        {   segs = swapsiz / (I32s) (UINT_MAX - 1);
            rem  = swapsiz % (I32s) (UINT_MAX - 1);
            dp = tp; sp = ptr;
            if(segs) for(i = 0; i < segs; i++)
            {   memcpy(dp, sp, (UINT_MAX - 1));
                dp += (UINT_MAX - 1);
                sp += (UINT_MAX - 1);
            }
            if(rem)
                memcpy(dp, sp, rem);
            thfree(ptr);
            ptr = NULL;
        }
    }
#else /* ARG */
    tp = (I8s  Hp) farrealloc(ptr, nsiz);
#endif /* ARG */
    return  tp;
#endif /* __TURBOC__ */

#ifdef unix
    I8s *  tp;
    I32u  swapsiz = (osiz < nsiz) ? osiz : nsiz;

#ifdef ALCOMM
    tp = (I8s *) ALCalloc(nsiz, 1);
#else
    tp = (I8s *) calloc(nsiz, 1);
#endif        /* ALCOMM */
    if(tp == NULL)
    {
#ifdef ARG 
        fprintf(stderr, "Tierra threcalloc() error: recalloc failed");
        exit(errno);
#else
        FEError(-701,EXIT,NOWRITE,
            "Tierra threcalloc() error: recalloc failed");
#endif
    }
    else if (ptr)
    {   memcpy(tp, ptr, swapsiz);
        tfree(ptr);
        ptr = NULL;
    }
    return tp;

#endif /* unix */

#ifdef OS2_MC
    I16u     NumSegs, Remain, Selector;
    I8s  Hp  tp;

    NumSegs = (I16u) nsiz / 65536ul;
    Remain  = (I16u) nsiz % 65536ul;
    Selector = ((I16u) ptr >> 16);
    DosReallocHuge(NumSegs,Remain,Selector);
    return (void Hp) ptr;
#endif

#ifdef IBM3090
    return (I8s  *) realloc(ptr,nsiz);
#endif
}

/* calloc for arrays not greater than 64K */
I8s Fp tcalloc(num, siz)
I32u   num;
I32u   siz;
{
#ifdef __TURBOC__
    I8s  Fp tp, Hp hp;

#ifndef ARG 
    hp = tp = (I8s  Fp) calloc(num, siz);
    while ((hp + (num * siz)) > GP || tp == NULL)
    {   if (tp)
        {   tfree(tp);
            tp = NULL;
        }
        if (!GeneBnker || !Swap || !reaped)
            break;
        if (!gq_swap())
            FEExit(-123);
        hp = tp = (I8s  Fp) calloc(num, siz);
    }
#else /* ARG */
    tp = (I8s  Fp) calloc(num,siz);
#endif /* ARG */

    return  tp;
#endif /* __TURBOC__ */

#ifdef unix
    I8s *  tp;

#ifdef ALCOMM
    tp = (I8s *) ALCalloc(num, siz);
#else /* ALCOMM */
    tp = (I8s *) calloc(num, siz);
#endif /* ALCOMM */
    if(tp == NULL)
    {
#ifdef ARG 
        fprintf(stderr, "Tierra tcalloc() error: calloc failed");
        exit(errno);
#else /* ARG */
        FEError(-703,EXIT,NOWRITE, "Tierra tcalloc() error: calloc failed");
#endif /* ARG */
    }
    return tp;

#endif /* unix */

#ifdef OS2_MC
    I32u     HugeSize, i, ASize;
    I16u     NumSegs, Remain, rc;
    I8s Hp  tp;
    SEL      Selector;

    HugeSize = num * siz;
    i = HugeSize / 65536ul;
    NumSegs = (I16u) i;
    Remain  = (I16u) HugeSize % 65536ul;
    rc = DosAllocHuge(NumSegs,Remain,&Selector,3 * NumSegs,0);
    if(rc)
    {
#ifdef ARG
        fprintf(stderr, "Tierra tcalloc() DosAllocHuge error = %u", rc);
        exit(errno);
#else  /* ARG */
        FEError(-702,EXIT,NOWRITE,
            "Tierra tcalloc() DosAllocHuge error = %u", rc);
#endif /* ARG */
    }
    tp = (I8s Fp) ((I32u ) Selector << (I32u) 16);
    for(i = 0; i < HugeSize; i++)
    {   *(tp + i) = (I8s) 0;
    }
    return (I8s Fp) tp;
#endif /* OS2_MC */

#ifdef IBM3090
    return (I8s  *) calloc(num,siz);
#endif /* IBM3090 */
}

/* calloc for arrays of greater than 64K */
I8s Hp thcalloc(num, siz)
I32u   num;
I32u   siz;
{
#ifdef __TURBOC__
    I8s  Hp tp, Hp hp;

#ifndef ARG 
    hp = tp = (I8s  Hp) farcalloc(num, siz);
    while ((hp + (num * siz)) > GP || tp == NULL)
    {   if (tp)
        {   thfree(tp);
            tp = NULL;
        }
        if (!GeneBnker || !Swap || !reaped)
            break;
        if (!gq_swap())
            FEExit(-123);
        hp = tp = (I8s  Hp) farcalloc(num, siz);
    }
#else  /* ARG */
    tp = (I8s  Hp) farcalloc(num,siz);
#endif /* ARG */

    return  tp;
#endif /* __TURBOC__ */

#ifdef OS2_MC
    I32u     HugeSize, i, ASize;
    I16u     NumSegs, Remain, rc;
    I8s Hp  tp;
    SEL      Selector;

    HugeSize = num * siz;
    i = HugeSize / 65536ul;
    NumSegs = (I16u) i;
    Remain  = (I16u) HugeSize % 65536ul;
    rc = DosAllocHuge(NumSegs,Remain,&Selector,3 * NumSegs,0);
    if(rc)
    {
#ifdef ARG 
        fprintf(stderr, "Tierra thcalloc() DosAllocHuge error = %u", rc);
        exit(errno);
#else  /* ARG */
        FEError(-704,EXIT,NOWRITE,
            "Tierra thcalloc() DosAllocHuge error = %u", rc);
#endif /* ARG */
    }
    tp = (I8s Hp) ((I32u ) Selector << (I32u) 16);
    for(i = 0; i < HugeSize; i++)
    {   *(tp + i) = (I8s) 0;
    }
    return (I8s Hp) tp;
#endif /* OS2_MC */

#ifdef unix
    I8s *  tp;

#ifdef ALCOMM
    tp = (I8s *) ALCalloc(num, siz);
#else  /* ALCOMM */
    tp = (I8s *) calloc(num, siz);
#endif /* ALCOMM */
    if(tp == NULL)
    {
#ifdef ARG 
        fprintf(stderr, "Tierra thcalloc() error: calloc failed");
        exit(errno);
#else  /* ARG */
        FEError(-705,EXIT,NOWRITE, "Tierra thcalloc() error: calloc failed");
#endif /* ARG */
    }
    return tp;

#endif /* unix */

#ifdef IBM3090
    return (I8s  *) calloc(num,siz);
#endif /* IBM3090 */
}

I32u tfread(ptr, size, n, stream)
I8s Hp  ptr;
I32s  size, n;
FILE  *stream;
{   I32u  r_size = 0;
#ifdef __TURBOC__
    I32s  segs, rem, i = 0;

    segs = n / (I32s) UINT_MAX;
    rem  = n % (I32s) UINT_MAX;
    if(segs) for(i = 0; i < segs; i++)
        r_size += fread((I8s *) ((I8s Hp) ptr + (i * size *
            (I32s) UINT_MAX)), (I16u) size, (I16u) UINT_MAX, stream);
    if(rem) r_size += fread((I8s *) ((I8s Hp) ptr + (segs * size *
            (I32s) UINT_MAX)), (I16u) size, (I16u) rem, stream);
#endif /* __TURBOC__ */

#ifdef OS2_MC
    I32s  segs, rem, i = 0;

    segs = n / (I32s) UINT_MAX;
    rem  = n % (I32s) UINT_MAX;
    if(segs) for(i = 0; i < segs; i++)
        r_size += fread((I8s *) ((I8s Hp) ptr + (i * size *
            (I32s) UINT_MAX)), (I16u) size, (I16u) UINT_MAX, stream);
    if(rem) r_size += fread((I8s *) ((I8s Hp) ptr + (segs * size *
            (I32s) UINT_MAX)), (I16u) size, (I16u) rem, stream);
#endif /* OS2_MC */

#ifdef unix
    r_size = fread(ptr, size, n, stream);
#endif /* unix */

#ifdef IBM3090
    r_size = fread(ptr, size, n, stream);
#endif /* IBM3090 */

    if(r_size != n)
    {   
#ifdef ARG 
        fprintf(stderr, 
        "Tierra tfread() inconsistency: n = %ld  r_size = %ld", n, r_size);
        exit(errno);
#else
    FEError(-706,NOEXIT,NOWRITE,
        "Tierra tfread() inconsistency: n = %ld  r_size = %ld", n, r_size);
#endif
    }
    return r_size;
}

I32u tfwrite(ptr, size, n, stream)
I8s Hp  ptr;
I32s  size, n;
FILE *  stream;
{   I32u  r_size = 0;
#ifdef __TURBOC__
    I32s  segs, rem, i = 0;

    segs = n / (I32s) UINT_MAX;
    rem  = n % (I32s) UINT_MAX;
    if(segs) for(i = 0; i < segs; i++)
        r_size += fwrite((const I8s *) ((I8s Hp) ptr + (i * size *
            (I32s) UINT_MAX)), (I16u) size, (I16u) UINT_MAX, stream);
    if(rem) r_size += fwrite((const I8s *) ((I8s Hp) ptr + (segs * size *
        (I32s) UINT_MAX)), (I16u) size, (I16u) rem, stream);
#endif

#ifdef OS2_MC
    I32s  segs, rem, i = 0;

    segs = n / (I32s) UINT_MAX;
    rem  = n % (I32s) UINT_MAX;
    if(segs) for(i = 0; i < segs; i++)
        r_size += fwrite((const I8s *) ((I8s Hp) ptr + (i * size *
            (I32s) UINT_MAX)), (I16u) size, (I16u) UINT_MAX, stream);
    if(rem) r_size += fwrite((const I8s *) ((I8s Hp) ptr + (segs * size *
        (I32s) UINT_MAX)), (I16u) size, (I16u) rem, stream);
#endif

#ifdef unix
    r_size = fwrite(ptr, size, n, stream);
#endif

#ifdef IBM3090
    r_size = fwrite(ptr, size, n, stream);
#endif

    if(r_size != n)
    {   
#ifdef ARG 
        fprintf(stderr, 
          "Tierra tfwrite() inconsistency: n = %ld  r_size = %ld", n, r_size);
#else
        FEError(-707,NOEXIT,NOWRITE,
          "Tierra tfwrite() inconsistency: n = %ld  r_size = %ld", n, r_size);
#endif
    }
    return r_size;
}
