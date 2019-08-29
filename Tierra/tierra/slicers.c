/* slicers.c   9-9-92  time slicing routines for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char     sccsid[] = "@(#)slicers.c	1.5     7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"

#ifdef MEM_CHK
#include <memcheck.h>
#endif

void SlicerPhoton()
{   Pcells  ce;
    I32s  size_slice;
    I32s   a;
    I8s   md;

    do a = tlrand() % SoupSize;
    while(IsFree(a));
    WhichCell(a, &ThisSlice, &md);
    size_slice = PhotonSlide(a, PhotonInst, PhotonSize, PhotonWidth);
    size_slice = (I32s) (10. * pow((double) size_slice,(double) PhotonPow));
    if(SizDepSlice)
    {   ce = ThisSlice;
        size_slice = (I32s) ((double) size_slice *
            pow((double) ce->mm.s / (double) AverageSize, (double) SlicePow));
    }
    TimeSlice(ThisSlice, size_slice);
}

I32s PhotonFit(a, PhotonInst, PhotonSize)
I32s  a;
I8s  *PhotonInst;
I32s  PhotonSize;
{   I32s  i, j, fit, tfit = 0;

    for(j = 0; j < PLOIDY; j++)
    {   fit = 0;
        for(i = 0; i < PhotonSize; i++)
#if PLOIDY == 1
            if(soup[ad(a + i)].inst == *(PhotonInst + i)) fit++;
#else /* PLOIDY > 1 */
            if(soup[ad(a + i)][j].inst == *(PhotonInst + i)) fit++;
#endif /* PLOIDY > 1 */
        if(fit > tfit)
            tfit = fit;
    }
    return tfit;
}

I32s PhotonSlide(a, PhotonInst, PhotonSize, PhotonWidth)
I32s  a;
I8s  *PhotonInst;
I32s  PhotonSize, PhotonWidth;
{   I32s  i, ws2, tfit, fit = 0;

    ws2 = (PhotonSize + PhotonWidth) / 2;
    for(i = 0; i < PhotonWidth; i++)
    {   tfit = PhotonFit(ad(a - ws2 + i), PhotonInst, PhotonSize);
        if(tfit > fit) fit = tfit;
    }
    return fit;
}

void PhotonTranslate(PhotonInst, PhotonWord)
I8s  *PhotonInst;
I8s  *PhotonWord;
{   I32s  i;

    for(i = 0; i < PhotonSize; i++)
    {   if(*(PhotonWord + i) > 47 && *(PhotonWord + i) < 58)
            *(PhotonInst + i) = *(PhotonWord + i) - 48;
        else if (*(PhotonWord + i) > 96 && *(PhotonWord + i) < 119)
            *(PhotonInst + i) = *(PhotonWord + i) - 87;
        else *(PhotonInst + i) = 0;
    }
}

void SlicerQueue()
{   Pcells  ce;
    I32s  size_slice;

    ce = ThisSlice; /* ThisSlice is current cell in queue */
    if(SizDepSlice)
        size_slice = (I32s) pow((double) ce->mm.s,(double) SlicePow);
    else size_slice = SliceSize;
    TimeSlice(ThisSlice, size_slice);
    IncrSliceQueue(); /* increment ThisSlice to next cell in queue */
}

void RanSlicerQueue()
{   Pcells  ce;
    I32s  size_slice;

    ce = ThisSlice; /* ThisSlice is current cell in queue */
    if(SizDepSlice)
	{
        if(SlicePow == 1.0)	/* Speed hack by dan july 92 */
	   size_slice = (ce->mm.s);
	else
           size_slice = (I32s) pow((double) ce->mm.s,(double) SlicePow);
	}
    else size_slice = SliceSize;
    size_slice = (I32s) SlicFixFrac * size_slice +
        tlrand() % (I32s) ((SlicRanFrac * size_slice) + 1);
    TimeSlice(ThisSlice, size_slice);
    IncrSliceQueue(); /* increment ThisSlice to next cell in queue */
}
