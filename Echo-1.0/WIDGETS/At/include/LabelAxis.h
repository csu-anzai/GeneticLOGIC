/*
 *      LabelAxis.h
 *
 *      The AthenaTools Plotter Widget Set - Version 6.0
 *
 *      klin, Tue Jul  7 13:59:47 1992
 *      klin, Sat Aug 15 10:31:50 1992, patchlevel 4
 *                                      Changed <At/..> to <X11/At/..>.
 *
 *      SCCSid[] = "@(#) Plotter V6.0  92/08/15  LabelAxis.h"
 */

/*

Copyright 1991 by Burdett, Buckeridge & Young Ltd.

All rights reserved.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of the firms, institutes
or employers of the authors not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

THE AUTHORS AND THEIR FIRMS, INSTITUTES OR EMPLOYERS DISCLAIM ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE AUTHORS AND THEIR FIRMS,
INSTITUTES OR EMPLOYERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

*/

/*
 * The label axis dets the tic positions and labels from the user
 * database.
 */

#ifndef _AtLabelAxis_h
#define _AtLabelAxis_h

#include "At.h"
#include "AxisCore.h"

/* declare specific AtLabelAxisWidget class and instance datatypes */

typedef struct _AtLabelAxisClassRec*    AtLabelAxisWidgetClass;
typedef struct _AtLabelAxisRec*         AtLabelAxisWidget;

/* declare the class constant */

extern WidgetClass atLabelAxisWidgetClass;

#define XtNautoScale "autoScale"
#define XtCAutoScale "AutoScale"

/*
 * The "member" routines
 */
extern void AtLabelAxisAttachData P((AtLabelAxisWidget law,
				     String *data, Cardinal stride,
				     Cardinal start, Cardinal num));

#endif /* _AtLabelAxis_h */
