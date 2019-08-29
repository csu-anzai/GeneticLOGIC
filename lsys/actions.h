/* actions.h - external definition of actions used in interpretation
 *  of an L-system.
 *
 * $Id: actions.h,v 1.2 90/10/12 18:48:16 leech Exp Locker: leech $
 *
 * Copyright (C) 1990, Jonathan P. Leech
 *
 * This software may be freely copied, modified, and redistributed,
 * provided that this copyright notice is preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is". Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 *
 * $Log:	actions.h,v $
 * Revision 1.2  90/10/12  18:48:16  leech
 * First public release.
 * 
 */

const int maxargs = 10;

// Default interpretation functions; this header should be included
//  after parser.h and Turtle.h

#define ACTION(name) \
    void name(ListIterator(Module) &mi, Turtle &t, DBGenerator &db, int nargs, float args[maxargs])

// Macro to shut up cfront warnings in action functions
#define ARGSUSED (void)mi; (void)t; (void)db; (void)nargs; (void)args[0];

// Pointer to an interpretation function
typedef ACTION((*Actionfunc));

// Canned interpretation functions
extern void Prelude(Turtle &t);
extern void Postscript(Turtle &t);

extern ACTION(Draw);
extern ACTION(Move);
extern ACTION(Flower);
extern ACTION(Leaf);
extern ACTION(TurnRight);
extern ACTION(TurnLeft);
extern ACTION(PitchUp);
extern ACTION(PitchDown);
extern ACTION(RollRight);
extern ACTION(RollLeft);
extern ACTION(Reverse);
extern ACTION(Push);
extern ACTION(Pop);
extern ACTION(RollHorizontal);
extern ACTION(LineWidth);
extern ACTION(ChangeColor);
extern ACTION(DrawObject);
extern ACTION(CutBranch);
extern ACTION(StartPolygon);
extern ACTION(PolygonVertex);
extern ACTION(PolygonMove);
extern ACTION(EndPolygon);
extern ACTION(Internode);
extern ACTION(FloweringApex);
extern ACTION(Tropism);
