/* interpret.c - interprets an L-system using a Turtle and taking
 *  actions to render the resulting database.
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
 * $Log:	interpret.c,v $
 * Revision 1.3  91/03/20  10:36:40  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:19  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: interpret.c,v 1.3 91/03/20 10:36:40 leech Exp Locker: leech $";

#include "global_headers.h"
#include "Symtab.h"
#include "Value.h"
#include "Expression.h"
#include "Module.h"
#include "Turtle.h"
#include "DBGenerator.h"
#include "interpret.h"
#include "actions.h"

// Set up the default actions for interpretation
// If 'plant' is true, bind the modules A I K L to plantlike interpretations
//  as in the text, rather than the default meaning (e.g. none). This is
//  a horrible unflexible (but simple) kludge which will soon change.
void setup_actions(Symtab(Anyptr) &st, boolean plant) {
    st.enter("F",  Anyptr(Draw));
    st.enter("Fl", Anyptr(Draw));
    st.enter("Fr", Anyptr(Draw));
    st.enter("f",  Anyptr(Move));
    st.enter("+",  Anyptr(TurnLeft));
    st.enter("-",  Anyptr(TurnRight));
    st.enter("&",  Anyptr(PitchDown));
    st.enter("^",  Anyptr(PitchUp));
    st.enter("\\", Anyptr(RollLeft));
    st.enter("/",  Anyptr(RollRight));
    st.enter("|",  Anyptr(Reverse));
    st.enter("[",  Anyptr(Push));
    st.enter("]",  Anyptr(Pop));
    st.enter("$",  Anyptr(RollHorizontal));
    st.enter("!",  Anyptr(LineWidth));
    st.enter("{",  Anyptr(StartPolygon));
    st.enter(".",  Anyptr(PolygonVertex));
    st.enter("G",  Anyptr(PolygonMove));
    st.enter("}",  Anyptr(EndPolygon));
    if (plant == true) {
	st.enter("A",  Anyptr(FloweringApex));
	st.enter("I",  Anyptr(Internode));
	st.enter("K",  Anyptr(Flower));
	st.enter("L",  Anyptr(Leaf));
    }
    st.enter("'",  Anyptr(ChangeColor));
    st.enter("~",  Anyptr(DrawObject));
    st.enter("%",  Anyptr(CutBranch));
    st.enter("t",  Anyptr(Tropism));
}

// Interpret a bound L-system, producing (at present) Postscript output
//  on the specified stream. Default values for line width,
//  movement, and turn angles are specified.
void interpret(
    List(Module) &ml,
    DBGenerator  &g,
    boolean	 plant,
    float	 turn,
    float	 width_scale)
{
    Symtab(Anyptr)	 action;
    ListIterator(Module) mi(ml);
    Module		*m;

    Turtle		 t(turn, width_scale);

    t.set_H(Vector(0,1,0));	// H = +Y
    t.set_L(Vector(-1,0,0));	// L = -X
    t.set_U(Vector(0,0,1));	// U = +Z
    t.set_gravity(Vector(0,1,0));

    setup_actions(action, plant);

    g.prelude(t);
    for (m = mi.first(); m; m = mi.next()) {
	float args[maxargs];
	Anyptr p;

	if (action.lookup(m->name(), p) == true) {
	    // Fetch defined parameters
	    for (int nargs = 0; nargs < maxargs; nargs++)
		if (m->getfloat(args[nargs], nargs) == false)
		    break;

	    Actionfunc f;
	    f = (Actionfunc)p;
	    (*f)(mi, t, g, nargs, args);
	} else {
	    PDEBUG(PD_INTERPRET, cerr << "Unknown action for " << *m << endl);
	}
	PDEBUG(PD_INTERPRET, cerr << t);
    }
    g.postscript(t);
}
