/****************************************************************************/
/*																			*/
/*	Make_Draw.c														 		*/
/*	Creates the population window.											*/
/*																			*/
/****************************************************************************/


/*--------------------------------------------------------------------------*/
/*			INCLUDE FILES 													*/
/*--------------------------------------------------------------------------*/

#include "Control.h"
#include "GA.h"
/*#include "Curves.h"*/

/*--------------------------------------------------------------------------*/
/*			EXTERNAL FUNCTIONS 												*/
/*--------------------------------------------------------------------------*/

extern	Repaint();
extern	Resize();

/*--------------------------------------------------------------------------*/
/*			GLOBALS DECLARED HERE											*/
/*--------------------------------------------------------------------------*/

int		G_current_width,	/*current height and width of population window */
		G_current_height;

/*--------------------------------------------------------------------------*/
/*																			*/
/*	MakeDrawWindow															*/
/*	Creates a population window; First it launches a Frame (population_frame)*/
/*	which is a sub-Frame of control_frame.  Each frame holds a Canvas called*/
/*	G_organism_window with a Pixwin called G_organism_pw, and a Panel called*/
/*	G_organism_panel which holds a single toggle.							*/
/*																			*/
/*--------------------------------------------------------------------------*/
extern Pixfont *xv_pf_open();

MakeDrawWindow ()
{
	Panel pop_label;	/*used to create the label for the population window*/


	/* Create population frame */
	G_population_frame =
		xv_create(G_control_frame,FRAME,
			XV_LABEL,"Population",
			WIN_SHOW,			TRUE,
			WIN_X,				DRAW_X,
			WIN_Y,				DRAW_Y,
			XV_WIDTH,			DRAW_WID,
			XV_HEIGHT,			DRAW_LEN,
			0
		);


	/* Create a panel in the population frame to hold messages */
	pop_label = xv_create(G_population_frame,PANEL,0);

/* XView CONVERSION - Due to a name space clash with Xlib, the SunView data-type Window is now Xv_Window in XView */

	xv_create(pop_label,PANEL_MESSAGE,
		PANEL_LABEL_STRING,"Population Window",
		0);
	window_fit_height(pop_label);

	/* Create the canvas */
	G_population_window =
		xv_create(G_population_frame, CANVAS,
/* XView CONVERSION - Events are no longer split between kbd and pick, use WIN_IGNORE_EVENT or WIN_IGNORE_EVENTS instead and combine these if both kbd and pick specified  Sect 3.2 */
			WIN_IGNORE_EVENTS,			MS_MIDDLE, MS_RIGHT, LOC_DRAG, 0,
			WIN_CONSUME_EVENT,			MS_LEFT,
			CANVAS_AUTO_EXPAND, 		TRUE,
			CANVAS_AUTO_SHRINK, 		TRUE,
/* XView CONVERSION - Now only a hint, must be prepared to repaint, read Sect 2.5 */
			CANVAS_RETAINED,    		FALSE,
			CANVAS_REPAINT_PROC,		Repaint,
			CANVAS_RESIZE_PROC,			Resize,
/* to insure that color works - dlh */
			WIN_DYNAMIC_VISUAL, TRUE,
			0
		);

	/* Get canvas pixwins */
	G_population_pw = canvas_pixwin(G_population_window);

	xv_set( (Xv_Window) G_population_pw,
                          WIN_IGNORE_EVENTS, MS_RIGHT, 0,
                          WIN_CONSUME_EVENTS,   MS_LEFT, MS_MIDDLE, LOC_DRAG,
                          LOC_WINENTER,LOC_MOVE,0,
	       0);

	/* Get canvas font */
	G_population_font = xv_pf_open("-b&h-lucida-medium-r-normal-sans-12-*-*-*-*-*-*-*");
	/* sunview fonts no longer valid !!! dlh */
}
