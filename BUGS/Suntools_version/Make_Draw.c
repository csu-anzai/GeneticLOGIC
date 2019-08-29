/****************************************************************************/
/*																			*/
/*	Make_Draw.c														 		*/
/*	Creates the population window.											*/
/*																			*/
/****************************************************************************/


/* -------------------------------------------------------------------------*/
/*			 CONSTANTS					    */
/* -------------------------------------------------------------------------*/


#define NUM_COLORS	8		/* VERY IMPORTANT NOTE !!	*/
#define	CMS_SIZE	NUM_COLORS	/* The number of colors must be	*/
				        /* a power of 2!		*/
#define COLORMAP	"BUGS"

#define BACKGROUND	0 /*Black*/
#define WHITE		1
#define YELLOW		2
#define ORANGE		3
#define RED			4
#define GREEN		5
#define BLUE		6
#define SELECTEDC	7 /*White*/


#define NUM_COLORS	8		/* VERY IMPORTANT NOTE !!	*/
#define	CMS_SIZE	NUM_COLORS	/* The number of colors must be	*/



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

MakeDrawWindow ()
{
	Panel pop_label;	/*used to create the label for the population window*/


	/* Create population frame */
	G_population_frame =
		window_create(G_control_frame,FRAME,
			FRAME_LABEL,"Population",
			WIN_SHOW,			TRUE,
			WIN_X,				DRAW_X,
			WIN_Y,				DRAW_Y,
			WIN_WIDTH,			DRAW_WID,
			WIN_HEIGHT,			DRAW_LEN,
			0
		);


	/* Create a panel in the population frame to hold messages */
	pop_label = window_create(G_population_frame,PANEL,0);
	panel_create_item(pop_label,PANEL_MESSAGE,
		PANEL_LABEL_STRING,"Population Window",
		0);
	window_fit_height(pop_label);

	/* Create the canvas */
	G_population_window =
		window_create(G_population_frame, CANVAS,
			CANVAS_AUTO_EXPAND, 		TRUE,
			CANVAS_AUTO_SHRINK, 		TRUE,
			CANVAS_RETAINED,    		TRUE,
			CANVAS_REPAINT_PROC,		Repaint,
			CANVAS_RESIZE_PROC,			Resize,
			0
		);

	/* Get canvas pixwins */
	G_population_pw = canvas_pixwin(G_population_window);

	/* Get canvas font */
	G_population_font = pf_open("usr/lib/fonts/fixedwidthfonts/screen.b.12");

        SetColorMap();
}






/*--------------------------------------------------------------------------*/
/*									    */
/*	SetColorMap							    */
/*	Checks to see if the screen is color, and resets the colormap	    */
/*	accordingly.							    */
/*									    */
/*--------------------------------------------------------------------------*/
SetColorMap()
{
	unsigned char	red[CMS_SIZE];
	unsigned char	green[CMS_SIZE];
	unsigned char	blue[CMS_SIZE];
/*
	red[BACKGROUND] =	0;
	green[BACKGROUND] =	0;
	blue[BACKGROUND] =	0;

	red[WHITE] =		127;
	green[WHITE] =		0;
	blue[WHITE] =		0;

	red[YELLOW] =		255;
	green[YELLOW] =		0;
	blue[YELLOW] =		0;

	red[ORANGE] =		0;
	green[ORANGE] =		127;
	blue[ORANGE] =		0;

	red[RED] =		0;
	green[RED] =		255;
	blue[RED] =		0;

	red[GREEN] =		0;
	green[GREEN] =		0;
	blue[GREEN] =		127;

	red[BLUE] =		0;
	green[BLUE] =		0;
	blue[BLUE] =		255;

	red[SELECTEDC] =	255;
	green[SELECTEDC] =	255;
	blue[SELECTEDC] =	255;
*/
/*
	red[BACKGROUND] =	0;
	green[BACKGROUND] =	0;
	blue[BACKGROUND] =	0;

	red[WHITE] =		255;
	green[WHITE] =		255;
	blue[WHITE] =		255;

	red[YELLOW] =		245;
	green[YELLOW] =		213;
	blue[YELLOW] =		0;

	red[ORANGE] =		200;
	green[ORANGE] =		104;
	blue[ORANGE] =		0;

	red[RED] =			255;
	green[RED] =		0;
	blue[RED] =			0;

	red[GREEN] =		56;
	green[GREEN] =		200;
	blue[GREEN] =		50;

	red[BLUE] =			0;
	green[BLUE] =		0;
	blue[BLUE] =		255;

	red[SELECTEDC] =	235;
	green[SELECTEDC] =	50;
	blue[SELECTEDC] =	250;
*/

	red[BACKGROUND] =	0;
	green[BACKGROUND] =	0;
	blue[BACKGROUND] =	0;

	red[WHITE] =		143;
	green[WHITE] =		143;
	blue[WHITE] =		143;

	red[YELLOW] =		175;
	green[YELLOW] =		175;
	blue[YELLOW] =		175;

	red[ORANGE] =		191;
	green[ORANGE] =		191;
	blue[ORANGE] =		191;

	red[RED] =		207;
	green[RED] =		207;
	blue[RED] =		207;

	red[GREEN] =		223;
	green[GREEN] =		223;
	blue[GREEN] =		223;

	red[BLUE] =		239;
	green[BLUE] =		239;
	blue[BLUE] =		239;

	red[SELECTEDC] =	255;
	green[SELECTEDC] =	255;
	blue[SELECTEDC] =	255;


	/* Setup color if available */
 	if ((G_population_pw -> pw_pixrect -> pr_depth) > 1) {
		pw_setcmsname(G_population_pw,COLORMAP);
		pw_putcolormap(G_population_pw,0,CMS_SIZE,red,green,blue);
		window_set(G_population_window, 
			CANVAS_RETAINED,    		TRUE,
			0
		);
    	/*terminal_type = COLOR;*/
	}
	else
    	/*terminal_type = MONOCHROME*/ ;
}
