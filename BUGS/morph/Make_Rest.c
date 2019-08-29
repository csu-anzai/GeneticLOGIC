/****************************************************************************/
/*                                                                          */
/*	Make_Rest.c                                                         */
/*	by Robert Allen,  Josh Smith, Mike McDougall	July-August 1988    */
/*									    */
/*      This file contains the initialization files not in other Make_XX    */
/*      files.                                                              */
/*	These are:				                            */
/*			MakeWindows					    */
/*			MakeCntrlPanel					    */
/*			MakeQuitInactive				    */
/*			SetColorMap					    */
/*									    */
/****************************************************************************/

/* -------------------------------------------------------------------------*/
/*			 CONSTANTS					    */
/* -------------------------------------------------------------------------*/


#define NUM_COLORS	8		/* VERY IMPORTANT NOTE !!	*/
#define	CMS_SIZE	NUM_COLORS	/* The number of colors must be	*/
				        /* a power of 2!		*/
#define FILTER_SEARCH_PATH FILTERPATH   /* defined in the makefile      */

#define COLORMAP	"GED4cms_rgb"

#define BACKGROUND	0 /*Black*/
#define WHITE		1
#define YELLOW		2
#define ORANGE		3
#define RED			4
#define GREEN		5
#define BLUE		6
#define SELECTEDC	7 /*White*/

/* -------------------------------------------------------------------------*/
/*			 INCLUDES					    */
/* -------------------------------------------------------------------------*/

#include CNTRL_H
#include DS_H




/*--------------------------------------------------------------------------*/
/*									    */
/*	MakeWindows							    */
/*	this creates the three windows, and alters the color map of the	    */
/*	drawing window.							    */
/*									    */
/*--------------------------------------------------------------------------*/
MakeWindows() {

	MakeCntrlPanel();
	MakeDrawWindow();
	MakeTextWindow();

#ifdef TEXTWINDOWYES
	LaunchTextWindow();
#endif

	SetColorMap();

}


/*--------------------------------------------------------------------------*/
/*									    */
/*	MakeCntrlPanel							    */
/*	This creates the control frame and panel, associates and icon	    */
/*	with it, makes the text fields, and then creates the buttons	    */
/*									    */
/*--------------------------------------------------------------------------*/
MakeCntrlPanel()  {


	/* Attach icon to drawing frame */
	static short	icon_image[] = {
#include ICON
	};
	DEFINE_ICON_FROM_IMAGE(GED_icon, icon_image);


	G_control_frame = window_create(NULL,FRAME,
				FRAME_LABEL,	"Ged 5.0 Control Panel",
				WIN_X,			CNTRL_X,
				WIN_Y,			CNTRL_Y,
				WIN_WIDTH,		CNTRL_WID,
				WIN_HEIGHT,		CNTRL_LEN,
				FRAME_ICON,		&GED_icon,
				0);
	G_control_panel = window_create(G_control_frame,PANEL,0);
	G_file_field = panel_create_item(G_control_panel,PANEL_TEXT,
				PANEL_LABEL_STRING,         "File  :",
				PANEL_VALUE_DISPLAY_LENGTH, 33,
				0);

	G_filter_field = panel_create_item(G_control_panel,PANEL_TEXT,
				PANEL_LABEL_STRING,    "Filter:",
				PANEL_VALUE_DISPLAY_LENGTH, 30,
				0);

	G_file_path_field = panel_create_item(G_control_panel,PANEL_TEXT,
				PANEL_LABEL_STRING,"File Path:",
				PANEL_VALUE_DISPLAY_LENGTH, 30,
				0);

	G_filter_path_field = panel_create_item(G_control_panel,PANEL_TEXT,
				PANEL_LABEL_STRING,"Filter Path:",
				PANEL_VALUE_DISPLAY_LENGTH, 30,
				0);

	panel_set_value(G_filter_path_field,FILTER_SEARCH_PATH);

	MakeQuitInactive();
	MakeColorWheel();
	MakeButtons();

}

/*--------------------------------------------------------------------------*/
/*									    */
/*	MakeQuitInactive						    */
/*	this sets the quit option on the control panel's menu to inactive.  */
/*									    */
/*--------------------------------------------------------------------------*/
MakeQuitInactive() {
	Menu frame_menu;
	Menu_item quit_item;

	frame_menu = window_get(G_control_frame,WIN_MENU);
	quit_item = menu_find(frame_menu,MENU_STRING,"Quit",0);
	menu_set(quit_item,MENU_INACTIVE,TRUE,0);
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


	/* Setup color if available */
 	if ((G_draw_pw -> pw_pixrect -> pr_depth) > 1) {
		pw_setcmsname(G_draw_pw,COLORMAP);
		pw_putcolormap(G_draw_pw,0,CMS_SIZE,red,green,blue);
		window_set(G_draw_window, 
			CANVAS_RETAINED,    		TRUE,
			0
		);
    	/*terminal_type = COLOR;*/
	}
	else
    	/*terminal_type = MONOCHROME*/ ;
}
