/****************************************************************************/
/*																			*/
/*	Make_Cntrl.c														 		*/
/*	Creates control window, calls Make_Buttons() and Make_Draw().			*/
/*																			*/
/****************************************************************************/


/* -------------------------------------------------------------------------*/
/*			 INCLUDES					    */
/* -------------------------------------------------------------------------*/

#include "Control.h"




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
/*	static short	icon_image[] = {
#include ICON
	};
	DEFINE_ICON_FROM_IMAGE(GED_icon, icon_image);*/


	G_control_frame = window_create(NULL,FRAME,
				FRAME_LABEL,	"BUGS  Copyright 1990 J. Smith",
				WIN_X,			CNTRL_X,
				WIN_Y,			CNTRL_Y,
				WIN_WIDTH,		CNTRL_WID,
				WIN_HEIGHT,		CNTRL_LEN,
			/*	FRAME_ICON,		&GED_icon,*/
				0);

	G_fitness_panel = window_create(G_control_frame,PANEL,0);
	MakeFitnessDevice();

	G_control_panel = window_create(G_control_frame,PANEL,
				WIN_RIGHT_OF,	G_fitness_panel,
				0);

/*	G_file_field = panel_create_item(G_control_panel,PANEL_TEXT,
				PANEL_LABEL_STRING,         "File  :",
				PANEL_VALUE_DISPLAY_LENGTH, 33,
				0);

	G_file_path_field = panel_create_item(G_control_panel,PANEL_TEXT,
				PANEL_LABEL_STRING,"File Path:",
				PANEL_VALUE_DISPLAY_LENGTH, 42,
				0);
*/
	MakeButtons();

}
