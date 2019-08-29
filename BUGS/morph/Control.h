/***************************************************************************/
/*																		   */
/* 	Control.h	(Tue Jul  5 14:30:30 EDT 1988)	MM						   */
/*																		   */
/* This contains the #include files, constants, and global variables used  */
/* to make the buttons and windows.										   */
/*																		   */
/***************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <suntool/sunview.h>
#include <suntool/panel.h>
#include <suntool/canvas.h>
#include <suntool/textsw.h>
#include <suntool/walkmenu.h>

/*-------------------------------------------------------------------------*/
/*			 GLOBAL VARIABLES	 			   */
/*-------------------------------------------------------------------------*/

extern	Panel		 G_control_panel;
extern	Panel		 G_fitness_panel;
extern	Panel_item	 G_fit_switch[];
extern	int			 G_switch_default;
extern	int			 G_show_genes;
extern	int			 G_print_out;
extern	Frame		 G_control_frame,
					 G_population_frame;
extern	Canvas		 G_population_window;
extern	Pixwin		*G_population_pw;
extern	Rect		*G_population_rect;
extern	Pixfont		*G_population_font;
extern  Panel_item	 G_file_field;
extern  Panel_item   G_file_path_field;
extern	int			 G_current_height;
extern	int			 G_current_width;
extern	int			 G_org_height;
extern	int			 G_org_width;
extern	int			 G_x_scale;
extern	int			 G_y_scale;
extern	int			 G_x_trans;
extern	int			 G_y_trans;
/*-------------------------------------------------------------------------*/
/*			 CONSTANTS	 												   */
/*-------------------------------------------------------------------------*/


#define	CNTRL_X		0
#define	CNTRL_Y		0
#define	CNTRL_WID	700
#define	CNTRL_LEN	125

#define	DRAW_X		0	
#define	DRAW_Y		125
#define	DRAW_WID	700	
#define	DRAW_LEN	(700/4+50)

#define TOGGLE_X	25
#define TOGGLE_Y	25

#define TERMCHAR	'\004'













