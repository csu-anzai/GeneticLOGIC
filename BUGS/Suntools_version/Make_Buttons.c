/* ************************************************************************	*/
/* 	Make_Buttons.c				(Thu Jul  7 09:40:20 EDT 1988)				*/
/*	by Mike McDougall														*/
/*																			*/
/*      Modified by SMALL, 1989                           */
/*      Added various buttons                             */

/* ************************************************************************	*/

/* ------------------------------------------------------------------------	*/
/*			INCLUDE FILES 													*/
/* ------------------------------------------------------------------------	*/

#include "Control.h"
#include "Text_Tokens.h"
#include "GA.h"

/* ------------------------------------------------------------------------	*/
/*			 CONSTANTS	 													*/
/* ------------------------------------------------------------------------	*/

#define NUM_BUTTONS 4


/* ------------------------------------------------------------------------	*/
/*			 VARIABLES	 (AND EXTERNS FOR INITIALIZATION)					*/
/* ------------------------------------------------------------------------	*/



	/* Labels for buttons on the control panel */
char*	button_label[NUM_BUTTONS] = {
			"Breed",
			"New",
/*			"Load",
			"Save",
			"Show",
*/			"Help",
			"Quit",
		};


	/* Functions called when left click on a button */

extern	int	HandleClickBREED(),
			HandleClickRANDOMIZE(),
/*			HandleClickREAD(),
			HandleClickWRITE(),
			HandleClickSHOW(),
*/			HandleClickHELP(),
			HandleClickQUIT();

int		(*HandleClick[NUM_BUTTONS])() = {
			HandleClickBREED,
			HandleClickRANDOMIZE,
/*			HandleClickREAD,
			HandleClickWRITE,
			HandleClickSHOW,
*/			HandleClickHELP,
			HandleClickQUIT
		};


	/* Menu functions called when a buttons menu is used */

extern	int	HandleMenuBREED();
extern	int	HandleMenuRANDOMIZE();
/*extern	int	HandleMenuREAD();
extern	int	HandleMenuWRITE();
extern	int	HandleMenuSHOW();
*/extern	int	HandleMenuHELP();
extern	int	HandleMenuQUIT();

int		(*HandleMenu[NUM_BUTTONS])() = {
			HandleMenuBREED,
			HandleMenuRANDOMIZE,
/*			HandleMenuREAD,
			HandleMenuWRITE,
			HandleMenuSHOW,
*/			HandleMenuHELP,
			HandleMenuQUIT
		};

	/* Menu associated with each button */

Menu	 cntrl_menu[NUM_BUTTONS];


PrintText(msg)
char* msg;
{printf(msg);}


/* ------------------------------------------------------------------------	*/
/*																			*/
/*	InitMenus	This creates the menu array. Essentially all that this does */
/*	is assign labels to the menus.	In general, each button should have its	*/
/*	primary (left click) action as the first menu option.					*/
/*																			*/
/* ------------------------------------------------------------------------	*/

InitMenus()
{

	cntrl_menu[BREED]	= menu_create(
						MENU_STRINGS,
						"Breed",
					0,
			0);
	cntrl_menu[RANDOMIZE]= menu_create(
						MENU_STRINGS,
						"New population!",
					0,
			0);
/*	cntrl_menu[SAVE]		= menu_create(
					MENU_STRINGS,
						"Save Population",
						"Save Sub-Population",
					0,
				0);
	cntrl_menu[LOAD]		= menu_create(
					MENU_STRINGS,
						"Load Population",
						"Add Sub-Population",
					0,
				0);
	cntrl_menu[SHOW]		= menu_create(
					MENU_STRINGS,
						"Redisplay Environment Window",
					0,
				0);
*/	cntrl_menu[HELP]		= menu_create(
					MENU_STRINGS,
						"Help!!!",
					0,
			0);
	cntrl_menu[QUIT]		= menu_create(
					MENU_STRINGS,
						"Quit",
					0,
				0);
}


/* ------------------------------------------------------------------------	*/
/*																			*/
/*	Handlers	There are two functions associated with each button.  The	*/
/*	first function -- HandleClick**** contains calls to the functions which	*/
/*	should be executed when a button is clicked.  HandleMenu*** takes one	*/
/*	argument -- the index of the menu option selected.  Using this			*/
/*	argument, it calls the functions which should be executed when a menu 	*/
/*	option is selected.														*/
/*																			*/
/* ------------------------------------------------------------------------	*/

/* BREED */

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Make next generation									*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleClickBREED()
		{
			Breed(G_Population);
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) Default action										*/
		/*															*/
		/* --------------------------------------------------------	*/


		HandleMenuBREED(index)
		int	 index;
		{
			/* FUNCTION CODE */

			switch(index) {
				case BREED_DEFAULT :			
					HandleClickBREED();
					break;
				default:
					break;
				}
		}

/* RANDOMIZE */

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Reset population										*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleClickRANDOMIZE()
		{
			Randomize_Pop(G_Population);
			Grow_Pop(G_Population);
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) Default action										*/
		/*															*/
		/* --------------------------------------------------------	*/


		HandleMenuRANDOMIZE(index)
		int	 index;
		{
			/* FUNCTION CODE */

			switch(index) {
				case RANDOMIZE_DEFAULT :			
					HandleClickRANDOMIZE();
					break;
				default:
					break;
				}
		}


/* READ */

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Read in the Population file named in the file field		*/
		/*															*/
		/* --------------------------------------------------------	*/


		HandleClickREAD()
		{ char message[50];

		  LoadPOP((char *)panel_get_value(G_file_field),FALSE);
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) Default action										*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleMenuREAD(index)
		int	 index;
		{
			/* FUNCTION CODE */

			switch(index) {
				case LOAD_NEW :
					HandleClickREAD();
					break;
				case LOAD_WITH_OVERLAY :
					LoadPOP((char *)panel_get_value(G_file_field),TRUE);
				default:
					break;
				}
		}


/* WRITE */

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Write out the Population file named in the file field	*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleClickWRITE()
		{char message[50];
			sprintf(message,"\nSaving file : %s\n",(char *)panel_get_value(G_file_field));
		        PrintText(message);
			if (SavePOP((char *)panel_get_value(G_file_field),FALSE))
			  PrintText("Save completed \n");
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) Default action										*/
		/*															*/
		/* --------------------------------------------------------	*/


		HandleMenuWRITE(index)
		int	 index;
		{char message[50];
			/* FUNCTION CODE */

			switch(index) {
				case SAVE_ALL :
					HandleClickWRITE();
					break;
				case  SAVE_SELECTION :
					sprintf(message,"\nSaving file : %s\n",(char *)panel_get_value(G_file_field));
					PrintText(message);
					if (SavePOP((char *)panel_get_value(G_file_field),TRUE))
					  PrintText("Save completed \n");
					break;
				default:
					break;
				}
		}

/* HELP	*/

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Currently the help button does nothing					*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleClickHELP()
		{
			PrintText("See 'BUGS: A Non-Technical Report' ");
			PrintText("by J. Smith\n");
			PrintText("You can get this via anonymous FTP ");
			PrintText("to cs.williams.edu.\n");
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Currently the help menu does nothing					*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleMenuHELP(index)
			int	 index;
		{
			/* FUNCTION CODE */

			switch(index) {
				case HELP_DEFAULT :
					PrintText("See 'BUGS: A Non-Technical Report' ");
					PrintText("by J. Smith\n");
					PrintText("You can get this via anonymous FTP ");
					PrintText("to cs.williams.edu.\n");
					break;
				default:
					break;
				}
		}

/* SHOW	*/

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Get back the Environment window after user has closed it*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleClickSHOW()
		{
			window_set(G_population_frame,WIN_SHOW,TRUE,0);
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) default action										*/
		/*	2) Show names											*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleMenuSHOW(index)
		int	 index;
		{
			/* FUNCTION CODE */

			switch(index) {
				case SHOW_DRAWING :
					HandleClickSHOW();
					break;}
		}

/* REDRAW	*/

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Redraw the graph										*/
		/*															*/
		/* --------------------------------------------------------	*/
/*
		HandleClickREDRAW()
		{	extern rect_node G_select_region;

			SetRectActive(G_select_region,FALSE);
			DrawGraph();
		}
*/
		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) Default Action										*/
		/*															*/
		/* --------------------------------------------------------	*/

/*
		HandleMenuREDRAW(index)
		int	 index;
		{


			switch(index) {
				case REDRAW_ALL :
					HandleClickREDRAW();
					break;
				case REDRAW_WITH_COLOR : 
					RedrawWithColor();
					break;
				default:
					break;
				}
		}
*/


/*	QUIT	*/

		/* --------------------------------------------------------	*/
		/*															*/
		/*	Close down ged by sending an EOT signal to the text		*/
		/*	window													*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleClickQUIT() {
			if(window_destroy(G_control_frame))
				printf("%c\n",TERMCHAR);
		}

		/* --------------------------------------------------------	*/
		/*															*/
		/*	1) Default action										*/
		/*															*/
		/* --------------------------------------------------------	*/

		HandleMenuQUIT(index)
		int index;
		{
			switch(index)	{
				case 1:
					HandleClickQUIT();
					break;
				default:
					break;
			}
		}



/* ------------------------------------------------------------------------	*/
/*		If you hit the right mouse button, find out which screen button was	*/
/*	hit, display the menu for that button, find out which item the user		*/
/*	selected and perform the appropriate action.							*/
/*																			*/
/* ------------------------------------------------------------------------	*/

HandleButtonEvent(button_item,button_event)
Panel_item	 button_item;
Event		*button_event;
{
	/* LOCAL VARIABLES */
	
	int	menu_choice;
	int	index;

	/* FUNCTION CODE */

	if ( event_id(button_event) == MS_RIGHT && event_is_down(button_event) ) {
		index = (int) panel_get(button_item,PANEL_CLIENT_DATA);
		menu_choice = (int) menu_show(cntrl_menu[index],G_control_panel,
									  button_event,
							0);
		(*HandleMenu[index])(menu_choice);
	}
	else 
		panel_default_handle_event(button_item,button_event);
}

/* ------------------------------------------------------------------------	*/
/*		Create each button.  Index holds which button is being created.		*/
/* ------------------------------------------------------------------------	*/


void MakeButtons()
{
int index;

	InitMenus();	

	for (index = 0; index < NUM_BUTTONS;index++) {
		panel_create_item(G_control_panel,PANEL_BUTTON,
			PANEL_CLIENT_DATA, index,
			PANEL_LABEL_IMAGE, panel_button_image(G_control_panel,
			button_label[index],0,0),
			PANEL_NOTIFY_PROC,HandleClick[index],
			PANEL_EVENT_PROC,HandleButtonEvent,
			0);
	}

}



void MakeFitnessDevice()
{
int i,j,org_i;

	for (i = 0; i < ORG_X; i++) {
	for (j = 0; j < ORG_Y; j++) {
	org_i = (i*ORG_Y)+j;
		G_fit_switch[org_i] = panel_create_item(G_fitness_panel, PANEL_TOGGLE,
			PANEL_ITEM_X,			(i * TOGGLE_X) + (TOGGLE_X/2),
			PANEL_ITEM_Y,			j * TOGGLE_Y,
			PANEL_TOGGLE_VALUE,		0, G_switch_default,
			0
		);
	}}
	window_fit_width(G_fitness_panel);

}
