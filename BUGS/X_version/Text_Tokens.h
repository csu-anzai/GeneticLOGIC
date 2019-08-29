/*	Text_Tokens.h */
/*	Codes used to identify/specify buttons, menu choices, and			*/
/*	default actions.													*/


/* These values WERE used by the procedures ParseTextCommand and        */
/* DecodeTextCommand, in Text_Funcs.c                                   */
#define UNKNOWN             -1
#define BREED                0
#define RANDOMIZE	     1
/*#define LOAD                 2
#define SAVE                 3
#define SHOW                 4*/
#define HELP                 2
#define QUIT                 3

#define LOAD_NEW             1
#define LOAD_WITH_OVERLAY    2

#define SAVE_ALL             1
#define SAVE_SELECTION       2

#define SHOW_DRAWING         1


/* These sepcify default actions										*/		
#define QUIT_DEFAULT         1
#define BREED_DEFAULT		 1
#define RANDOMIZE_DEFAULT	 1
#define LOAD_DEFAULT         1
#define SAVE_DEFAULT         1
#define HELP_DEFAULT         1
#define SHOW_DEFAULT         1
