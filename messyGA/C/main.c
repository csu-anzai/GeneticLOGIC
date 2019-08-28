/*==============================================
       file : main.c
  
    purpose : main file
 
  developed : 1991

     author : Kalyanmoy Deb
=================================================*/

#include "mga.h"


main()
{
	FILE *fopen();
	int i;

	/*  get input parameters and create template  */
	get_input();
	
	/*  open input file for era dependent information  */
	fin = fopen(Erafilename,"r");

	/*  for each era, till the maximum era */
	while (era <= max_era) 
	{
		printf("BEGIN ERA %d . . .\n",era);
		/*  initialize variables and create initial population  */
		initialize();

		/*  GA loop  */
		while (++gen <= maxgen && ! stopmgaflag) {
			generate();
		}

		/* The best string so far is assigned to the template
	           for next era  */
		assign_beststring_to_template();

		/*  free all memory stored for variables  */
		freeallmemory();

		printf("END   ERA %d . . .\n\n",era);

		/*  increment era  */
		era++;
	}
	fclose(fin);
}
