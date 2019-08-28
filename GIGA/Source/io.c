#include "giga.h"

/*

	***********************  POPPRINT   *********************
	*	Print Populations Etc				*
	*********************************************************

* Copyright
* Author: Joseph Culberson
* Date: April 24, 1992
* Permission is hereby granted to copy all or any part of
* this program for free distribution.  The author's name
* and this copyright notice must be included in any copy.
* This program is provided ``as is'' without any express or implied
* warranties.

* Comments and suggestions may be emailed to joe@cs.ualberta.ca
* but this implies no obligation on the part of the author.


* Users are encouraged to modify and extend this program,
* and such modifications should be commented with appropriate attribution.
* This program should not be considered a final product, but rather it is hoped
* that it will serve as a stimulus for further experimentation and development.

* Initial Version Created April 12, 1992.


*/

char chr[] = { 
'0','1','2','3','4','5','6','7','8','9',
'A','B','C','D',
'E','F','G','H','I',
'J','K','L','M','N',
'O','P','Q','R','S',
'T','U','V','W','X',
'Y','Z',
'a','b','c','d','e',
'f','g','h','i','j',
'k','l','m','n','o',
'p','q','r','s','t',
'u','v','w','x','y',
'z',
'<','>','?','+','-','.',';',':','{','}',
'[',']','(',')','*','&','^','%','$','#',
'@','!',',','/','|','='
};

void printpop()
/*
	print out population, strings and values
*/
{
	int i,j;
	struct popmember_st *p;

	if (OPTIONS.alphasize > 88) {
		printf("Cannot print this population ");
		printf("Alphabet too large (>88)\n");
		exit(1);
	}
	
	printf(" POPULATION \n");
	for(i=0;i<OPTIONS.popsize;i++) {
		p = getmember(i);
		printf("%f\t",p->value);
		for(j=0;j<OPTIONS.stringsize;j++) {
			printf("%c",chr[p->individual[j]]);
		}
		printf("\n");
	}
}

#ifdef DEBUG
void printrange(x,y)
/*
	print out the members of population indicated
*/
int x,y;
{
        int i,j;
        struct popmember_st *p;

        printf(" POPULATION \n");
        for(i=x;i<=y;i++) {
                p = getmember(i);
                printf("%f\t",p->value);
                for(j=0;j<OPTIONS.stringsize;j++) {
                        printf("%c",chr[p->individual[j]]);
                }
                printf("\n");
        }
}
#endif

