%{
/* lex.l - lexical analyzer for L-systems.
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
 * $Log:	lex.l,v $
 * Revision 1.4  91/03/20  21:34:36  leech
 * Fix malloc declaration problem with flex.
 * 
 * Revision 1.3  91/03/20  10:33:56  leech
 * Support for flex and gcc.
 *
 * Revision 1.2  90/10/12  18:48:20  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: lex.l,v 1.4 91/03/20 21:34:36 leech Exp Locker: leech $";

#include <stdio.h>
#include <string.h>
#include "lexdefs.h"
#include "debug.h"

/* We go through contortions to obtain the token IDs from y.tab.h,
 *  since we can't include it directly and compile lex.yy.c with C++.
 * And why can't we compile it with C++? Because <stream.h> defines
 *  variables input and output, named the same as macros generated
 *  by lex! Ick.
 */
#include "token.h"

int lexcol = 1, lexline = 1;

#ifndef __GNUC__
/* Prototypes of lex library functions */
/* Actually this should probably be '#ifndef _USES_FLEX_' or some such */

/* flex tries to be clever about declaring malloc()/free(), lex doesn't */
#include <malloc.h>

/* in libln:allprint.o */
int allprint(/* char c */);
int sprint(/* char *s */);
int printable(/* int c */);

/* in libln:reject.o */
int yyreject();
int yyracc(/* int m */);

/* in libln:yyless.o */
int yyless(/* int x */);

/* in libln:yywrap.o */
int yywrap();

/* functions declared locally in lex output file lex.yy.c */
int yylook();
int yyback(/* int *p, int m */);
int yyinput();
int yyoutput(/* int c */);
int yyunput(/* int c */);
#endif /*__GNUC__*/

/* user-defined functions */
void lex_in_expr(/* int flag */);
static int spaceadd(/* int curlen, char *s */);
static int countnewlines(/* char *s, int *width */);
%}
U	[A-Z]
L	[a-z_]
A	[A-Za-z_]
D	[0-9]
E	[Ee][-+]?{D}+

%START MODULE EXPRESSION
%%
"/*"(\*[^/]|[^*]+)*"*/"     { lexcol += strlen(yytext);
			      lexline += countnewlines(yytext, &lexcol);
			    }
#ignore			    { lexcol += 7; return IGNORE; }
#include		    { lexcol += 8; return INCLUDE; }
#define			    { lexcol += 7; return DEFINE; }
START			    { lexcol += 5; return START; }
->			    { lexcol += 2; return YIELDS; }
<MODULE>{A}{D}*|Fl|Fr	    { lexcol += strlen(yytext);
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: module name -> %s\n", yytext));
			      return NAME;
			    }
<EXPRESSION>{A}({A}|{D})*   { lexcol += strlen(yytext);
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: expression name -> %s\n", yytext));
			      return NAME;
			    }
<MODULE>{D}		    { lexcol++;
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: module name -> %s\n", yytext));
			      return NAME;
			    }
<EXPRESSION>{D}+	    { lexcol += strlen(yytext);
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: integer -> %s\n", yytext));
			      return INTEGER;
			    }
<EXPRESSION>{D}+"."{D}*({E})? |
{D}*"."{D}+({E})? |
{D}+{E}E		    { lexcol += strlen(yytext);
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: real -> %s\n", yytext));
			      return REAL;
			    }
<EXPRESSION>\<=		    { lexcol += 2; return LE; }
<EXPRESSION>==		    { lexcol += 2; return EQ; }
<EXPRESSION>=		    { lexcol++; return EQ; }
<EXPRESSION>>=		    { lexcol += 2; return GE; }
<EXPRESSION>!=		    { lexcol += 2; return NE; }
<EXPRESSION>&&		    { lexcol += 2; return AND; }
<EXPRESSION>\|\|	    { lexcol += 2; return OR; }
<MODULE>[+^/&|~%!-]	    { lexcol++;
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: module name -> %s\n", yytext));
			      return NAME;
			    }
<EXPRESSION>[+^/&|~%!-]     { lexcol++; return yytext[0]; }
[*:(),<>]		    { lexcol++; return yytext[0]; }
[[\]{}\\$\.']		    { lexcol++;
			      save_name(yytext);
			      PDEBUG(PD_LEXER, fprintf(stderr, "lex: name -> %s\n", yytext));
			      return NAME;
			    }
[ \t]+			    { lexcol = spaceadd(lexcol, yytext); }
\\\n			    { lexcol = 1; lexline++; }
\n			    { lexcol = 1; lexline++; return '\n'; }
.			    { lexcol++; return ERROR; }
%%
/* Stack of start states adjusted by the parser in lsys.y */
/* lexstate[lexstack] is current state (lexstack < 0 -> start state) */
#define STACKSIZE 10
static int lexstate[STACKSIZE] = { LEX_START };
static int lexstack = 0;

/* Change the start state to that indicated by 'state' */
static void switchstate(state, msg)
int state;
char *msg;
{
    switch (state) {

	case LEX_EXPRESSION:
	    BEGIN EXPRESSION;
	    PDEBUG(PD_LEXER, fprintf(stderr, "%s: BEGIN EXPRESSION\n", msg));
	    break;

	case LEX_MODULE:
	    BEGIN MODULE;
	    PDEBUG(PD_LEXER, fprintf(stderr, "%s: BEGIN MODULE\n", msg));
	    break;

	case LEX_START:
	default:
	    BEGIN 0;
	    PDEBUG(PD_LEXER, fprintf(stderr, "%s: BEGIN START\n", msg));
	    break;
    }
}

/* Push a start state onto the stack and change lex's start state to match */
void lex_pushstate(type)
int type;
{
    if (lexstack >= STACKSIZE-1) {
	fprintf(stderr, "lex_pushstate: stack of start states full!\n");
	exit(1);
    }

    lexstate[++lexstack] = type;

    switchstate(type, "pushstate");
}

/* Pop a start state from the stack and change lex's start state to match */
void lex_popstate()
{
    int type;

    if (lexstack <= 0) {
	fprintf(stderr, "lex_popstate: stack of start states empty!\n");
	exit(1);
    }

    type = lexstate[--lexstack];

    switchstate(type, "popstate");
}

static int spaceadd(curlen, s)
int curlen;
char *s;
{
    while (*s != '\0') {
	if (*s++ == '\t')
	    curlen = (curlen + 8) & ~7;
	else
	    curlen++;
    }
    return curlen;
}

/* Count number of newlines in s; keep track of the current column in
 *  *col.
 */
static int countnewlines(s, col)
char *s;
int *col;
{
    int nl = 0;

    while (*s != '\0') {
	if (*s == '\n') {
	    nl++;
	    *col = 1;
	} else if (*s == '\t')
	    *col = (*col + 8) & ~7;
	else
	    (*col)++;

	s++;
    }

    return nl;
}

/* Make a copy of a string */
char *strdup(s)
char *s;
{
    int   size = strlen(s) + 1;
    char *n = malloc(size);

    if (n == NULL) {
	fprintf(stderr, "Can't allocate %d bytes in strdup\n", size);
	exit(1);
    }

    strcpy(n, s);
    return n;
}

char *lex_token_ptr = NULL;
int   lex_token_length = 0;

/* Save the token string pointed to by s for access by the parser */
save_name(s)
char *s;
{
    int len = strlen(s);

    if (len >= lex_token_length) {
	/* Reallocate the buffer */
	if (lex_token_ptr != NULL)
	    free(lex_token_ptr);
	if ((lex_token_ptr = malloc(len+1)) == NULL) {
	    fprintf(stderr, "FATAL: can't allocate %d bytes to buffer token in lexer!\n", len+1);
	    exit(1);
	}
	lex_token_length = len+1;
    }

    strcpy(lex_token_ptr, s);
}

/* Return the token string (called by parser) */
char *lex_token() {
    return lex_token_ptr;
}
