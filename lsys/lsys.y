%{
/* lsys.y - parser for L-systems.
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
 * $Log:	lsys.y,v $
 * Revision 1.3  91/03/20  10:33:14  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:23  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: lsys.y,v 1.3 91/03/20 10:33:14 leech Exp Locker: leech $";

#include <ctype.h>
#ifndef __GNUG__
#include <libc.h>
#else
#include <std.h>
#endif /*__GNUG__*/
#include <malloc.h>

#include "parser.h"

/* default version in liby:yyerror.o as extern "C" void yyerror(char *); */
void yyerror(char *);

/* lexical scanner (in lex.yy.o when automatically produced by lex) */
#include "lexdefs.h"
extern "C" int	yylex();

extern "C" void lex_pushstate(int), lex_popstate();
extern "C" int	lexcol, lexline;

/* Return a pointer to the last string token parsed; in lexer */
extern "C" char *lex_token();


// A separate list of CF productions is maintained until all
//  productions have been read.
static List(Production) Context_free_rules;

// Should expressions be evaluated when read in?
static boolean BindExpression = false;
%}

%start lsystem

%union {
    int		      name;		/* module or variable name id */
    float	      probability;	/* production probability */
    Value	     *value;		/* numeric value */
    Module	     *module;
    Expression	     *expression;
    List(Module)     *moduleList;	/* list of modules as in a production successor */
    List(Expression) *expressionList;	/* list of expressions within a module */
    Predecessor      *predecessor;	/* a production predecessor */
    Successor	     *successor;	/* a production successor */
    List(Successor)  *successors;	/* list of production successors with probabilities */
    }

%type <moduleList>	modules right_list
%type <module>		module
%type <expressionList>	arguments exprlist
%type <expression>	expression conditional optional_conditional
%type <value>		value
%type <predecessor>	predecessor center_list
%type <successor>	successor
%type <successors>	successors optional_successors
%type <probability>	probability
%type <name>		name

%token ERROR VALUE INTEGER REAL FUNCTION
%token IGNORE INCLUDE DEFINE YIELDS
%token <name> NAME START
%left OR
%left AND
%left '|'
%left '&'
%left EQ NE
%left '<' LE GE '>'
%left '+' '-'
%left '*' '/' '%'
%left '^'
%left UMINUS
%left '!'
%left '~'
%%
lsystem     :	    {
		      // Get lex into correct start condition
		      lex_pushstate(LEX_EXPRESSION);
		    }
		lines
		    { // This ensures that CF productions are applied last,
		      //  since they are at the end of the list of productions.
		      Rules.append(&Context_free_rules);
		      lex_popstate();
		    }
	    ;

lines	    :	lines line
	    |	/* empty */
	    ;

line	    :	INCLUDE modules '\n'
	    ;

line	    :	IGNORE
		    { lex_pushstate(LEX_MODULE); }
		names '\n'
		    { lex_popstate(); }
	    ;

line	    :	DEFINE name expression
		    { SymbolTable.enter(Name($2), $3->evaluate(SymbolTable));
		      delete $3;
		    }
		'\n'
	    ;

line	    :	START ':'
		    { lex_pushstate(LEX_MODULE);
		      // Bind starting list expressions according to symtab
		      BindExpression = true;
		    }
		modules '\n'
		    { BindExpression = false;
		      lex_popstate();
		      Start = $4;
		    }
	    ;

line	    :	'\n'
	    ;

line	    :	':'
		    { lex_pushstate(LEX_EXPRESSION); }
		expression
		    { lex_popstate();
		      cerr << "Evaluating expression: " << *$3 << endl;
		      cerr << $3->evaluate(SymbolTable) << endl;
		      delete $3;
		    }
	    ;

line	    :	name
		    { lex_pushstate(LEX_MODULE); }
		':' predecessor
		optional_conditional
		successors
		    { lex_popstate();
		      Production *p = new Production($1, $4, $5, $6);
		      PDEBUG(PD_PARSER, cerr << "Parsed production: " << *p << endl);
		      if (p->context_free())
			Context_free_rules.append(p);
		      else
			Rules.append(p);
		    }
	    ;

/* A list of names to be ignored in context matching; just add
 *  them to the table of ignored names.
 */
names	    :	names single_name
	    |	/* empty */
	    ;

single_name :	name
		    {
		      PDEBUG(PD_PARSER, cerr << "Ignoring name " << Name($1) << endl);
		      IgnoreTable.enter(Name($1), Value(1));
		    }
	    ;

/* The convolutions below ensure that at least one successor appears
 *  for a production. This not only makes sense intuitively, it
 *  avoids parsing ambiguities.
 */
successors  :	successor
		optional_successors
		    { $$ = new List(Successor);
		      $$->append($1);
		      $$->append($2);
		      delete $2;
		    }
	    ;

optional_successors :
		optional_successors successor
		    { $$ = $1;
		      $$->append($2);
		    }
	    |	/* empty */
		    { $$ = new List(Successor); }
	    ;

successor   :	YIELDS probability modules '\n'
		    { $$ = new Successor($3, $2);
		      PDEBUG(PD_PARSER, cerr << "Parsed successor: " << *$$ << endl);
		    }
	    ;

probability :	'(' value ')'
		    { float f;
		      if ($2->value(f))
			$$ = f;
		      else
			$$ = 0.0;
		      delete $2;
		    }
	    |	/* empty */
		    { $$ = 1.0; }
	    ;

predecessor :	modules
		    { PDEBUG(PD_PARSER, cerr << "\ttentative LHS is " << *$1 << endl); }
		center_list
		    {
		      PDEBUG(PD_PARSER,
			     cerr << "\tcenter,right are " << *$3 << endl);
		      $$ = $3;
		      if ($3->center == NULL) {
			// Two possibilities:
			//  A
			//  A > B
			// In both cases, 'modules' is center module alone;
			//  should verify list has only one module and
			//  delete the list after removing that module.

			ListIterator(Module) mi($1);
			$$->center = mi.first();
			PDEBUG(PD_PARSER, cerr << "Predecessor (no context) is: " << *$$ << endl);
		      } else {
			// Two possibilities:
			//  A < B
			//  A < B > C
			// In both cases, 'modules' is left context

			$$->left = $1;
			PDEBUG(PD_PARSER, cerr << "Predecessor (context) is: " << *$$ << endl);
		      }
		    }
	    ;

center_list :	'<' module right_list
		    { $$ = new Predecessor(NULL, $2, $3); }
	    |	right_list
		    { $$ = new Predecessor(NULL, NULL, $1); }
	    ;

right_list  :	'>' modules
		    { $$ = $2; }
	    |	/* empty */
		    { $$ = NULL; }
	    ;

modules     :	modules module
		    { $1->append($2);
		      $$ = $1;
		    }
	    |	/* empty */
		    { $$ = new List(Module); }
	    ;

module	    :	name
		arguments
		    { Value v;
		      boolean ignore = IgnoreTable.lookup(Name($1), v);

		      $$ = new Module($1, $2, ignore);
		    }
	    ;

arguments   :	'('
		    { lex_pushstate(LEX_EXPRESSION); }
		exprlist ')'
		    { lex_popstate();
		      $$ = $3;
		    }
	    |	/* empty */
		    { $$ = NULL; }
	    ;

exprlist    :	exprlist ',' expression
		    { if (BindExpression == true) {
			Value v = $3->evaluate(SymbolTable);
			$1->append(new Expression(v));
			delete $3;
		      } else {
			$1->append($3);
		      }
		      PDEBUG(PD_PARSER, cerr << "Parsed additional expression: " << *$3 << endl);
		      $$ = $1;
		    }
	    |	expression
		    { $$ = new List(Expression);
		      PDEBUG(PD_PARSER, cerr << "Parsed expression: " << *$1 << endl);
		      if (BindExpression == true) {
			Value v = $1->evaluate(SymbolTable);
			$$->append(new Expression(v));
			delete $1;
		      } else {
			$$->append($1);
		      }
		    }
	    |	/* empty */
		    { $$ = new List(Expression); }
	    ;

optional_conditional :
		':'
		    { lex_pushstate(LEX_EXPRESSION); }
		conditional
		    { lex_popstate();
		      $$ = $3;
		    }
	    | /* empty */
		    { $$ = NULL; }

conditional :	'*'
		    { $$ = NULL; }
	    |	expression
		    { PDEBUG(PD_PARSER, cerr << "Parsed conditional expression: " << *$1 << endl);
		      $$ = $1; }
	    ;

expression  :	expression OR  expression
		    { $$ = new Expression(OR ,$1,$3); }
	    |	expression AND expression
		    { $$ = new Expression(AND,$1,$3); }
	    |	expression '|' expression
		    { $$ = new Expression('|',$1,$3); }
	    |	expression '&' expression
		    { $$ = new Expression('&',$1,$3); }
	    |	expression EQ  expression
		    { $$ = new Expression(EQ ,$1,$3); }
	    |	expression NE  expression
		    { $$ = new Expression(NE ,$1,$3); }
	    |	expression '<' expression
		    { $$ = new Expression('<',$1,$3); }
	    |	expression LE  expression
		    { $$ = new Expression(LE ,$1,$3); }
	    |	expression GE  expression
		    { $$ = new Expression(GE ,$1,$3); }
	    |	expression '>' expression
		    { $$ = new Expression('>',$1,$3); }
	    |	expression '+' expression
		    { $$ = new Expression('+',$1,$3); }
	    |	expression '-' expression
		    { $$ = new Expression('-',$1,$3); }
	    |	expression '*' expression
		    { $$ = new Expression('*',$1,$3); }
	    |	expression '/' expression
		    { $$ = new Expression('/',$1,$3); }
	    |	expression '%' expression
		    { $$ = new Expression('%',$1,$3); }
	    |	expression '^' expression
		    { $$ = new Expression('^',$1,$3); }
	    |	'-' expression		    %prec UMINUS
		    { $$ = new Expression(UMINUS,$2,(Expression *)NULL); }
	    |	'!' expression
		    { $$ = new Expression('!'	,$2,(Expression *)NULL); }
	    |	'~' expression
		    { $$ = new Expression('~'	,$2,(Expression *)NULL); }
	    |	'(' expression ')'
		    { $$ = $2; }
	    |	name
		arguments
		    { $$ = new Expression($1, $2); }
	    |	value
		    { $$ = new Expression(*$1); delete $1; }
	    ;

value	    :	INTEGER
		    { $$ = new Value(atoi(lex_token())); }
	    |	REAL
		    { $$ = new Value(atof(lex_token())); }
	    ;

name	    :	NAME
		    { PDEBUG(PD_NAME, cerr << "Calling Name::Name(token @" << ((void *)lex_token()) << " = " << lex_token() << ')' << endl);
		      $$ = Name(lex_token());
		    }
%%

void yyerror(char *msg) {
    cout << flush;
    cerr << msg << " at line " << lexline << ", column "
	 << lexcol << "\n" << flush;
}

ostream &operator<<(ostream &o, Predecessor &c) {
    if (c.left != NULL)
	o << *c.left << " < ";
    // Should always be non-NULL
    if (c.center != NULL)
	o << *c.center;
    if (c.right != NULL)
	o << " > " << *c.right;

    return o;
}

#ifdef sun
// Probably should be #ifndef _USES_BISON_ or some such
#ifndef __GNUG__
// /usr/lib/yaccpar is old-C and calls to free() should cast to (int *).
// Since there's no way to do that, we overload free() instead.
// This probably applies to other System V yaccs, too.
void free(int *ip) {
    free((char *)ip);
}

void free(YYSTYPE *yp) {
    free((char *)yp);
}
#endif /*!__GNUG__*/
#endif /*sun*/
