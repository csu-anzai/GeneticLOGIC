/*
 *   ReadIn() reads in a programmable life generation.
 */
#include "stdio.h"
extern int wmodulo, modulo, vsize ;
#define MAXSIZE 8000L
#define MAXSTACK 20
#define MINX 1
#define MAXX (modulo - 2)
#define MINY 1
#define MAXY (vsize - 2)
static char *arr[26] ;
static short dirs[] = { -1, 0, 0, -1, 1, 0, 0, 1 } ;
static int sp ;
static int xstack[MAXSTACK], ystack[MAXSTACK] ;
static char dirstack[MAXSTACK], onstack[MAXSTACK], ddirstack[MAXSTACK] ;
static int x, y, dir, on, ddir ;
static short *globala ;
readin(a)
short *a ;
{
   int c ;
   char *p ;
   int i ;
   char *prog ;

   globala = a ;
   for (i=0; i<26; i++)
      arr[i] = NULL ;
   prog = (char *)AllocMem(MAXSIZE, 0L) ;
   if (prog == NULL) return ;
/*
 *   First, we read the program into memory, stripping comments and
 *   other nonsense characters.  We enclose the whole thing in an
 *   extra pair of parenthesis.
 */
   p = prog ;
   *p++ = '(' ;
   while ((c=getchar())!=EOF) {
      if (c=='<') {
         while ((c=getchar())!=EOF && c!='>') ;
         if (c==EOF || (c=getchar())==EOF) break ;
      }
      if (c >= 'A' && c <= 'Z')
         c += 'a' - 'A' ;
      if ((c >= 'a' && c <= 'z') || c=='+' || c=='-' || c=='(' || c==')' ||
          c=='[' || c==']' || c=='=' || (c >= '0' && c <= '9') || c=='.' ||
          c=='*' || c==',') {
         *p++ = c ;
         if (prog + MAXSIZE - 3 <= p) {
            printf("Out of room in program space.\n") ;
            goto out ;
         }
      }
   }
   *p++ = ')' ;
/*
 *   Now we process the thing.
 */
   x = modulo / 2 ;
   y = vsize / 2 ;
   dir = 2 ;
   ddir = 2 ;
   on = 0 ;
   sp = MAXSTACK - 1 ;
   process(prog) ;
out:   
   FreeMem(prog, MAXSIZE) ;
}
/*
 *   This routine actually does the processing.
 */
process(where)
char *where ;
{
   long param ;

   if (*where != '(')
      printf("Expected open paren!\n") ;
   where++ ;
   while (*where != ')' && *where != 0) {
      if (*where >= '0' && *where <= '9') {
         param = 0 ;
         while (*where >= '0' && *where <= '9')
            param = 10 * param + *where++ - '0' ;
      } else
         param = 1 ;
      if (*where == '(') {
         while (param > 0) {
            process(where) ;
            param-- ;
         }
         skip(&where) ;
      } else if (*where=='=') {
         where++ ;
         if (*where < 'a' || *where > 'z') {
            printf("Can't define that char!\n") ;
            *where = 'a' ;
         }
         if (where[1] != '(')
            printf("Expected definition to start with (!\n") ;
         arr[*where-'a'] = where + 1 ;
         where++ ;
         skip(&where) ;
      } else
         doone(*where++, param) ;
   }
}
/*
 *   This routine handles a single character.
 */
doone(c, param)
char c ;
long param ;
{
   switch(c) {
case '[' :
      if (sp < 0) {
         printf("Stack overflow!\n") ;
         sp = 0 ;
      }
      xstack[sp] = x ;
      ystack[sp] = y ;
      dirstack[sp] = dir ;
      ddirstack[sp] = ddir ;
      onstack[sp] = on ;
      sp-- ;
      goto setpoint ;
case ']' :
      sp++ ;
      if (sp >= MAXSTACK) {
         printf("Stack underflow!\n") ;
         sp = MAXSTACK-1 ;
      }
      x = xstack[sp] ;
      y = ystack[sp] ;
      dir = dirstack[sp] ;
      ddir = ddirstack[sp] ;
      on = onstack[sp] ;
      goto setpoint ;
case 'x' :
      x = param ;
      if (x < MINX)
         x = MINX ;
      if (x > MAXX)
         x = MAXX ;
      goto setpoint ;
case 'y' :
      y = param ;
      if (y < MINY)
         x = MINY ;
      if (y > MAXY)
         y = MAXY ;
      goto setpoint ;
case '.' :
      while (param > 0) {
         x += dirs[dir] ;
         y += dirs[dir+1] ;
         if (x < MINX)
            x = MAXX ;
         if (x > MAXX)
            x = MINX ;
         if (y < MINY)
            y = MAXY ;
         if (y > MAXY)
            y = MINY ;
         param-- ;
      }
      break ;
case 'f' :
      while (param > 0) {
         x += dirs[dir] ;
         y += dirs[dir+1] ;
         if (x < MINX)
            x = MAXX ;
         if (x > MAXX)
            x = MINX ;
         if (y < MINY)
            y = MAXY ;
         if (y > MAXY)
            y = MINY ;
         if (on)
            set(x, y) ;
         param-- ;
      }
      break ;
case '*' :
      while (param > 0) {
         x += dirs[dir] ;
         y += dirs[dir+1] ;
         if (x < MINX)
            x = MAXX ;
         if (x > MAXX)
            x = MINX ;
         if (y < MINY)
            y = MAXY ;
         if (y > MAXY)
            y = MINY ;
         set(x, y) ;
         param-- ;
      }
      break ;
case ',' :
      while (param > 0) {
         x += dirs[(dir + ddir) & 6] ;
         y += dirs[((dir + ddir) & 6) + 1] ;
         if (x < MINX)
            x = MAXX ;
         if (x > MAXX)
            x = MINX ;
         if (y < MINY)
            y = MAXY ;
         if (y > MAXY)
            y = MINY ;
         param-- ;
      }
      break ;
case 'l' :
      dir = (dir - ddir) & 6 ;
      break ;
case 'r' :
      dir = (dir + ddir) & 6 ;
      break ;
case 'b' :
      dir = (dir + 4) & 6 ;
      break ;
case 'u' :
      on = 0 ;
      break ;
case 'd' :
      on = 1 ;
      goto setpoint ;
case '+' :
      ddir = 2 ;
      break ;
case '-' :
      ddir = - ddir ;
      break ;
default:
      if (c < 'a' || c > 'z' || 
            arr[c - 'a']==NULL) {
         printf("Bogus! %d\n", c) ;
      } else {
         while (param > 0) {
            process(arr[c-'a']) ;
            param-- ;
         }
      }
      break ;
setpoint:
      if (on)
         set(x, y) ;
      break ;
   }
}
/*
 *   This routine skips to the end of a routine by counting
 *   parenthesis, if any.
 */
skip(where)
char **where ;
{
   char *p = *where ;
   int parencount = 1 ;

   if (*p++ == '(') {
      while (parencount > 0) {
         if (*p == '(')
            parencount++ ;
         else if (*p == ')')
            parencount-- ;
         else if (*p == 0) {
            *where = p ;
            break ;
         }
         p++ ;
      }
   }
   *where = p ;
}
/*
 *   This routine turns on a pixel.
 */
static bits[16] = { 0x8000, 0x4000, 0x2000, 0x1000, 0x800, 0x400, 0x200,
   0x100, 0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 0x2, 0x1 } ;
set(x, y)
int x, y ;
{
   globala[y * wmodulo + (x >> 4)] |= bits[x & 15] ;
}
