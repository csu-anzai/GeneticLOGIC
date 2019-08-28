#include <stdio.h>
#include <math.h>
#include <stdarg.h>


double:physical Pmaxx(va_alist)
va_dcl
{
  va_list pvar;
  int i;
  int count;
  double:physical maximum, temp;
  
  with(physical) {
    va_start(pvar);
    count = va_arg(pvar, int);
    maximum = *(double:physical *)va_arg(pvar, double:physical *);
    for (i = 1; i < count; i++)
      {
	temp = *(double:physical *)va_arg(pvar, double:physical *);
	where (temp > maximum) maximum = temp;
      }
    va_end(ap);
    return(maximum);
  }
}

double:physical Pavep(va_alist)
va_dcl
{
  va_list pvar;
  register int i;
  int count;
  double:physical result;
  
  with(physical) {
    va_start(pvar);
    count = va_arg(pvar, int);
    result = 0.0;
    for (i = 1; i <= count; i++)
      {
	result = result + *(double:physical *)va_arg(pvar, double:physical *);
      }
    va_end(ap);
    result = pow(result/(double:physical)count, 2.0);
    return(result);
  }
}
