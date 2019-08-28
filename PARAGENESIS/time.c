#include "extern.h"

void Time(endflag,str)
int endflag;
char str[];
{
  int ticks;

  if (Timeflag) {
    if (endflag) {
      ticks = clock() - Time_array[Time_index];
      Time_index--;
      printf("%s : %f\n",str,(ticks/1000000.0));
   }
    else {
      Time_index++;
      Time_array[Time_index] = clock();
    }
  }
}
