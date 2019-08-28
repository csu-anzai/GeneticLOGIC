double:physical Peval(str, length, vect, genes)
char:physical str[];
int length;
double:physical vect[];
int genes;
{
  register int i;
  double:physical sum;

  with(physical) {
    sum = 0.0;
    for (i = 0; i < length; i++)
      sum += (str[i] == '0');
    
    return (sum);
  }
}
