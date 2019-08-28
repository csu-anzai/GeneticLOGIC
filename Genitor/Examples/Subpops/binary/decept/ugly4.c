/********************************************************/
/* eval   This is an "ugly" order-4 deceptive problem.  */
/*      THIS problem was created here at CSU and is     */
/*      described in the GENITOR II paper.              */
/*   Darrell Whitley  December 1989                     */
/********************************************************/

float
eval(buff,length) 
char buff [40];
int length;
{
double count;
int param[10];
int i;
double sum;
double result;


  for (i=0; i < 10; i++)
    {
     param[i] = 0;
     if (buff[0 + i] ==  1) param[i] = 8;
     if (buff[10 + i] == 1) param[i] = param[i] + 4;
     if (buff[20 + i] == 1) param[i] = param[i] + 2;
     if (buff[30 + i] == 1) param[i] = param[i] + 1;
    }

  sum = 0.0;

  for (i=0; i < 10; i++)
    {
     if (param[i] == 0) sum += 28;
     if (param[i] == 1) sum += 26;
     if (param[i] == 2) sum += 24;
     if (param[i] == 3) sum += 18;
     if (param[i] == 4) sum += 22;
     if (param[i] == 5) sum += 16;
     if (param[i] == 6) sum += 12;
     if (param[i] == 7) sum += 6;
     if (param[i] == 8) sum += 20;
     if (param[i] == 9) sum += 14;
     if (param[i] == 10) sum += 10;
     if (param[i] == 11) sum += 4;
     if (param[i] == 12) sum += 8;
     if (param[i] == 13) sum += 2;
     if (param[i] == 14) sum += 0;
     if (param[i] == 15) sum += 30;
    }

    result = 300 - sum;
    return(float)(result);    
}
