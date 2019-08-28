#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* concnt-1.c */
int main PRO((int argc, char *argv[]));
VOID ReadCon PRO((void));
VOID ReadConBoole PRO((void));
VOID Converge PRO((void));
VOID Getstrng PRO((char *cp, char tbuff[]));
VOID PrntCon PRO((void));
VOID PrntTSta PRO((void));
int strcmpn PRO((char Str1[], char Str2[], int N));
int IntInLst PRO((int Test, int List[], int Max));
int fInLst PRO((double Test, float List[], int Max));
VOID Help PRO((void));

#undef PRO
