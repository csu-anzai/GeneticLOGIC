#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* ccls1.c */
int main PRO((int argc, char *argv[]));
VOID ReadCon PRO((void));
VOID Converge PRO((char *Cf));
VOID PrntCon PRO((void));
VOID PrntTSta PRO((void));
VOID PrntTSum PRO((void));
VOID Help PRO((void));
int strcmpn PRO((char Str1[], char Str2[], int N));
int IntInLst PRO((int Test, int List[], int Max));

#undef PRO
