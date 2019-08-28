#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* getperf.c */
VOID main PRO((int argc, char *argv[]));
VOID StoreGue PRO((void));
VOID PrntGue PRO((void));
VOID PrntTGs PRO((void));
int strcmpn PRO((char Str1[], char Str2[], int N));
int IntInLst PRO((int Test, int List[], int Max));
VOID PrntPerf PRO((int Which));
VOID PrntTPerf PRO((int Which));
VOID Help PRO((void));

#undef PRO
