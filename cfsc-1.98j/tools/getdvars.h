#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* getdvars.c */
VOID main PRO((int argc, char *argv[]));
VOID StoreVar PRO((void));
VOID PrntFull PRO((int Plain));
VOID PrntTFull PRO((int Plain));
VOID PrntVar PRO((int Type));
VOID Help PRO((void));
int strcmpn PRO((char Str1[], char Str2[], int N));

#undef PRO
