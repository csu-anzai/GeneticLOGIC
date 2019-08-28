#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* getvc.c */
VOID main PRO((int argc, char *argv[]));
VOID StoreVC PRO((void));
VOID PrntVC PRO((int PlainFlg));
VOID PrntTVC PRO((int PlainFlg));
VOID PrntTR PRO((int PlainFlg));
VOID PrntTTR PRO((int PlainFlg));
int strcmpn PRO((char Str1[], char Str2[], int N));
int IntInLst PRO((int Test, int List[], int Max));
VOID Help PRO((void));

#undef PRO
