#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* getstr1.c */
VOID main PRO((int argc, char *argv[]));
VOID Init PRO((void));
VOID PrntTStr PRO((int PlainFlg));
VOID GetIDPar PRO((char *idptr));
VOID AskIDs PRO((void));
VOID StoreStr PRO((void));
VOID PrntStrs PRO((int PlainFlg));
VOID Help PRO((void));
int strcmpn PRO((char Str1[], char Str2[], int N));
int IntInLst PRO((int Test, int List[], int Max));

#undef PRO
