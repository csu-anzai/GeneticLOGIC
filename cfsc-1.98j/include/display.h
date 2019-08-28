#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* display.c */
VOID Display PRO((char CmdPars[]));
int DsplVar PRO((char *VarName, char *FDName));
VOID AuDisplay PRO((void));
int DsplVars PRO((char *FDName, int Format));
VOID PrnVars PRO((FILE *FilePtr, unsigned int Format));
int WrtVars PRO((FILE *FilePtr, unsigned int Format));
int DsplSets PRO((char *FDName, int Format));
VOID Help PRO((char ParStr[]));

#undef PRO
