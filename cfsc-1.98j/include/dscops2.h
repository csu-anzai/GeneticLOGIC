#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* dscops2.c */
VOID DscCSS PRO((void));
int PckCC PRO((void));
VOID DscACPC PRO((void));
VOID DscTLB PRO((void));
VOID DscCEff PRO((void));
int SpclzNCf PRO((struct NCfNode *NewCf));

#undef PRO
