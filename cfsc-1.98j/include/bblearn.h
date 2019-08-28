#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* bblearn.c */
VOID UpdCfStr PRO((void));
VOID ApplyBB PRO((void));
VOID ApplyBB1 PRO((void));
VOID GetDMCnt PRO((struct CfNode *CfPtr));
float PaySuppl PRO((struct CfNode *Supplier, double Payment));
VOID DemoBB PRO((void));

#undef PRO

extern char *GOutBuff;
