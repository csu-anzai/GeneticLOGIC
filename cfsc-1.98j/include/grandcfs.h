#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* grandcfs.c */
VOID GnRndCfs PRO((char *ParString));
int GenRndCf PRO((double MeanStr, double MeanBR, int PassFlag));
VOID GnRndCA PRO((char String[], unsigned int *Type, int What));
struct CfOpNode *GtCOpRnd PRO((void));

#undef PRO
