/*		CFSIO.H 
 
#define's for various constants used by CFSIO.DEF, CFSIO.EXT, and 
therefore, by CFSIO.C .
 
*/ 
 
#define		FNAMESZ		64				/* Buffer size for file names */ 
#define		FNAMEMX		(FNAMESZ-1)

#define		COMCHAR		';'				/* comment character -- lines that begin with this are ignored. */

#define		GOBUFFSZ	256				/* Size of GOutBuff buffer defined in CFSIO.DEF */
#define		GOBUFFMX	(GOBUFFSZ-1)
#define		GIBUFFSZ	256				/* Size of GInBuff buffer defined in CFSIO.DEF */
#define		GIBUFFMX	(GOBUFFSZ-1)

#if  MPWC
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif

#if  MACAZTEC
#define		SCREENNAME	".con"
#define		KEYBOARDNAME	".con"
#define		PRINTERNAME	".bout"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		 -1
#endif

#if  LATTICEC
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif

#if  CI86
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		 FCLOSERR		 -1
#endif

#if  APOLLOC
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif

#if  PERKINEL
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif

#if	SUN3
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		 WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif

#if  VAX
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE     "w"
#define		APPENDMODE    "a" 
#define		FCLOSERR      -1
#endif

#if  CBELLMTS
#define		SCREENNAME		"*msink*"
#define		KEYBOARDNAME	"*msource*"
#define		PRINTERNAME		"*print*"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif


#if MACLSC
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		 FCLOSERR		 -1
#endif

#if DECSTATION
#define		SCREENNAME		"con:"
#define		KEYBOARDNAME	"con:"
#define		PRINTERNAME		"prn:"
#define		READMODE		"r"
#define		 WRITEMODE		"w"
#define		APPENDMODE		"a" 
#define		FCLOSERR		-1
#endif

