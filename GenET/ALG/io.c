/* io.c contain robust io routines */ 

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "io.h"
#define SUCCESS 0
#define FAIL_EOF 1
#define FAIL_RANGE 2
#define FAIL_OTHER 3
#define FAIL_SHORT 4
#define BELL 7
#ifndef BUFSIZ
#define BUFSIZ 128
#endif

static char buf1[BUFSIZ];            /* assumed input not longer than BUFSIZ */
static char buf2[BUFSIZ];

/* read from stdin until in the allowed range                                */
/* no additional non-WS allowed before or after; non-interactive             */
/*   (\n needed and is removed); return status code:                         */
/*   0 = successfully initialized result                                     */
/*   1 = failed on eof, 2 = failed on low/high range, 3 = other fail         */
static int getInt(const char *prompt,int low,int high,int *result)
{ int what;
  register int status=0;
  int c;
  if (low>high) 
    return(FAIL_RANGE);
  do
  { if (status==1)
      fprintf(stderr,"\tOut of range entry %c%c%c\n",BELL,BELL,BELL);
    printf("%s [%d..%d]: ",prompt,low,high);
    status=scanf("%d",&what);
    if (status==EOF)
      return(FAIL_EOF);
    while ((c=getchar())!='\n')                        /* check rest of line */
      status=status && isspace(c);          /* force read to be unsuccessful */
    if (status!=1)   /* not-eof, but reading failed directly or in isspace() */
    { fprintf(stderr,"\tInvalid entry on input line %c%c%c\n",BELL,BELL,BELL);
      what=low-1;                                      /* force another read */
    }
  } while((what<low) || (what>high));
  *result=what;
  return(SUCCESS);
}

/* read from open file in the range only                                     */
/* no additional non-WS allowed before; non-interactive                      */
/* return status code:                                                       */
/*   0 = successfully intialized result                                      */
/*   1 = failed on eof, 2 = failed on range, 3 = other fail                  */
static int fgetInt(FILE *fp,int low,int high,int *result)
{ int what;
  register int status=0;
  if (low>high) 
    return(FAIL_RANGE);
  status=fscanf(fp,"%d",&what);
  if (status==EOF)
    return(FAIL_EOF);
  if (status!=1)                              /* not-eof, but reading failed */
  { fprintf(stderr,"\tInvalid entry when reading from file %c%c%c\n",
            BELL,BELL,BELL);
    return(FAIL_OTHER);
  }
  /* status==1; succesful read */
  if ((what<low) || (what>high))
  { fprintf(stderr,"\tOut of range when reading from file %c%c%c\n",BELL,BELL,
            BELL);
    return(FAIL_RANGE);
  }
  *result=what;
  return(SUCCESS);
}

/* getIntLtd() behaves differently on stdin and file                         */
/*   read from stdin until in the allowed range                              */
/*     no additional non-WS allowed before or after; non-interactive         */
/*     (\n needed and is removed); return status code:                       */ 
/*   read from non-stdin open file in the range only (fail otherwise)        */
/*     no additional non-WS allowed before; non-interactive                  */
/*   returned status codes                                                   */
/*   0 = successfully initialized result                                     */
/*   1 = failed on eof                                                       */
/*   2 = failed on low/high range                                            */
/*   3 = other fail                                                          */
int getIntLtd(FILE *fp, const char *prompt, int low, int high, int *result)
{ if (fp==stdin)
    return(getInt(prompt,low,high,result));
  else
    return(fgetInt(fp,low,high,result));
}

/* read from stdin until in the allowed range                                */
/* no additional non-WS allowed before or after; non-interactive             */
/*   (\n needed and is removed); return status code:                         */
/*   0 = successfully initialized result                                     */
/*   1 = failed on eof, 2 = failed on low/high range, 3 = other fail         */
static int getDouble(const char *prompt,double low,double high,double *result)
{ double what;
  register int status=0;
  int c;
  if (low>high) 
    return(FAIL_RANGE);
  do
  { if (status==1)
      fprintf(stderr,"\tOut of range entry %c%c%c\n",BELL,BELL,BELL);
    printf("%s [%g..%g]: ",prompt,low,high);
    status=scanf("%lf",&what);
    if (status==EOF)
      return(FAIL_EOF);
    while ((c=getchar())!='\n')                        /* check rest of line */
      status=status && isspace(c);          /* force read to be unsuccessful */
    if (status!=1)   /* not-eof, but reading failed directly or in isspace() */
    { fprintf(stderr,"\tInvalid entry on input line %c%c%c\n",BELL,BELL,BELL);
      what=low-1;                                      /* force another read */
    }
  } while((what<low) || (what>high));
  *result=what;
  return(SUCCESS);
}

/* read from open file in the range only                                     */
/* no additional non-WS allowed before; non-interactive                      */
/* return status code:                                                       */
/*   0 = successfully intialized result                                      */
/*   1 = failed on eof, 2 = failed on range, 3 = other fail                  */
static int fgetDouble(FILE *fp,double low,double high,double *result)
{ double what;
  register int status=0;
  if (low>high) 
    return(FAIL_RANGE);
  status=fscanf(fp,"%lf",&what);
  if (status==EOF)
    return(FAIL_EOF);
  if (status!=1)                              /* not-eof, but reading failed */
  { fprintf(stderr,"\tInvalid entry when reading from file %c%c%c\n",
            BELL,BELL,BELL);
    return(FAIL_OTHER);
  }
                                                /* status==1; succesful read */
  if ((what<low) || (what>high))
  { fprintf(stderr,"\tOut of range when reading from file %c%c%c\n",BELL,BELL,
            BELL);
    return(FAIL_RANGE);
  }
  *result=what;
  return(SUCCESS);
}

/* getDoubleLtd() behaves differently on stdin and file                      */
/*   read from stdin until in the allowed range                              */
/*     no additional non-WS allowed before or after; non-interactive         */
/*     (\n needed and is removed); return status code:                       */
/*   read from non-stdin open file in the range only (fail otherwise)        */
/*     no additional non-WS allowed before; non-interactive                  */
/*   returned status codes                                                   */
/*   0 = successfully initialized result                                     */
/*   1 = failed on eof                                                       */
/*   2 = failed on low/high range                                            */
/*   3 = other fail                                                          */
int getDoubleLtd(FILE *fp, const char *prompt, double low, double high, 
                 double *result)
{ if (fp==stdin)
    return(getDouble(prompt,low,high,result));
  else
    return(fgetDouble(fp,low,high,result));
}

/* read from stdin until in the allowed range                                */
/* no additional non-WS allowed before or after; non-interactive             */
/*   (\n needed and is removed); return status code:                         */
/*   0 = successfully initialized result                                     */
/*   1 = failed on eof, 2 = failed on low/high range, 3 = other fail         */
static int getLUns(const char *prompt,long unsigned low,long unsigned high,
                   long unsigned *result)
{ long unsigned what;
  register int status=0;
  int c;
  if (low>high) 
    return(FAIL_RANGE);
  do
  { if (status==1)
      fprintf(stderr,"\tOut of range entry %c%c%c\n",BELL,BELL,BELL);
    printf("%s [%lu..%lu]: ",prompt,low,high);
    status=scanf("%lu",&what);
    if (status==EOF)
      return(FAIL_EOF);
    while ((c=getchar())!='\n')                        /* check rest of line */
      status=status && isspace(c);          /* force read to be unsuccessful */
    if (status!=1)   /* not-eof, but reading failed directly or in isspace() */
    { fprintf(stderr,"\tInvalid entry on input line %c%c%c\n",BELL,BELL,BELL);
      what=low-1;                                      /* force another read */
    }
  } while((what<low) || (what>high));
  *result=what;
  return(SUCCESS);
}

/* read from open file in the range only                                     */
/* no additional non-WS allowed before; non-interactive                      */
/* return status code:                                                       */
/*   0 = successfully intialized result                                      */
/*   1 = failed on eof, 2 = failed on range, 3 = other fail                  */
static int fgetLUns(FILE *fp,long unsigned low,long unsigned high,
                   long unsigned *result)
{ long unsigned what;
  register int status=0;
  if (low>high) 
    return(FAIL_RANGE);
  status=fscanf(fp,"%lu",&what);
  if (status==EOF)
    return(FAIL_EOF);
  if (status!=1)                              /* not-eof, but reading failed */
  { fprintf(stderr,"\tInvalid entry when reading from file %c%c%c\n",
            BELL,BELL,BELL);
    return(FAIL_OTHER);
  }
                                                /* status==1; succesful read */
  if ((what<low) || (what>high))
  { fprintf(stderr,"\tOut of range when reading from file %c%c%c\n",BELL,BELL,
            BELL);
    return(FAIL_RANGE);
  }
  *result=what;
  return(SUCCESS);
}

/* getLUnsLtd() behaves differently on stdin and file */
/*   read from stdin until in the allowed range */
/*     no additional non-WS allowed before or after; non-interactive */
/*     (\n needed and is removed); return status code: */
/*   read from non-stdin open file in the range only (fail otherwise) */
/*     no additional non-WS allowed before; non-interactive */
/*   returned status codes */
/*   0 = successfully initialized result */
/*   1 = failed on eof */
/*   2 = failed on low/high range */
/*   3 = other fail */
int getLUnsLtd(FILE *fp, const char *prompt, long unsigned low, 
               long unsigned high, long unsigned *result)
{ if (fp==stdin)
    return(getLUns(prompt,low,high,result));
  else
    return(fgetLUns(fp,low,high,result));
}

/* read from stdin until in the allowed range */
/* no additional non-WS allowed before or after; non-interactive */
/*   (\n needed and is removed); return status code: */
/*   0 = successfully initialized result */
/*   1 = failed on eof, 2 = failed on low/high range, 3 = other fail */
static int getUns(const char *prompt,unsigned low,unsigned high,
                  unsigned *result)
{ unsigned what;
  register int status=0;
  int c;
  if (low>high) 
    return(FAIL_RANGE);
  do
  { if (status==1)
      fprintf(stderr,"\tOut of range entry %c%c%c\n",BELL,BELL,BELL);
    printf("%s [%u..%u]: ",prompt,low,high);
    status=scanf("%u",&what);
    if (status==EOF)
      return(FAIL_EOF);
    while ((c=getchar())!='\n')                        /* check rest of line */
      status=status && isspace(c);          /* force read to be unsuccessful */
    if (status!=1)   /* not-eof, but reading failed directly or in isspace() */
    { fprintf(stderr,"\tInvalid entry on input line %c%c%c\n",BELL,BELL,BELL);
      what=low-1;                                      /* force another read */
    }
  } while((what<low) || (what>high));
  *result=what;
  return(SUCCESS);
}

/* read from open file in the range only */
/* no additional non-WS allowed before; non-interactive */
/* return status code:  */
/*   0 = successfully intialized result */
/*   1 = failed on eof, 2 = failed on range, 3 = other fail */
static int fgetUns(FILE *fp,unsigned low,unsigned high,
                   unsigned *result)
{ unsigned what;
  register int status=0;
  if (low>high) 
    return(FAIL_RANGE);
  status=fscanf(fp,"%u",&what);
  if (status==EOF)
    return(FAIL_EOF);
  if (status!=1)                              /* not-eof, but reading failed */
  { fprintf(stderr,"\tInvalid entry when reading from file %c%c%c\n",
            BELL,BELL,BELL);
    return(FAIL_OTHER);
  }
                                                /* status==1; succesful read */
  if ((what<low) || (what>high))
  { fprintf(stderr,"\tOut of range when reading from file %c%c%c\n",BELL,BELL,
            BELL);
    return(FAIL_RANGE);
  }
  *result=what;
  return(SUCCESS);
}

/* getUnsLtd() behaves differently on stdin and file */
/*   read from stdin until in the allowed range */
/*     no additional non-WS allowed before or after; non-interactive */
/*     (\n needed and is removed); return status code: */
/*   read from non-stdin open file in the range only (fail otherwise) */
/*     no additional non-WS allowed before; non-interactive */
/*   returned status codes */
/*   0 = successfully initialized result */
/*   1 = failed on eof */
/*   2 = failed on low/high range */
/*   3 = other fail */
int getUnsLtd(FILE *fp, const char *prompt, unsigned low, 
              unsigned high, unsigned *result)
{ if (fp==stdin)
    return(getUns(prompt,low,high,result));
  else
    return(fgetUns(fp,low,high,result));
}

/* read a string from stdin and put it into destination */
/* read only [min..max] characters (destination must be at least max+1 size */
/*   and min must be >=1 */
/* remove all remaining characters until and including \n */
/* return read status: */
/*   0 = dest initialized with min..max characters, possibly characters lost */
/*   1 = dest not initialized, eof */
/*   2 = dest not initialized, improper range */
static int getStr(const char *prompt, char *dest, int min, int max)
{ int len=0,status;
  if (min>max || min<1 || max >BUFSIZ-1)
    return(FAIL_RANGE);
  printf("%s [#characters %d..%d]: ",prompt,min,max);
  if (fgets(buf1,BUFSIZ,stdin)==NULL)
    return(FAIL_EOF);
  status=sscanf(buf1,"%s",buf2);   /* this read will also remove \n if there */
  len=strlen(buf2);
  while (len<min || status!=1)            /* must be at least min characters */
  { if (fgets(buf1,BUFSIZ,stdin)==NULL)
      return(FAIL_EOF);
    status=sscanf(buf1,"%s",buf2); /* this read will also remove \n if there */
    len=strlen(buf2);
  }
  strncpy(dest,buf2,(size_t)max);
  dest[max]='\0';                       /* to ensure dest is null-terminated */
  return(SUCCESS);
}

/* read a string from open file and put it into destination */
/* read only [min..max] characters (destination must be at least max+1 size1 */
/*   and min must be >=1 */
/* if string is longer than max, remove all remaining characters until WS */
/* return read status: */
/*   0 = dest initialized with min..max characters, possibly characters lost */
/*   1 = dest not initialized, eof */
/*   2 = dest not initialized, improper range */
/*   3 = dest not initialized, other reading errors */
/*   4 = dest not initialized, too short */
static int fgetStr(FILE *fp, char *dest, int min, int max)
{ int len=0, status;
  if (min>max || min<1 || max >BUFSIZ-1)
    return(FAIL_RANGE);
  status=fscanf(fp,"%s",buf1);
  if (status==EOF)
    return(FAIL_EOF);
  else if (status==0)
         return(FAIL_OTHER);
  len=strlen(buf1);
  if (len<min)
  { fprintf(stderr,"\tToo short string while reading from file%c%c%c\n",BELL,
            BELL,BELL);
    return(FAIL_SHORT);
  }
  strncpy(dest,buf1,(size_t)max);
  dest[max]='\0';                       /* to ensure dest is null-terminated */
  return(SUCCESS);
}

/* getStrLtd reads string into dest, behaves differently on stdin and */
/*   other file; dest must be of at least max+1 size, min >=1; */
/*   strings with more than max chars are truncated and removed from input */
/*   read a string from stdin */
/*     read only [min..max] characters (read until >=min) */
/*     consider only fisrt string per line */
/*     remove all remaining characters until and including \n */
/*     return read status: */
/*       0 = dest initialized with min..max characters, possibly chars lost */
/*       1 = dest not initialized, eof */
/*       2 = dest not initialized, improper range */
/*   read a string from non-stdin open file */
/*     read only [min..max] characters (fail if <min) */
/*     if string is longer than max, remove all remaining characters until WS */
/*     return read status: */
/*       0 = dest initialized with min..max characters, possibly chars lost */
/*       1 = dest not initialized, eof */
/*       2 = dest not initialized, improper range */
/*       3 = dest not initialized, other reading errors */
/*       4 = dest not initialized, too short */
int getStrLtd(FILE *fp, const char *prompt, char *dest, int min, int max)
{ if (fp==stdin)
    return(getStr(prompt,dest,min,max));
  else
    return(fgetStr(fp,dest,min,max));
}



/* read line from open file fp and put it into destination */
/* read only [1..max] characters (destination must be at least max+1 size */
/* remove all remaining characters until and including \n if input too long */
/* \n is not included in dest */
/* return read status: */
/*   0 = dest initialized with min..max characters, possibly characters lost */
/*   1 = dest not initialized, eof */
/*   2 = dest not initialized, improper range */
int getLineLtd(FILE *fp, const char *prompt, char *dest, int max)
{ int len=0, len1;
  if (max >BUFSIZ-1)
    return(FAIL_RANGE);
  if (fp==stdin)
    printf("%s [#characters 1..%d]: ",prompt,max);
  if (fgets(buf1,BUFSIZ,fp)==NULL)
    return(FAIL_EOF);
  len=strlen(buf1);
  if (buf1[len-1]=='\n')
    buf1[--len]='\0';                                 /* remove \n character */
  else do                                      /* consume rest of input line */
       { if (fgets(buf2,BUFSIZ,fp)==NULL)
           break;                                       /* eof after reading */
         len1=strlen(buf2);
         if (buf2[len1-1]=='\n')                            /* finally found */
           break;
       } while (1);
  while (len<1)                             /* must be at least 1 characters */
  { if (fgets(buf1,BUFSIZ,fp)==NULL)
      return(FAIL_EOF);
    len=strlen(buf1);
    if (buf1[len-1]=='\n')
      buf1[--len]='\0';                               /* remove \n character */
    else do                                    /* consume rest of input line */
         { if (fgets(buf2,BUFSIZ,fp)==NULL)
             break;                                     /* eof after reading */
           len1=strlen(buf2);
           if (buf2[len1-1]=='\n')                          /* finally found */
             break;
         } while (1); 
  }
  strncpy(dest,buf1,(size_t)max);
  dest[max]='\0';                       /* to ensure dest is null-terminated */
  return(SUCCESS);
}
