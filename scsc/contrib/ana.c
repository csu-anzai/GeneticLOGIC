/*From Informatik.Uni-Dortmund.DE!Germany.EU.net!mcsun!uunet!news.moneng.mei.com!howland.reston.ans.net!agate!ucbvax!hplabs!unix.sri.com!csl.sri.com!redwood.csl.sri.com!not-for-mail Sat Nov  6 18:19:10 1993
Article: 1565 of comp.ai.genetic
Path: Informatik.Uni-Dortmund.DE!Germany.EU.net!mcsun!uunet!news.moneng.mei.com!howland.reston.ans.net!agate!ucbvax!hplabs!unix.sri.com!csl.sri.com!redwood.csl.sri.com!not-for-mail
From: boucher@csl.sri.com (Peter K. Boucher)
Newsgroups: comp.ai.genetic,sci.crypt
Subject: Re: Strong random number generators?
Message-ID: <2bfl7tINN3ne@redwood.csl.sri.com>
Date: 6 Nov 93 07:54:37 GMT
References: <1993Nov5.183248.29604@cs.tcd.ie>
Organization: Computer Science Lab, SRI International
Lines: 285
NNTP-Posting-Host: redwood.csl.sri.com
Xref: Informatik.Uni-Dortmund.DE comp.ai.genetic:1565 sci.crypt:12301

In article <1993Nov5.183248.29604@cs.tcd.ie> rwallace@cs.tcd.ie (Russell Wallace) writes:
>Could someone explain just what test a 'cryptographically strong' random
>number generator (e.g. Blum-Blum-Shub) passes, that an LCG fails?  To be
>sure, in principle the LCG is not random, but then neither is any
>algorithm for generating random numbers; and LCG passes every test I've
>ever heard about.

It fails this.

*/
/* 
 * anal.c --
 *
 * Copyright 1993 Peter K. Boucher
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.
 *
 * Usage:  anal [input_file [output_file]]
 *
 * This program counts the occurances of each character in a file
 * and notifies the user when a character appears a certain amount
 * more (or less) than the average.  It returns with an error if
 * the disparity is too great.  It is intended to be used to test
 * the output of crypto-systems or pseudo-random number generators,
 * to see if the output is evenly distributed across the character-
 * space.
 *
 * Because the chance of getting byte B after byte A should be 1:256
 * (for all A's and B's), the program also checks that the successors
 * to each byte are evenly distributed.  This means that for each byte
 * value (0 - 255) that occurs in the text, a count is kept of the
 * byte value that followed in the text, and the frequency distribution
 * off these succeeding bytes is also checked.
 *
 * For example, whenever an 'A' appears in the text, the next character
 * is noted.  If the characters that followed 'A's in the text are not
 * evenly distributed, the test failes.
 *
 */

#include <stdio.h>
#include <ctype.h>

#define BYTESIZE 256
#define BUFSIZE 8192
#ifdef DEBUG
#define PASSED_FFNAME "/tmp/analocc.pss"
#define PASSED_SFNAME "/tmp/analsuc.pss"
#define FAILED_FNAME "/tmp/anal.fld"
#endif

#define SHOW_RESULT(F) \
     fprintf(F, "%-15s%13s%-15s%13s%-14s\n", \
      avgstr, lostr, minstr, histr, maxstr);

unsigned long cnt[BYTESIZE] = {0}; /* should be all zeros. */
unsigned long succeed[BYTESIZE][BYTESIZE] = {{0}}; /* should be all zeros. */

static unsigned char buf[BUFSIZE];
static char avgstr[64];
static char lostr[64];
static char histr[64];
static char minstr[64];
static char maxstr[64];
static FILE *ifp, *ofp;


FILE *my_fopen(file, type)
char *file, *type;
{
  FILE *fp;

  if ((fp = fopen(file, type)) == NULL) {
      (void)fprintf(stderr, "Can't open '%s' for '%s'\n", file, type);
      exit(1);
  }
  return(fp);
}

void get_thresholds(avg, lothresh, hithresh, min, max)
unsigned long avg, *lothresh, *hithresh, *min, *max;
{
  unsigned long tmp, sigbits;
  unsigned int exp=0;
  float threshtmp = avg, fraction;

  for (tmp=avg; (tmp); tmp /= 10L) {
    ++exp;
    sigbits = tmp;
  }
  switch (exp) {
    case 0:
    case 1: /* avg 0 to 9 */
      *lothresh = 1L;
      *hithresh = 4L + avg;
      *min = 0L;
      *max = ((avg+1L) * 2L) + 20L; /* 22 to 40 */
      return;
    case 2: /* avg 10 to 90 */
      fraction = 5.5 / (sigbits + 4.5); /* 100% to 41% */
      threshtmp *= (sigbits + 7.0);
      threshtmp /= 20.0; /* 40% to 80% of avg */
      break;
    case 3: /* avg 100 to 900 */
      fraction = 1.2 / (sigbits + 3.0);  /* 30% to 10% */
      threshtmp *= (sigbits + 48.23077);
      threshtmp /= 61.53846; /* 80% to 93% of avg */
      break;
    case 4: /* avg 1000 to 9000 */
      fraction = 0.34286 / (sigbits + 2.42857); /* 10% to 3% */
      threshtmp *= (sigbits + 147.8);
      threshtmp /= 160.0; /* 93% to 98% of avg */
      break;
    case 5: /* avg 10000 to 90000 */
      fraction = 0.24 / (sigbits + 7.0); /* 3% to 1.5% */
      threshtmp *= (sigbits + 652.33333);
      threshtmp /= 666.66667; /* 98% to 99.2% of avg */
      break;
    case 6: /* avg 100000 to 900000 */
      fraction = 0.04 / (sigbits + 1.66667); /* 1.5% to 0.375% */
      threshtmp *= (sigbits + 1587.8);
      threshtmp /= 1600.0; /* 99.3% to 99.8% of avg */
      break;
    default: /* avg 1000000 and up */
      fraction = 0.03 / (sigbits + 7.0); /* 0.375% to 0.1875% */
      threshtmp -= 10000.0;
  }
  threshtmp += 0.5; /* round up */
  *lothresh = threshtmp;
  *hithresh = avg + (avg - *lothresh);
  *min = *lothresh - (unsigned long)(threshtmp * fraction);
  *max = *hithresh + (*hithresh * fraction);
}

void anal_ize_text()
{
   int   	  i, l, ch, next, err = 0;
   int   	  occ_excell=0, suc_excell=0;
   long  	  lo, hi, clo, chi;
   float          avg = 0.0;
   unsigned long  size;
   unsigned long  lothresh, hithresh, min, max;
   unsigned long  clothresh, chithresh, cmin, cmax;

   ch = getc(ifp); /* prime the pump */
   if (ch == EOF) {
       fprintf(stderr, "input file zero-length\n");
       exit(-1);
   } else {
       cnt[ch] = size = 1L;
   }
   while ((l = fread(buf, 1, BUFSIZE, ifp)) > 0) {
       for (i=0; i<l; i++) {
	   size++;
	   next = buf[i];
	   cnt[next]++;
	   succeed[ch][next]++;
	   ch = next;
       }
   }
   fclose(ifp);

   avg = (size/256.0);
   hi = lo = cnt[0];

   for (ch = 1; ch < BYTESIZE; ch++) {
      if (cnt[ch] < lo) {
	  lo = cnt[ch];
      } else if (cnt[ch] > hi) {
	  hi = cnt[ch];
      }
   }

   get_thresholds(size>>8, &lothresh, &hithresh, &min, &max);
   sprintf(avgstr, "avg = %.2f, ", avg);
   sprintf(lostr, "lo = %ld ", lo);
   sprintf(minstr, "(min %ld), ", min);
   sprintf(histr, "hi = %ld ", hi);
   sprintf(maxstr, "(max %ld) ", max);
   if ((lo < lothresh) || (hi > hithresh)) {
       if (lo < lothresh) strcat(lostr, "* ");
       if (hi > hithresh) strcat(histr, "* ");
       if ((lo < min) || (hi > max)) {
#ifdef DEBUG
	   FILE *failed=my_fopen(FAILED_FNAME, "a");
#endif
	   if (lo < min) {
	     strcat(lostr, "* ");
	     err |= 1;
	   }
	   if (hi > max) {
	     strcat(histr, "* ");
	     err |= 2;
	   }
#ifdef DEBUG
	   SHOW_RESULT(failed);
	   fclose(failed);
#endif
       }
   } else {
#ifdef DEBUG
     FILE *excell=my_fopen(PASSED_FFNAME, "a");
     SHOW_RESULT(excell);
     fclose(excell);
#endif
     occ_excell = 1;
   }
   for (ch=0; ch<BYTESIZE; ch++) {
       chi = clo = succeed[ch][0];
       for (next = 1; next < BYTESIZE; next++) {
	  if (succeed[ch][next] < clo) {
	      clo = succeed[ch][next];
	  } else if (succeed[ch][next] > chi) {
	      chi = succeed[ch][next];
	  }
       }
       get_thresholds(cnt[ch]>>8, &clothresh, &chithresh, &cmin, &cmax);
       if ((clo < cmin) || (chi > cmax)) {
#ifdef DEBUG
	   FILE *failed=my_fopen(FAILED_FNAME, "a");
#endif
	   if (clo < cmin) {
	       err |= 4;
	   }
	   if (chi > cmax) {
	       err |= 8;
	   }
#ifdef DEBUG
	   fprintf(failed,
"Successor randomness failed for char %.2x, (lo=%ld, avg=%ld, hi=%ld)\n",
                                      ch,       clo, cnt[ch]>>8, chi);
	       fclose(failed);
#endif
       } else if ((clo >= clothresh) && (chi <= chithresh)) {
#ifdef DEBUG
	 FILE *excell=my_fopen(PASSED_SFNAME, "a");
	 fprintf(excell,
"Successor randomness excellent for char %.2x, (lo=%ld, avg=%ld, hi=%ld)\n",
                                         ch,       clo, cnt[ch]>>8, chi);
	 fclose(excell);
#endif
	 suc_excell = 1;
       }
   }
   SHOW_RESULT(ofp);
   if (err != 0) {
#ifdef DEBUG
       unlink(PASSED_FFNAME);
       unlink(PASSED_SFNAME);
#endif
       if (err & 1)
	   fprintf(stderr, "Some char occurs too infrequently\n");
       if (err & 2)
	   fprintf(stderr, "Some char occurs too frequently\n");
       if (err & 4)
	   fprintf(stderr,
		   "Some char is succeeeded by another too infrequently\n");
       if (err & 8)
	   fprintf(stderr,
		   "Some char is succeeeded by another too frequently\n");
       if (!(err & 15))
	   fprintf(stderr, "\n\nINTERNAL UNKNOWN ERROR!!! (%d)\n\n", err);
       exit(err);
   }
   if (occ_excell) fprintf(ofp,
"================ Frequency distribution excellent! ====================\n");
   if (suc_excell) fprintf(ofp,
"================= Successor randomness excellent! =====================\n");
}  

int
main (int argc, char* argv[])
{
   ifp = (argc > 1) ? my_fopen(argv[1],"r") : stdin;
   ofp = (argc > 2) ? my_fopen(argv[2],"w") : stdout;
   anal_ize_text();

   return(0);
}
/*
-- 
Peter K. Boucher
--
DISCLAIMER:  The above does not necessarily represent the opinions of my employer.
*/
