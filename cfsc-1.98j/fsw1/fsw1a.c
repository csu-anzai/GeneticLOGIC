
/*              FSW1A.C

   Second file with subroutines for FSW1 environment.
   See also FSW1.C.

 */

#include "config.h"

#include "utility.h"
#include "cfsio.ext"
#include "core.ext"
#include "cfops.ext"
#include "fsw1.def"

extern unsigned int DemoLev;

#if MPWC
#define __SEG__ CFSCWRITE
#endif

/**
 DisCfIE        Display classifiers, Interpreted with respect to the Environment.

 Cp             Pointer to CfNode.
 Buff           Char buffer for the output.
 Fmt            Format to use.

 This is called by WrtCfs() in the CFMSGIO.C file when the format
 for displaying classifiers is ENVFMT (21). The cf is displayed:

 id> stag: string [states]; stag: string [states] =actt=> stag: string [r's] {s} use

 where:
 id             cf id number
 stag   source-tag, from leftmost 2 bits: 00->D, 10->G, 01->X, 11->Y
 string string without source tag
 states list of any states matched
 r's            list of any r values supported

 actt   action type, eg, PASS, AND, etc. (If its PASS, don't write it)
 s              cf strength, bidratio, support
 use    in last step: b bid, w won, p posted (after effector confl. resol.)

 Note: If there are Variable Loci in the attribute strings for the states,
 then the corresponding loci in the classifiers are treated as #'s.
 Thus the user will have to examine the variable loci to determine
 exactly which subset of the variable loci combinations is matched by rule.
 */

VOID
DisCfIE (Cp, Buff, Fmt)
     struct CfNode *Cp;
     char *Buff;
     unsigned int Fmt;
{
  char cfbuff[STRNGSZ + 10], cond1[STRNGSZ + 1], tbuff[STRNGSZ + 10];
  int i, j, padstop;

  sprintf (Buff, "\n%5u> ", Cp->Cf_Id);
  padstop = DisCndSz + 5;

  BCndtoA (Cp->Cnd1Bits, Cp->Cnd1DCs, cond1);
  cond1[STRNGSZ] = '\0';
  DsCfSTag (cond1, Buff);
  if (Fmt == 22)
    {				/* display string form, too */
      for (i = 2, j = 0; i < STRNGSZ; ++i, ++j)
	{
	  if (i % 4 == 0)
	    tbuff[j++] = ' ';
	  tbuff[j] = cond1[i];
	}
      tbuff[j] = '\0';
      strcat (Buff, tbuff);
    }
  DsCfSta (cond1, Buff, Cp, 1);
  strcat (Buff, "; ");

  if (Fmt == 21)
    {
      for (i = 0; Buff[i] != '\0' && i < 26; ++i)	/* pad out */
	;
      if (i < 26)
	{
	  for (; i < 26; ++i)
	    Buff[i] = ' ';
	  Buff[i] = '\0';
	}
    }
  else
    {
      for (i = 0; Buff[i] != '\0' && i < padstop; ++i)	/* pad out */
	;
      if (i < padstop)
	{
	  for (; i < padstop; ++i)
	    Buff[i] = ' ';
	  Buff[i] = '\0';
	}
    }

  BCndtoA (Cp->Cnd2Bits, Cp->Cnd2DCs, cfbuff);
  cfbuff[STRNGSZ] = '\0';
  if (Cp->Cnd2Type != 0)
    strcat (Buff, "~");
  else
    strcat (Buff, " ");
  DsCfSTag (cfbuff, Buff);
  if (Fmt == 22)
    {
      for (i = 2, j = 0; i < STRNGSZ; ++i, ++j)
	{
	  if (i % 4 == 0)
	    tbuff[j++] = ' ';
	  tbuff[j] = cfbuff[i];
	}
      tbuff[j] = '\0';
      strcat (Buff, tbuff);
    }
  DsCfSta (cfbuff, Buff, Cp, 2);

  if (Fmt == 21)
    {
      for (i = 0; Buff[i] != '\0' && i < 47; ++i)	/* pad out */
	;
      if (i < 47)
	{
	  for (; i < 47; ++i)
	    Buff[i] = ' ';
	  Buff[i] = '\0';
	}
    }
  else
    {
      padstop = (2 * DisCndSz) + 5;
      for (i = 0; Buff[i] != '\0' && i < padstop; ++i);
      if (i < padstop)
	{
	  for (; i < padstop; ++i)
	    Buff[i] = ' ';
	  Buff[i] = '\0';
	}
    }

  DsCfAct (Cp, Buff);

  BActtoA (Cp->ActBits, Cp->ActDCs, cfbuff);
  cfbuff[STRNGSZ] = '\0';
  for (i = 0; i <= STRNGMX; ++i)
    if (cfbuff[i] == '#')	/* use condition1 to fill #'s in action string */
      cfbuff[i] = cond1[i];
  DsCfSTag (cfbuff, Buff);
  if (Fmt == 22)
    {
      for (i = 2, j = 0; i < STRNGSZ; ++i, ++j)
	{
	  if (i % 4 == 0)
	    tbuff[j++] = ' ';
	  tbuff[j] = cfbuff[i];
	}
      tbuff[j] = '\0';
      strcat (Buff, tbuff);
    }
  DsCfEffS (cfbuff, Buff, Fmt);

  if (Fmt == 21)
    {
      for (i = 0; Buff[i] != '\0' && i < 74; ++i)	/* pad out to 75 */
	;
      if (i < 74)
	{
	  for (; i < 74; ++i)
	    Buff[i] = ' ';
	  Buff[i] = '\0';
	}
    }
  else
    {
      padstop = (3 * DisCndSz) + 10;
      for (i = 0; Buff[i] != '\0' && i < padstop; ++i);
      if (i < padstop)
	{
	  for (; i < padstop; ++i)
	    Buff[i] = ' ';
	  Buff[i] = '\0';
	}
    }

  if (SuppBid)
    sprintf (cfbuff, " {%7.1f,%3.2f,%.2f}", Cp->Strength, Cp->BidRatio, Cp->Support);
  else
    sprintf (cfbuff, " {%7.1f,%3.2f}", Cp->Strength, Cp->BidRatio);
  strcat (Buff, cfbuff);

  if (Cp->NmPost > 0)
    strcat (Buff, " p");
  else if (Cp->NmProd > 0)
    strcat (Buff, " w");
  else if (Cp->NmMtch > 0)
    strcat (Buff, " B");

}				/* DisCfEI */

/**/

VOID
DsCfSTag (CfBuff, OutBuff)
     char CfBuff[], OutBuff[];
{

  if (CfBuff[0] == '0' && CfBuff[1] == '0')
    strcat (OutBuff, "   D: ");
  else if (CfBuff[0] == '0' && CfBuff[1] == '1')
    strcat (OutBuff, "X   : ");
  else if (CfBuff[0] == '0' && CfBuff[1] == '#')
    strcat (OutBuff, "X  D: ");
  else if (CfBuff[0] == '1' && CfBuff[1] == '0')
    strcat (OutBuff, "  G : ");
  else if (CfBuff[0] == '1' && CfBuff[1] == '1')
    strcat (OutBuff, " Y  : ");
  else if (CfBuff[0] == '1' && CfBuff[1] == '#')
    strcat (OutBuff, " YG : ");
  else if (CfBuff[0] == '#' && CfBuff[1] == '0')
    strcat (OutBuff, "  GD: ");
  else if (CfBuff[0] == '#' && CfBuff[1] == '1')
    strcat (OutBuff, "XY  : ");
  else if (CfBuff[0] == '#' && CfBuff[1] == '#')
    strcat (OutBuff, "XYGD: ");

}				/* DsCfSTag */


VOID
DsCfSta (CfBuff, Buff, Cp, Cond)
     char CfBuff[], Buff[];	/* ====> Buff better be big enough! <====== */
     struct CfNode *Cp;
     int Cond;			/* tells which condition is being displayed */
{
  register int i, j, loci, bi, bj;
  int matched, start, endstate;
  unsigned int pounds[INTPRSTR], bits[INTPRSTR], dcmask[INTPRSTR], bitmask[INTPRSTR],
    savepounds[INTPRSTR], savebits[INTPRSTR];
  char tbuf[10];
  struct StateNd *snode;
  struct VLociNd *vnode;

  char OutBuff[512];
  OutBuff[0] = '\0';
  tbuf[0] = '\0';
  strcat (OutBuff, " [");

  if (Cond == 1)		/* make a copy in savepounds/bits arrays */
    for (j = 0; j < INTPRSTR; ++j)
      {
	savepounds[j] = Cp->Cnd1DCs[j];
	savebits[j] = Cp->Cnd1Bits[j];
      }
  else
    for (j = 0; j < INTPRSTR; ++j)
      {
	savepounds[j] = Cp->Cnd2DCs[j];
	savebits[j] = Cp->Cnd2Bits[j];
      }
  savepounds[0] <<= 2;
  savepounds[0] >>= 2;		/* make it start ## */
  savebits[0] <<= 2;
  savebits[0] >>= 2;

  /* write states out in form: 1,3,5-8,10,15-38
     use start to indicate start of a range we are working on,
     end to indicate current last state in range we are working on.
     when test-state (i) is not equal to end + 1, the range
     we are working on is over, so write it out
     and start collecting the range again.
   */

  start = endstate = -2;
  for (i = 0, matched = FALSE, snode = States; i < NmStates; ++i, ++snode)
    {
      if (!snode->NoAttr)
	{			/* must have attr to match */

	  if (snode->VarLoci == NULL)
	    {			/* no variable bits */
	      for (bi = 0; bi < INTPRSTR; ++bi)
		{		/* just copy */
		  bits[bi] = savebits[bi];
		  pounds[bi] = savepounds[bi];
		}
	    }
	  else
	    {			/* its got variable bits...get mask for variable loci for this state, then mask over condition (bits,pounts) */
	      for (bi = 0, loci = 0; bi < INTPRSTR; ++bi)
		{
		  bitmask[bi] = dcmask[bi] = 0;
		  for (bj = 0; bj < LOCIPRI; ++bj, ++loci)
		    {
		      bitmask[bi] <<= 1;	/* zero into lsb */
		      dcmask[bi] <<= 1;

		      for (vnode = snode->VarLoci; vnode != NULL; vnode = vnode->NxtVL)
			if (vnode->VarLocus == loci)	/* stop if loci is variable */
			  break;

		      if (vnode == NULL)
			{	/* it is NOT variable! */
			  dcmask[bi] |= LSB;	/* so masks get 1's on all non-variable bits, */
			  bitmask[bi] |= LSB;	/* 0's on variable bits */
			}
		    }		/* } each loci in an part of the string */
		}		/* endfor each unsigned int part of string */

	      /* got mask for this state, so mask the condition parts */
	      /* recall bit=0,pound=0 is a # */

	      for (bi = 0; bi < INTPRSTR; ++bi)
		{
		  bits[bi] = savebits[bi] & bitmask[bi];
		  pounds[bi] = savepounds[bi] & dcmask[bi];
		}
	    }			/* endif variable loci masking */


	  for (j = 0; j < INTPRSTR; ++j)
	    if (((snode->Attrib[j] & pounds[j]) ^ bits[j]))
	      break;
	  if (j == INTPRSTR)
	    {			/* Must have matched the condition. */
	      if (i == endstate + 1)	/* its next in range, so make it new end */
		endstate = i;
	      else if (endstate == -2)	/* its first state: initialize */
		start = endstate = i;
	      else
		{		/* range ended: print range we were working on */
		  if (endstate == start)
		    {		/* just the one in the range */
		      if (matched)
			sprintf (tbuf, ",%d", start);	/* some before, so precede with comma */
		      else
			{
			  sprintf (tbuf, "%d", start);	/* just write it */
			  matched = TRUE;
			}
		    }
		  else
		    {		/* print the old range */
		      if (matched)
			sprintf (tbuf, ",%d-%d", start, endstate);
		      else
			{
			  sprintf (tbuf, "%d-%d", start, endstate);
			  matched = TRUE;
			}
		    }
		  start = endstate = i;		/* in any case, begin next range */
		  strcat (OutBuff, tbuf);	/* and write the range or state to end */
		}
	    }
	}
    }

  /* we looked at all the states: now print the last state or range */

  if (endstate != -2)
    {				/* it did match something */
      if (endstate == start)
	{			/* just the one in the range */
	  if (matched)
	    sprintf (tbuf, ",%d", start);	/* some before, so precede with comma */
	  else
	    sprintf (tbuf, "%d", start);	/* just write it */
	}
      else
	{			/* print the old range */
	  if (matched)
	    sprintf (tbuf, ",%d-%d", start, endstate);
	  else
	    sprintf (tbuf, "%d-%d", start, endstate);
	}
      strcat (OutBuff, tbuf);	/* append the last range. */
    }
  strcat (OutBuff, "]");

  if (strlen (OutBuff) > 100)
    strcat (Buff, "[*** len > 100 ***]");
  else
    strcat (Buff, OutBuff);

}				/* DsCfSta */


VOID
DsCfAct (CPtr, Buff)
     struct CfNode *CPtr;
     char Buff[];
{
  struct CfOpNode *coptr, *GetCOpCd ();		/* pointer to classifier operator (ActType) node. */
  int retval;

  strcat (Buff, " =");
  coptr = GetCOpCd (CPtr->ActType, &retval);
  if (strcmp (coptr->CfOpName, "PASS") != 0)
    strcat (Buff, coptr->CfOpName);
  strcat (Buff, "=> ");

}				/* DsCfAct */


VOID
DsCfEffS (CfBuff, OutBuff, Fmt)
     char CfBuff[], OutBuff[];
     int Fmt;
{
  register int i, s;
  int start, endstate;
  short int found;
  char setbuff[EFFATTSZ + 1], membuff[(EFFVALMX + 1) * 3], tbuf[10];

  /* pairs of eff bits specify string that defines a supported set:
     00        =>      0
     01        =>      1
     0#        |
     1#        |=> # (support both 0 and 1)
     ##        |
   */

  for (i = STRNGSZ - StrAttSz, s = 0; i < STRNGSZ; i += 2, ++s)
    if (CfBuff[i] == '0')
      setbuff[s] = CfBuff[i + 1];
    else
      setbuff[s] = '#';
  setbuff[s] = '\0';

  membuff[0] = ' ';
  membuff[1] = '[';
  membuff[2] = '\0';

  start = endstate = -2;
  for (s = 0, found = FALSE; s <= EffValMx; ++s)
    {				/* i counts number written */
      if (IsMemSym (s, setbuff))
	{
	  if (s == endstate + 1)	/* continue the range */
	    endstate = s;
	  else if (endstate == -2)	/* first one--init */
	    start = endstate = s;
	  else
	    {			/* end of range: print it, start a new range */
	      if (start == endstate)
		{		/* print 1 */
		  if (found)
		    sprintf (tbuf, ",%d", start);
		  else
		    {
		      sprintf (tbuf, "%d", start);
		      found = TRUE;
		    }
		}
	      else
		{		/* print range */
		  if (found)
		    sprintf (tbuf, ",%d-%d", start, endstate);
		  else
		    {
		      sprintf (tbuf, "%d-%d", start, endstate);
		      found = TRUE;
		    }
		}
	      strcat (membuff, tbuf);
	      start = endstate = s;	/* start over */
	    }
	}
    }

  if (endstate != -2)
    {				/* something left to print */
      if (start == endstate)
	{			/* print 1 */
	  if (found)
	    sprintf (tbuf, ",%d", start);
	  else
	    sprintf (tbuf, "%d", start);
	}
      else
	{			/* print range */
	  if (found)
	    sprintf (tbuf, ",%d-%d", start, endstate);
	  else
	    sprintf (tbuf, "%d-%d", start, endstate);
	}
      strcat (membuff, tbuf);
    }
  strcat (membuff, "]");

  strcat (OutBuff, membuff);

}				/* DsCfEffS */

/**/
/**
 DoEnvCmd               Execute an environment/domain-specific command.

 ParString      The name of the command and any parameters to it (\0 termintated).

 **/

VOID
DoEnvCmd (ParStrng)
     char *ParStrng;
{
  char cmd[60], *nxtchar, dir;
  int i, steps, err;
  float f;
  struct StateNd *snode;

  /* Get the command name into cmd, and leave nxtchar pointing at the command parameter. */

  while (*ParStrng == ' ')
    ++ParStrng;
  for (i = 0, nxtchar = ParStrng; i < 7 && *nxtchar != ' ' && *nxtchar != '\0'; ++i, ++nxtchar)
    cmd[i] = *nxtchar;
  cmd[i] = '\0';
  while (*nxtchar != '\0' && *nxtchar == ' ')
    ++nxtchar;

  if (strcmp (cmd, "dm") == 0)
    {
      BMsgtoA (CurState->Attrib, GInBuff);
      *(GInBuff + STRNGSZ) = '\0';
      sprintf (GOutBuff, "\nDetector Message will be '%s'.\n", GInBuff);
      WriteStd (GOutBuff);
    }
  else if (strcmp (cmd, "cs") == 0)
    {
      nxtchar = GetInt (nxtchar, &i, -1, ",", &err);
      if (i >= 0 && i < NmStates)
	{
	  OldState = CurState;
	  CurState = States + i;
	  CurState->VisitCnt += 1;
	  if (*nxtchar != '\0')
	    {
	      if (*nxtchar == 'e')	/* Empty current message list */
		EmptyML ('c');
	    }
	}
      else
	{
	  sprintf (GOutBuff, "\nCurState must be 0 .. %d.", NmStates - 1);
	  WriteStd (GOutBuff);
	}
    }
  else if (strcmp (cmd, "eas") == 0)
    {
      GetInt (nxtchar, &i, -1, "", &err);
      if (i >= 1 && i <= 4)
	{
	  EffAttSz = i;
	  EffAttMx = EffAttSz - 1;

	  EffValMx = (2 << EffAttMx) - 1;

	  StrAttSz = EffAttSz * 2;
	  StrAttMx = StrAttSz - 1;

	  for (steps = 0, i = STRNGSZ - StrAttSz; steps < StrAttSz && steps < STRATTSZ; ++steps, ++i)
	    EffLoci[steps] = i;

	  sprintf (GOutBuff, "\nStrAttSz %d, EffAttSz %d, EffValMx %d, EffLoci: ",
		   StrAttSz, EffAttSz, EffValMx);
	  WriteStd (GOutBuff);

	  for (steps = 0; steps < StrAttSz && steps < STRATTSZ; ++steps)
	    {
	      sprintf (GOutBuff, " %d", EffLoci[steps]);
	      WriteStd (GOutBuff);
	    }
	  WriteStd ("\n");
	}
      else
	WriteStd ("\nEffAttSz must be 1..4.");
    }
  else if (strcmp (cmd, "el") == 0)
    {
      for (steps = 0; steps < STRATTSZ; ++steps)
	{
	  nxtchar = GetInt (nxtchar, &i, -1, ",", &err);
	  if (i == -1)
	    break;
	  else if (i < 2 || i > STRNGMX)
	    WriteStd ("\n**ECMD: eff-loci out of range.\n");
	  else
	    EffLoci[steps] = i;
	}

      if (steps != 0)
	{
	  if (steps % 2 != 0)
	    WriteStd ("\n**ECMD: Eff-Loci cnt not even!\n");
	  StrAttSz = steps;
	  StrAttMx = StrAttSz - 1;
	  EffAttSz = StrAttSz / 2;
	  EffAttMx = EffAttSz - 1;

	  EffValMx = (2 << EffAttMx) - 1;
	}
      sprintf (GOutBuff, "\nStrAttSz %d, EffAttSz %d, EffValMx %d, EffLoci: ",
	       StrAttSz, EffAttSz, EffValMx);
      WriteStd (GOutBuff);
      for (steps = 0; steps < StrAttSz; ++steps)
	{
	  sprintf (GOutBuff, " %d", EffLoci[steps]);
	  WriteStd (GOutBuff);
	}
      WriteStd ("\n");
    }
  else if (strcmp (cmd, "htr") == 0)
    {
      GetInt (nxtchar, &i, -1, "", &err);
      if (i == 0 || i == 1)
	HiTrRsl = i;
      else
	WriteStd ("\nHiTrRsl must be 0 or 1.");
    }
  else if (strcmp (cmd, "dftr") == 0)
    {
      GetInt (nxtchar, &i, -1, "", &err);
      if (i == 0 || i == 1)
	DftRand = i;
      else
	WriteStd ("\nDftRand must be 0 or 1.");
    }
  else if (strcmp (cmd, "dftm") == 0)
    {
      GetInt (nxtchar, &i, -1, "", &err);
      if (i == 0 || i == 1)
	DftMstk = i;
      else
	WriteStd ("\nDFTM must be 0 or 1.");
    }
  else if (strcmp (cmd, "nbs") == 0)
    {
      nxtchar = GetInt (nxtchar, &i, -1, ",", &err);
      if (i < 0 || i >= NmStates)
	WriteStd ("\nillegal orig s");
      else
	{
	  GetInt (nxtchar, &steps, -1, "", &steps);
	  if (steps < 0 || steps >= NmStates)
	    WriteStd ("\nillegal nbs");
	  else
	    {
	      snode = States + i;
	      snode->BestNxt = steps;
	      snode->NmBest = 0;
	      snode->NmNeg = 0;
	    }
	}
    }
  else if (strcmp (cmd, "dcs") == 0)
    {
      GetInt (nxtchar, &i, -1, "", &err);
      if (i >= STRNGSZ && i <= 40)
	DisCndSz = i;
      else
	WriteStd ("\nDisCndSz must be STRNGSZ..40.");
    }
  else if (strcmp (cmd, "cf") == 0)
    FSWGetCf (nxtchar);

  else if (strcmp (cmd, "sb") == 0)
    FSWGetSB (nxtchar);

  else if (strcmp (cmd, "sba") == 0)
    {
      f = atof (nxtchar);
      WriteStd ("Id  Bound\n");
      for (i = 0; i < FSWNmCfs; ++i)
	{
	  FSWCSBds[i] = f;
	  sprintf (GOutBuff, "%3d   %6.1f\n", FSWCfs[i]->Cf_Id, FSWCSBds[i]);
	  WriteStd (GOutBuff);
	}
    }
  else if (strcmp (cmd, "help") == 0)
    HelpEnv ();

  else
    WriteStd ("\nUnknown ECmd.");

}				/* DoEnvCmd */

/**/
/*
   FSWGetCf     Mark list of classifiers (or "all") as ones to check for
   strength going over a specified value.
   FSWGetSB     Set strength value criteria for classifiers to be checked.

   These are called to process the associated DoEnvCmd commands.
   * */

VOID
FSWGetCf (Cfs)
     char *Cfs;
{
  int cfid, err, i;
  struct CfNode *cfptr;

  if (strcmp (Cfs, "all") == 0)
    {
      for (FSWNmCfs = 0, cfptr = CurCfs; FSWNmCfs < NmCfs && FSWNmCfs < 20; ++FSWNmCfs)
	{
	  FSWCfs[FSWNmCfs] = cfptr++;
	  FSWCBdWr[FSWNmCfs] = FALSE;
	  FSWCSBds[FSWNmCfs] = 10000.0;
	}
    }
  else
    {
      for (FSWNmCfs = 0; FSWNmCfs < 20;)
	{
	  Cfs = GetInt (Cfs, &cfid, -1, ",", &err);
	  if (err)
	    {
	      printf ("\nIllegal Cf_Id in parameter.\n");
	      break;
	    }
	  else if (cfid == -1)
	    break;
	  else
	    {
	      for (cfptr = CurCfs, i = 0; i < NmCfs; ++i, ++cfptr)	/* find node in cflist array */
		if (cfptr->Cf_Id == cfid)
		  {
		    FSWCfs[FSWNmCfs] = cfptr;
		    FSWCBdWr[FSWNmCfs] = FALSE;
		    FSWCSBds[FSWNmCfs] = 10000.0;
		    ++FSWNmCfs;
		    break;
		  }
	      if (i == NmCfs)
		{
		  sprintf (GOutBuff, "\nCouldn't find cf %d.", cfid);
		  WriteStd (GOutBuff);
		}
	    }
	}
    }

  WriteStd ("\nCfs to be checked:\n");
  for (i = 0; i < FSWNmCfs; ++i)
    {
      sprintf (GOutBuff, "%3d  %6.1f\n", FSWCfs[i]->Cf_Id, FSWCSBds[i]);
      WriteStd (GOutBuff);
    }

}				/* GSWGetCf */


VOID
FSWGetSB (Bds)
     char *Bds;
{
  int err, i;
  float str;
  char *GetFloat ();
  struct CfNode *cfptr;

  for (i = 0; i < FSWNmCfs; ++i)
    {
      Bds = GetFloat (Bds, &str, -1.0, ",", &err);
      if (err)
	{
	  printf ("\nIllegal parameter.\n");
	  break;
	}
      else if (str == -1)
	break;
      else
	FSWCSBds[i] = str;
    }

  WriteStd ("\nCfs to be checked:\n");
  for (i = 0; i < FSWNmCfs; ++i)
    {
      sprintf (GOutBuff, "%3d  %6.1f\n", FSWCfs[i]->Cf_Id, FSWCSBds[i]);
      WriteStd (GOutBuff);
    }

}				/* FSWGetSB */


VOID
HelpEnv ()
{

  WriteStd ("\n\ncs n,e set state to 'n'; e=>empty MList.");
  WriteStd ("\ndm	   dis det msg");
  WriteStd ("\ndcs s	dis cond sz");
  WriteStd ("\neas e	effector attribute size: 1,2,3, or 4.\n");
  WriteStd ("\nel i,... set effector loci (up to 8).\n");
  WriteStd ("\nrn,f	 set reward for state n to f (not yet implemented).");
  WriteStd ("\nhtr n	Set HiTrRsl to n, where n means:");
  WriteStd ("\n		 0   Use default transition.");
  WriteStd ("\n		 1   Pick at Random from allowed settings.");
  WriteStd ("\ndftr n   Set DftRand to n, where n means:");
  WriteStd ("\n		 0   Use first eff setting.");
  WriteStd ("\n		 1   Pick eff setting at random from allowed settings.");
  WriteStd ("\ndftm e   if 1, DftTrans sets MadeMstk TRUE; if 0, no mistake.");
  WriteStd ("\nnbs s,b  next best state b from state s.");
  WriteStd ("\ncf list  list of cf ids or ALL");
  WriteStd ("\nsb list  list of cf strength bds");
  WriteStd ("\nsba f	strength bound for all");
  WriteStd ("\n");

}				/* HelpEnv() */
