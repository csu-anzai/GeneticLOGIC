/*-----------------------------------------------------------------------*/
/* 92-3-23 a new tool to slice trough the genebank files on disk ...     */
/* Tierra Simulator V3.1: Copyright (c) 1992 Dan Pirone & Virtual Life   */
/*-----------------------------------------------------------------------*/

/* #define _DBG_  */
/* #define X  */	/* use xterms for unix shells */
#ifdef __TURBOC__
#define SLASH '\\'
#else
#define SLASH '/'
#endif /* else of __TURBOC__ */

#include <stdio.h>
#include <sys/types.h>
#ifdef __TURBOC__
#include <dir.h>
#include <dos.h>
#else
#include <dirent.h>
#ifndef ultrix
#include <sys/dirent.h>
#endif /* ndef ultrix */
#endif /* else of __TURBOC__ */

#include "tierra.h"
#include "declare.h"
#include "arginst.h"

I32s usize = 80L
     ,uLbl = 1L
     ,umpp = 50
     ,umpi = 50;
Event uOeventB,uOeventE,uMeventB,uMeventE;
I8s file_type[4]="gen",
#ifdef __TURBOC__
    gb_in[255] = "gb\\",
    gb_out[255] = "td\\",
    viewer[255] = "more < ",
#else
    gb_in[255] = "gb/",
    gb_out[255] = "td/",
    viewer[255] = "vi",
#endif
    imap[255] = "opcode.map",
    data[600],
    ubits[33] =  "????????????????????????????????";
FILE *afp;
/*-----------------------------------------------------------------------*/
#define ARG
#include "portable.c"
#include "genio.c"
#undef ARG
/*-----------------------------------------------------------------------*/
I8s * bit_names[] = {
"permanent genotype name, saved in .gen file ",
"swapped out to disk from the rambank, saved in .mem file ",
"EXs = executes own instructions (self) ",
"EXd = executes daughter's instructions ",
"EXo = executes other cell's instructions ",
"EXf = executes instructions in free memory ",
"EXh = own instructions are executed by other creature (host) ",
"TCs = matches template complement of self ",
"TCd = matches template complement of daughter ",
"TCo = matches template complement of other ",
"TCf = matches template complement of free memory ",
"TCh = own template complement is matched by other creature (host) ",
"TPs = uses template pattern of self ",
"TPd = uses template pattern of daughter ",
"TPo = uses template pattern of other ",
"TPf = uses template pattern of free memory ",
"TPh = own template pattern is used by other creature (host) ",
"MFs = moves instruction from self ",
"MFd = moves instruction from daughter ",
"MFo = moves instruction from other cell ",
"MFf = moves instruction from free memory ",
"MFh = own instructions are moved by other creature (host) ",
"MTs = moves instruction to self ",
"MTd = moves instruction to daughter ",
"MTo = moves instruction to other cell ",
"MTf = moves instruction to free memory ",
"MTh = is written on by another creature (host) ",
"MBs = executing other creatures code, moves inst from self ",
"MBd = executing other creatures code, moves inst from daughter ",
"MBo = executing other creatures code, moves inst from other cell ",
"MBf = executing other creatures code, moves inst from free memory ",
"MBh = other creature uses another cpu to move your instructions "};

/*-----------------------------------------------------------------------*/
void FEClrscr()
{
#ifdef __TURBOC__
clrscr();
#else
system("clear");
#endif
}
/*-----------------------------------------------------------------------*/
int FEGetch()
{
#ifdef __TURBOC__
    return printf("\n"),getch();
#else
    char data[84],c;
    fgets(data,84,stdin);
    sscanf(data,"%c",&c);
    return c;
#endif
}

/*-----------------------------------------------------------------------*/
init_uparams(mode)
I32s mode;
{
if(mode == 0)
   {
   usize = -1L;
   uLbl = -1L;
   umpp = -1L;
   umpi = -1L;
   uOeventB.m = -1L;
   uOeventB.i = -1L;
   uOeventE.m = -1L;
   uOeventE.i = -1L;
   uMeventB.m = -1L;
   uMeventB.i = -1L;
   uMeventE.m = -1L;
   uMeventE.i = -1L;
   strcpy(ubits, "????????????????????????????????");
   strcpy(file_type,"gen");
   /* strcpy(gb_in,"gb/");
   strcpy(gb_out,"td/");
   strcpy(viewer,"vi"); */
   }

}

/*-----------------------------------------------------------------------*/
set_imap()
{   I32s i;

    sprintf(data,"%s/%s",gb_in,imap);
#ifdef _DBG_
    printf("SET_IMAP: file is %s\n",data);
#endif
    GetAMap(data);
#ifdef _DBG_
    for (i=0; i < INSTNUM; i++)
        printf("SET_IMAP: Aid [%ld] = %hx, %s\n",i,aid[i].op,aid[i].mn);
    FEGetch();
#endif
}
/*-----------------------------------------------------------------------*/
I16s Lbl2Int(s)
    I8s *s;
{
    if (s[0] == '-')
    return -1;
    return (s[2] - 'a') + (26 * (s[1] - 'a')) + (676 * (s[0] - 'a'));
}

/*-----------------------------------------------------------------------*/
I8s *Int2Lbl(i)
    I32s i;
{
    static I8s s[4];

    if (i < 0) {
    strcpy(s, "---");
    return s;
    }
    s[0] = 'a' + (I16s) i / 676;
    i %= 676;
    s[1] = 'a' + (I16s) i / 26;
    i %= 26;
    s[2] = 'a' + (I16s) i;
    s[3] = 0;
    return s;
}

/*-----------------------------------------------------------------------*/
I8s IsInBitSet(bstr,bits)
I8s bstr[32];
I32s bits;
{
I32s b,p;
/* WARNING first 2 bits are not watch bits! */
for(b=1,p=0; p < 32;p++, b = b << 1L)
   {
   if ((bstr[p] == '?') ||
       (bstr[p] == '1' && (b&bits)) ||
       (bstr[p] == '0' && !(b&bits)) )
       continue;
   return 0;
   }
return 1;
}
/*-----------------------------------------------------------------------*/
void WritEcoFuzzyB(bits, buf)
    I8s bits[33];
    I8s *buf;   
{
    I32s b,i, j;

    if(!buf) return;
    sprintf(buf,"EX      TC      TP      MF      MT      MB      ");
    for (b=0,i = 0, j = 0; i < 6; i++, j = 0) {
        if (bits[b++]!='0')
            buf[2+(i*8)+j++] = 's';
        if (bits[b++]!='0')
            buf[2+(i*8)+j++] = 'd';
        if (bits[b++]!='0')
            buf[2+(i*8)+j++] = 'o';
        if (bits[b++]!='0')
            buf[2+(i*8)+j++] = 'f';
        if (bits[b++]!='0')
            buf[2+(i*8)+j++] = 'h';
    }
}

/*-----------------------------------------------------------------------*/

I8s MatchesProfile(usize,uLbl,umpp,umpi,uOeventB,uOeventE,uMeventB,uMeventE,
                   ubits,Gen)
I32s usize,uLbl,umpp,umpi;
Event uOeventB,uOeventE,uMeventB,uMeventE;
I8s ubits[32];
Pgl Gen;
{

if (
    (Gen == NULL) ||
    ((usize > -1L) && (Gen->gen.size != usize)) ||
    ((uLbl > -1L) && (Lbl2Int(Gen->gen.label) != uLbl)) ||  
    ((uOeventB.m > -1L) && (Gen->originI.m < uOeventB.m)) ||
    ((uOeventB.i > -1L) && (Gen->originI.i < uOeventB.i)) ||
    ((uOeventE.m > -1L) && (Gen->originI.m > uOeventE.m)) ||
    ((uOeventE.i > -1L) && (Gen->originI.i > uOeventE.i)) ||
    ((uMeventB.m > -1L) && (GFormat  < 2L)) || 
    ((uMeventB.m > -1L) && (Gen->mpp_time.m < uMeventB.m)) ||
    ((uMeventB.i > -1L) && (GFormat < 2L)) || 
    ((uMeventB.i > -1L) && (Gen->mpp_time.i < uMeventB.i)) ||
    ((uMeventE.m > -1L) && (GFormat < 2L)) ||
    ((uMeventE.m > -1L) && (Gen->mpp_time.m > uMeventE.m)) ||
    ((uMeventE.i > -1L) && (GFormat < 2L)) ||
    ((uMeventE.i > -1L) && (Gen->mpp_time.i > uMeventE.i)) ||
    ((umpp > -1L) && (100.0*Gen->MaxPropPop < umpp)) ||
    ((umpi > -1L) && (100.0*Gen->MaxPropInst < umpi)) ||
    !IsInBitSet(ubits,Gen->bits)) return 0;
else return 1;

}
/*-----------------------------------------------------------------------*/
void Probe()
{
I32s i,done=0L;
I8s c, file[255];
head_t head;
indx_t *indx, *tindx, gindx;
GList *g = NULL;
#ifdef __TURBOC__
struct ffblk ffblk;
#else /* unix ... */
DIR *dirp;
struct dirent *dep;
#endif /* unix ... */

printf("\nBEGIN Probing Genebank\n\n");

#ifdef __TURBOC__
sprintf(file,"%s%c*.%3s",gb_in,SLASH,file_type);
if (findfirst(file,&ffblk,0))
      {perror("PROBE :");exit(-666);}

while(!done)
  {
   if (kbhit() && ( FEGetch() == 0x1b)) return;
   sprintf(file,"%s%c%s",gb_in,SLASH,ffblk.ff_name);

#else /* unix ... */
if ((dirp = opendir(gb_in)) == NULL) 
      {perror("PROBE :");exit(-666);}

while ( (dep = readdir(dirp)) != NULL)
   {
   if ((dep->d_namlen <= 3)  || 
       (strcmp(&dep->d_name[dep->d_namlen-3],file_type))) continue;
        sprintf(file,"%s%c%s",gb_in,SLASH,dep->d_name);

#endif /* unix ... */
   printf(" scanning archive  |%s|\r",file);
   fflush(stdout);

        if (!(afp = fopen(file, "rb"))) {
            perror("PROBE open archive ");
            exit(-666);
        }
        head = read_head(afp);
        if (strncmp(head.magic, "tie", 3))
            {
            fprintf(stderr, "%s: bad magic number", file);
	    fclose(afp);
            continue;
            }
#ifdef __TURBOC__
        indx = &gindx;
#else  /* __TURBOC__ */
        indx = read_indx(afp, &head);
#endif /* __TURBOC__ */
           for (i=0; i<head.n; i++) 
             {
#ifdef __TURBOC__
                find_gen(afp, indx, "---", i);
                tindx = indx;
#else  /* __TURBOC__ */
                tindx = &indx[i];
#endif /* __TURBOC__ */
                g = get_gen(afp, &head, tindx, i);

		if(!MatchesProfile(usize,uLbl,umpp,umpi,uOeventB,uOeventE,
				   uMeventB,uMeventE, ubits,g)) continue;
                printf("\n\t%04ld%3s\n",g->gen.size,g->gen.label);
                printf(
	"Enter y to show , ESC to break out, any other key to continue ->");
  	        c = FEGetch();
                if (c == 0x1b)
                {  if (g)
                   {  if (g->genome)
                      {  free(g->genome);
                         g->genome = NULL;
                      }
                      if (g->gbits)
                      {  free(g->gbits);
                         g->gbits = NULL;
                      }
                      free(g);
                      g = NULL;
                   }
                   fclose(afp);
                   return;
                }
                
                if (c == 'y')
                   {
                   sprintf(file, 
		          "%s%c%04hd%3s", gb_out, SLASH,
			  head.size, g->gen.label); 
                   WritAscFile(g, file);
#ifdef __TURBOC__
                   sprintf(file, 
		      "%s  %s%c%04hd%3s", 
	               viewer,gb_out,SLASH,head.size, g->gen.label);
#else
#ifdef X
                   sprintf(file, 
		      "xterm -title \"%04hd%3s\" -e %s  %s%c%04hd%3s &", 
	               head.size, g->gen.label, viewer,gb_out,SLASH,
		       head.size, g->gen.label);
#else
                   sprintf(file, 
		      "%s  %s%c%04hd%3s ", 
	               viewer,gb_out,SLASH,head.size, g->gen.label);
#endif /* X */
#endif
                   system(file);
                   }
                if (g)
                {  if (g->genome)
                   {  free(g->genome);
                      g->genome = NULL;
                   }
                   if (g->gbits)
                   {  free(g->gbits);
                      g->gbits = NULL;
                   }
                   free(g);
                   g = NULL;
                }
             }
	fclose(afp);
   

#ifdef __TURBOC__
   done = findnext(&ffblk);
#endif
   }



printf("\n\nEND   Probing Genebank\n\n");
printf("Enter to continue \n");
c = FEGetch();

}
/*-----------------------------------------------------------------------*/
void GetBits()
{
I32s b;
I8s c;
for(b = 0; b < 32; b++)
  {
  printf("Bit %2ld %65.65s (%c%)>",b,bit_names[b],ubits[b]);
  c= FEGetch();
  if( c == 0x1b) return; 
  if( c == '1') ubits[b] = '1'; 
  else
  if( c == '0') ubits[b] = '0'; 
  else
  if( c == '?') ubits[b] = '?'; 

}
}
/*-----------------------------------------------------------------------*/
void Probe_ascii_io()
{
I8s  answer = ' ',
     bbuf[4],
     data[120];
     int ll;
while(1)
  {
FEClrscr();
printf("\nTIERRA Genebank Probe\t\t\tby Daniel Pirone\n\n");
printf("1 - Size                                     (%ld)\n",usize);
printf("2 - Label                                    (%3s)\n",
	(uLbl > -1)? Int2Lbl(uLbl) :"-1");
printf("3 - MaxPropPop  threshold [0-100]%%           (%ld) \n",umpp);
printf("4 - MaxPropInst threshold [0-100]%%           (%ld) \n",umpi);
printf("5 - Begin Time of MaxPropPop threshold cross (%ld,%ld)\n",
	uMeventB.m,uMeventB.i);
printf("6 - End   Time of MaxPropPop threshold cross (%ld,%ld)\n",
	uMeventE.m,uMeventE.i);
printf("7 - Begin Time of Origin                     (%ld,%ld) \n",
	uOeventB.m,uOeventB.i);
printf("8 - End   Time of Origin                     (%ld,%ld) \n",
	uOeventE.m,uOeventE.i);
printf("9 - Watch Bit String [0,1,?] (%32s) \n",ubits);
WritEcoFuzzyB(ubits,Buff); 
printf("    Watch Bit String (%s) \n",Buff);
printf("f - Toggle file type                         (%3s)\n",file_type);
printf("i - Genebank Input directory                 (%s)\n",gb_in);
printf("o - Match    Output directory                (%s)\n",gb_out);
printf("m - Opcode - Instruction Map file            (%s)\n",imap);
printf("v - Set Gene Output viewer                   (%s)\n",viewer);
printf("G - Begin Probe\n\n");
printf(
"PROBE | (1-9) = edit param, 0 = init params, G=go <ESC> = quit      -> ");
answer = FEGetch();
    switch (answer)
       {
       case 0x1b : {exit(-1);}
       case 'G' : {set_imap();Probe(); break;}
       case '5' : 
       case '6' : 
       case '7' : 
       case '8' : 
		 printf("Enter millions , insts (-1 = don't care)  -> ");
                 fgets(data,84,stdin);
	}
    switch (answer)
       {
       case 'f' : {
                  (file_type[0] == 'g') ? strcpy(file_type,"tmp") 
					: strcpy(file_type,"gen");
		  break;
		  }
       case 'i' : {
		  printf("Enter Input Direcotry -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%s",gb_in);
		  set_imap();
		  break;
		  }
       case 'o' : {
		  printf("Enter Output Direcotry -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%s",gb_out);
		  break;
		  }
       case 'm' : {
		  printf(
		  "Enter Imap filename (assumed to be in Genebank dir)-> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%s",imap);
		  set_imap();
		  break;
		  }
       case 'v' : {
		  printf("Enter text viewer -> ");
                  fgets(data,84,stdin);
		  /* sscanf(data,"%s",viewer); */
		  for(ll=0;ll < 80;viewer[ll++] = '\0');
		  strncpy(viewer,data,(strlen(data)-1));
		  break;
		  }
       case '0' : {
		  init_uparams(0L);
		  break;
		  }
       case '1' : {
		  printf("Enter Size [-1 = don't care] -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%ld",&usize);
		  break;
		  }
       case '2' : {
		  printf("Enter Label [-1 = don't care,aaa-zzz] -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%s",bbuf);
		  if (atoi(bbuf) < 0) {uLbl = -1;break;}
		  uLbl = Lbl2Int(bbuf);
		  break;
		  }
       case '3' : {
		  printf( "Enter threshold [-1 = don't care, 0-100]%% -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%ld",&umpp);
		  break;
		  }
       case '4' : {
		  printf("Enter threshold [-1 = don't care, 0-100]%% -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%ld",&umpi);
		  break;
		  }
       case '5' : {
		  sscanf(data,"%ld,%ld",&uMeventB.m,&uMeventB.i);
		  break;
		  }
       case '6' : {
		  sscanf(data,"%ld,%ld",&uMeventE.m,&uMeventE.i);
		  break;
		  }
       case '7' : {
		  sscanf(data,"%ld,%ld",&uOeventB.m,&uOeventB.i);
		  break;
		  }
       case '8' : {
		  sscanf(data,"%ld,%ld",&uOeventE.m,&uOeventE.i);
		  break;
		  }
       case '9' : {
		  /* 
                  printf("Enter Watch Bits [? = don't care,0-1] -> ");
                  fgets(data,84,stdin);
		  sscanf(data,"%32s",ubits);
                  */
                  FEClrscr();
                  GetBits();
		  break;
		  }
		  
       default : answer = ' ';
       }
  }
}
/*-----------------------------------------------------------------------*/
int main (argc,argv)
int argc;
char *argv[];
{
FEClrscr();
init_uparams(0L);

Probe_ascii_io();
FEClrscr();
}
/*-----------------------------------------------------------------------*/
