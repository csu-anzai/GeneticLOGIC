
/*-----------------------------------------------------------------------*/
/* 15-6-92 a new tool to randomize imap files for tierra                 */
/* Tierra Simulator V3.13: Copyright (c) 1992 Dan Pirone & Virtual Life  */
/*-----------------------------------------------------------------------*/

/* #define _DBG_    */

#include <stdio.h>
#include <time.h>
#include "license.h"
#include "tierra.h"
#include "declare.h"
#ifdef __TURBOC__
#define random rand
#define srandom srand
#endif /* __TURBOC__ */
/*-----------------------------------------------------------------------*/
typedef struct 
   {
   I32s op;
   I8s  mnem[84];
   } I_set;

I_set *InstSet_In, *InstSet_Out; 

     /* # of instructions, dyanmic on size of Infile */
I32s t_i_num , PLACE_1 = -1L, InstNum = INSTNUM;  
FILE *infile = stdin, *outfile = stdout; 
I8s data[120];

I8s *usage = "syntax - mix-o-inst infile outfile [op1 op2 hamm_dist]\n\
        infile, outfile can be '-' for stdin, stdout\n\
        op1, op2 are mnemonics, \n\
        & hamm_dist is the desired Hamming distance between them\n";
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
void FlipBit(seed, bit )
    I32s *seed, bit;
{
    if ((*seed) & (ONE << bit))        /* if bit in seed in on */
        (*seed) &= (~(ONE << bit));
    else
        (*seed) |= (ONE << bit);
}
/*-----------------------------------------------------------------------*/
void init_uparams(size)
I32s size;
{
I32s i;

InstSet_In  = (I_set*) calloc(size, sizeof(I_set));
InstSet_Out = (I_set*) calloc(size, sizeof(I_set));

if(!InstSet_In || !InstSet_Out)
   {
   fprintf(stderr,"Problem getting memory for %ld elements!\n",size);
   exit(-666);
   }
for(i =0;i < size; i++)
     {
     InstSet_Out[i].op = -1L;
     InstSet_Out[i].mnem[0] = '\0';
     }

}
/*-----------------------------------------------------------------------*/
I32s place_rest_of_inst(i_left)
I32s i_left;
{
I32s t_op_dist,
     op_p,
     t_p,in_p
     ;

while(i_left > 0)
   {
   t_p= t_op_dist =  random() % i_left+1;
#ifdef _DBG_
     for(in_p=0;in_p < InstNum; in_p++)
      fprintf(outfile,"%ld %ld %ld %ld %s\n",in_p,i_left,
              t_op_dist,InstSet_Out[in_p].op, InstSet_Out[in_p].mnem);
#endif
   if ( PLACE_1 < 0L)
      {
      for(in_p=0;in_p < InstNum  ; in_p++)
         if (InstSet_In[in_p].op > -1L) break;
      }
    else in_p = PLACE_1;
   if(in_p == InstNum)
      {
      fprintf(stderr,"Problem, all input placed ??? \n");
      exit(-666);
      }
   t_p= t_op_dist;
   for(op_p = 0; op_p < InstNum;op_p++ )
      {
      if ((InstSet_Out[op_p].op < 0L) && (--t_p < 1L)) 
           {
           InstSet_Out[op_p] = InstSet_In[in_p]; 
           InstSet_Out[op_p].op = op_p;
           InstSet_In[in_p].op = -1L;
           i_left--;
           if(PLACE_1 > -1L) return op_p;
           break;
           }
      }

   }
return op_p;        /* for hamming dist stuff in place first */
}

/*-----------------------------------------------------------------------*/
I32s read_in_inst(in_name)
I8s *in_name;
{
    I32s inum, op;
    int  rest;

    if ((!in_name) || ((in_name[0] != '-') && 
        ((infile = fopen(in_name,"r")) == NULL)))
    {   fprintf(stderr,"Problem opening infile %s",in_name);
        exit(-666);
    }

    inum = 0L;
    fgets(data, 84, infile);
    while (strlen(data) > 3)
    {   if (fgets(data, 84, infile) == NULL)
            break;
        inum++;
    }
    rewind(infile);
    init_uparams(inum);        /* allocate space for maps */

    fgets(data, 84, infile);
    while (strlen(data) > 3)
    {   rest = op = -1;
        data[119] = '\0';        /* just in case */
        if ((( sscanf(data,"%*[^x]x%lx%*[^\"]%n",&op,&rest)) < 1)||((op <0)
            ||(op >INSTNUM ) || (rest < 1L))) 
        {   fprintf(stderr,"bad IMapFile line: %s", data);
            exit(-667);
        }
        InstSet_In[op].op = op;
        strcpy(InstSet_In[op].mnem,&data[rest]);
        if (fgets(data, 84, infile) == NULL)
            break;
    }
    fclose(infile);
    return inum;
}
/*-----------------------------------------------------------------------*/
void place_first_inst(i_num,argc,argv)
I32s *i_num;
int argc;
char *argv[];
{
    I32s op_p, i, l, op1, op2, h_dist, u_b_left,  bits[255];
    I32s t_bit_dist, bit_p, bit_dist;

    if ((*argv[2] != '-') && ((outfile = fopen(argv[2],"w")) == NULL))
    {    fprintf(stderr,"Problem opening outfile %s",argv[2]);
         exit(-666);
    }
    if(!argc)
    {   op_p = random() % (*i_num)--;
        InstSet_Out[op_p]= InstSet_In[op_p];
        InstSet_In[op_p].op = -1L;
        PLACE_1 = -1L;
    }
    else
    {   if (argc%3) 
        {   fprintf(stderr,"Problem incomplete op1 op2 ham triplet!\n");
            exit(-666);
        }
        for(i= 3; i < argc; i++)
        {   
            for(op1= 0; op1 < InstNum; op1++)
            {   if(!strncmp(&(InstSet_In[op1].mnem[1]), argv[i],
                    strlen(argv[i])))
                    break;
            } 
            if(op1==InstNum) 
            {   fprintf(stderr,"Problem op1 %8s not in %s!\n",argv[i],
                    (*argv[1] == '-')?"stdin": argv[1]);
                exit(-666);
            }
            i++;
            for(op2= 0; op2 < InstNum; op2++)
            {   if(!strncmp(&(InstSet_In[op2].mnem[1]), argv[i],
                    strlen(argv[i])))
                    break;
            } 
            if(op2==InstNum) 
            {   fprintf(stderr,"Problem op2 %8s not in %s!\n",argv[i],
                    (*argv[1] == '-')?"stdin": argv[1]);
                exit(-666);
            }
            i++;
            if((sscanf(argv[i],"%ld", &h_dist) < 1)
                || (h_dist < 1 || h_dist >= InstNum))
            {   fprintf(stderr,
                    "Problem bad magic value on hamming dist %8s !\n",
                    argv[i]);
                exit(-666);
            }
            for (l=0; l < InstNum; bits[l++] = -1L) ;   /*init bits */
            u_b_left = h_dist;
            /* place first one in uniqe location of Out */
            /* InstSet_Out[op1] = InstSet_In[op1]; */
            /* copy Out[op1].op to In[op2].op */
            PLACE_1 = InstSet_In[op1].op;
            InstSet_In[op2].op = place_rest_of_inst((*i_num));
            /* place second one in uniqe location of Out, accordin to mutated
               op op2, if not unique bitch and quit ! */

            while(u_b_left > 0)
            {   t_bit_dist =  random() % INSTBITNUM;
                for(bit_p = 0; bit_p < INSTBITNUM; bit_p++)
                {   if ((bits[bit_p] < 0L) && (--t_bit_dist < 0L))
                    {   FlipBit(&(InstSet_In[op2].op) , bit_p);
                        bits[bit_p] = 1L;
                        u_b_left --;
                        break;
                    }
                }
            }
            if (InstSet_Out[InstSet_In[op2].op].op > -1L)
            {   fprintf(stderr,
                    "Problem Hamming conflict at %s %s %s !\nTry again...\n",
                    argv[i-2],argv[i-1],argv[i]);
                exit(-666);
            }
            else
            {   InstSet_Out[InstSet_In[op2].op] = InstSet_In[op2];
                InstSet_In[op2].op = -1L;
                (*i_num) -= 2; /* one for each part of the pair */
            }
        }
        PLACE_1 = -1L;
    }
}

/*-----------------------------------------------------------------------*/
int main (argc,argv)
int argc;
char *argv[];
{
    I32s i, seed;
    if (argc < 3) {perror(usage);exit(-1);}

    seed = (I32s) time(NULL);
    srandom(seed);
    seed = random();
    for (i = random() % 256; i >= 0; i--)
        seed = random();
    srandom(seed);
    random();

    InstNum = t_i_num = read_in_inst(argv[1]);
    place_first_inst(&t_i_num ,(argc>3)?argc:0,argv);
    place_rest_of_inst(t_i_num);
    for(i=0;i < InstNum; i++)
        fprintf(outfile,
            "    {0x%02lx, %s",InstSet_Out[i].op,InstSet_Out[i].mnem);
    return 0;
}
/*-----------------------------------------------------------------------*/
