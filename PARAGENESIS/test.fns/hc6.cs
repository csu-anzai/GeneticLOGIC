#include "Pstuff.h"
double:physical Peval(str, length, vect, genes)
char:physical str[];
int length;
double:physical vect[];
int genes;
{
  with(physical) {
   double:physical Pmaxx();
   double:physical Pavep();
   double:physical AB,AC,AD,AE,BC,BD,BE,BF;
   double:physical CD,CE,CF,DE,DF,EF,FA;
   double:physical NAB,NAC,NAD,NAE,NBC,NBD,NBE,NBF;
   double:physical NCD,NCE,NCF,NDE,NDF,NEF,NFA;
   double:physical T1002,T1033,T1034,T1042,T1050,T1058,T1066,T1067;
   double:physical T1075,T1083,T1091,T1099,T1100,T1108,T1116,T1124;
   double:physical T1132,T1133,T1141,T1149,T1157;
   double:physical T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14;
     
     AB = vect[0];
     AC = vect[1];
     AD = vect[2];
     AE = vect[3];
     BC = vect[4];
     BD = vect[5];
     BE = vect[6];
     BF = vect[7];
     CD = vect[8];
     CE = vect[9];
     CF = vect[10];
     DE = vect[11];
     DF = vect[12];
     EF = vect[13];
     FA = vect[14];
     NAB = 1-vect[0];
     NAC = 1-vect[1];
     NAD = 1-vect[2];
     NAE = 1-vect[3];
     NBC = 1-vect[4];
     NBD = 1-vect[5];
     NBE = 1-vect[6];
     NBF = 1-vect[7];
     NCD = 1-vect[8];
     NCE = 1-vect[9];
     NCF = 1-vect[10];
     NDE = 1-vect[11];
     NDF = 1-vect[12];
     NEF = 1-vect[13];
     NFA = 1-vect[14];
     T1034 = Pavep(4, &BF, &NBE, &NBD, &NBC);
     T1042 = Pavep(4, &NBF, &BE, &NBD, &NBC);
     T1050 = Pavep(4, &NBF, &NBE, &BD, &NBC);
     T1058 = Pavep(4, &NBF, &NBE, &NBD, &BC);
     T1033 = Pmaxx(4, &T1034, &T1042, &T1050, &T1058);
     T1067 = Pavep(4, &AE, &NAD, &NAC, &NAB);
     T1075 = Pavep(4, &NAE, &AD, &NAC, &NAB);
     T1083 = Pavep(4, &NAE, &NAD, &AC, &NAB);
     T1091 = Pavep(4, &NAE, &NAD, &NAC, &AB);
     T1066 = Pmaxx(4, &T1067, &T1075, &T1083, &T1091);
     T1100 = Pavep(4, &EF, &NDF, &NCF, &NBF);
     T1108 = Pavep(4, &NEF, &DF, &NCF, &NBF);
     T1116 = Pavep(4, &NEF, &NDF, &CF, &NBF);
     T1124 = Pavep(4, &NEF, &NDF, &NCF, &BF);
     T1099 = Pmaxx(4, &T1100, &T1108, &T1116, &T1124);
     T1133 = Pavep(4, &DE, &NCE, &NBE, &NAE);
     T1141 = Pavep(4, &NDE, &CE, &NBE, &NAE);
     T1149 = Pavep(4, &NDE, &NCE, &BE, &NAE);
     T1157 = Pavep(4, &NDE, &NCE, &NBE, &AE);
     T1132 = Pmaxx(4, &T1133, &T1141, &T1149, &T1157);
     T1 = Pavep(2, &DF, &NDE);
     T2 = Pavep(2, &NDF, &DE);
     T3 = Pavep(3, &CF, &NCE, &NCD);
     T4 = Pavep(3, &NCF, &CE, &NCD);
     T5 = Pavep(3, &NCF, &NCE, &CD);
     T6 = Pavep(3, &CD, &NBD, &NAD);
     T7 = Pavep(3, &NCD, &BD, &NAD);
     T8 = Pavep(3, &NCD, &NBD, &AD);
     T9 = Pavep(2, &BC, &NAC);
     T10 = Pavep(2, &NBC, &AC);
     T11 = Pmaxx(2, &T1, &T2);
     T12 = Pmaxx(3, &T3, &T4, &T5);
     T13 = Pmaxx(3, &T6, &T7, &T8);
     T14 = Pmaxx(2, &T9, &T10);
     T1002 = Pavep(11, 
		   &FA, 
		   &EF, 
		   &T11,
		   &T12, 
		   &T1033, 
		   &T1066, 
		   &T1099, 
		   &T1132, 
		   &T13, 
		   &T14, 
		   &AB);
     
     return(T1002);
   }
}

