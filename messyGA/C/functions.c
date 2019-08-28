/*===============================================
       file : functions.c
  
    purpose : define objective functions
 
  developed : 1991

     author : Kalyanmoy Deb
=================================================*/

#include "mga.ext"


double file0(chr,len)
int chr[];
int len;
/*  Liepins and Vose's fully deceptive function  */
{
	double x,val;
	int d;

	d = ones(chr,len);
	if (d == 0)         /* local  solution: all 0's */
		x = 1.0 - 1.0/(2.0*len);
	else if (d == len)  /* global solution: all 1's */
		x = 1.0;
	else                /* otherwise */
		x = 1.0 - (1.0 + d)/len;
	return((double) x * len);
}



double file1(chr,len)
int chr[];
int len;
/* Ackley's one max problem */
{
	double x,val;

	x = ones(chr,len);
	return((double) 10*x);
}



double file2(chr,len)
int chr[];
int len;
/* Ackley's two max function */
{
	double x,val;
	float fabs();

	x = ones(chr,len);
	val = fabs(18.0 * x - 8.0 * len);
	return((double) val);
}



double file3(chr,len)
int chr[];
int len;
/* Ackley's trap function */
{
	double x,z,val;

	x = ones(chr,len);
	z = 0.75 * len;
	if (x <= z)
		val = 8 * len * (z-x) / z;
	else 
		val = 10 * len * (x-z) / (len-z);
	return((double) val);
}

double file4(chr,len)
int chr[];
int len;
/* Ackley's Porcupine function */
{
	double x,val;

	x = ones(chr,len);
	val = 10 * x - 15 * (1 - parity(x));
	return((double) val);
}


double file5(chr,len)
int chr[];
int len;
/* Ackley's Plateaus function */
{
	double x, val, z;
	float fabs();
	int ones(), parity();

	x = ones(chr,len);
	if (x >= 1.0*len) 
		val = 2.5 * len;
	else val = 0.0;
	return((double) val);
}

double file9(chr,len)
int chr[];
int len;
/* User defined function */
{
	double x,val;

	x = decode(chr,len);
	val = x;
	return((double) val);
}

double file7(chr,len)
int chr[];
int len;
/* User defined function */
{
	double x,val;

	x = decode(chr,len);
	val = x;
	return((double) val);
}

double file8(chr,len)
int chr[];
int len;
/* User defined function */
{
	double x,val;

	x = decode(chr,len);
	val = x;
	return((double) val);
}

double file6(chr,len)
int chr[];
int len;
/* User defined function */
{
	double tval=0.0;
	int val,i;

	for (i=0; i<len; i++) {
	/*	if (coeff[i] <= 0.5) val = 0;
		else val = 1; */
		if (val == chr[i]) tval += 1.0;
	}	
	return((double) tval);
}

double get_func_val(id, chr, len)
int id;
unsigned chr[];
int len;
{
	switch (id) {
	case 0:
		return(file0(chr,len));
		break;
	case 1:
		return(file1(chr,len));
		break;
	case 2:
		return(file2(chr,len));
		break;
	case 3:
		return(file3(chr,len));
		break;
	case 4:
		return(file4(chr,len));
		break;
	case 5:
		return(file5(chr,len));
		break;
	case 6:
		return(file6(chr,len));
		break;
	case 7:
		return(file7(chr,len));
		break;
	case 8:
		return(file8(chr,len));
		break;
	case 9:
		return(file9(chr,len));
		break;
	default:
		return(0.0);
	}
}
