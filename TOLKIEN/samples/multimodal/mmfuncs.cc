//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//
//      This file contains a number of multimodal functions
//

#include "hpldbin.h"
#include "mmfuncs.h"

//
//              0.0 <= x <= 10.0
//              use 20 bits
//
//              y = x / 2.5     if   0.0 <= x <= 2.5
//                = 2 - x / 2.5      2.5  < x <= 5.0
//                = (x - 5) / 2.5    5.0  < x <= 7.5
//                = 4 - x / 2.5      7.5  < x <= 10.0
//
//
double  twopeaks(RCTGAObj ind)
{
        double  flX = ((RCBinHaploid) ind).asFloat() * prec2 * 10;

        if (flX > 7.5)
            return 4 - flX / 2.5;
        if (flX > 5)
            return (flX - 5) / 2.5;
        if (flX > 2.5)
            return 2 - flX / 2.5;
        return flX / 2.5;
}
//
//      A modified version of Himmelblau's Function
//
//
//                      2           2           2     2                       
//      F(x1,x2) = - (x1  + x2 - 11)  - (x1 + x2  - 7)    + 200
//
//              -5.0 <= xi <= 5.0
//              use 20 bits for xi, therefore chromosome length is 40
//
//
double  hmb(RCTGAObj ind)
{
        double  flX1 = ((RCHmbInd) ind).x1();
        double  flX2 = ((RCHmbInd) ind).x2();

	return 200 - pow(pow(flX1,2.0) + flX2 - 11, 2.0) - pow(flX1 + pow(flX2,2.0) - 7, 2.0);
}

//
//      A function of five equal maxima
//
//                6
//	f(x) = sin (5 * PI * x)
//
//	   0.0 <= x <= 1.0
//		use 30 bits for chromosome
//
double	evenpeaks(RCTGAObj ind)
{
        double  flX = ((RCBinHaploid) ind).asFloat() * prec3;

	return pow(sin(5 * M_PI * flX), 6);
}

//
//      Five Unevenly spaced, but equal maxima
//
//                6             3/4
//	f(x) = sin (5 * PI * ( x     - 0.05))
//
//	   0.0 <= x <= 1.0
//		use 30 bits for chromosome
//
//
double	unevenpeaks(RCTGAObj ind)
{
        double  flX = ((RCBinHaploid) ind).asFloat() * prec3;

	return pow(sin(5 * M_PI * (pow(flX, 0.75) - 0.05)), 6.0);
}

//
//      A function with five uneven decreasing maxima
//
//                                                   2        6             3/4
//	f(x) = exp(-2 * log(2) * ((x - 0.08) / 0.854)  ) * sin (5 * PI * ( x    - 0.05))
//
//	   0.0 <= x <= 1.0
//		use 30 bits for chromosome
//
double	unevendecpeaks(RCTGAObj ind)
{
	double  flX = ((RCBinHaploid) ind).asFloat() * prec3;

	return exp(-2 * log(2) * pow((flX - 0.08)/0.854,2.0)) *
	       pow(sin(5 * M_PI * (pow(flX, 0.75) - 0.05)), 6.0);
}

//
// 	f(x) = 160/10 * x if x < 10
//	     = 200/5 * (x - 15) if x > 15
//	     = 160/5 * (15 - x) otherwise
//
//	    0.0 <= x <= 20.0
//		use 20 bits for chromosome
//
double	twopeaktrap(RCTGAObj ind)
{
        double  flX = ((RCBinHaploid) ind).asFloat() * prec2 * 20;
	if (flX < 10.0)
            return 16 * flX;
	else if (flX > 15.0)
               return 40 * (flX - 15);
	else
	    return 32 * (15 - flX);
}

//
//                             2
//  	    f(x) = 1 + cos(4*M_PI*|x|  )
//
//	    0.0 <= x <= 1.0
//		use 30 bits for chromosome
//
double	unevencos(RCTGAObj ind)
{
        double  flX = ((RCBinHaploid) ind).asFloat() * prec3;

	return 1 + cos(FOUR_PI * pow(fabs(flX),2.0));
}
