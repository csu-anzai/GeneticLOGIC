/****************************************************************************/
/*																			*/
/*	Grow.c															 		*/
/*	Embryology routines for breed.c.  Grow takes an organism and grows it on*/
/*	the screen; in other words it graphs a polynomial.	This file also has	*/
/*	a bunch of general graphic support routines.							*/
/*																			*/
/****************************************************************************/



/* -------------------------------------------------------------------------*/
/*			 INCLUDES	 												    */
/* -------------------------------------------------------------------------*/

#include "Control.h"
#include "GA.h"
#include "Curves.h"
#include <math.h>

/*If canvas is retained locking is a no-op */
#define Freeze()	pw_batch_on (G_population_pw); pw_lock(G_population_pw,G_population_rect)
#define Unlock()	pw_unlock(G_population_pw); pw_batch_off (G_population_pw)




/*--------------------------------------------------------------------------*/
/*	DPOW-- take a double to an integer power								*/
/*--------------------------------------------------------------------------*/

double dpow(x, n)			/* raise x to the nth power								*/
double x;
int n;
{
	int i;
	double p;

	p = 1.0;
	for (i=1; i <= n; ++i)
		p = p*x;
	return(p);
}



/*--------------------------------------------------------------------------*/
/*	DRAW_BOX-- draws a box in specified pixwin								*/
/*--------------------------------------------------------------------------*/

draw_box(pw, x1, y1, x2, y2)
Pixwin*	pw;
int		x1,y1,x2,y2;
{
	pw_vector(pw, x1,y1, x2,y1, PIX_SRC,1);
	pw_vector(pw, x2,y1, x2,y2, PIX_SRC,1);
	pw_vector(pw, x2,y2, x1,y2, PIX_SRC,1);
	pw_vector(pw, x1,y2, x1,y1, PIX_SRC,1);
}





/*--------------------------------------------------------------------------*/
/*	RESIZE-- when size of Population window is changed, refigures all scales*/
/*--------------------------------------------------------------------------*/
Resize (TheCanvas, new_width, new_height)
{
	G_current_width	=	new_width;
	G_current_height=	new_height;

	G_org_width		=	G_current_width/ORG_X;
	G_org_height	=	G_current_height/ORG_Y;

	G_x_scale		=	G_org_width/10;
	G_y_scale		=	G_org_height/10;

	G_x_trans		=	G_org_width;
	G_y_trans		=	G_org_height;
}




/*--------------------------------------------------------------------------*/
/*	REPAINT-- causes redisplay												*/
/*--------------------------------------------------------------------------*/

Repaint (TheCanvas, ThePixwin, TheArea)
	Canvas    TheCanvas;
	Pixwin   *ThePixwin;
	Rectlist *TheArea;

{
	Grow_Pop();
}




/*--------------------------------------------------------------------------*/
/*	GROW_POP-- grows each organism on the screen							*/
/*--------------------------------------------------------------------------*/

Grow_Pop()
{
	extern Print_Pop();
	int i,j;

	/* Erase the screen */
	G_population_rect = (Rect *)window_get(G_population_frame,WIN_RECT);
	pw_writebackground(G_population_pw, 0, 0, G_current_width, G_current_height, PIX_SRC);

	for (i=0; i < ORG_X; i++) {
		for (j=0; j < ORG_Y; j++) {
			Grow(i,j);
			if (G_show_genes == TRUE) {
				Display_Genes(i,j);
			}
		}
	}
	if (G_print_out == TRUE) {
		Print_Pop();
	}
}




/*--------------------------------------------------------------------------*/
/*																			*/
/*	GROW																	*/
/*																			*/
/*--------------------------------------------------------------------------*/


/*	transformations grow uses to draw					 				    */

/* scale x and y to fit inside cannonical screen box						*/
#define Tsx(x,xs)			((int) (x*xs))
#define Tsy(y,ys)			((int) (y*ys))

/* translate scaled drawing to center of canonical screen box				*/
#define	Tt1x(x,xt)			(x+(xt/2))
#define	Tt1y(y,yt)			(y+(yt/2))

/* translate cannonical screen box to correct screen-box location 			*/
#define Tt2x(x,i,xt)		(Tt1x(x,xt)+(i*xt))
#define Tt2y(y,j,yt)		(Tt1y(y,yt)+(j*yt))

/* translate and scale x and y -- this uses global variables				*/
#define Tx(x)				(Tt2x(Tsx(x,G_x_scale), org_i, G_x_trans))
#define Ty(y)				(Tt2y(Tsy(y,G_y_scale), org_j, G_y_trans))



/*--------------------------------------------------------------------------*/
/*	Grow() actually draws an organism  			            */
/* To see color, change calls to DevelopF to DevelopFC , and extra          */
/* color param to this call, and switch in call to pw_vector commented      */
/* *color* below; also switch in code in Make_Rest.c to set up color table. */
/* Color turns out to be really ugly, which is why it's not in.             */
/* You could make the colors evolve by adding some color genes... that      */
/* would be nice.                                                           */
/*--------------------------------------------------------------------------*/
Grow(org_i,org_j)
int org_i,org_j;
{
	double	t, dt;
	double	X, Y;
	int		X_scr, Y_scr, C_scr, X_scr_old, Y_scr_old, C_scr_old;
	double 	dpow();

	dt = (ORG_T_MAX - ORG_T_MIN)/ORG_SEGMENTS;
	Freeze();
	draw_box(G_population_pw,	Tx(ORG_X_MIN), Ty(ORG_Y_MIN),
					Tx(ORG_X_MAX), Ty(ORG_Y_MAX));


	/* Base case: t = ORG_T_MIN */
	DevelopF(org_i, org_j, ORG_T_MIN, &X_scr, &Y_scr);
	X_scr_old = X_scr;
	Y_scr_old = Y_scr;
	C_scr_old = C_scr;

	for (t = ORG_T_MIN + dt; t <= ORG_T_MAX; t+= dt) {

		DevelopF(org_i, org_j, t, &X_scr, &Y_scr);

		if ((X_scr_old	>= Tx(ORG_X_MIN)) && (X_scr_old	<= Tx(ORG_X_MAX)) &&
			(Y_scr_old >= Ty(ORG_Y_MIN)) && (Y_scr_old <= Ty(ORG_Y_MAX)) &&
			(X_scr     >= Tx(ORG_X_MIN)) && (X_scr	   <= Tx(ORG_X_MAX)) &&
			(Y_scr     >= Ty(ORG_Y_MIN)) && (Y_scr	   <= Ty(ORG_Y_MAX))) {
                                /* color */
/*				pw_vector(G_population_pw, X_scr_old, Y_scr_old,
							   X_scr, Y_scr, PIX_SRC,
					                   (C_scr_old + C_scr)/2);
*/
                                /* black and white*/
				pw_vector(G_population_pw, X_scr_old, Y_scr_old,
							   X_scr, Y_scr, PIX_SRC,
					                   1);
	}
		X_scr_old = X_scr;
		Y_scr_old = Y_scr;
		C_scr_old = C_scr;
}

	Unlock();
}



/*--------------------------------------------------------------------------*/
/*	DEVELOP-- grows next point--evaluates organism at time t				*/
/*--------------------------------------------------------------------------*/
Develop(org_i, org_j, t, X_scr, Y_scr)
int		 org_i, org_j;
double	 t;
int		*X_scr;
int 	*Y_scr;
{
	double	X,Y;
	double	t_var;
	int 	org;
	int		gene_i;


	org = (org_i*ORG_X) + org_j;
	X = 0.0;
	Y = 0.0;

	for (gene_i = 0; gene_i < G_Population[org].size_chrom; gene_i++) {
		t_var = dpow(t, gene_i);
		X += t_var * G_Population[org].X_Chrom[gene_i];
		Y += t_var * G_Population[org].Y_Chrom[gene_i];
	}
	*X_scr = Tx(X);
	*Y_scr = Ty(Y);
}





/*--------------------------------------------------------------------------*/
/*	DEVELOPF-- grows next point using Fourier--evaluates organism at time t	*/
/*--------------------------------------------------------------------------*/
DevelopF(org_i, org_j, t, X_scr, Y_scr)
int		 org_i, org_j;
double	 t;
int		*X_scr;
int 	*Y_scr;
{
	double	X,Y;
	double	t_var;
	int 	org;
	int		gene_i;


	org = (org_i*ORG_X) + org_j;
	X = 0.0;
	Y = 0.0;

	for (gene_i = 0; gene_i < G_Population[org].size_chrom; gene_i++) {
		t_var = gene_i*t;
		X += cos(t_var) * G_Population[org].X_Chrom[gene_i];
		Y += sin(t_var) * G_Population[org].Y_Chrom[gene_i];
	}
	*X_scr = Tx(X);
	*Y_scr = Ty(Y);
}



/*--------------------------------------------------------------------------*/
/*	DEVELOPFC-- grows next point using Fourier-- with color             */
/*--------------------------------------------------------------------------*/
DevelopFC(org_i, org_j, t, X_scr, Y_scr, C_scr)
int		 org_i, org_j;
double	 t;
int	*X_scr;
int 	*Y_scr;
int     *C_scr;
{
	double	X,Y,C;
	double	t_var;
	int 	org;
	int		gene_i;


	org = (org_i*ORG_X) + org_j;
	X = 0.0;
	Y = 0.0;

	for (gene_i = 0; gene_i < G_Population[org].size_chrom; gene_i++) {
		t_var = gene_i*t;
		X += cos(t_var) * G_Population[org].X_Chrom[gene_i];
		Y += sin(t_var) * G_Population[org].Y_Chrom[gene_i];
		C += cos(t_var) * G_Population[org].C_Chrom[gene_i];
	}
	*X_scr = Tx(X);
	*Y_scr = Ty(Y);
	*C_scr = (int) (127*abs(C))+1; /* 127 is very ad hoc color scaling */
}







/*--------------------------------------------------------------------------*/
/*	DEVELOPFG-- grows next point using Fourier--evaluates organism at time t	*/
/*--------------------------------------------------------------------------*/
DevelopFG(org_i, org_j, t, X_scr, Y_scr)
int		 org_i, org_j;
double	 t;
int		*X_scr;
int 	*Y_scr;
{
	double	X,Y;
	double	freq;
	double	t_var;
	int 	org;
	int		gene_i;


	org = (org_i*ORG_X) + org_j;
	X = 0.0;
	Y = 0.0;

	for (gene_i = 0; gene_i < G_Population[org].size_chrom; gene_i++) {
		freq = 1<<gene_i;
		t_var = freq*t;
		X += (1.0/freq) * cos(t_var) * G_Population[org].X_Chrom[gene_i];
		Y += (1.0/freq) * sin(t_var) * G_Population[org].Y_Chrom[gene_i];
	}
	*X_scr = Tx(X);
	*Y_scr = Ty(Y);
}







/*--------------------------------------------------------------------------*/
/*	DISPLAY GENES-- makes a bargraph of the genes							*/
/*--------------------------------------------------------------------------*/
Display_Genes(org_i,org_j)
int org_i,org_j;
{
	int org, X_low, X_high, Y_low, Y_high;

	org = (org_i*ORG_X) + org_j;
	X_low	= Tx(ORG_X_MIN) + CHROM_OFFSET;
	X_high	= Tx(0.0) - CHROM_OFFSET/2;
	Y_low	= Ty(ORG_Y_MAX);
	Y_high	= Ty((double) (ORG_Y_MAX - ( (ORG_Y_MAX - ORG_Y_MIN)*CHROM_WIND)));
	Graph_Chrom(G_Population[org].X_Chrom, G_Population[org].size_chrom,
				X_low, X_high, Y_low, Y_high);

	X_low	= Tx(0.0) + CHROM_OFFSET/2;
	X_high	= Tx(ORG_X_MAX) - CHROM_OFFSET;
	Graph_Chrom(G_Population[org].Y_Chrom, G_Population[org].size_chrom,
				X_low, X_high, Y_low, Y_high);
}



/*--------------------------------------------------------------------------*/
/*	GRAPH CHROM-- graph one chromosome										*/
/*--------------------------------------------------------------------------*/
Graph_Chrom(chrom, size_chrom, Xl, Xh, Yl, Yh)
Gene*	chrom;
int		size_chrom;
int		Xl, Xh, Yl, Yh;

{
	int i;
	int g, g_width;
	int height;

	int Yo;
	Yo = (Yl + Yh)/2;

	g_width = (Xh-Xl)/(size_chrom+1);
	pw_vector(G_population_pw,	Xl, Yo,
								Xh, Yo,
								PIX_SRC, 1);

	g = Xl;
	for (i=0; i < size_chrom; i++) {
		height = Yl+(Yh-Yl)/2+chrom[i]*(Yh-Yl);
		pw_vector(G_population_pw,	g,	height,
									g+g_width, height,
									PIX_SRC, 1);
		g += g_width;
	}
}
