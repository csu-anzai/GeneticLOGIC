/* $Id: xtsp.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : xtsp.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef PARIX
#include <malloc.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "define.h"
#include "graphics.h"


#define MIN(x,y)	((x < y) ? x : y)

#define GR_WIDTH	400		/* width of terminal window */
#define GR_HEIGHT	400		/* height of terminal window */
#define BORDER		30		/* width of border */


char	wintitle[] = "XTSP - ParaTSP tour viewer";
Display	*mydisplay;			/* graphics variables */
Window	mywindow;
GC	mygc;
int	pixelsX, pixelsY;

TOWN_COORD minx, maxx, miny, maxy;	/* interval of town coordinates */


/*****************************************************************************/
/* Initialize xterm                                                          */
/*****************************************************************************/
void initX()
{ XSizeHints myhint;
  char *s;
  int myscreen;
  unsigned long myforeground, mybackground;

  s = getenv("DISPLAY");
  if (s != NULL)
  { mydisplay = XOpenDisplay(s);
  }
  else
  { mydisplay = XOpenDisplay("");
  }
  if (mydisplay == NULL)
  { fprintf(stderr, "XTSP: Can't open display...\n");
    exit(1);
  }
  myscreen = DefaultScreen(mydisplay);

  mybackground = WhitePixel(mydisplay, myscreen);
  myforeground = BlackPixel(mydisplay, myscreen);

  myhint.x = 100;
  myhint.y = 100;
  myhint.width = pixelsX;
  myhint.height = pixelsY;
  myhint.flags = PPosition | PSize;

  mywindow = XCreateSimpleWindow(mydisplay, DefaultRootWindow(mydisplay),
    myhint.x, myhint.y, myhint.width, myhint.height, 5, myforeground,
    mybackground);
  XSetStandardProperties(mydisplay, mywindow, wintitle, wintitle, None,
    0, 0, &myhint);

  mygc = XCreateGC(mydisplay, mywindow, 0, 0);
  XSetBackground(mydisplay, mygc, mybackground);
  XSetForeground(mydisplay, mygc, myforeground);

  XSelectInput(mydisplay, mywindow, ButtonPressMask | KeyPressMask);

  XMapWindow(mydisplay, mywindow);
}


/*****************************************************************************/
/* Close xterm                                                               */
/*****************************************************************************/
void closeX()
{
  XFreeGC(mydisplay, mygc);
  XDestroyWindow(mydisplay, mywindow);
  XCloseDisplay(mydisplay);
}


/*****************************************************************************/
/* XTSP for ParaTSP                                                          */
/*****************************************************************************/
int main(argc, argv)
  int argc;		/* number of arguments */
  char **argv;		/* pointer to arguments */
{ XEvent report;
  MYREP tour;
  BOOLEAN ok = TRUE, key = TRUE;
  TOWN *coord;
  TOUR num, i;
  char fname[MAX_STR], s[MAX_STR];
  int gen, x1, y1, x2, y2, mx, my;
  double quality;

  if (argc == 2)
  { sprintf(fname, "%s", argv[1]);
  }
  else
  { sprintf(fname, "graph");
  }

  pixelsX = GR_WIDTH;
  pixelsY = GR_HEIGHT;

  init_X_graph(fname);
  initX();

  num = receive_nodes(&coord);

  minx = coord[0].x;
  maxx = coord[0].x;
  miny = coord[0].y;
  maxy = coord[0].y;
  for (i = 1; i < num; i++)
  { minx = MIN(coord[i].x, minx);
    maxx = MAX(coord[i].x, maxx);
    miny = MIN(coord[i].y, miny);
    maxy = MAX(coord[i].y, maxy);
  }
  for (i = 0; i < num; i++)
  { coord[i].x -= minx;
  }
  for (i = 0; i < num; i++)
  { coord[i].y -= miny;
  }
  maxx -= minx;
  maxy -= miny;
  minx = 0.0;
  miny = 0.0;

  mx = pixelsX - (2 * BORDER);
  my = pixelsY - (2 * BORDER);

  tour.n = num;
  tour.job = (TOUR *) malloc((unsigned long) num * sizeof(TOUR));

  while (key)
  { XNextEvent(mydisplay, &report);
    if (report.type == KeyPress)
    { key = FALSE;
    }
    else
    { if (ok)
      { receive_tour(num, &gen, &quality, &tour);
        if (gen != SEND_EOF)
        { XClearWindow(mydisplay, mywindow);
          sprintf(s, "Generation: %d --> %f km", gen, quality);
          XDrawString(mydisplay, mywindow, mygc, 5, 15, s, strlen(s));
          for (i = 0; i < num; i++)
          { x1 = coord[tour.job[i]].x * mx / maxx + BORDER;
            y1 = coord[tour.job[i]].y * my / maxy + BORDER;
            x2 = coord[tour.job[(i + 1) % num]].x * mx / maxx + BORDER;
            y2 = coord[tour.job[(i + 1) % num]].y * my / maxy + BORDER;
            y1 = pixelsY - y1;
            y2 = pixelsY - y2;
            XDrawLine(mydisplay, mywindow, mygc, x1, y1, x2, y2);
          }
        }
        else
        { ok = FALSE;
        }
      }
    }
  }

  free(tour.job);
  free(coord);

  closeX();
  close_X_graph();

  return(0);
}


/*** end of file ***/
