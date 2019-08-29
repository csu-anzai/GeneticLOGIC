
#include "INCLUDE:Lattice/math.h"
#include "INCLUDE:Lattice/stdio.h"
#include "INCLUDE:exec/types.h"
#include "INCLUDE:graphics/gfx.h"
#include "INCLUDE:graphics/gfxmacros.h"
#include "INCLUDE:exec/exec.h"
#include "INCLUDE:exec/execbase.h"
#include "INCLUDE:graphics/view.h"
#include "INCLUDE:graphics/gfxbase.h"
#include "INCLUDE:exec/libraries.h"
#include "INCLUDE:intuition/intuition.h"
#include "INCLUDE:graphics/text.h"

struct  NewScreen  ns;
struct  NewWindow  nw;

struct  Screen     *evo_screen;
struct  Window     *evo_window, *map_window, *text_window;

struct  RastPort   *rp, *rp2;
struct  ViewPort   *vp;

struct  IntuiMessage  *message;

struct  TextAttr       Ruby_Eight;
struct  TextFont       *fontptr;

struct  GfxBase       *GfxBase;
struct  IntuitionBase *IntuitionBase;
struct  Library       *DiskfontBase;

SHORT   hominid;
SHORT   hominid_index[621];

int     i, j, x, y, oldx, class, code, rc, color, parity;
int     delta_t, delta_x, delta_y, s, fossil_index;
int     point, selected_box;
BOOL    button_is_down, first_pass;

UBYTE   profile[ 621 ][ 52 ][ 2 ];

int     SetUpEvographics(), DrawMap(), DisplayText();

#include "evo_species.include"
#include "evo_data.include"
#include "evo_africa.include"
#include "evo_text.include"

/*---------------------------------------*/
/*                                       */
/*             //  Evo  //               */
/*                                       */
/*    A human evolution demonstration    */
/*        program for the Amiga          */
/*                                       */
/*            Steve Bonner               */
/*            August 1987                */
/*                                       */
/*---------------------------------------*/

main()
{
  register int k;

GfxBase = (struct GfxBase *)OpenLibrary("graphics.library",0);
IntuitionBase = (struct IntuitionBase *)OpenLibrary("intuition.library",0);

if( (GfxBase == NULL) || (IntuitionBase == NULL)) exit();

DiskfontBase = OpenLibrary("diskfont.library",0);
if ( DiskfontBase == NULL ) exit();


SetUpEvographics();

SetDrMd( rp, JAM2 );
SetAPen( rp, 2 );

RectFill( rp, 45, 20, 295, 120 );
Move( rp, 110, 55 );

SetAPen( rp, 7 );
SetBPen( rp, 2 );
Text( rp, "(please wait)", 13 );

SetAPen( rp, 2 );
SetBPen( rp, 0 );

Move( rp,  20, 140 );
Draw( rp, 620, 140 );

for ( i = 0; i <= 20; i++)        {
  Move( rp, gradation[i], 138 );
  Draw( rp, gradation[i], 142 );  }

Move( rp, 9, 137 );
Text( rp,
  "20            10            5   4   3      2        1  ", 55 );
Move( rp, 9, 152 );
Text( rp,
  "        Miocene         Pliocene            Pleistocene", 55 );

Move( rp, 325, 45 )    ;
Text( rp, "Genus       ", 12 );
Move( rp, 325, 60 )    ;
Text( rp, "Species     ", 12 );
Move( rp, 325, 75 )    ;
Text( rp, "Subspecies  ", 12 );
Move( rp, 325, 90 )    ;
Text( rp, "Mill yrs ago", 12 );

Move( rp, 435, 45 );
Text( rp, ":",  1 );
Move( rp, 435, 60 );
Text( rp, ":",  1 );
Move( rp, 435, 75 );
Text( rp, ":",  1 );
Move( rp, 435, 90 );
Text( rp, ":",  1 );


/* Draw the boxes to surround user options:  */
for ( k = 0; k < 5; k++)           {
  SetAPen( rp, 3 );
  RectFill( rp, 20 + k * 122, 172, 132 + k * 122, 192 );
  SetAPen( rp, 6 );
  Move( rp,  20 + k * 122 , 172 );
  Draw( rp, 132 + k * 122 , 172 );
  Draw( rp, 132 + k * 122 , 192 );
  Draw( rp,  20 + k * 122 , 192 );
  Draw( rp,  20 + k * 122 , 172 ); }

SetDrMd( rp, JAM1 );
Move( rp, 20, 185 );
/*  Here are the various options:  */
Text( rp,
   "    Help         Text          Map        Glossary      Notes",
   61 );

SetAPen( rp, 2 );
SetDrMd( rp, JAM2 );

x = 0;
y = 1;

/*  The cranium for each hominid species consists of 52 points    */
/*  which define its outline.  Here we are copying the outlines   */
/*  of the nine known hominids into our working array, 'profile'  */
for ( i = 0; i <= 51; i++) {
  j = 2*i;
  k = j+1;

  profile[  20 ][ i ][ x ] = proconsul_profile[ j ];
  profile[  20 ][ i ][ y ] = proconsul_profile[ k ];
  profile[ 147 ][ i ][ x ] = kenyapith_profile[ j ];
  profile[ 147 ][ i ][ y ] = kenyapith_profile[ k ];
  profile[ 333 ][ i ][ x ] = afarensis_profile[ j ];
  profile[ 333 ][ i ][ y ] = afarensis_profile[ k ];
  profile[ 360 ][ i ][ x ] = africanus_profile[ j ];
  profile[ 360 ][ i ][ y ] = africanus_profile[ k ];
  profile[ 403 ][ i ][ x ] = habilis_profile  [ j ];
  profile[ 403 ][ i ][ y ] = habilis_profile  [ k ];
  profile[ 439 ][ i ][ x ] = erectus_profile  [ j ];
  profile[ 439 ][ i ][ y ] = erectus_profile  [ k ];
  profile[ 554 ][ i ][ x ] = esapiens_profile [ j ];
  profile[ 554 ][ i ][ y ] = esapiens_profile [ k ];
  profile[ 611 ][ i ][ x ] = neander_profile  [ j ];
  profile[ 611 ][ i ][ y ] = neander_profile  [ k ];
  profile[ 620 ][ i ][ x ] = msapiens_profile [ j ];
  profile[ 620 ][ i ][ y ] = msapiens_profile [ k ];  }


/*  Interpolate between known hominid forms:   */
for ( i = PROCONSUL; i <= NEANDER; i++)  {
  delta_t = fossil_dates[i+1] - fossil_dates[i];
  fossil_index = fossil_dates[i];

  Forbid();

  /* Now consider each point along the profile: */
  for ( j = 0; j <= 51; j++ ) {

    delta_x = profile[ fossil_dates[i+1] ][ j ][ x ] -
              profile[ fossil_index      ][ j ][ x ] ;
    delta_y = profile[ fossil_dates[i+1] ][ j ][ y ] -
              profile[ fossil_index      ][ j ][ y ] ;
    s = 1;

    /*  Here we do the linear interpolation.                         */
    /*  (I interpolate on the logarithmic scale, since it's easier.) */
    for ( k = fossil_dates[i]+1; k < fossil_dates[i+1]; k++)        {
      profile[ k ][ j ][ x ] = profile[ fossil_index ][ j ][ x ] +
              s * delta_x / delta_t;
      profile[ k ][ j ][ y ] = profile[ fossil_index ][ j ][ y ] +
              s * delta_y / delta_t;
      s++;
                                                                    }
                           }  /*  j  */
    /* Let's give our disk drive a chance to catch up: */
    Permit();
                           }  /*  i  */



button_is_down = FALSE;

oldx = 403;
SetAPen( rp, 1 );
WritePixel( rp, oldx, 140 );
hominid = 4;

Move( rp, 460, 45 );
Text( rp, genera[ hominid ], 18 );
Move( rp, 460, 60 );
Text( rp, species[ hominid ], 18 );
Move( rp, 460, 90 );
Text( rp, yearstring[oldx], 6 );


/*  Hominid_divisions allows us to rapidly map an x-coordinate    */
/*  into its corresponding hominid form (an integer from 0 to 8)  */
hominid = 8;
for (i = 620; i >= 20; i--)   {
  if ( i < hominid_divisions[ hominid ] ) hominid--;
  hominid_index[i] = hominid; }

color = 8;

hominid = 4;
first_pass = TRUE;

Forbid();

while ( 1 == 1) {

  if (first_pass == FALSE )    {
    WaitPort( evo_window -> UserPort );
    message = GetMsg( evo_window -> UserPort );

    class = message -> Class;
    code  = message -> Code;
    x     = message -> MouseX;
    y     = message -> MouseY;

    ReplyMsg( message );       }

  else                         {
    class = 0;
    code  = SELECTUP;
    x     = oldx;
    y     = 140;
    first_pass = FALSE;        }


  if ( class == CLOSEWINDOW )   {
    CloseWindow( evo_window );
    CloseScreen( evo_screen );
    exit();                     }

  if ( code == SELECTDOWN ) {

    point = ReadPixel( rp, x, y );

    /*  Is our current pixel a 'user-option' color?. . .  */
    if ( (point == 3) || (point == 6) ) {
      selected_box = (x - 10)/122;

      if (selected_box == 2)
        DrawMap();
      else
        DisplayText();

      goto NoSkull;                     }

    button_is_down = TRUE;
    color = 8;              }

  if ( code == SELECTUP   ) button_is_down = FALSE;

  if ( (button_is_down) || (code == SELECTUP) )   {

    if ((x >= 20) && (x <= 620)) {
    SetAPen( rp, 2 );
    WritePixel( rp, oldx, 140 );
    SetAPen( rp, 1 );
    WritePixel( rp,    x, 140 );
    oldx = x;

    /*  If the species has changed, update the labels:  */
    if ( hominid != hominid_index[x] )       {
      hominid = hominid_index[x];

      Move( rp, 460, 45 );
      Text( rp, genera[ hominid ], 18 );
      Move( rp, 460, 60 );
      Text( rp, species[ hominid ], 18 );
      Move( rp, 460, 75 );
      Text( rp, subspecies[ hominid ], 18 ); }

    Move( rp, 460, 90 );
    Text( rp, yearstring[x], 6 );

    if ( code == SELECTUP )              {
      color = 1;
      SetAPen( rp, 2 );
      RectFill( rp, 45, 20, 295, 120 );  }
    else                                 {
      color++;
      color%=16;
      if (color < 8) color += 8;         }

    parity = x % 2;

    /*  we cheat a little to speed things up:   */
    if ((code != SELECTUP) && (code != SELECTDOWN) && (parity == 1)) {
      goto FastForward; }

    SetAPen( rp, color );

    Move( rp, profile[ x ][ 0 ][ 0 ], profile[ x ][ 0 ][ 1 ] );

    /* Draw the outside of the cranium: */
    Disable();
    /* OK, let's see if it can keep up. . .       */
    for ( k = 1; k < 27; k++ ) {
      Draw( rp, profile[x][k][0], profile[x][k][1] ); }
    Enable();

    Draw( rp, profile[x][0][0], profile[x][0][1] );

    /* Now for the lower cheekbone */
    Move( rp, profile[x][27][0], profile[x][27][1] );
    for ( k=28; k < 30; k++) Draw( rp, profile[x][k][0], profile[x][k][1] );

    /* Now the upper cheekbone:    */
    Move( rp, profile[x][30][0], profile[x][30][1] );
    for ( k=31; k < 39; k++) Draw( rp, profile[x][k][0], profile[x][k][1] );

    /* Now draw around the eye:    */
    Move( rp, profile[x][39][0], profile[x][39][1] );
    for ( k=40; k < 47; k++) Draw( rp, profile[x][k][0], profile[x][k][1] );
    Draw( rp, profile[x][39][0], profile[x][39][1] );

    /* Now draw around the ear:    */
    Move( rp, profile[x][47][0], profile[x][47][1] );
    for ( k=48; k < 52; k++) Draw( rp, profile[x][k][0], profile[x][k][1] );
    Draw( rp, profile[x][47][0], profile[x][47][1] );

    FastForward:
                      }    /*   x in range 20..620   */

                      }    /*     Button is down     */
    NoSkull: 
                      }    /*       While End        */


}  /* main */

/*--------------------------------------------*/

SetUpEvographics()
{
  int  font_avail;
  BOOL response;
  struct IntuiText nofont, stop;
  
  /*  Use ruby font in 80-column size:  */
  Ruby_Eight.ta_Name = "ruby.font";
  Ruby_Eight.ta_YSize = 8;
  Ruby_Eight.ta_Style = 0;
  Ruby_Eight.ta_Flags = 0;

  

  font_avail = OpenDiskFont(&Ruby_Eight);


  /*  If the ruby font is not available, display a requester and exit:  */
  if ( font_avail == FALSE ) {
    nofont.FrontPen  = 3;
    nofont.BackPen   = 2;
    nofont.DrawMode  = JAM1;
    nofont.LeftEdge  = 10;
    nofont.TopEdge   =  5;
    nofont.ITextFont = NULL;
    nofont.IText     = "Ruby font not in font directory.\0";
    nofont.NextText  = NULL;

    stop.FrontPen  =  2;
    stop.BackPen   =  3;
    stop.DrawMode  =  JAM1;
    stop.LeftEdge  =  4;
    stop.TopEdge   =  3;
    stop.ITextFont = NULL;
    stop.IText     = "exit\0";
    stop.NextText  = NULL;

    response = AutoRequest(NULL, &nofont, NULL, &stop, 0,0,420,60);
    exit();         }

  /*  Define properties new screen is to have:        */
  ns.LeftEdge     =   0;
  ns.TopEdge      =   0;
  ns.Width        = 640;
  ns.Height       = 200;
  ns.Depth        =   4;
  ns.DetailPen    =   1;
  ns.BlockPen     =   2;
  ns.ViewModes    = HIRES;
  ns.Type         = CUSTOMSCREEN;
  ns.Font         = &Ruby_Eight;
  ns.DefaultTitle = "    \0";
  ns.Gadgets      = NULL;
  ns.CustomBitMap = NULL;

  evo_screen = OpenScreen( &ns );
  if ( evo_screen == NULL ) exit();

  /*   Describe new window:      */
  nw.LeftEdge    =    0;
  nw.TopEdge     =    0;
  nw.Width       =  640;
  nw.Height      =  200;
  nw.DetailPen   =   -1;
  nw.BlockPen    =   -1;
  nw.IDCMPFlags  = CLOSEWINDOW   | MOUSEBUTTONS  | MOUSEMOVE;
  nw.Type        = CUSTOMSCREEN;
  nw.Flags       = WINDOWCLOSE   | SMART_REFRESH | ACTIVATE |
                   NOCAREREFRESH | REPORTMOUSE   ;
  nw.FirstGadget = NULL;
  nw.CheckMark   = NULL;
  nw.Screen      = evo_screen;
  nw.Title       = " Evo ";
  nw.BitMap      = NULL;
  nw.MinWidth    =    0;
  nw.MinHeight   =    0;
  nw.MaxWidth    =    0;
  nw.MaxHeight   =    0;

  evo_window = OpenWindow( &nw );
  if (evo_window == NULL)    exit();

  WindowToFront(evo_window);

  rp = evo_window  -> RPort;
  vp = &evo_screen -> ViewPort;

  SetRGB4( vp,  0,  7,  2,  3 );     /*  plum    */
  SetRGB4( vp,  1, 10, 10,  7 );     /*  brown   */
  SetRGB4( vp,  2,  0,  0,  0 );     /*  black   */
  SetRGB4( vp,  3, 10,  5,  0 );     /*  brown   */
  SetRGB4( vp,  4, 13, 13,  0 );     /*  yellow  */
  SetRGB4( vp,  5, 11,  7,  0 );     /*  tan     */
  SetRGB4( vp,  6,  0,  0,  0 );     /*  black   */
  SetRGB4( vp,  7,  0, 10,  0 );     /*  green   */

  /*  The colors used to slide from one hominid into another: */
  SetRGB4( vp,  8,  15,  0,  1 );
  SetRGB4( vp,  9,  14,  0,  2 );
  SetRGB4( vp, 10,  13,  0,  3 );
  SetRGB4( vp, 11,  12,  0,  4 );
  SetRGB4( vp, 12,  11,  0,  5 );
  SetRGB4( vp, 13,  10,  0,  6 );
  SetRGB4( vp, 14,   9,  0,  7 );
  SetRGB4( vp, 15,   8,  0,  8 );

}

/*-------------------------------------------------*/
DrawMap()
{
  PLANEPTR  temp_bitplane;
  struct    TmpRas         temp_rast;
  struct    IntuiMessage  *message2;
  int       num_ticks, blue_level, b;

  message = 5;
  /*  Dispose of any extraneous messages waiting at our old port:  */
  while ( message != NULL )                        {
    message = GetMsg( evo_window -> UserPort );
    message2 = message;
    if ( message2 != NULL ) ReplyMsg( message2 );   }


  /*   Describe map window:      */
  nw.IDCMPFlags  = CLOSEWINDOW | INTUITICKS;
  nw.Flags       = WINDOWCLOSE | ACTIVATE;
  nw.Title       = " Map : African Hominid Fossil Sites ";

  for( i = 0; i < 8; i++)  SetRGB4( vp,  8+i,    0,  0,  i+6 );

  map_window = OpenWindow( &nw );
  if (map_window == NULL)    exit();

  rp2 = map_window -> RPort;

  temp_bitplane = AllocRaster(640,200);
  if (temp_bitplane == 0) exit();

  rp2 -> TmpRas = InitTmpRas( &temp_rast, temp_bitplane, RASSIZE(640,200));

  color = 8;
  for ( i = 10; i < 200; i+= 2) {
    color ++;
    color %= 8;
    color += 8;
    SetAPen( rp2, color );
    Move( rp2,   1,  i  );
    Draw( rp2, 638,  i  );
    Move( rp2,   1, i+1 );
    Draw( rp2, 638, i+1 );      } 

  SetAPen( rp2, 2 );

  Move( rp2, africa[0], africa[1] + 12 );
  for (i = 2; i < sizeof(africa) / 2; i+=2)   {
    Draw( rp2, africa[i], africa[i+1] + 12 ); }
  Draw( rp2, africa[0], africa[1] + 12 );

  for( i = 0; i < 142; i+=2)            {
    features[ i ] = feature[ i ];
    features[i+1] = feature[i+1] + 12;  }

  Move( rp2, features[0], features[1] );
  for (i = 0; i < 5; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[0], features[1] );

  Move( rp2, features[6], features[7] );
  for (i = 6; i < 13; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[6], features[7] );

  Move( rp2, features[14], features[15] );
  for (i = 14; i < 21; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[14], features[15] );

  Move( rp2, features[22], features[23] );
  for (i = 22; i < 39; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[22], features[23] );

  Move( rp2, features[40], features[41] );
  for (i = 40; i < 45; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[40], features[41] );

  Move( rp2, features[46], features[47] );
  for (i = 46; i < 55; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[46], features[47] );

  Move( rp2, features[56], features[57] );
  for (i = 56; i < 63; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[56], features[57] );

  Move( rp2, features[64], features[65] );
  for (i = 64; i < 73; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[64], features[65] );

  Move( rp2, features[74], features[75] );
  for (i = 74; i < 81; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[74], features[75] );

  Move( rp2, features[82], features[83] );
  for (i = 82; i < 109; i+=2) Draw(rp2,features[i],features[i+1]);
  Draw( rp2, features[82], features[83] );

  SetAPen( rp2, 5 );
  SetOPen( rp2, 2 );
  Flood( rp2, 0, 320, 100 );

  /*  Plot the fossil sites:  */
  SetAPen( rp2, 2 );
  for (i = 110; i < 141; i+=2)                     {
    WritePixel( rp2,features[i],  features[i+1]);
    WritePixel( rp2,features[i]+1,features[i+1]);  }

  SetDrMd( rp2, JAM1 );
  SetAPen( rp2, 4 );

  for (i = 0; i < 16; i++)            {
    Move( rp2, 510, 30 + i*10 );
    Text( rp2, fossil_sites[i], 13 ); }

  SetAPen( rp2, 2 );
  for ( i = 0; i < 16; i++ )     {
    Move( rp2, features[110 + 2*i] + 4, features[111 + 2*i] );
    Draw( rp2, 505, 26 + i*10 ); }

  class      = 0;
  num_ticks  = 0;
  blue_level = 6;

  while ( class != CLOSEWINDOW )        {
    WaitPort( map_window -> UserPort );
    message2 = GetMsg( map_window -> UserPort );
    class = message2 -> Class;
    ReplyMsg( message2 );

    if ( class == INTUITICKS )     {

      /*  Every fourth tick, cycle the background colors: */
      if ( num_ticks++ > 3 ) {
        num_ticks = 0;
        blue_level ++;
        if (blue_level > 13) blue_level = 7;
        
        for( i = 0; i < 8; i++)               {
          b = blue_level + i;
          b %= 8;
          b += 6;
          SetRGB4( vp,  8+i,    0,  0,  b );  }
                             }
                                   }
                                       }

  CloseWindow( map_window );

  /*  Restore colors to those used by the main Evo window: */
  SetRGB4( vp,  8,  15,  0,  1 );
  SetRGB4( vp,  9,  14,  0,  2 );
  SetRGB4( vp, 10,  13,  0,  3 );
  SetRGB4( vp, 11,  12,  0,  4 );
  SetRGB4( vp, 12,  11,  0,  5 );
  SetRGB4( vp, 13,  10,  0,  6 );
  SetRGB4( vp, 14,   9,  0,  7 );
  SetRGB4( vp, 15,   8,  0,  8 );

}

/*----------------------------------------------*/

DisplayText()
{
  struct    IntuiMessage  *message2;
  char     *text_ptr, delimiter;
  int       red, green, blue, intensity;
  ULONG     seconds, micros;


  message = 5;
  /*  Dispose of any extraneous messages waiting at our old port:  */
  while ( message != NULL )                         {
    message = GetMsg( evo_window -> UserPort );
    message2 = message;
    if ( message2 != NULL ) ReplyMsg( message2 );   }


  /*   Describe map window:      */
  nw.IDCMPFlags  = CLOSEWINDOW;
  nw.Flags       = WINDOWCLOSE | ACTIVATE;

  if ( selected_box == 1 )
    nw.Title = hominid_title[ hominid ];
  else
    nw.Title = window_title[ selected_box ];

  text_window = OpenWindow( &nw );
  if (text_window == NULL)    exit();

  rp2 = text_window -> RPort;

  CurrentTime(&seconds, &micros);
  srand(micros);

  red   = rand() % 15;
  green = rand() % 12;
  blue  = rand() %  9;

  intensity = red + green + blue;

  if (intensity < 26)             {
    red   +=2;
    green +=2;
    if ( red   > 15 ) red   = 15;
    if ( green > 15 ) green = 15; }

  SetRGB4( vp, 0, red, green, blue );

  SetDrMd( rp2, JAM1 );
  SetAPen( rp2, 2    );

  i = 0;
  delimiter = ' ';

  while ( delimiter != '?' )    {

    if ( selected_box == 1 )                                       {
      /*   We'll pretend there are just 3 for now:                */

      if ( hominid == PROCONSUL ) text_ptr = proconsul_text[i];
      if ( hominid == KENYAPITH ) text_ptr = kenyapith_text[i];
      if ( hominid == AFARENSIS ) text_ptr = afarensis_text[i];
      if ( hominid == AFRICANUS ) text_ptr = africanus_text[i];
      if ( hominid == HABILIS   ) text_ptr = habilis_text[i];
      if ( hominid == ERECTUS   ) text_ptr = erectus_text[i];
      if ( hominid == ESAPIENS  ) text_ptr = esapiens_text[i];
      if ( hominid == NEANDER   ) text_ptr = neander_text[i];
      if ( hominid == MSAPIENS  ) text_ptr = msapiens_text[i];
                                                                   }
    else                                                           {
      if (selected_box == 0) text_ptr = help_text[i];
      if (selected_box == 3) text_ptr = glossary_text[i];
      if (selected_box == 4) text_ptr = notes_text[i];             }

    delimiter = *text_ptr;

    if ( delimiter != '?' ) {
      Move( rp2, 40, 18  + 10 * i );
      Text( rp2, text_ptr, 56 );
      i++;                  }
                                }

  WaitPort( text_window -> UserPort );
  message2 = GetMsg( text_window -> UserPort );
  ReplyMsg( message2 );

  SetRGB4( vp,  0,  7,  2,  3 );     /*  plum    */

  /*  The only message we were expecting was the window closure:  */
  CloseWindow( text_window );

}
