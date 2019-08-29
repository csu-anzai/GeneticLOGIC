\ Evolution by Russ Yost
\
\ Simulate the evolution of bugs, the insect kind, that eat
\ bacteria.  The bugs genes control the directions they turn.
\ Bugs that find the most bacteria to eat survive better
\ and pass their genes to their offspring.  Mutation and
\ other aspects of evolution are modelled. See ReadMe file.
\
\ This program has been placed in the Public Domain by the
\ author and may be freely redistributed if accompanied
\ by the ReadMe file.
\
\ This program was written using JForth Professional 2.0
\ from Delta Research

getmodule includes
include? gr.init ju:amiga_graph
include? ?closebox ju:amiga_events 
include? :struct ju:c_struct
include? wchoose ju:random
include? value ju:value

anew task-bugs
forth definitions

newwindow bugwindow

10 constant gr_xmin   620 constant gr_xmax
gr_xmax gr_xmin - constant gr_xspan

5 constant gr_ymin   137 constant gr_ymax
gr_ymax gr_ymin - constant gr_yspan

65534 constant sf \ scale factor for random integer genes
65535 constant sf+1 \ random number multiplier

750 constant bact0   variable bact-rate variable bugs-to-do
variable g.e.mode   false g.e.mode !
4 constant g.e.lim \ of each of 12  uniform dist'n summed to get
\ pseudo-Gaussian distribution.
10 constant max.tries 

\ variable n-time   

32 constant n-rpt \ output status after processing n-rpt bugs.

40 constant bacte    600 constant bugemax 10 constant bugs0

variable numbugs     

variable mature   200 mature ! variable too.old  
mature @ 10 * too.old !
variable strong   400 strong !
variable child.e   40 child.e !
variable child.e.half   false child.e.half !

create xmoves   0 , 4 , 4 , 0 , -4 , -4 ,
create ymoves   2 , 1 , -1 , -2 , -1 , 1 ,

: get.params \ future use to set parameters. May require changing
\ some constants to variables.
;

6 array geneavs

:struct bugdat
  aptr bg_prev
  aptr bg_next
  short bg_x
  short bg_y
  short bg_dir
  7 4 * bytes bg_gene
  short bg_e
  short bg_a
;struct

\ Set up initial bugstructs in fast mem.

0 value bug0    0 value bug1 \ Inlz self-fetching bugpointers.

: make.bug0 ( -- bgptr )
  memf_fast sizeof() bugdat allocblock
  dup 0= abort" Can't make bug0." dup -> bug0
;

: make.bug1 ( --  bgptr )
  memf_fast sizeof() bugdat allocblock
  dup 0= abort" Can't make bug1." dup -> bug1
;

: bact.set ( x, y -- , paint color 3 bacterium,  )
  gr.color@ >r 3 gr.color!
  2dup gr.move  gr.draw
  r> gr.color! ;


: init.rand rand-seed >abs dup call intuition_lib CurrentTime drop ;




: rx gr_xmax gr_xmin wchoose 2/ 2* ; \ To align bugs and bacts
: ry gr_ymax gr_ymin wchoose ;       \ and to reduce search time in eating.
: rdir 6 choose ;

: grx ( -- x ) \ Gaussian distn at ctr of window
    0   12 0 do
    g.e.lim 2* 1+ dup negate 1+ wchoose +
               loop
    gr_xmax gr_xmin + 2/ + 2/ 2*
;

: gry ( -- y ) \ Gaussian distn at ctr of window
      0   12 0 do
               g.e.lim 1+ dup negate 1+ wchoose + 
	       loop
      gr_ymax gr_ymin + 2/ +
;

: g.e.rand.bact.set ( -- ) \ look for clear spot for max.tries, 
                           \ put bact in Raleigh Distn.
  0 ( inlz counter ) 0 dup ( inlz x, y )
  begin
    2drop 1+ ( inc. cntr ) grx gry 2dup
    gr-currport @ -rot call graphics_lib ReadPixel
    0=
    3 pick max.tries >   or
  until
  >r >r drop r> r>
  bact.set
;
: rand.bact.set ( --   ; look for a clear spot before putting a bact. ) 
  0 dup
  begin
    2drop rx ry 2dup
    gr-currport @ -rot call graphics_lib ReadPixel
    0=
  until
  bact.set
;

: rand.bacts.add ( -- )
  bact-rate @
  begin
  dup 0>
  while
    g.e.mode @ if g.e.rand.bact.set
               else rand.bact.set
	       then
    1-
  repeat
  drop
;


: getxy ( bgptr -- bgptr x y )
  dup ..@ bg_x over ..@ bg_y
;

: bug.paint ( bgptr  -- bgptr ) ( uses existing gr.color )
 \ if = background color, clears existing bug.
  getxy ( -- bgptr, x, y  ; x,y is lower left corner of 4x2 bug )
  1- over 3 + over 1+ gr.rect
;

: bug.set ( bgptr  -- bgptr )
  gr.color@ >r 
  dup ..@ bg_e dup 
  child.e @ 80 + >
  if drop 1
  else child.e @ > 
    if   3 else  2
    then
  then
  gr.color!
  bug.paint
  r> gr.color!
;
: bug.clear ( bgptr -- bgptr )
  gr.color@ >r 0 gr.color!
  bug.paint  
  r> gr.color!
;



: make,link.inl.pair ( -- )
  bug0 dup 0= if drop make.bug0 then
  bug1 dup 0= if drop make.bug1 then
  over over ..! bg_prev
  over over ..! bg_next
  swap
  over over ..! bg_prev
            ..! bg_next
;

: inlz.initial.bug.genes ( bgptr -- bgptr )
  dup .. bg_gene
  0 over !
  7 1 do 6000 choose
         over i 1- cells + @ +  
         over i cells + ! loop drop
;

: inlz.xydea ( bgptr -- bgptr )
  rx over ..! bg_x
  ry over ..! bg_y
  rdir over ..! bg_dir
  bacte over ..! bg_e
  0 over ..! bg_a
;

: make.nu.bug ( bgptr --  nubgptr )
  memf_fast   sizeof() bugdat allocblock ( bgptr,  nubgptr | false)
  dup 0= abort" Can't make new bug. " 
  ( bgptr, nubgptr )
    over ..@ bg_next dup >r ( b0ptr , nubptr, nxtbptr )
  over ..! bg_next ( b0ptr, nubgptr )
  r@ ..@ bg_prev over ..! bg_prev
  dup rot ..! bg_next
  dup r> ..! bg_prev
  1 numbugs +!
;  


: inlz.bug.data ( bgptr -- bgptr )
  inlz.initial.bug.genes inlz.xydea bug.set
;

: inlz.set.bugs ( -- )
  make,link.inl.pair
  bug0 inlz.bug.data drop bug1 inlz.bug.data drop
  2 numbugs !
  bug0 \ add nu bugs after bug0.
  bugs0 2 - 0 do make.nu.bug inlz.bug.data  loop drop
;

: get.next.bug ( bgptr -- nextbgptr )
  ..@ bg_next
;

: .gr.prob. ( n --  ; print n as decimal fractn )
  " ." gr.text   dup 100 < 
  if 0 gr.number
  then  dup 10 <
  if 0 gr.number
  then
  gr.number
;

: .avg.genes ( bgptr -- bgptr )
  6 0 do 0 i geneavs ! loop
  dup >r \ save for comparison to detect complete chain
  begin ( bgptr )
    dup .. bg_gene ( bgptr, genebase )
    7 1 do   dup i cells + @
          over i 1- cells + @
	  -   1000   2 pick 24 + @   */
	  i 1- geneavs +!
	loop   drop ( bgptr )
    get.next.bug ( nextbgptr )   dup r@ =
  until 
  r> drop   numbugs @
  6 0 do i geneavs @ over / .gr.prob.  " ; " gr.text 
      loop   drop
;

: report ( bgptr, -- bgptr )
    0 145
" Turn, degrees :                       0     60    120   180   240   300"
    gr.xytext
    0 155 " # Bugs: " gr.xytext numbugs @ 
    dup 100 < if "  " gr.text then
    dup 10  < if "  " gr.text then gr.number
    " ; Avg Gene Probabilities: " gr.text    .avg.genes
    520 170 "      " gr.xytext
    520 170 gr.move bact-rate @ 
    bacte * 2/   gr.number 
;

: test.kill.bug ( bgptr -- bgptr | nextbgptr )
  numbugs @ 2 >
  if
    dup ..@ bg_e 0=
    over ..@ bg_a   too.old @ >   or
    if
      bug.clear
      dup ..@ bg_prev
      over ..@ bg_next
      dup 2 pick ..! bg_next
      over over ..! bg_prev
      -rot drop ( nextbugptr, bgptr )
      dup bug0 = if 0 -> bug0  else dup bug1 = if 0 -> bug1  then then
      freeblock \ Return dead bug's memory space.
      -1 numbugs +!
      report
    then
  else
    dup ..@ bg_e 0 max over ..! bg_e \ Keep last two bugs energy positive. 
  then
; 

: getturn ( bugstructureptr -- same; bugdir[n] changed per genes )
  dup .. bg_gene
  dup 6 cells + @ choose ( bptr, g0, pturn )
  0 dup ( bptr, g0, pturn, ix, T0 )
  begin
    2 pick <   
    swap 4 +    dup 24 <   rot and
  while  ( bp, g0, pturn, index )
    dup 3 pick + @   
  repeat ( bp, g0, pturn, selected index )
  4 / 2-   >r 2drop r> ( bp, selected.turn in 0..5 range )
  over ..@ bg_dir + 6 mod
  over ..! bg_dir
;

: getnuxy ( bgptr -- bgptr )
  dup ..@ bg_dir dup >r
  cells   xmoves + @ (  bugptr, delx )
  over ..@ bg_x + (  bugptr, newx )
  gr_xmax over <
  if gr_xspan -
  else gr_xmin over >
    if gr_xspan +
    then
  then
  over ..! bg_x (  bugptr )
  r> ( get dir )
  cells ymoves + @
  over ..@ bg_y + ( bgptr, ytrial )
  gr_ymax over <
  if gr_yspan -
  else gr_ymin over >
    if gr_yspan +
    then
  then
  over ..! bg_y
  dup ..@ bg_a 1+
  over ..! bg_a
  dup ..@ bg_e 1-
  over ..! bg_e
;

: eat.bact ( bgptr, x, y, port, xf, port, xf, yf --- bgptr, x, y, port, xf )
  call graphics_lib ReadPixel ( bgptr, x, y, port, xf, color.id )
  3 =
  if
     
     4 pick ..@ bg_e   bacte +
     dup bugemax  >
     if
       drop bugemax
     then
     5 pick ..! bg_e
  then
;
: eat.bg.bacts ( bgptr -- bgptr )
  dup  ..@ bg_x over ..@ bg_y  gr-currport @ ( bgptr, x, y, port )
  4 0 do
          2 pick  i + ( bp, x, y, port, xf
	  2 0  do 
	          2dup 4 pick i -   eat.bact
               loop drop 
          2 +loop 2drop drop
;


: copy.bug.data ( srcbgptr, destbgptr -- same )
  over ..@ bg_x    over ..! bg_x
  over ..@ bg_y    over ..! bg_y
  over ..@ bg_dir  over ..! bg_dir
  over ..  bg_gene over ..  bg_gene 28 move
;

: div.all.genes.by.2 ( bptr -- bptr )
  dup .. bg_gene
  28 4 do dup i + dup @ 2/ swap !
          cell +loop drop
;

: inc.rand.gene ( bgptr -- )
  rdir   1+   dup >r
  over .. bg_gene   dup >r
  swap cells +    dup @
  swap 4 - @   - ( bgptr, Ti-Ti-1 )
  r> r> cells
  begin ( bgptr, delT, g0, ix )
    2dup  +   3 pick swap +!
    4 +   dup 24 >
  until
  + 4 - @ ( bgptr, delT, T6 [=sum] )
  >r drop r>   
  65535 > if div.all.genes.by.2   then drop
; 
    
: dec.rand.gene ( bgptr -- )
  rdir 1+ dup >r
  over .. bg_gene   dup >r
  swap cells + dup @
  swap 4 - @   -   2/ negate ( bgptr, delT )
  r> r> cells ( bp, delT, g0, ix )
  begin
    2dup + dup @ 4 pick +   swap !
    4 +   dup 24 >
  until 
  2drop 2drop
;

: fission ( bgptr -- bgptr )
  dup ..@ bg_a   mature @ >
  if
    dup ..@ bg_e   strong @ >
    if
      dup make.nu.bug ( oldbgptr, nubgptr )
      copy.bug.data ( oldbgptr, nubgptr )
      over inc.rand.gene
      dup dec.rand.gene
      child.e.half @ 
      if
        over ..@ bg_e 2/
      else
        child.e @
      then
      dup 3 pick ..! bg_e
      over ..! bg_e ( oldbgptr, nubugptr )
      0 dup 2 pick ..! bg_a 2 pick ..! bg_a   drop
      report
    then
  then
;




: chng.bact ( 67 | 68 | 69 --  )
\ If n = 67, toggle g.e.mode.
\ If n = 68, decrements bact-rate by 1, but not < 0;
\ If n = 69, increments bact-rate by 1;
\ else doesn't change nubact.
  case 67 of g.e.mode dup @ true xor swap ! 0 endof
       68 of -1     endof
       69 of  1     endof
       0 swap
  endcase    bact-rate +!
  bact-rate dup @ 0<
  if 0 swap ! else drop then  
;

: make.buttons ( -- ) ( Adapted from Delta Research )
  0 160   60 175   gr.rect
  80 160   140 175   gr.rect
  160 160   310 175   gr.rect
  JAM1 gr.mode!   0 gr.color!
  10 170 " Dec." gr.xytext
  90 170 " Inc." gr.xytext
  170 170 " Uniform/Conc." gr.xytext
  JAM2 gr.mode!   1 gr.color!
;

: check.input ( bugptr -- bugptr quitflag )
	gr-curwindow @ ev.getclass ?dup
	IF
		CASE
			MOUSEBUTTONS OF
				ev-last-code @ SELECTDOWN =  
				IF ev.getxy00 ( Xm, Ym -- )   150 >
         		  IF ( Xm )
					  dup 320 <
					  if
					    dup 80 <
							if
							  drop 68 chng.bact report
							else
						    160 <
								if 
									69 chng.bact report
								else
								  67 chng.bact
								then
							then
					  else drop
					  then
				  else drop
				  then
				then false
			endof
			CLOSEWINDOW of   true   endof
			false   swap
		endcase
	else false
	then
;   

: evolve ( -- )
  gr.init ( to be certain )
  bugwindow  newwindow.setup 
  bugwindow 
  0" Bug Evolution R.Yost" >abs over  ..! nw_Title
  190 swap ..! nw_Height
  CLOSEWINDOW MOUSEBUTTONS | bugwindow ..! nw_idcmpflags
  bugwindow gr.opencurw not abort" Couldn't open window."
  1 gr.color! \ Set foreground color to white.
	make.buttons
	320 170 " New bacts supplied for ~       bugs" gr.xytext
  init.rand
  get.params 
  bact0 bact-rate ! 
  rand.bacts.add
  1 bact-rate !
   inlz.set.bugs
  numbugs @ 2* bugs-to-do !
  bug0
  report
  begin
	n-rpt 0 DO
    eat.bg.bacts
    bug.set
    test.kill.bug
    get.next.bug
    fission
    getturn
    bug.clear
    getnuxy
    bugs-to-do dup >r @ 1- dup r> ! 0=
    if 
       rand.bacts.add
       numbugs @ 2* bugs-to-do ! 
    then
	LOOP
	check.input    
  until
  drop  gr.closecurw gr.term 
;

cr ." Enter:    EVOLVE     to run program." cr
