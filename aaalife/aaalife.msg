From: acero@tank.uchicago.edu (Anibal Acero)
Newsgroups: comp.binaries.ibm.pc
Subject: v04i004: aaalife, Conway's "Life" simulation
Date: 13 Jul 89 04:49:17 GMT

AAALIFE is a very fast implementation of Conway's Life using high
resolution graphics.  [The archive contains] aaalife.c (the source code
with more explanatory comments), aaalife.exe the executable program
which supports EGA/VGA, CGA and Hercules graphics, makelife.bat a batch
file showing the Turbo C switches used to compile the program, and
rpent.lif, a data.

tony acero

[
In the seventies John Conway, a mathematician, invented a set of rules
for the growth of cells.  He called it the game of Life.  Martin
Gardener popularized this in his "Mathematical Games" column in the
Scientific American.  Numerous articles appeared about Life in computer
magazines, and there was even a newsletter (called "Lifelines," I
think) dedicated to this game.  If you're new to the microcomputer
world, you might want to go through old issues of Byte, to capture some
of the excitement the old days when microcomputers were in their
infancy and a lot of people (mostly wives of hobbyists) were asking,
"But what do you DO with it?"  For many, the best answer they could
find was, "Well...er...you can play Life with it" (word processing
hadn't yet been invented).

What is fascinating about Life was that its rules are extremely simple,
yet they can result in complex "organisms" some of which recreate
themselves at intervals, some of which move at a steady rate, and one
or two that actually shoot out projectiles.  I seem to remember an old
issue of Byte that was dedicated to Life.

This implementation is colorful and fast.  The data file needs to be
edited by hand to initialize the configuration, but one configuration
is included.  Be patient: it may seem rather slow.

Supports CGA, VGA, EGA, and HGC.

-- R.D.
]
