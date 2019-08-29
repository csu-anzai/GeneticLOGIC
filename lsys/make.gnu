#!/bin/sh
# This invokes make with command-line options to override the compiler
#   and parsing tools defined therein, forcing use of g++ and the GNU
#   tools. This should be changed to define INCLUDEDIR to the appropriate
#   location of your GNU header files, but this doesn't affect anything
#   except 'make depend'.
# You can add MEMLIB=-lmalloc for DECstations; the script attempts to
#   determine this automatically.

MEMLIB=
if fgrep -is ultrix /usr/include/stdio.h ; then
    echo Assuming you\'re using Ultrix, linking with -lmalloc
    MEMLIB=-lmalloc
fi

make CC=g++ OLDC=gcc YACC="bison -y" LEX=flex PARSELIBS= \
    INCLUDEDIR=/usr/softlab/contrib/lib/mips_ultrix/g++-1.39-include $*
