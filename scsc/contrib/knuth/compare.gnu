set title "Differences between Knuth's original and Goldberg's implementations"
set xlabel "Application times"
set ylabel "Pseudo random value"
set xrange [1:20]
plot "a" title "Knuth (FORTRAN)" with linespoints, \
     "b" title "Knuth (C)" with linespoints, \
     "c" title "Goldberg (SCS-C)" with linespoints, \
     "d" title "Goldberg (SGA-C)" with linespoints
pause -1 "[PRESS RETURN]"
set xrange [980:1000]
replot
pause -1 "[PRESS RETURN]"
