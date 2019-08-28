set title "Goldberg's fig 6.19"
set xlabel "Iterations"
set ylabel "Proportion Correct"
set grid
set yrange [0.3:1]
plot "plot.out" using "%f %f %*f" title "Overall Avg." with lines, \
"plot.out" using "%f %*f %f" title "Last 50" with linespoints
pause -1 "[PRESS RETURN]"
