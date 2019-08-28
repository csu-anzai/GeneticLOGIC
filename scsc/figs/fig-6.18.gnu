set title "Goldberg's perfect rule set"
set xlabel "Iterations"
set ylabel "Proportion Correct"
set grid
set yrange [0.90:1.0]
set format y "%.02f"
set terminal postscript eps monochrome "Helvetica" 18
set output "fig-6.18.ps"
plot "fig-6.18.plt" using "%f %f %*f" title "Overall Avg." with lines, \
"fig-6.18.plt" using "%f %*f %f" title "Last 50" with linespoints
