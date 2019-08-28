set title "Goldberg's default hierarchy rule set (specifity turned off)"
set xlabel "Iterations"
set ylabel "Proportion Correct"
set grid
set yrange [0.80:1.0]
set format y "%.02f"
set terminal postscript eps monochrome "Helvetica" 18
set output "fig-6.19.ps"
plot "fig-6.19.plt" using "%f %f %*f" title "Overall Avg." with lines, \
"fig-6.19.plt" using "%f %*f %f" title "Last 50" with linespoints
