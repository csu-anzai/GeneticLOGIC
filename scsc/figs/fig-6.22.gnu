set title "Goldberg's tabula rasa rule set (GA turned on)"
set xlabel "Iterations"
set ylabel "Proportion Correct"
set grid
set yrange [0.70:1.0]
set format y "%.02f"
set xtics 0,10000
set terminal postscript eps monochrome "Helvetica" 18
set output "fig-6.22.ps"
plot "fig-6.22.plt" using "%f %f %*f" title "Overall Avg." with lines, \
"fig-6.22.plt" using "%f %*f %f" title "Last 50" with linespoints
