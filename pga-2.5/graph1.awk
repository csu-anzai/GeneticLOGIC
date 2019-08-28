# A simple AWK script to show how one can plot a graph from
# pga output, pretty simply. First collect pga output, using
#     pga ...options... filename
# Then run awk on that filename using this script, by
#     awk -f graph1.awk filename
# (use the excellent gawk from GNU so you can keep your AWK
# scripts in a single standard place?)
#
# In the script below, the `best' fitness for population 1
# gets plotted. The `$1 == 1' selects lines whose first field
# is "1", the third field in such a line is the `best' fitness
# (the second, $2, is the average) and n is just a counter,
# auto-initialised  to zero.

BEGIN                 { print "YUnitText: fitness" | "xgraph"}
/reporting-interval/  { print "XUnitText: gens x " $3 | "xgraph"}
$1 == 1               { print n " " $3 | "xgraph"
                        n++ }
END                   { print "\"pop 1" | "xgraph"}
 
# The average fitness typically rises smoothly, the best fitness
# typically increases stepwise. But it's problem dependent.
#
# The output to file is laid out in such a way that it is easy
# for an awk script to pick up the values of the various parameters,
# such as the reporting interval, used above when labelling the X axis.
#
# Note that xgraph accepts lines such as
#   XUnitText: gens x 10
#   YUnitText: fitness
# to label the axes, and (anywhere in the data set, eg at the end)
#   "pop 1
# to label the line (the quote does not have to be paired with a closing
# quote). You may also like to add
#   TitleText: whatever
# - see the xgraph man page for this and other options.
