# A slightly fancier GAWK script to show how one can plot a graph from
# pga output. This one plots multiple lines on the same graph.
# First collect pga output, using
#     pga ...options... filename
# Then run gawk on that filename using this script, by
#     gawk -f graph2.awk filename
#

BEGIN                     { average = 2
                            best = 3
                            print "YUnitText: fitness" | "xgraph" }
/reporting-interval/      { print "XUnitText: generations" | "xgraph"
                            repint = $3 }

$1 == "pops"              { popc = ($3) - 1 }

($1 > 0)  && ($1 < popc)  { data[$1,n] = $average }
$1 == popc                { data[$1,n] = $average
                            n++ }

END                       { for(x=0;x<=popc;x++) {
                              for(y=0;y<n;y++) {
                              printf("%d %f\n", y*repint, data[x,y]) | "xgraph"
                              }
                              printf("\"pop %d\n\n", x) | "xgraph"
                            }
                          }

# Users of versions of awk other than gawk, please note:
# some versions of awk do not like multidimensional arrays
# as used above, eg data[$1,n]. Gawk, like all awks, treats
# all array subscripts as strings and manufactures a string
# when it finds a multidimensional array. If your awk doesn't
# handle this for you, you can just replace a term such as
#    data[$1,n]
# with
#    data[$1 "," n]
# and it should then work. Such a replacement has to happen
# in three places in the above script.
