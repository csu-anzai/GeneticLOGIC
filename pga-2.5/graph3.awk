# A GAWK script that processes pga output, assuming one population:
# plots average fitness and best fitness on same graph.
# First collect pga output, using
#     pga ...options... filename
# Then run gawk on that filename using this script, by
#     gawk -f graph3.awk filename
#

BEGIN                     { xg = "xgraph -bb -tk"
                            average = 2
                            best = 3
                            print "YUnitText: fitness" | xg }
/reporting-interval/      { print "XUnitText: generations" | xg 
                            repint = $3 }

$1 == "0"                 { avf[n] = $average
                            bestf[n] = $best
                            n++ }

END                       { for(y=0;y<n;y++) {
                              printf("%d %f\n", y*repint, avf[y]) | xg 
                            }
                            printf("\"average\n\n") | xg 
                            for(y=0;y<n;y++) {
                              printf("%d %f\n", y*repint, bestf[y]) | xg 
                            }
                            printf("\"best\n\n") | xg 
                          }
