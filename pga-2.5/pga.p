.TH PGA 1 LOCAL "2 March 1992"
.SH NAME
pga - the (pseudo)-parallel genetic algorithm testbed
.SH SYNOPSIS
.B pga
[ 
.B \-P<n>
] [
.B \-p<n>
] [
.B -l<n>
] [
.B -i<n>
] [
.B -m<n>
] [
.B -n<n>
] [
.B -c<n>
] [
.B -b<n>
] [
.B -a
] [
.B -t
] [
.B -C<op>
] [
.B -s<op>
] [
.B -r<op>
] [
.B -e<fn>
] [
.B -S<n>
] [
.B -h
] [
.I file
]
.SH DESCRIPTION
.I pga
runs a multi-population genetic algorithm, as determined 
by the various parameter settings, and displays the reuslts
on the screen. If a file is given, results are also
periodically appended to it.
.sp
The possible options are:
.TP
.B \-P
sets the number of populations.
.TP
.B \-p
sets the number of chromosomes per populations.
.TP
.B \-l
sets the number of generations to cycle before the program
asks you whether to restart, continue or quit.
.TP
.B \-i
sets the interval in generations between screen updates.
.TP
.B \-M
sets the interval in generations between migration of some selected
chromosome to all populations. An interval of zero prevents any migrations.
.TP
.B \-m
sets the per-bit mutation rate.
.TP
.B \-n
sets the chromosome length. The chrosmosome decoding algorithm copes
with this.
.TP
.B \-c
sets the crossover rate to be used in
.I gen
reproduction.
.TP
.B \-b
sets the selection bias to be used in
.I rank
type reproduction
.TP
.B \-a
selects adaptive mutation so that mutation rate gets lower if parents
are very dissimilar.
.TP
.B \-t
makes crossover produce two complementary children rather than one.
.TP
.B \-C
sets the crossover type: either
.I one
or
.I two
or
.I uniform
.TP
.B \-s
sets the selection operator: either
.I rank
or
.I fitprop
or
.I tn<n>
or
.I tm<n>
.TP
.B \-r
sets the reproduction operator: either
.I one
or
.I gen
or
.I ssone<n>
or
.I ssgen<n>
.TP
.B \-e
chooses the problem function: one of
.I max,
.I dj1,
.I dj2,
.I dj3,
.I dj5,
.I bf6,
.I knap
or
.I rr.
.TP
.B -S<n>
seeds the random number generator.
.TP
.B \-h
prints a brief option summary and exits.
.SH "SEE ALSO"
The document "About PGA" by Peter Ross
.SH "DIAGNOSTICS"
Should be self explanatory.
.SH AUTHOR
Peter Ross, Geoffrey H. Ballinger.
