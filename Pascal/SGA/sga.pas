program sga;
{ A Simple Genetic Algorithm - SGA - v1.0 }
{ (c)   David Edward Goldberg  1986       }
{       All Rights Reserved               }

const maxpop       = 100;
      maxstring    = 30;

type  allele       = boolean; { Allele = bit position }
      chromosome   = array[1..maxstring] of allele; { String of bits }
      individual   = record
                       chrom:chromosome; { Genotype = bit string }
                       x:real;           { Phenotype = unsigned integer }
                       fitness:real;     { Objective function value }
                       parent1, parent2, xsite:integer; { parents & cross pt }
                     end;
      population   = array[1..maxpop] of individual;

var   oldpop, newpop:population;            { Two non-overlapping populations }
      popsize, lchrom, gen, maxgen:integer; { Integer global variables }
      pcross, pmutation, sumfitness:real;   { Real global variables }
      nmutation, ncross:integer;            { Integer statistics }
      avg, max, min:real;                   { Real statistics }

 { Include utility procedures and functions }
 {$I utility.sga }

 { Include pseudo-random number generator and random utilities }
 {$I random.apb }

 { Include interface routines: decode and objfunc }
 {$I interfac.sga }

 { Include statistics calculations: statistics }
 {$I stats.sga }

 { Include init. routines: initialize, initdata, initpop, initreport }
 {$I initial.sga }

 { Include report routines: report, writechrom }
 {$I report.sga }

 { Include the 3 operators: select (reproduction), crossover, mutation }
 {$I triops.sga }

 { Include new population generation routine: generation }
 {$I generate.sga }

begin      { Main program }
 gen := 0;    { Set things up }
 initialize;
 repeat       { Main iterative loop }
  gen := gen + 1;
  generation;
  statistics(popsize, max, avg, min, sumfitness, newpop);
  report(gen);
  oldpop := newpop; { advance the generation }
 until (gen >= maxgen)
end.       { End main program }



