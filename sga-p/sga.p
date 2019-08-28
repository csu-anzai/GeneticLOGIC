(************************************************************************)
(*									*)
(*									*)
(*  DESCRIPTION								*)
(*	This is the SGA code from Appendix C of Goldberg's book 	*)
(*	"Genetic Algorithms in Search, Optimisation & Machine		*)
(*	Learning" minus the comments (I couldn't be bothered		*)
(*	typing them in).						*)
(*									*)
(*	It appears to be a working version but don't quote me		*)
(*	on that	since I pulled it out of archive in response to		*)
(*	requests for it on the net -- I actually typed it in over	*)
(*	18 months ago so my memory is rather hazy as to what it		*)
(*	was even supposed to do.					*)
(*									*)
(*  NOTE								*)
(*	Note that the Turbo Pascal PC compiler (Borland Int'l) is	*)
(*	needed to compile it.						*)
(*									*)
(*  CHANGES								*)
(*	21/08/93  should now work with all Pascal dialects (-joke)	*)
(*									*)
(*  POSTED								*)
(*	To comp.ai.genetics: Fri, 16 Apr 1993 22:17:02 GMT,		*)
(*	by Mark Juric <mjuric@aisun4.ai.uga.edu>			*)
(*									*)
(*  AUTHOR(S)								*)
(*	Copyright (c) 1989 David E. Goldberg				*)
(*		Illinois Genetic Algorithms Lab				*)
(*      	University of Illinois at Urbana-Champaign		*)
(*      	117 Transportation Building				*)
(*      	104 South Mathews Avenue				*)
(*      	Urbana IL, 61801, USA					*)
(*									*)
(*	Copyright (c) 1992 Gary Gibson, CIS Dept.,			*)
(*		University of South Australia				*)
(*									*)
(*	Copyright (c) 1993 Joerg Heitkoetter, CS Dept., LSXI		*)
(*		University of Dortmund, Germany				*)
(*		Net: <joke@santafe.edu>					*)
(*									*)
(************************************************************************)

program sga;

const
  maxpop		= 100;
  maxstring		= 30;

type
  allele		= boolean;

  chromosome		= array [1..maxstring] of allele;

  individual		= record
			    chrom: chromosome;
			    x: real;
			    fitness: real;
			    parent1,
			    parent2,
			    xsite: integer;
			  end;

  population		= array [1..maxpop] of individual;

var
  oldpop,
  newpop		: population;

  popsize,
  lchrom,
  ncross,
  gen,
  maxgen		: integer;

  pcross,
  pmutation,
  sumfitness,
  nmutation,
  avg,
  max,
  min			: real;

(******************************************************************)

function power (x, y: real): real;
begin
  power := exp (y * ln (x));
end;

function rand: real;
begin
  rand := (random (32767) * 32767.0) / 23767.0;
end;

function flip (probability: real): boolean;
begin
  if probability = 1.0 then
    flip := true
  else
    flip := (rand <= probability);
end;

function rnd (low, high: integer): integer;
begin
  rnd := trunc (random (high - low)) + low;
end;

procedure clrscr;
begin
end;

procedure pause;
begin
  writeln;
  writeln ('Press <RETURN> to continue...');
  readln;
end;


function objfunc (x: real): real;
const
  coef = 1073741823.0;
  n    = 10;
begin
  objfunc := power (x / coef, n);
end;

function decode (chrom: chromosome; lbits: integer): real;
var
  j: integer;
  accum,
  powerof2: real;
begin
  accum := 0.0;
  powerof2 := 1.0;
  for j := 1 to lbits do begin
    if chrom[j] then
      accum:= accum + powerof2;
    powerof2 := powerof2 * 2;
  end;
  decode:= accum;
end;


procedure statistics (pop: population);
var
  j: integer;
begin
  sumfitness := pop[1].fitness;
  min := pop[1].fitness;
  max := pop[1].fitness;
  for j:= 2 to popsize do
    with pop[j] do
      begin
        sumfitness := sumfitness + fitness;
        if fitness > max then
	  max:= fitness;
        if fitness < min then
	  min:= fitness;
      end;
  avg := sumfitness / popsize;
end;

procedure initdata;
begin
  clrscr;
  writeln ('       SGA: A Simple Genetic Algorithm Program');
  writeln ('       ---------------------------------------');
  writeln;
  write ('Enter Population Size: '); readln (popsize);
  write ('    Chromosome Length: '); readln (lchrom);
  write ('      Max Generations: '); readln (maxgen);
  write ('Crossover Probability: '); readln (pcross);
  write (' Mutation Probability: '); readln (pmutation);

  nmutation := 0;
  ncross := 0;
  writeln;
  pause;
end;

procedure initreport;
begin
  writeln ('Initial Generation Statistics');
  writeln;
  writeln ('Maximum Fitness: ', max);
  writeln ('Average Fitness: ', avg);
  writeln ('Minimum Fitness: ', min);
  writeln (' Sum of Fitness: ', sumfitness);
  pause;
end;

procedure initpop;
var
  j, j1: integer;
begin
  for j := 1 to popsize do
    with oldpop[j] do
      begin
        for j1:= 1 to lchrom do
	  chrom[j1]:= flip (0.5);
        x := decode (chrom, lchrom);
        fitness := objfunc (x);
        parent1 := 0;
        parent2 := 0;
        xsite := 0;
  end;
end;

procedure initialize;
begin
  initdata;
  initpop;
  statistics (oldpop);
  initreport;
end;

procedure writechrom (chrom: chromosome);
var
  j: integer;
begin
  for j:= lchrom downto 1 do
    if chrom[j] then
      write ('1')
    else
      write ('0')
end;

procedure report;
var
  j: integer;
begin
  clrscr;
  writeln;
  writeln ('---------------------------------------------------------------');
  writeln ('                    Population Report');
  writeln;
  writeln ('Generation: ',gen);
  writeln;
  writeln ('------- String ---------------- X ---------- Fitness ------');
  for j:= 1 to popsize do
    begin
      with newpop[j] do
        begin
          writechrom (chrom);
          writeln (' ', x:10, ' ', fitness:6:4);
        end;
    end;
  writeln;
  writeln;
  writeln ('       max: ', max:6:4,'      min: ', min:6:4, '    avg: ', avg:6:4);
  writeln ('sumfitness: ', sumfitness:6:4, ' nmutation: ', nmutation:6,
    ' ncross: ', ncross);

  writeln('---------------------------------------------------------------');
  pause;
end;


function select: integer;
var
  r, partsum: real;
  j: integer;
begin
  partsum := 0.0;
  j := 0;
  r := rand * sumfitness;
  repeat
    j:= j + 1;
    partsum := partsum + oldpop[j].fitness;
  until (partsum >= r) or (j = popsize);
  select:= j;
end;

function mutation (alleleval: allele): allele;
var
  mutate: boolean;
begin
  mutate := flip (pmutation);
  if mutate then
    begin
      nmutation := nmutation + 1;
      mutation := not alleleval;
    end
  else
    mutation := alleleval;
end;

procedure crossover(parent1, parent2: chromosome;
                    var child1, child2: chromosome;
                    var jcross: integer);
var
  j: integer;
begin
  if flip (pcross) then
    begin
      jcross:= rnd (1, lchrom-1);
      ncross:= ncross + 1;
    end
  else
    jcross:= lchrom;

  for j:= 1 to jcross do
    begin
      child1[j]:= mutation (parent1[j]);
      child2[j]:= mutation (parent2[j]);
    end;
  if jcross<> lchrom then
    begin
      for j:= jcross+1 to lchrom do
        begin
          child1[j]:= mutation (parent2[j]);
          child2[j]:= mutation (parent1[j]);
        end;
    end;
end;


procedure generation;
var
  j, mate1, mate2, jcross: integer;
begin
  j := 1;
  repeat
    mate1:= select;
    mate2:= select;
    crossover (oldpop[mate1].chrom, oldpop[mate2].chrom, newpop[j].chrom,
              newpop[j+1].chrom,jcross);
    with newpop[j] do
      begin
        x := decode (chrom,lchrom);
        fitness := objfunc (x);
        parent1 := mate1;
        parent2 := mate2;
        xsite := jcross;
      end;
    with newpop[j+1] do
      begin
        x:= decode (chrom, lchrom);
        fitness := objfunc (x);
        parent1 := mate1;
        parent2 := mate2;
        xsite := jcross;
      end;
    j := j + 2;
  until j>= popsize;
end;


begin
  gen:= 0;
  initialize;
  repeat
    gen:= gen + 1;
    generation;
    statistics (newpop);
    report;
    oldpop := newpop;
  until (gen >= maxgen);
end.
