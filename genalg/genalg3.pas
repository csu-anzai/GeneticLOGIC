{$R-}
PROGRAM genetic_algorithm_demo_one_dimensional (input, output);
{program to demonstrate the operation of a genetic algorithm.
from randomly generated population of real numbers, attempts to find
a number arbitrarily close to a goal real number.

Calling convention:

gademo <k> <g> <e> <m> <c> <t> <i>

OR

gademo

Calling without parameters, user will be prompted for values.
k = carrying capacity, integer such that 1 <= k <= 1000
g = goal value, and arbitrary real number
e = error level, integer value, the larger the number, the less error is
     tolerated
m,c,t = real values between 0 and 1 which give the probability of the
         normal operation occurring, where
           m is the probability that mutation will not occur
           c is the probability that crossover will occur and not inversion
           t is the probability that transcription will not occur
i is an integer iteration factor

}

{

Copyright 1990 by Wesley R. Elsberry.  All rights reserved.

Commercial use of this software is prohibited without written consent of
the author.

For information, bug reports, and updates contact

Wesley R. Elsberry
528 Chambers Creek Drive South
Everman, Texas 76140
Telephone: (817) 551-7018 (Voice)

Another point of contact is:

  Central Neural System BBS
  RBBS-Net 8:930/303
  (817) 551-9363
  9600 HST, 2400, 1200, and 300 Baud connects
  8 data bits, no parity, 1 stop bit
  C.N.S. features neural network discussion and simulation related file
  downloads.  C.N.S. is the home of the Neural_Net Echo.  There are no
  access fees charged for use of the C.N.S. BBS.

}


USES DOS, CRT;

const
  cr = ^M;
  lf = ^J;

type
  command_string_ = string[127];
  param_array_ = array[1..9] of command_string_;
  param_rec_ = record
    k : integer;
    g : real;
    e : integer;
    m : real;
    c : real;
    t : real;
    i : integer;
    end;

var
  trr, tss : real;    {variables used in testing of the code}
  tii, tjj : integer; {ditto}
  tinch : char;
  toutf : text;
  paramstrings : param_array_;
  params : param_rec_;

procedure wait;
begin
  tinch := readkey;
end;

procedure demo;
const
  max_k = 999;
  mach_inf = 1E37;
(*  k = 49;                {carrying capacity - 1}
  {smaller carrying capacity = faster generations}
  goal = 3.141592653589793;       {the value to strive for}
    {goal is arbitrary, change to whatever you like}
  error_level = 4096;  {inverse of amount of error allowed in goal completion}
  mutation_threshold = 0.7; {probability that chromosome will not undergo
                              mutation}
  cross2invers = 0.7;   {probability that reproduction method will be crossover
                          as opposed to inversion}
  transcribe = 0.7; {1 - probability that transcription occurs} *)
  debug : boolean = FALSE;

type
  byte_map_ = array[0..5] of byte;    {structure to allow bit manipulation of real}
  population_ = array[0..max_k] of real;  {structure to hold the candidates}
  out_rec_ = record
    s : string[80];
    end;

var
  k : integer;
  goal : real;
  error_level : integer;
  mutation_threshold : real;
  cross2invers : real;
  transcribe : real;
  generation_number : integer; {count of generations}
  max_generations : real; {frustration capacity}
  pop : population_;           {current candidates}
  sel_dist : array[0..max_k] of real;  {probability of each candidate to reproduce one time}
  how_far : array[0..max_k] of real;   {distance of candidate from goal}
  success, finish : boolean;
  close_enough : real; {range about goal that fulfills the requirements}
  ii, jj : integer;
  how_close : real;
  outf : text;
  inch : char;
  raoutf : file of out_rec_;
  tstr1, tstr2 : out_rec_;
  str1 : string[80];
  fom : real;  {Figure Of Merit: Improvement over average random search}

procedure initialize;
var ii, jj : integer;
  rr, ss : real;
begin
  k := params.k - 1; {adjust for base zero}
  goal := params.g;
  error_level := params.e;
  mutation_threshold := params.m;
  cross2invers := params.c;
  transcribe := params.t;

  generation_number := 1;
  success := false;
  close_enough := goal / error_level;
  rr := error_level;
  for jj := 1 to 9 do
    rr := rr * 2;
    {have to match sign and exponent bits as well}
  max_generations := rr / (k+1) / 2.0;
  writeln('Maximum # of generations = ',max_generations:8:3);
  {k is base zero; average random search will find in n/2 experiments}
  randomize;
end;

procedure generate_starting_population; {got to start somewhere}
var ii, jj, kk, ll : integer;
  rr, ss : real;
begin
  for jj := 0 to k do {for each candidate}
    begin  {value of candidate to be spread over several orders of magnitude}
      kk := random(37);
      rr := 1;
      for ii := 1 to kk do
        rr := rr * 10;
      ll :=  random(2);
      if ll = 0 then
        ss := -1.0
      else
        ss := 1.0;
      pop[jj] := random * rr * ss;
    end;
end;

function distance (spot : integer): real;
var  ii : integer;
  rr : real;
begin
  distance := abs(goal - pop[spot]);
end;

function min_distance : real;
var
  ii : integer;
  rr : real;
begin
  rr := mach_inf;
  for ii := 0 to k do
    begin
      if how_far[ii] < rr then
        rr := how_far[ii];
    end;
  min_distance := rr;
end;

function done: boolean;
begin
  success := false;
  done := false;
  how_close := min_distance;
  if how_close <= close_enough then
    success := true;  {success!}
  if (generation_number >= max_generations) then
    done := true; {failure  :(  }
  if success then done := true;
end;

procedure generate_selectionist_distribution;
var
  sum_distance : real;
  ii, jj : integer;
  dist : real;
begin
  sum_distance := 0.0;
  for ii := 0 to k do
    begin
      dist := ln(distance(ii));
      sel_dist[ii] := 1- (1 /(1+exp(- dist)));
      sum_distance := sum_distance +  sel_dist[ii];
    end;
  if debug then
    writeln('Population:      Selectionist Distribution: ');
  for ii := 0 to k do
    begin
      sel_dist[ii] := sel_dist[ii] / sum_distance;
      if debug then
        writeln(pop[ii]:12:6,'     ',sel_dist[ii]:12:6);
    end;
      if debug then
        writeln;
end;

procedure evaluate_population;
var
  ii : integer;
  rr : real;
begin
  for ii := 0 to k do
    begin
      how_far[ii] := distance(ii);
    end;
end;

procedure generate_new_population;
var
  new_pop : population_;
  ogive1, ogive2 : real;
  ii, jj : integer;
  p1, p2 : integer;
  pr1, pr2 : real;
  finished : boolean;

function find_parent(prob : real) : integer;
var
  ii : integer;
  parent : integer;
  so_far : real;
  found : boolean;
begin
  so_far := 0.0;
  found := false;
  ii := 0;
  while not found do
    begin
      so_far := so_far + sel_dist[ii];
      if (prob <= so_far) then
        begin
          found := true;
          parent := ii;
        end;
      ii := ii + 1;
    end;
  find_parent := parent;
end;

function generate_offspring(par1, par2 : real): real;
const
  bits : array[0..7] of byte = (1,2,4,8,16,32,64,128);
  bit7 = 128;
  bit6 = 64;
  bit5 = 32;
  bit4 = 16;
  bit3 = 8;
  bit2 = 4;
  bit1 = 2;
  bit0 = 1;
type
  allele_ = 0..1;                       {will have binary values}
  chromosome_ = array[0..47] of allele_;  {48 bit positions total}
var
  c1, c2, c3, cz : chromosome_;
  rr : real;

procedure print_genotype(gene : chromosome_);
var ii, jj : integer;
begin
 if FALSE then
   begin
  for ii := 0 to 47 do
    begin
      write(gene[ii]:1);
    end;
  writeln;
   end;
end;

procedure phenotype_to_genotype(parent : real; var gene : chromosome_);
var
  ii, gptr : integer;
  b1 : byte_map_ absolute parent;
begin
  gptr := 0;
  for ii := 0 to 5 do
    begin

      if ((b1[ii] and bit7) >= 1) then
        gene[gptr] := 1
      else
        gene[gptr] := 0;

      if ((b1[ii] and bit6) >= 1) then
        gene[gptr+1] := 1
      else
        gene[gptr+1] := 0;

      if ((b1[ii] and bit5) >= 1) then
        gene[gptr+2] := 1
      else
        gene[gptr+2] := 0;

      if ((b1[ii] and bit4) >= 1) then
        gene[gptr+3] := 1
      else
        gene[gptr+3] := 0;

      if ((b1[ii] and bit3) >= 1) then
        gene[gptr+4] := 1
      else
        gene[gptr+4] := 0;

      if ((b1[ii] and bit2) >= 1) then
        gene[gptr+5] := 1
      else
        gene[gptr+5] := 0;

      if ((b1[ii] and bit1) >= 1) then
        gene[gptr+6] := 1
      else
        gene[gptr+6] := 0;

      if ((b1[ii] and bit0) >= 1) then
        gene[gptr+7] := 1
      else
        gene[gptr+7] := 0;

      gptr := gptr + 8;
    end;
end;

procedure genotype_to_phenotype(var parent : real; gene : chromosome_);
var
  bptr, gptr : integer;
  br : real;
  b1 : byte_map_ absolute br;
begin
  gptr := 0;
  for bptr := 0 to 5 do
    begin
      b1[bptr] :=  (gene[gptr] * bit7) + (gene[gptr+1] * bit6) +
                   (gene[gptr+2] * bit5) + (gene[gptr+3] * bit4) +
                   (gene[gptr+4] * bit3) + (gene[gptr+5] * bit2) +
                   (gene[gptr+6] * bit1) + (gene[gptr+7] * bit0);
      gptr := gptr + 8;
    end;
  parent := br;
end;

procedure crossover(gene1, gene2 : chromosome_; var zygote : chromosome_;
                    start, length : integer);
var
  ii, split, jj : integer;
begin
  zygote := gene1;
  for ii := start to (start + length) do
    begin
      jj := ii mod 48;
      zygote [jj] := gene2 [jj];
    end;
end;

procedure inversion (gene1, gene2 : chromosome_; var zygote : chromosome_;
                     start, length : integer);
var
  ii, split, jj, kk : integer;
begin
  zygote := gene1;
  jj := length + 48;
  for ii := start to (start + length) do
    begin
      zygote [jj mod 48] := gene2 [ii mod 48];
      jj := jj - 1;
    end;
end;

procedure mutation (var gene1 : chromosome_; position : integer);
var
  ii : integer;
begin
  if gene1[position] > 0 then
    gene1[position] := 0
  else
    gene1[position] := 1;
end;

procedure transcription (gene1, gene2 : chromosome_; var zygote : chromosome_;
                         start, length : integer);
var
  ii, jj : integer;
  ctmp : chromosome_;
begin
  ii := random(48);
  for jj := start to (start + length) do
    begin
      ctmp[jj mod 48] := gene2[ii mod 48];
      ii := ii + 1;
    end;
  if random <= cross2invers then
    crossover(gene1,ctmp,zygote,start,length)
  else
    inversion(gene1,ctmp,zygote,start,length);
end;


begin  {generate_offspring}
{have not yet included transcription}
  phenotype_to_genotype(par1,c1);
  phenotype_to_genotype(par2,c2);
  rr := random;
  if (rr > mutation_threshold) then
    mutation(c1,random(48));
  rr := random;
  if (rr > mutation_threshold) then
    mutation(c2,random(48));
  rr := random;
  if (rr > transcribe) then
    begin
      phenotype_to_genotype(pop[random(k+1)],c3);
      rr := random(2);
      if (rr = 0) then
        transcription(c1,c3,c1,random(48),random(24))
      else
        transcription(c2,c3,c2,random(48),random(24));
    end;

  rr := random;
  if (rr > cross2invers) then
    begin
      inversion(c1, c2, cz, (random(48)), (random(48)));
    end
  else
    begin
      crossover(c1, c2, cz, random(48), random(48));
    end;

  print_genotype(c1);
  print_genotype(c2);
  print_genotype(cz);
  genotype_to_phenotype(rr, cz);
  generate_offspring := rr;
end;


begin  {generate_new_population}
{  writeln('New population made by: ');}
  for ii := 0 to k do
    begin
      finished := false;
      jj := 0;
      p1 := find_parent(random);
      p2 := find_parent(random);
      pr1 := pop[p1];
      pr2 := pop[p2];
      if debug then
        write(pr1:8:5,' x ',pr2:8:5);
      new_pop[ii] := generate_offspring(pr1,pr2);
      if debug then
        writeln(' = ',new_pop[ii]:8:5);
    end;
      if debug then
        writeln;
  pop := new_pop;
end;

procedure report;
{give out lines that indicate population distribution}
var
  ii, jj : integer;
  ss : real;
  fdist : array[-38..38] of integer;

function order_of_magnitude(rr : real): integer;
const
  ln2log = 0.4342944819;
var ii : integer;
begin
  if (rr <= 0.0) then
    ii := 0
  else
    ii := round(ln2log * ln(rr));
  if ii < -38 then
    ii := -38;
  if ii > 38 then
    ii := 38;
  order_of_magnitude := ii;
end;

function signum(rr : real): integer;
begin
  if (rr >= 0) then
    signum := 1
  else
    signum := -1;
end;


begin {report}
  fillchar(fdist,sizeof(fdist),0);
(*  for ii := 0 to k do
    begin
      jj := signum(pop[ii])*order_of_magnitude(abs(pop[ii]));
      writeln(jj);
      fdist[jj] := fdist[jj] + 1;
    end;  *)
                             {
  if  ((generation_number mod 10) = 0) then
    writeln('Generation ',generation_number);}
  for ii := -38 to 38 do
    begin
      if debug then
        begin
      if fdist[ii] < 1 then
        write('_')
      else
      if fdist[ii] > 9 then
        write('^')
      else
        write(fdist[ii]:1);
        end;
    end;
      if debug then
        begin
  writeln;
  writeln('0                                      E1                                  E38');
  writeln;
        end;
end;

begin  {demo}
  initialize;
  generate_starting_population;
  repeat
      evaluate_population;
      generate_selectionist_distribution;
      finish := done;
      if not finish then
        begin
          generate_new_population;
          report;
          writeln('Within ',(how_close/goal*100):8:5,
                  '% of goal condition in generation ',generation_number);
          generation_number := generation_number + 1;

        end;
  until (finish);

{$I-}
    assign(raoutf,'garesult.dat');
    reset(raoutf);
    CASE IORESULT OF
      $02 :  BEGIN
               ASSIGN(RAOUTF,'GARESULT.DAT');
               REWRITE(RAOUTF);
             END;
    END;
{$I+}
    seek(raoutf,filesize(raoutf)); {go to end of file}
    fillchar(tstr1,sizeof(tstr1),' ');

    fom := max_generations / generation_number;

    str((k+1),str1);
    tstr1.s := 'K:' + str1;
    str(goal:12:8,str1);
    tstr1.s := tstr1.s + ' G:' + str1;
    str(error_level,str1);
    tstr1.s := tstr1.s + ' E:' + str1;
    str(mutation_threshold:5:3,str1);
    tstr1.s := tstr1.s + ' M:' + str1;
    str(cross2invers:5:3,str1);
    tstr1.s := tstr1.s + ' CI:' + str1;
    str(transcribe:5:3,str1);
    tstr1.s := tstr1.s + ' T:' + str1;
    str(fom:8:3,str1);
    tstr1.s := tstr1.s + ' FOM:' + str1;

    tstr1.s[79] := CR;
    tstr1.s[80] := LF;

    write(raoutf,tstr1);
    writeln(tstr1.s);
    fillchar(tstr1,sizeof(tstr1),' ');

  if success then
   begin
    tstr1.s := 'Achieved goal in generation ';
    str(generation_number:6,str1);
    tstr1.s := tstr1.s + str1;
    tstr1.s[79] := CR;
    tstr1.s[80] := LF;
    write(raoutf,tstr1);
    writeln(tstr1.s);
   end
  else
   begin
    tstr1.s := 'No better than ave. random search ';
    str(max_generations:16:0,str1);
    tstr1.s := tstr1.s + str1;
    write(raoutf,tstr1);
    writeln(tstr1.s);
   end;
  close(raoutf);
end;

begin {main}
  if paramcount > 0 then
    begin
      with params do
        begin
          val(paramstr(1),k,tii);
          val(paramstr(2),g,tii);
          val(paramstr(3),e,tii);
          val(paramstr(4),m,tii);
          val(paramstr(5),c,tii);
          val(paramstr(6),t,tii);
          val(paramstr(7),i,tii);
        end;
    end
  else
    begin
      with params do
        begin
          writeln;
          repeat
            write('Enter carrying capacity (1 <= k <= 1000) : ');
            readln(k);
          until (k >= 1) and (k <= 1000);
          repeat
            write('Enter goal value (real number) : ');
            readln(g);
          until true;
          repeat
            write('Enter error level (larger = less error) : ');
            readln(e);
          until (e > 0) and (e <= 32501);
          repeat
            write('Enter mutation threshold : ');
            readln(m);
          until (m > 0.0) and (m <= 1.0);
          repeat
            write('Enter crossover/inversion ratio : ');
            readln(c);
          until (c > 0.0) and (c <= 1.0);
          repeat
            write('Enter transcription threshold : ');
            readln(t);
          until (t > 0.0) and (t <= 1.0);
          repeat
            write('Enter number of experiments : ');
            readln(i);
          until (i > 0) and (i < 101);
        end;
    end;

   for tjj := 1 to params.i do
     begin
       demo;
     end;
end.