
function select(popsize:integer; var pop:population):integer;
(* selection like a weighted roulette wheel *)
var spin, partsum:real; j:integer;
begin
 partsum := 0.0;
 j:= 0;
 spin := random;
 repeat
  j:=j+1;
  partsum := partsum + pop[j].pselect;
 until (partsum >= spin) or (j = popsize);
 if traceison then writeln('partsum,spin=',partsum,spin);
 select := j;
end;

procedure scale(popsize:integer; max,sumfitness:real; var pop:population);
var j:integer;
begin
 for j:=1 to popsize do with pop[j] do
  begin
   fitness:=max-cost;
   pselect:=fitness/sumfitness;
   if traceison then writeln('j,fitness,pselect=',j,' ',fitness,' ',pselect);
  end
end;

procedure cross_tour(n_city,lo_cross,hi_cross:city;
                     var tour1_old,tour2_old,tour1_new,tour2_new:tourarray);
var j1,hi_test:integer;
begin
 if traceison then writeln('lo_cross,hi_cross=',lo_cross,' ',hi_cross);
 hi_test := hi_cross + 1; if (hi_test>n_city) then hi_test:=1;
 tour1_new := tour1_old;
 tour2_new := tour2_old;
 if ( (lo_cross <> hi_cross) and (lo_cross <> hi_test) ) then begin
   j1 := lo_cross;
   while (j1<>hi_test) do begin (* mapped crossover on both tours *)
    swap_city(j1,find_city(tour1_old[j1],n_city,tour2_new),tour2_new);
    swap_city(j1,find_city(tour2_old[j1],n_city,tour1_new),tour1_new);
    j1:=j1+1; if (j1>n_city) then j1:= 1;
   end;
  end;
end;

function add_one(j,n_city:integer):city;
begin j:=j+1; if (j>n_city) then j:=1; add_one:=j end;

function sub_one(j,n_city:integer):city;
begin j:=j-1; if (j<=0) then j:=n_city ;sub_one:=j end;

procedure inversion (lo_inv,hi_inv,n_city:city; var tour:tourarray);
var j1,n_swaps:integer;
begin
 n_swaps := hi_inv - lo_inv + 1; if (n_swaps<=0) then n_swaps:=n_swaps+n_city;
 n_swaps:=n_swaps div 2;
 if traceison then writeln('lo_inv,hi_inv,n_swaps=',lo_inv,' ',hi_inv,' '
                           ,n_swaps);
  if (n_swaps>0) then for j1:=1 to n_swaps do begin
   swap_city(lo_inv,hi_inv,tour);
   lo_inv := add_one(lo_inv,n_city);
   hi_inv := sub_one(hi_inv,n_city);
  end;
end;

procedure generation;
var j,j1,mate1,mate2:integer;

begin (* generation *)
 j:=1;
 repeat
  mate1:= select(popsize,oldpop);
  mate2:= select(popsize,oldpop);
  if traceison then writeln('mate1,mate2=',mate1,' ',mate2);
    (* partially matched crossover *)
  if flip(pcross) then begin cross_tour(n_city,rnd(1,lchrom),rnd(1,lchrom),
                                       oldpop[mate1].chrom,oldpop[mate2].chrom,
                                       newpop[j].chrom,newpop[j+1].chrom);
                             ncross:=ncross+1 end
     else (*no cross*) begin
       newpop[j].chrom   := oldpop[mate1].chrom;
       newpop[j+1].chrom := oldpop[mate2].chrom;
     end;
    (* inversion *)
  if flip(pinversion) then begin inversion(rnd(1,lchrom),rnd(1,lchrom),n_city,
                                           newpop[j].chrom);
                                 ninversion:=ninversion+1; end;
  if flip(pinversion) then begin inversion(rnd(1,lchrom),rnd(1,lchrom),n_city,
                                           newpop[j+1].chrom);
                                 ninversion:=ninversion+1; end;
    (* cost evaluation *)
  for j1:=j to j+1 do with newpop[j1] do begin
     tour_norm(1,n_city,chrom);           (* normalize tour to city 1 *)
     cost:=objfunc(n_city,chrom,costmat); (* evaluate objective function *)
    end;
    (* increment pop counter *)
  j:=j+2;
 until j>popsize;
end;




