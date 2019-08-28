program simple3a;
{ simple 3a: a program to flip 20 coins and keep track of heads ans tails
  uses repeat--until construct }

{ $Id: simple3a.p,v 1.1 1993/02/15 10:06:27 joke Exp $ }

const
	ncoins = 20;		{ number of coins }
	probability = 0.5;	{ probability of heads turning up }

var
	heads_or_tails: array[1..2] of integer; { heads or tails count }
	j: integer;		{ loop counter }
	toss: boolean;		{ toss: true=heads, false=tails }

begin	{ Main program }
	heads_or_tails[1] := 0;
	heads_or_tails[2] := 0;
	randomize;
	j := 0;
	repeat			{ coin toss loop }
		toss := flip (probability);
		if toss then
			heads_or_tails[1] := heads_or_tails[1] + 1
		else
			heads_or_tails[2] := heads_or_tails[2] + 1;
		j := j + 1
	until (j = ncoins);
	writeln ('In ', ncoins, ' coin tosses there were ',
		heads_or_tails[1], ' heads and ', heads_or_tails[2],
		' tails')
end.	{ Main program }
