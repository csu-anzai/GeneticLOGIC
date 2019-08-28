program scs;

{ SCS -  A Simple Classifier System }
{   (C)  David E. Goldberg, 1987    }
{        All Rights Reserved        }

{$I declare.scs }
{$I random.apb }
{$I io.scs }
{$I utility.scs }
{$I environ.scs }
{$I detector.scs }
{$I perform.scs }
{$I aoc.scs }
{$I effector.scs }
{$I reinforc.scs }
{$I timekeep.scs }
{$I advance.scs }
{$I ga.scs }
{$I report.scs }
{$I initial.scs }

begin { main }
  initialization;
  detectors(environrec, detectrec, envmessage);
  report(rep);
  with timekeeprec do repeat
    timekeeper(timekeeprec);
    environment(environrec);
    detectors(environrec, detectrec, envmessage);
    matchclassifiers(population, envmessage, matchlist);
    aoc(population, matchlist, clearingrec);
    effector(population, clearingrec, environrec);
    reinforcement(reinforcementrec, population, clearingrec, environrec);
    if reportflag then report(rep);
    if consolereportflag then  consolereport(reinforcementrec);
    if plotreportflag then plotreport(pfile, reinforcementrec);
    advance(clearingrec);
    if gaflag then begin
      ga(garec, population);
      if reportflag then reportga(rep, garec, population);
     end;
  until halt;
  report(rep);  { final report }
  close(pfile); { close plot file }
end.

