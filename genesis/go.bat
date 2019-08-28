echo off
echo making executables ...
make
echo running ga %1 ...
ga %1
echo making report ...
if x == x%1 goto NULL
echo rep.%1 for ga %1 > rep.%1
report %1 >> rep.%1
goto DONE
:NULL
echo rep for ga > rep
report >> rep
:DONE
echo done.
