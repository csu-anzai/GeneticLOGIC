ECHO OFF
REM  Compile  LETSEQ1.C module, link with CFS-C, M model, 80286
if  exist e:\loaded.MM  GOTO   BEGIN
ECHO ON
PAUSE  The Microsoft M model is not loaded!
ECHO OFF
GOTO END

:BEGIN
IF "%1"=="l"  GOTO LINK
cl /c /G2 /AM /FPi /Ic:\letseq1 /FoE:\cfscm\letseq1.obj /Oilt c:\letseq1\letseq1.c ;
if errorlevel 1  GOTO END
copy  E:\cfscm\letseq1.obj   
:LINK
if not exist E:\cfscm\letseq1.obj  copy  letseq1.obj  E:\cfscm
link /NOD @makelet.lnk
del  E:\cfscm\letseq1.obj
:END
