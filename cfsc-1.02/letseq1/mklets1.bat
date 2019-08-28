ECHO OFF
REM  Compile  LETSEQ1.C module, link with CFS-C, M model, F point
if  exist e:\loaded.MM  GOTO   BEGIN
ECHO ON
PAUSE  The Microsoft M model is not loaded!
ECHO OFF
GOTO END

:BEGIN
IF "%1"=="l"  GOTO LINK
if exist e:lets1.exe  del e:lets1.exe
cl /c /G2 /AM /FPi /Ic:\letseq1 /FoE:\cfscm\lets1.obj /Oilt c:\letseq1\letseq1.c ;
if errorlevel 1  GOTO END
copy  E:\cfscm\lets1.obj

:LINK
if not exist E:\cfscm\lets1.obj   copy  lets1.obj   E:\cfscm
link /NOD @mklets1.lnk
REM copy lets1.exe e:
:END
