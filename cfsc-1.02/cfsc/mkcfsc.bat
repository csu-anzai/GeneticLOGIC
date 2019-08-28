if  exist e:\loaded.MM  GOTO   TLOAD
PAUSE  The Microsoft M model is not loaded!
GOTO END
:TLOAD

SET INCLUDE=E:\CFSCM\;E:\MC\

mkdir                           E:\cfscm
mkdir                           E:\cfscm.f
copy  C:\cfsc\include\*.*       E:\cfscm
datetime >time1
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\bblearn.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\cfmsgio.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\cfops.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\cfsc.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\cfsio.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\cfsutil.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\core.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\display.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\dscops.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\dscops2.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\dsclearn.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\grandcfs.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\usercmd.c ;
if errorlevel 1  GOTO END
cl  /c /G2 /AM /FPi /FoE:\cfscm\ /Oilt     c:\cfsc\utility.c ;
if errorlevel 1  GOTO END

:END
copy e:\cfscm\*.obj  
datetime >time2
type time1
type time2
del time1
del time2
