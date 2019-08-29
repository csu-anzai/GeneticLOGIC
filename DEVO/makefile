
ALL: devo.exe

devo.exe: devo.obj my_scree.obj devospac.obj getstuff.obj save_res.obj \
	energy.obj repo.obj block.obj draw.obj
	link /ST:20000 devo+my_scree+devospac+getstuff+save_res+energy+\
	repo+block+draw;

devo.obj: devo.c devospac.h devo.h
	qcl /c /AC /G2 /Gs /O devo.c

repo.obj: repo.c devospac.h devo.h
	qcl /c /AC /G2 /Gs /O repo.c

draw.obj: draw.c devospac.h devo.h
	qcl /c /AC /G2 /Gs /O draw.c

my_scree.obj: my_scree.c devo.h
	qcl /c /AC /G2 /Gs /O my_scree.c

devospac.obj: devospac.c devo.h
	qcl /c /AC /G2 /Gs devospac.c

getstuff.obj: getstuff.c devo.h
	qcl /c /AC /G2 /Gs getstuff.c

save_res.obj: save_res.c devo.h
	qcl /c /AC /G2 /Gs save_res.c

energy.obj: energy.c devo.h
	qcl /c /AC /G2 /Gs /O energy.c

block.obj: block.c devo.h
	qcl /c /AC /G2 /Gs /O block.c

