/*


*/
#include <stdio.h>
#include <io.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <fcntl.h>

#include "devospac.h"

/*
----------------------------------------------------------------------------
*/
file_stuff()
{
char c;

	printf("\n\n\n\n\tS - Save DEVO to a file.\n");
	printf("\tR - Restore DEVO from a file.\n");
	c = getachar();
	if((c == 's')||(c == 'S')){
		do_save();
	}else if((c == 'r')||(c == 'R')){
		do_restore();
	}else{
		printf("Learn to TYPE!\n");
	}


}
/*
----------------------------------------------------------------------------
*/
do_save()
{
char fname[50];
char odata[500];
int i,j,k;
int status;
char *tp;
int fp;

 printf("\n\nSave DEVO.\n\n");
 printf("Enter Filename to save DEVO states in :> ");
 gets(fname);
 if(strlen(fname) == 0){
  my_setcolor(12,0);
  printf("Abandoned.\n");
  my_setcolor(7,0);
  return;
 }
 fp = open(fname,O_RDWR|O_CREAT|O_TRUNC|O_BINARY,S_IWRITE);
 if(fp == 0){
  my_setcolor(12,0);
  printf("Error opening file.\n");
  my_setcolor(7,0);
  return;
 }
 printf("Saving...\n");

 sprintf(odata,"%4.4x %4.4x %4.4x %4.4x\n",XSIZE,YSIZE,MAX_THINGS,VERS);
 status = write(fp,odata,strlen(odata));
 if(status != strlen(odata)){
  my_setcolor(12,0);
  printf("Parameter Write error.\n");
  my_setcolor(7,0);
  close(fp);
  return;
 }

 sprintf(odata,"%4.4x %4.4x %4.4x %4.4x %4.4x %4.4x\n",
 free_list,free_num,num_things,stn_next,v_total_off,v_diff);
 status = write(fp,odata,strlen(odata));
 if(status != strlen(odata)){
  my_setcolor(12,0);
  printf("Variable Write error.\n");
  my_setcolor(7,0);
  close(fp);
  return;
 }
 sprintf(odata,"%4.4x %4.4x %4.4x %4.4x %4.4x %4.4x %4.4x\n",
 v_k,v_smx,v_total_equal,v_slowdown,v_life,xoff,yoff);
 status = write(fp,odata,strlen(odata));
 if(status != strlen(odata)){
  my_setcolor(12,0);
  printf("Variable Write error.\n");
  my_setcolor(7,0);
  close(fp);
  return;
 }

 sprintf(odata,"%8.8lx %4.4x %4.4x %4.4x %4.4x %4.4x %4.4x\n",
 loopcount,lastact,ssmode,cursor_mode,cur_x,cur_y,status_line);
 status = write(fp,odata,strlen(odata));
 if(status != strlen(odata)){
  my_setcolor(12,0);
  printf("Variable Write error.\n");
  my_setcolor(7,0);
  close(fp);
  return;
 }

 sprintf(odata,"%4.4x %4.4x %4.4x %4.4x %8.8lx %8.8lx %4.4x %4.4x\n",
 repo_rate,repos,max_size,v_steal,births,deaths,v_fatactive,v_fatlife);
 status = write(fp,odata,strlen(odata));
 if(status != strlen(odata)){
  my_setcolor(12,0);
  printf("Variable Write error.\n");
  my_setcolor(7,0);
  close(fp);
  return;
 }

 for(i=0;i<YSIZE;i++){
  tp = (char *)&frame[i][0];
  status = write(fp,tp,(XSIZE*sizeof(int)));
  if(status != (XSIZE*sizeof(int))){
   my_setcolor(12,0);
   printf("Frame Write error.\n");
   my_setcolor(7,0);
   close(fp);
   return;
  }
 }

 for(i=0;i<MAX_THINGS;i++){
  status = write(fp,things[i],sizeof(struct thing));
  if(status != sizeof(struct thing)){
   my_setcolor(12,0);
   printf("Thing Write error.\n");
   my_setcolor(7,0);
   close(fp);
   return;
  }
 }

 close(fp);
 printf("Done.\n");
}

/*
----------------------------------------------------------------------------
*/
do_restore()
{
char fname[50];
char odata[500];
int i,j,k,l;
int status;
int fp;
char *tp;

 printf("\n\nRestore DEVO States.\n\n");
 printf("Enter Filename to restore DEVO states from :> ");
 gets(fname);
 if(strlen(fname) == 0){
  my_setcolor(12,0);
  printf("Abandoned.\n");
  my_setcolor(7,0);
  return;
 }
 fp = open(fname,O_RDONLY|O_BINARY);
 if(fp == 0){
  my_setcolor(12,0);
  printf("Error opening file.\n");
  my_setcolor(7,0);
  return;
 }
 printf("Restoring...\n");

 status = mread(fp,odata,20);
 if(status != 20){
  my_setcolor(12,0);
  printf("Parameter Read error. status = %d\n",status);
  my_setcolor(7,0);
  close(fp);
  return;
 }
 sscanf(odata,"%4x %4x %4x %4x\n",&i,&j,&k,&l);
 if((i!=XSIZE)||(j!=YSIZE)||(k!=MAX_THINGS)||(l!=VERS)){
  my_setcolor(12,0);
  printf("Incompatible Save File !\n");
  my_setcolor(7,0);
  close(fp);
  return;
 }

 status = mread(fp,odata,30);
 if(status != 30){
  my_setcolor(12,0);
  printf("Variable Read error. Status = %d\n",status);
  my_setcolor(7,0);
  close(fp);
  return;
 }
 sscanf(odata,"%4x %4x %4x %4x %4x %4x\n",
 &free_list,&free_num,&num_things,&stn_next,&v_total_off,&v_diff);

 status = mread(fp,odata,35);
 if(status != 35){
  my_setcolor(12,0);
  printf("Variable Read error. Status = %d\n",status);
  my_setcolor(7,0);
  close(fp);
  return;
 }
 sscanf(odata,"%4x %4x %4x %4x %4x %4x %4x\n",
 &v_k,&v_smx,&v_total_equal,&v_slowdown,&v_life,&xoff,&yoff);

 status = mread(fp,odata,39);
 if(status != 39){
  my_setcolor(12,0);
  printf("Variable Read error. Status = %d\n",status);
  my_setcolor(7,0);
  close(fp);
  return;
 }
 sscanf(odata,"%8lx %4x %4x %4x %4x %4x %4x\n",
 &loopcount,&lastact,&ssmode,&cursor_mode,&cur_x,&cur_y,&status_line);

 status = mread(fp,odata,48);
 if(status != 48){
  my_setcolor(12,0);
  printf("Variable Read error. Status = %d\n",status);
  my_setcolor(7,0);
  close(fp);
  return;
 }
 sscanf(odata,"%4x %4x %4x %4x %8lx %8lx %4x %4x\n",
 &repo_rate,&repos,&max_size,&v_steal,&births,&deaths,&v_fatactive,&v_fatlife);

 for(i=0;i<YSIZE;i++){
  tp = (char *)&frame[i][0];
  status = mread(fp,tp,(XSIZE*sizeof(int)));
  if(status != (XSIZE*sizeof(int))){
   my_setcolor(12,0);
   printf("Frame Read error. status = %d\n",status);
   my_setcolor(7,0);
   close(fp);
   return;
  }
 }

 for(i=0;i<MAX_THINGS;i++){
  status = mread(fp,things[i],sizeof(struct thing));
  if(status != sizeof(struct thing)){
   my_setcolor(12,0);
   printf("Thing Read error.\n");
   my_setcolor(7,0);
   close(fp);
   return;
  }
 }

 close(fp);
 printf("Done.\n");
}

int mread(fp,cdata,clen)
int fp;
char *cdata;
int clen;
{
int done;
int status;

	for(done = 0;done < clen;){
		status = read(fp,&cdata[done],clen-done);
		if(status <= 0){
			done = status;
			break;
		}
		done += status;
	}
	return(done);
}