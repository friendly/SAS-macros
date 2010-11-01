%macro NEXTCLR;
%let devtyp=PS;
%let dev=pscolor; 

*let sasfn =%scan(%SYSGET(SASFILE),1,'.'); 
*let gsasfile=~/sasuser/gout/&sasfn..&SYSJOBID..&DEV..ps;

%let gsasfn =%SYSGET(GSASFILE); 
%if &gsasfn=%str() %then %let gsasfn=grfout.ps;
%let gsasfile=&gsasfn;

%put gsasfile is: "&gsasfile";
filename gsasfile  "&gsasfile";
goptions device=&dev gaccess=sasgaedt gsfname=gsasfile gsflen=80 gsfmode=append 
		  gprolog='2521'x;	
%mend;