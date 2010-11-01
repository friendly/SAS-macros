%* Macro for postscript SAS/GRAPH output;
%macro ps(filename,dev);

%if "&filename" = ""
   %then %do;    %*-- See if sasfile has been set;
	%global sasfile;
   %if %defined(&sasfile)=NO%then %do;    %*-- Get name from window;
      %getfn;
      %end;
   %let gsasfile = &sasfile;
   %end;

   %else %do;    %*-- Use given filename as output filename;
   %let gsasfile = &filename;
   %end;

%if "&dev" = ""
   %then %do;
   %let dev = pscolor;
   %end;

%let gsasfile = %scan(&gsasfile,1,.);
%let gsasfile = &gsasfile..ps;
%put gsasfile is: &gsasfile;

filename gsasfile "&gsasfile";

goptions targetdevice=&dev gaccess=sasgaedt gsfname=gsasfile
   gsflen=80 gsfmode=append gprolog='2521'x;
%mend;

%macro defined(name);
options noserror;
   %if %nrquote(&&&name)=%nrstr(&)&name%then %let yesno=NO;
        %else %let yesno=YES;
   &yesno
%mend;
;

options mprint;
%ps(test);
%ps();

