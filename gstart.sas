%GLOBAL devtyp;
%let devtyp=;
%global gstarted;
%macro gstart(device);
%*-------------------------------------------------------------------*
 * Used to insert the appropriate GOPTIONS specification into the    *
 * SAS job for the intended graphics device.                         *
 *-------------------------------------------------------------------*;
%local DEVS;
%let DEVS = PSCOLOR PSLPR EPS GIF JPG CGM PNG PDF;

%if &sysver > 9 %then %do;
	%if %symexist(gstarted) %then %do;
		%if &gstarted=TRUE %then %do;
		%put GSTART: already started: gstarted=&gstarted;
		%goto exit;
		%end;
		%end;
	%end;
 
 %if &SYSDEVIC.NULL ^= NULL %then %let device = &SYSDEVIC;
 %if &device.NULL = NULL    %then %let device = XCOLOR;

 %let device = %UPCASE(&device);
 %put GSTART:  Setting up for device &device;

    %*-- Assumes each device-specific macro sets &devtyp;
 %if &device = JPEG  %then %JPG;
 %else %if %index(&DEVS, &device) > 0 %then %&device;
/*
 %if &device = PSCOLOR         %then %PSCOLOR;
   %else %if &device = PSLPR   %then %PSLPR;
   %else %if &device = EPS     %then %EPS;
   %else %if &device = GIF     %then %GIF;
*/  
   %else %do;
        %let devtyp=SCREEN ;
		%if &device = XCOLOR     %then %str(goptions device=xcolor;);
		%else %if &device = WIN  %then %WIN;
		%else %do;
        %GERR(&device);
        %end;
   %end;
 run;
 %put GSTART: DEVTYP initialized to "&devtyp";
 %let gstarted=TRUE;

 %exit:
%MEND;

%macro gerr(device);
%*-------------------------------------------------------------------*
 * Called from GSTART when an invalid graphics device is specified.  *
 *-------------------------------------------------------------------*;
 %IF &device ^= ? %THEN
   %PUT WARNING:  Unknown graphics device specified -- DEVICE=&device;
 %PUT GSTART: Known devices are:;
 %PUT GSTART:   &DEVS;
%MEND GERR;
