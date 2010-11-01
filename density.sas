 /*-------------------------------------------------------------------*
  *    Name: density.sas                                              *
  *   Title: Nonparametric density estimates from a sample.           *
        Doc: http://www.datavis.ca/sasmac/density.html            
  *                                                                   *
  * User chooses a bandwidth parameter to balance smoothness and bias *
  * and the range of the data over which the density is to be fit.    *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  23 Mar 1989 16:21:12                                    *
  * Revised:  28 May 2004 16:21:271                                    *
  * Version:  1.1                                                     *
  *  -handle special missing values (.A-.Z)                           *
  *  -fixed variable names for V7+ (thx: Dietrich Alte)               *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*
  *      Original program by: C. ROGER LONGBOTHAM                     *
  *      while at Rockwell International, Rocky Flats Plant           *
  * From: SAS SUGI 12, 1987, 907-909.                                 *
  *-------------------------------------------------------------------*/
 
%macro density(
     data=_LAST_,        /* Name of input data set              */
     out=DENSPLOT,       /* Name of output data set             */
     var=X,              /* Input variable (numeric)            */
     window=,            /* Bandwidth (H)                       */
     xfirst=.,           /* . or any real; smallest X value     */
     xlast=.,            /* . or any real; largest X value      */
     xinc=.,             /* . or value>0; X-value increment     */
                         /* Default: (XLAST-XFIRST)/60          */
     gplot=YES,
     symbol=,
     haxis=,       /* AXIS statement for horizontal axis        */
     vaxis=,       /* and for vertical axis- use to equate axes */
     plotopt=,     /* other plot options, eg %str(areas=1) to fill */
     anno=,
     showobs=YES,
	 name=density
	  );

%*-- Remove missing data;
%local abort;
%let abort=0;
data _in_;
   set &data;
   keep &var;
   if &var > .Z;
run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%*-- Get the variable label;
proc contents data=_in_ out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

data _null_;
   set _work_(keep=name type label);
	if upcase(name) = upcase("&var") then do;
		if label=' ' then label="&var";
		call symput('vlabel', label);
	end;
run;

proc sort data=_in_;
   by &var;
 
proc iml;
 
start WINDOW;    *-- Calculate default window width;
   mean = xa[+,]/n;
   css = ssq(xa - mean);
   stddev = sqrt(css/(n-1));
   q1 = floor(((n+3)/4) || ((n+6)/4));
   q1 = (xa[q1,]) [+,]/2;
   q3 = ceil(((3*n+1)/4) || ((3*n-2)/4));
   q3 = (xa[q3,]) [+,]/2;
   quartsig = (q3 - q1)/1.349;
   h  = .9*min(stddev,quartsig) * n##(-.2);  * Silvermans formula;
	file log;
	put 'NOTE: Using calculated default window width, window=' h;
   finish;
 
start INITIAL;   *-- Translate parameter options;
   if xf=. then xf=xa[1,1];
   if xl=. then xl=xa[n,1];
   if xl <= xf then do;
		file log;
      put  'ERROR: Either largest X value chosen is too small';
      put  '     or all data values are the same';
      stop;
      end;
   if dx=. | dx <= 0 then do;
      inc = (xl-xf)/60;
      rinc = 10 ## (floor(log10(inc))-1);
      dx = round(inc,rinc);
      end;
   if xf=xa[1,1] then xf=xf-dx;
   nx = int((xl-xf)/dx) +  3;
   finish;
 
*-- calculate density at specified x values;
start DENSITY;
   fnx = j(nx,3,0);
   vars = {"DENSITY"  "&VAR" "WINDOW"};
   create &out from fnx [colname=vars];
   sigmasqr = .32653;                   * scale constant for kernel  ;
   gconst = sqrt(2*3.14159*sigmasqr);
   nuh = n*h;
   x = xf - dx;
   do i = 1 to nx;
      x = x + dx;
      y = (j(n,1,x) - xa)/h;
      ky = exp(-.5*y#y / sigmasqr) / gconst;      * Gaussian kernel;
      fnx[i,1] = sum(ky)/(nuh);
      fnx[i,2] = x;
      end;
   fnx[,3] = round(h,.001);
   append from fnx;
   finish;
 
*-- Main routine ;
   use _in_;
   read all var "&var" into xa [colname=invar];
   n = nrow(xa);
   %if &window=%str() %then %do;
      run window;
      %end;
   %else %do;
      h = &window ;
      %end;
 
   xf    = &xfirst;
   xl    = &xlast;
   dx    = &xinc;
   run initial;
   run density;
   close &out;
   quit;

%*-- Restore variable label;
data &out;
	label &var = "&vlabel";
	set &out;
	
%if &gplot = YES %then %do;

   %if %length(&symbol)=0 %then %do;
    symbol1 v=star i=join c=black;
		%end;

	%let annoopt=;
	%if %length(&anno) > 0 %then %let annoopt = anno=&anno;
	
	%if &showobs = YES %then %do;
	data annoobs;
		set &data;
		retain xsys '2' ysys '1' when 'A';
		x = &var;
		y = 0; function='MOVE    '; output;
		y = 3; function='DRAW    '; output;
	%let annoopt=anno=annoobs;
	%if %length(&anno) > 0 %then %do;
		data annoobs;
			set annoobs &anno;
			when='A';
		%end;
	%end;
		
proc gplot data=&out ;
    plot density * &var  / frame &plotopt &annoopt
		name="&name"
		des="Density plot of &var"
   %if %length(&haxis)>0 %then %do;
		haxis = &haxis
		%end;
   %if %length(&vaxis)>0 %then %do;
		vaxis = &vaxis
		%end;	
	 ;
    run ;
%end;

%done:

%mend;
