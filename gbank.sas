 /*--------------------------------------------------------------*
  *    Name: gbank.sas                                           *
  *   Title: Calculate width or height to bank a plot to 45 deg  *
        Doc: http://www.datavis.ca/sasmac/gbank.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 12 Mar 2008 11:52:35                                *
  * Revised: 21 Apr 2008 16:29:36                                *
  * Version: 1.0-0                                               *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 Cleveland (1988) introduced the idea of the `shape parameter' of a
 bivariate graph and showed that often patterns, trends and
 differences can be most easily seen when the slopes of successive
 line segments is about 45 degrees on average.  Setting the relative
 height/width ratio of a graph on this basis is called 'banking to
 45 degrees.'

 The GBANK macro calculates *either* the desired height or width of
 a plot so that the average slope of successive line segments is
 approximately 45 degrees, facilitating perception of trends in 
 the data.  The results are returned in an output data set, global
 macro variables HSIZE and VSIZE are set, and a GOPTIONS statement
 of the form GOPTIONS HSIZE=&HSIZE VSIZE=&VSIZE is executed.
 

==Method:

 Calculate the successive slopes in a data step and use PROC SUMMARY.

==Warning:

 The graphic device parameters XMAX and YMAX set the maximum values
 for HSIZE and VSIZE, and these differ widely across SAS/Graph devices.
 If a calculated value is outside the allowed maximum for the current 
 GDEVICE,  SAS/Graph will reset it to the maximum. 

=Usage:

 The GBANK macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%gbank(data=sunspot, y=sunspots, x=year, height=15, gunit=pt);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* Y=          Vertical variable for the plot [Default: Y=Y]

* X=          Horizontal variable for the plot [Default: X=X]

* FUNCTION=   Function of the absolute slope to calculate: MEDIAN or MEAN
              [Default: FUNCTION=MEDIAN]

* HEIGHT=     Desired graph height; the corresponding width is calculated.
              You should set a value for either HEIGHT= or WIDTH=,
			  but not both. Specifying HEIGHT=AUTO is the same a specifying
			  a null (empty) value.

* WIDTH=      Desired graph width; the corresponding height is calculated.

* GUNIT=      Units for HEIGHT= and WIDTH=. Typical values are GUNIT=IN,
              GUNIT=CM and GUNIT=PT (for pixel-based drivers) [Default: GUNIT=IN]

* OUT=        The name of the output data set. This contains the variables
              XMIN, XMAX, YMIN, YMAX and MSLOPE [Default: OUT=_bank_]
                

=Examples:

  %include data(sunspots);

  proc summary data=sunspots nway;
	  class year;
	  var sunspots;
	  output out=sunspot mean=sunspots;

  *-- standard plot;
  goptions hsize=4in vsize=4in;

  proc gplot data=sunspot;
	  plot sunspots * year / vaxis=axis1 haxis=axis2;
	  symbol1 v=none i=join;
	  axis1 label=(a=90);
	  axis2 order=(1750 to 2000 by 50);
	  run;

  *-- banked plot, width=400 pt;
  %gbank(data=sunspot, y=sunspots, x=year, width=400, height=AUTO, gunit=pt);
  proc gplot data=sunspot;
	  plot sunspots * year / vaxis=axis1  haxis=axis2 noaxis;
	  symbol1 v=none i=join;
	  axis1 label=none value=none;
	  axis2 order=(1750 to 2000 by 50);
	  run;


 =*/



%macro gbank(
	data=_last_,       /* name of input data set                 */
	y=y,               /* vertical variable for the plot         */
	x=x,               /* horizontal variable for the plot       */
	function=median,   /* function of absolute slope             */
	height=,           /* desired graph height                   */
	width=,            /* desired graph width                    */
	gunit=IN,          /* graphic unit for height and width      */
	out=_bank_         /* returned output data set               */
	);

%local me abort;
%let abort=0; %let me=&sysmacroname;
%if %upcase(&height)=AUTO %then %let height=;
%if %upcase(&width)=AUTO %then %let width=;
%if %length(&height)=0 and %length(&width)=0 %then %do;
	%put ERROR: &me: Either HEIGHT= or WIDTH= must be specified;
	%let abort=1;
	%goto DONE;
	%end;
%if %length(&height)>0 and %length(&width)>0 %then %do;
	%put WARNING: &me: Specifying both HEIGHT= and WIDTH= does nothing useful;
	%end;

%*-- Reset required global options;
%if %sysevalf(&sysver  >= 7) %then %do;
    %local o1;
    %let o1 = %sysfunc(getoption(notes));
    options nonotes;
    %end;
%else %do;
   options nonotes;
    %end;

*-- Sort the data set to calculate sucessive slopes;
proc sort data=&data. out=__sorted; 
	where (&x ^= . and &y ^= .);
    by &x ;
	run;


/*
%iml:
proc iml;
	use __sorted;
	read all var{&y} into y;
	read all var{&x} into x;
	n = nrow(y);
	slope = (y[2:n] - y[1:(n-1)]) / (x[2:n] - x[1:(n-1)]);
	slope = abs(slope);
	mslope = median(slope);
	print 'Median absolute slope:' mslope;
	minx = min(x); maxx = max(x);
	miny = min(y); maxy = max(y);
	vars = {mslope minx maxx miny maxy};
    create &out var vars;
	append;
	quit;
*/


%noiml:
data __aspect;
   set __sorted;
   by &x.;
   ydif=dif(&y);
   xdif=dif(&x);
   slope = abs(ydif/xdif);
   run;
proc summary nway noprint;
   var &x &y slope ;
   output out=&out(drop=_type_ _freq_) min(&x &y)=minx miny max(&x &y)=maxx maxy median(slope)=mslope;
   run;


%global hsize vsize;

data &out.;
   set &out;
   height = .; width = .;
   %if %length(&height) %then %do;
    	height = &height;
		%if %length(&width)=0 %then %do;
			width = height * mslope;
			%end;
		%else %do;
			width = &width;
			%end;
		%end;
		
	%else %do;  /* height not specified */
		%if %length(&width) %then %do;
			width = &width;
			height = width / mslope;
			%end;
		%else %do;
			height = &height;
			%end;
		%end;
	%if %upcase(&gunit)=PT %then %do;
		height = round(height);
		width = round(width);
		%end;
	call symput('hsize', trim(left(put(width, best6.))) || " &gunit");
	call symput('vsize', trim(left(put(height, best6.))) || " &gunit");
   run;

goptions hsize=&hsize vsize=&vsize;
%put &me: Setting GOPTIONS hsize=&hsize vsize=&vsize%str(;);

*proc print data=&out;


*-- Delete temporary data sets;
  proc datasets lib=work nolist;
  delete   __aspect;
  run; quit;

%*-- Restore global options;
%if %sysevalf(&sysver  >= 7) %then %do;
    options &o1;
    %end;
%else %do;
   options notes;
    %end;
%done:

%mend;

