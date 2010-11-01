 /*-------------------------------------------------------------------*
  *    Name: rootgram.sas                                             *
  *   Title: Hanging rootograms for discrete distributions            *
        Doc: http://www.datavis.ca/sasmac/rootgram.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 23 Dec 97 16:28                                          *
  * Revised: 16 Jan 2009 16:10:11                                     *
  * Version: 1.0-1                                                    *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The ROOTGRAM macro produces histograms, rootograms, and hanging
 rootograms for the distribution of a discrete variable compared
 with expected frequencies according to a theoretical distribution.

=Usage:

 The VAR= and  OBS= variables  must be specified.  The expected
 frequencies may be obtained with the GOODFIT macro.
 
 The ROOTGRAM macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%include catdata(madison);
	%goodfit(data=madison, var=count, freq=blocks, dist=poisson);
	%rootgram(data=fit, var=count, obs=blocks);
 
==Parameters:

* DATA=       Specifies the name of the input data set [Default: DATA=_LAST_]

* VAR=        Specifies the name of the analysis variable, used as the
              abscissa in the plot.

* OBS=        Specifies the observed frequency variable

* EXP=        Expected/fitted frequency [Default: EXP=EXP]

* FUNC=       Function applied to ordinate [Default: FUNC=SQRT]

* BWIDTH=     Bar width [Default: BWIDTH=.5]

* BCOLOR=     Bar color [Default: BCOLOR=GRAYB0]

* BTYPE=      Bar type: One of HANG, DEV, or NEEDLE. [Default: BTYPE=HANG]

* ANNO=       The name of an input annotate dataset

* NAME=       Name of the graphics catalog entry

 =*/

%macro rootgram(
	data=_last_,   /* input dataset                */
	var=,          /* Analysis variable            */
	obs=,          /* observed frequency           */
	exp=exp,       /* expected/fitted frequency    */
	func=Sqrt,     /* function applied to ordinate */
	bwidth=.5,     /* bar width                    */
	bcolor=grayb0, /* bar color                    */
	btype=hang,    /* bar type: HANG, DEV, NEEDLE  */
	anno=,         /* input annotate dataset       */
	name=rootgram  /* graphics catalog entry name  */
	);

%let me=ROOTGRAM;
%let btype=%upcase(&btype);
options nonotes;
data roots;
	set &data;
	
	%if %upcase(&func)^=NONE %then %do;
		&obs = &func(&obs+.000001);
		&exp = &func(&exp+.000001);
		label &exp = "&func(frequency)";
	%end;
	%else %do;	
		label &exp = "Frequency";
	%end;

data bars;
	set roots(keep=&var &obs &exp) end=eof;
	xsys='2'; ysys='2';
	retain min 0 max 0;
	drop inc;
	style = 'solid  ';
	color = "&bcolor";

	%*-- top of bar;
	x = &var - &bwidth/2;
	%if &btype=HANG %then %do;
		y = &exp;
		%end;
	%else %if &btype=DEV %then %do;
		y = &exp - &obs ;
		min = min(y,min);
		max = max(&exp,max);
		%end; 
	%else %do;
		y = &obs;
		%end; 
	function = 'move    '; output;
	max = max(y,max);

	%*-- bottom of bar;
	x = &var + &bwidth/2;
	%if &btype=HANG %then %do;
		y = &exp - &obs;
		%end;
	%else %do;
		y = 0;
		%end; 
	function = 'bar     '; output;
	min = min(y,min);

	if eof then do;
		drop pow nice ut best;
		inc= abs(max - min)/6;
		pow = 10**floor( log10(inc) );
		nice=1000;
		do in = 1, 2, 2.5, 4, 5;
			ut = in * pow;
			if abs(inc-ut) < nice then do;
				nice = abs(inc-ut);
				best = ut;
			end;
		end;
		inc=best;
		min = inc * floor(min/inc);
		max = inc * ceil (max/inc);
		*put "&me: " min= max= inc=;
		call symput('max', left(put(max,4.1)));
		call symput('min', left(put(min,4.1)));
		call symput('inc', left(put(inc,4.1)));
		end;
	run;
%put &me: min=&min max=&max inc=&inc;

%if %length(&anno) %then %do;
data bars;
	set bars &anno;
	run;
%end;
options notes;
	
proc gplot data=roots;
	plot &exp * &var / vaxis=axis1 haxis=axis2
		anno=bars hminor=0 vminor=1 vref=0 lvref=7;
	symbol i=spline v=dot c=red h=1.5;
	axis1 label=(a=90) order=(&min to &max by &inc);
	axis2 offset=(5,5);
	run; quit;
%done:
goptions reset=symbol;
%mend;
