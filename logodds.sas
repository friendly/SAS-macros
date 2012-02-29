 /*-------------------------------------------------------------------*
  *    Name: logodds.sas                                              *
  *   Title: Plot empirical log-odds for logistic regression          *
        Doc: http://www.datavis.ca/sasmac/logodds.html             
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:   6 Nov 1997 08:45:22                                    *
  * Revised:  26 Sep 2002 11:56:31                                    *
  * Version: 1.2                                                      *
  *  Updated for V7+ (VALIDVARNAME)                                   *
  *  Added HSYM= to control height of plot symbol                     *
  *  Added VAXIS= to control vertical axis description                *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 For a binary response variable, Y, taking values 0 or 1, and a
 continuous independent variable, X, the LOGODDS macro groups the
 X variable into some number of ordered, non-overlapping intervals.
 It plots the empirical log-odds of Y=1 (and/or Pr{Y=1}) against
 X for each interval of X, together with the fitted linear logistic
 relation, an optional smoothed curve (using the LOWESS macro),
 and the observed binary responses.
 

=Usage:

 The input data to be plotted must be in case form.
 The LOGODDS macro is called with keyword parameters.  The X= and Y=
 variables are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%include catdata(icu);
	%logodds(data=icu, x=age, y=died, smooth=0.25, ncat=16,
		options=order=data);
 
==Parameters:

* X=          Name of the continuous independent variable

* Y=          Name of the binary response variable (must be numeric)

* EVENT=      Value of Y for the event of interest [Default: EVENT=1]

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* OPTIONS=    Options for PROC LOGISTIC, for example, OPTIONS=DESCENDING.

* NCAT=       Number of categories of the X variable.
              For example, if deciles of X are desired, use NCAT=10. 
				  [Default: NCAT=10]

* PLOT=       Scale(s) for the response. PLOT=LOGIT gives a plot on the
              logit scale, PLOT=PROB on the probability scale.
              [Default: PLOT=LOGIT PROB]

* SMOOTH=     Smoothing parameter for a lowess smooth, in the interval
              (0-1).  No smooth curve is produced unless a SMOOTH=
				  value is specified.  [Default: SMOOTH=]

* HSYM=       Height of observation plot symbols.

* SHOW=       Specifies whether to plot the binary observations.
              [Default: SHOW=OBS]

* OBS=        Specifies how to display the binary observations.  If
              OBS=STACK, the observations are plotted in vertical
				  columns at the top (Y=1) or bottom (Y=0) of the plot.
				  If OBS=JITTER a small random quantity is added (Y=0)
				  or subtracted (Y=1) to the Y value.  In addition, you can
				  add two parameters, <space-factor> and <symbol> to control
				  the vertical spacing and plot symbol used. 
				  [Default: OBS=STACK 2 DOT]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=LOGODDS]

* GOUT=       The name of the graphic catalog [Default: GOUT=GSEG]
                
=Dependencies:

 If you use the SMOOTH= parameter, the LOWESS macro is required. See
 http://datavis.ca/sasmac/lowess.html
 
 =*/
 
%macro logodds(
	x=,               /* Name of the continuous independent variable */
	y=,               /* Name of the binary response variable        */
	event=1,          /* Value of Y for the event of interest        */
	data=_last_,      /* Name of the input data set */
	options=,         /* Options for PROC LOGISTIC                   */
	ncat=10,          /* Number of categories of the X variable      */
	plot=logit prob,  /* Scale(s) for the response                   */
	smooth=,          /* Smoothing parameter for a lowess smooth     */
	hsym=1.3,         /* Height of observation plot symbols          */
	csym=red,         /* Color for observation plot symbols          */
	show=obs,         /* Whether to plot the binary obs.             */
	obs=stack,        /* How to plot the binary obs.                 */
	name=logodds,     /* Name of graphics catalog entry              */
	vaxis=,           /* Name of an AXIS statement for y-axis        */
	vlabel=&y=&event, /* Label suffix for y-axis                     */
	gout=gseg
	);

%let plot=%upcase(&plot);
%let show=%upcase(&show);

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let abort=0;
%if &x=%str() or &y=%str() %then %do;
   %put ERROR: The X= and Y= variables must be specified;
	%let abort=1;
   %goto DONE;
   %end;
 	

proc logistic /*noprint*/ data=&data &options;
   model  &y = &x ;
   output out=results p=predict l=lower u=upper xbeta=plogit stdxbeta=selogit;

data results;
	set results;
	uplogit = plogit + selogit;
	lologit = plogit - selogit;
	logit = log(((&y=&event)+.25)/((1-(&y=&event))+.25));
proc sort;
	by &x;

proc sort data=&data;
   by &x &y;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

proc rank data=&data groups=&ncat out=_grouped;
	var &x;
	ranks _gp_;

proc summary data=_grouped nway;
	class _gp_;
	var &x &y;
	output out=_groups_ mean(&x)=xmean sum(&y)=ysum min(&x)=xmin;

data _logits_;
	set _groups_(rename=(_freq_=n) drop=_type_) end=eof;
	label xmean="Mean &x"
	      logit="Log Odds &y=&event";
	p = (ysum+.5) / (n+.5);
	logit = log( (ysum+.5) / (n-ysum+.5)  );
	nobs + n;
	if eof then call symput('nobs', put(nobs,8.));
run;
proc print;
	id _gp_;
	var xmin ysum n logit p;

data _pts_;
	set _logits_;
	xsys = '2'; ysys='2';
	x = xmean; y=logit; function='symbol'; size=2; text='square';

%*-- Mark the boundaries between adjacent groups;
data _marks_;
	set _logits_;
	xsys = '2'; ysys = '1';
	color = 'red'; when='A';
	x = xmin; y=0;    function='MOVE    '; output;
	x = xmin; y=2.25; function='DRAW    '; output;

%if %index(&show,OBS) %then %do;
%let otype = %upcase(%scan(&obs,1));
%let oparm = %scan(&obs,2,%str( ));
%let osym  = %scan(&obs,3,%str( ));
%if %length(&osym)=0 %then %let osym=dot;
data _obs_;
	set &data(keep=&x &y);
	by &x &y;
	drop i;
	length text color $8;
	if first.&y then i=0;
	xsys = '2'; ysys='1';
	x = &x;
	%if &otype=STACK %then %do;
		%if %length(&oparm)=0 %then %let oparm=2;
		y=100*&y + (3+(&oparm)*i)*sign(.5-&y);
		%end;
	%else %do; /* &otype=JITTER */
		%if %length(&oparm)=0 %then %let oparm=10;
		y=100*&y + (3+(&oparm)*uniform(0))*sign(.5-&y);
		%end;
	function='symbol';
	%if %length(&hsym) %then %do;
		size=&hsym;
		%end; 
	text="&osym"; color="&csym";
	i+1;
%end;

data _pts_;
	set _pts_
		 %if %index(&show,OBS) %then _obs_ ;
		 _marks_;


%if %length(&smooth)>0 %then %do;
%lowess(data=results, x=&x, y=logit, gplot=NO, pplot=NO, outanno=_smooth_,
	silent=YES, robust=0, iter=1, f=&smooth, line=22);

data _pts_;
	set _pts_ _smooth_;
%end;

%if %index(&PLOT,LOGIT) %then %do;
%if %length(&vaxis)=0
	%then %do;
		%let yaxis=axis1;
   	axis1 label=(a=90) offset=(3pct);
		%end;
	%else %let yaxis=&vaxis;
		
proc gplot data=results gout=&gout;
   plot
        plogit  * &x = 1
        uplogit * &x = 2
        lologit * &x = 2
        / frame overlay vaxis=&yaxis anno=_pts_ hminor=1 vminor=1
		  name="&name"
		  des="Empirical log-odds plot of &data";
   symbol1 v=none   i=join l=1  w=3 c=blue;
   symbol2 v=none   i=join l=20 w=2 c=blue;
	label  plogit="Log Odds &vlabel";
run; quit;
%gskip;
%end;

%if %index(&PLOT,PROB) %then %do;
data _pts_;
	set _logits_;
	xsys = '2'; ysys='2';
	x = xmean; y=p; function='symbol'; size=2; text='square';

data _pts_;
	set _pts_
		 %if %index(&show,OBS) %then _obs_ ;
		 _marks_;

%if %length(&smooth)>0 %then %do;
%lowess(data=&data, x=&x, y=&y, gplot=NO, pplot=NO, outanno=_smooth_,
	silent=YES, robust=0, iter=1, f=&smooth, line=22);

data _pts_;
	set _pts_ _smooth_;
%end;

%if %length(&vaxis)=0
	%then %do;
		%let yaxis=axis1;
	   axis1 label=(a=90) offset=(3pct) order=(0 to 1 by .2);
		%end;
	%else %let yaxis=&vaxis;

proc gplot data=results gout=&gout;
   plot 
        predict * &x = 1
        upper * &x = 2
        lower * &x = 2
        / frame overlay vaxis=&yaxis anno=_pts_ hminor=1 vminor=1
		  name="&name"
		  des="Empirical probability plot of &data";
		 
   symbol1 v=none   i=join l=1  w=3 c=blue;
   symbol2 v=none   i=join l=20 w=2 c=blue;
	label predict = "Probability &vlabel";
	format &y 4.1;
	run; quit;
	goptions reset=symbol;
%end;

%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%if &abort %then %put ERROR: The LOGODDS macro ended abnormally.;
%mend;

