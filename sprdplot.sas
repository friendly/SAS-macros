 /*--------------------------------------------------------------*
  *    Name: sprdplot.sas                                        *
  *   Title: Find power transformations to equalize variance     *
        Doc: http://www.datavis.ca/sasmac/sprdplot.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  2 Feb 1997 10:59:29                                *
  * Revised: 27 Feb 2013 16:17:35                                *
  * Version:  1.2-1                                              *
     - Combined sprdplot and eqvar to give plot plus transform    
     - Added GROUPS= to handle a continuous covariate by grouping 
     - Added GOUT=                                                
     - Removed WEIGHT from REG step                               
     - Added VAXIS=, HAXIS=                                       
     - Fixed buglet when power<0                                  
     - Replaced %label with %labels

  * Requires: %labels                                            *
                                                                  
  *--------------------------------------------------------------*/
 /*=
=Description:

 The sprdplot macro produces a spread-level plot to determine if a simple
 power transformation can equalize within-group variance of a response
 variable in a dataset classified by one or more classification
 variables.  This method may be extended to regression data by
 dividing a continuous covariate into ordered groups by use of the
 GROUPS= parameter.
 
 The spread-level plot calculates log10(Spread) and log10(Level),
 where Spread is usually the Interquartile Range, and Level is usually
 the Median. 

 This plot has the property that *if* the relationship between
 log10(Interquartile range) and log10(Median) is reasonably linear,
 then the recommended power is p = 1 - slope, and the transformation
 is

		      / y**p,      p > 0
      y --> | log(p),    p = 0
		      \ -100y**p,  p < 0

 The macro chooses the best power(s) from a list of simple integers
 and half-integers (PLIST=), and creates new variables using those
 transformations.
 
=Usage:

 The SPRDPLOT macro is defined with keyword parameters.
 The VAR= and CLASS= parameters are required.  
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%sprdplot(data=animals, var=survive, class=treat poison);
 
==Parameters:

* DATA=       Name of the input dataset [Default: DATA=_LAST_]

* VAR=        [R] Name of the variable to be transformed.  Must be
              numeric, and should contain all positive values.

* CLASS=      [R] Names of one or more class variables.  Only the first
              CLASS= variable is used as point labels in the graphics
				  plot.

* GROUPS=     If the CLASS= variable is continuous, use the GROUPS= 
              parameter to specify the number of groups formed from
              the ordered values of the CLASS= variable.  e.g.,
				  GROUPS=10 divides the CLASS= variable into deciles.
				  [Default: GROUPS=]

* LEVEL=      The measure of location used for each group, either
              MEDIAN or MEAN [Default: LEVEL=Median]

* SPREAD=     The measure of spread used for each group, either
              QRANGE or STD [Default: SPREAD=Qrange]

* OFFSET=     Constant added to the VAR= variable before transformation.
              If the variable contains negative values, OFFSET is
				  set equal to the abs(minimum) value, to ensure that all values
				  are positive.

* PREFIX=     Prefix for name of transformed variable.  If the PREFIX
              is T_ and BEST=1, the transformed variable is named T_&var.  
				  If BEST>1, the variables are named T_1&var, T_2&var, ...
				  [Default: PREFIX=T_]

* PLIST=      List of powers to consider.  Should be a blank-separated
              list of numbers in increasing order.
				  [Default: PLIST=-3 -2 -1 -.5 0 .5 1 2 3]

* BEST=       Number of best powers to transform &var [Default: BEST=1]

* PPLOT=      Produce a printer plot? [Default: PPLOT=N]

* GPLOT=      Produce a graphics plot? [Default: GPLOT=Y]

* HTEXT=      Height of text in graphics plot [Default: HTEXT=1.7]

* VAXIS=      Name of an AXIS statement for the vertical (spread) axis

* HAXIS=      Name of an AXIS statement for the horizontal (level) axis

* OUT=        Name of the output dataset containing the transformed
              variable [Default: OUT=&DATA]
                

 =*/
 
%macro sprdplot(
	data=_last_,    /* name of input dataset                        */
	class=,         /* names of one or more class variables [R]     */
	var=,           /* name of the variable to be transformed [R]   */
	offset=0,       /* constant added to &var before transformation */
	prefix=t_,      /* prefix for name of transformed variable      */
	plist=-3 -2 -1 -.5 0 .5 1 2 3,   /* list of powers to consider*/
	best=1,         /* number of best powers to transform &var      */
	groups=,        /* number of groups, for continous CLASS=       */
	level=Median,   /* location measure */
	spread=Qrange,  /* spread measure */
	pplot=N,
	gplot=Y,
	htext=1.7,
	vaxis=,
	haxis=,
	sploc=30 10,    /* (x,y)% location for slope, power annotation  */
	name=sprdplt,   /* name of graphics catalog entry */
	out=&data,      /* name of output dataset                       */
	gout=           /* graphics catalog */
	);
	
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
%if %length(&VAR)=0 %then %do;
   %put ERROR: No analysis variable has been specified;
   %put %str(       )Specify a VAR= variable.;
   %goto done;
   %end;

%if %length(&CLASS)=0 %then %do;
   %put ERROR: No CLASS variable(s) have been specified;
   %put %str(       )Specify a CLASS= variable.;
   %goto done;
   %end;

%*-- Check for a valid input data set;
%let data=%upcase(&data);
%if (&data=_LAST_ and &syslast=_NULL_) or &data=_NULL_ %then %do;
   %put ERROR:  There is no default input data set.;
   %goto done;
	%end;
%else %if &data=_LAST_ %then %let data=&syslast;

%if &gout^=%str()  %then %let gout=GOUT=&gout;
%let pplot=%substr(%upcase(&pplot),1,1);
%let gplot=%substr(%upcase(&gplot),1,1);

%if %length(&groups) > 0 %then %do;
	proc rank data=&data out=&data groups=&groups;
		var &class;
		ranks _class_;
		run;
	%let class = _class_;
	%end;

proc sort data=&data out=_sorted_;
   by &class;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

proc univariate data=_sorted_ noprint;
     by  &class;
     var &var;
     output out=_sumry_ &level=&level &spread=&spread  n=n min=min;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

*-- Check whether offset is required;
data _null_;
	set _sumry_ end=eof;
	retain minvar 1E8;
	minvar = min(min, minvar);
	if eof then do;
		if minvar<0 then do;
			offset = abs(ceil(minvar));
			put 'WARNING:  Data with negative values require setting OFFSET=';
			put '          ' offset ' or larger';
			call symput('offset', left(put(offset, best8.)));
			end;
		end;
	run;

title2 'Spread-level plot to determine power transformation';
data _sumry_;
	set _sumry_;
	logm =  log10(&level + &offset);
	logs = log10(&spread + &offset);
	label logm="log &level, &var"
	%if %upcase(&spread)=QRANGE
		%then logs="log IQR, &var" ;
		%else logs="log &spread, &var" ;
			;
	run;

proc print label;
	id &class;
	var n &level &spread logm logs;

*-- Determine whether there is a need to transform the data
    to equalize variability;
 
%if &pplot=Y %then %do;
proc plot data=_sumry_;
     plot logs * logm = %scan(&class,1);
	  run;
	%end;

*-- Find the slope. Suggested power = 1 - slope;
proc reg data=_sumry_ outest=_parms_ noprint;
     model logs = logm;
*	  weight n;

%if &gplot=Y %then %do;
%labels(data=_sumry_, x=logm, y=logs, text=left(%scan(&class,1)),
       size=&htext, pos=2, out=_label_);

data _slope_;
   set _parms_(keep=logm);
	drop logm power;	
   xsys='1'; ysys='1';
   length text $16 function $8;
   x = %scan(&sploc,1);   
   y = %scan(&sploc,2);
   function = 'LABEL';
   size = &htext;
   power = round(1-logm, .5);
   position='6'; text = 'Slope: ' || put(logm,f5.2);  output;
   position='9'; text = 'Power: ' || put(power,  f6.1);  output;

data _label_;
	set _slope_ _label_;
title2;
%if %length(&vaxis)=0 %then %do;
	%let vaxis=axis1;
	axis1 label=(a=90);
	%end;
proc gplot data=_sumry_ &gout;
     plot logs * logm  / 
	  		frame anno=_label_ vm=1 hm=1 vaxis=&vaxis
			name="&name"
			des = "Spread-Level plot of &var by &class";
	  symbol i=rl v=dot h=1.5 c=black ci=red w=2;
	  axis1 label=(a=90);
	  run; quit;
	%end;

data _powers_;
	set _parms_(keep=logm rename=(logm=slope));
	array pow[9] _temporary_ (&plist);
	powr = 1-slope;
	do i=1 to dim(pow);
		dif = abs( powr - pow[i] );
		power = pow[i];
		output;
		end;
proc sort;
	by dif;
*proc print;	

*-- find the &best good powers, store their values in macro variables;
%let power=;
data _null_;
	set _powers_(firstobs=1 obs=&best);
	pn = 'p'||put(_n_,1.);
	call symput(pn, left(put(power,best4.)));
	put 'SPRDPLOT:  Slope =' slope 5.2 '   Good power (' +0 pn ') = round(1-slope) = ' power;

	if _n_=1 then do;
		if power=1 then do;
		put "SPRDPLOT:  No apparent need to transform variable &var";
		end;
		else do;
		put 'SPRDPLOT: This transformation is acceptable ONLY if the Spread-Level' /
			 '          plot is reasonably close to linear.  You be the judge!!';	
		call symput('power', left(put(power,best4.)));
		end;
	end;
	run;

%*--- Construct output dataset;
%if %length(&power)>0 %then %do;
	%let lp = %length(&prefix);
	data &out;
		set &data;
		%if &best=1 %then %do;
			%let tvar = &prefix.&var;
			%if %length(&tvar)>8 %then
				%let tvar = %substr(&tvar,1,8);
			&tvar =  %ladder(&var, &power, &offset);
			label &tvar = "%lname(&var, &power)";
			%end;
		%else %do i=1 %to &best;
			%let tvar = &prefix.&i.&var;
			%if %length(&tvar)>8 %then
				%let tvar = %substr(&tvar,1,8);
			%*let tvar = &prefix.&i.%substr(&var,1,%eval(8-&lp-1));
			&tvar =  %ladder(&var, &&p&i, &offset);
			label &tvar = "%lname(&var, &&p&i)";
		%end;
	%end;

*proc print label;	

%*-- Clean up temporary data sets;
proc datasets nolist nowarn library=work memtype=(data);
    delete _sumry_ _slope_ _label_ _powers_ _parms_;
	 run; quit;

%done:;
%if &abort %then %put ERROR: The SPRDPLOT macro ended abnormally.;

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;

%* -- Computer ladder of powers transformation, except multiply by 100
      for negative powers;
%macro ladder(var, p, c);
	%if %bquote(&p) = %bquote(0) %then log10(&var + &c);
	%else %if %bquote(&p) = %bquote(0) %then ((&var + &c)**&p)/&p;
	%else                  (100*(&var + &c)**&p)/&p;
%mend;

%* -- Construct a label for transformed variable;
%macro lname(var, p);
	%if %bquote(&p) = %bquote(0) %then log10(%upcase(&var));
	%else %if %bquote(&p)= %bquote(0.5) %then sqrt(%upcase(&var));
	%else %if %bquote(&p) = %bquote(-0.5) %then 1/sqrt(%upcase(&var));
	%else %if %bquote(&p) = %bquote(-1) %then 1/%upcase(&var);
	%else %upcase(&var)**&p;
%mend;
