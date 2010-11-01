 /*-----------------------------------------------------------------*
  |     Name: genscat.sas                                           |
  |    Title: Generalized scatterplot matrix                        |
         Doc: http://www.datavis.ca/sasmac/genscat.html       
  | ----------------------------------------------------------------|
  |    Procs: summary contents sort freq means gplot catalog        |
  |  Macdefs: genscat                                               |
  |   Macros: gdispla gensym boxaxis contour table mosaic boxplot   |
  |           panels                                                |
  | ----------------------------------------------------------------|
  |   Author: Michael Friendly <friendly@yorku.ca>                  |
  |  Created: 17 Jul 2001 09:41:39                                  |
  | Revised:  17 Jul 2001 09:41:39                                  |
  |  Version: 1.0                                                   |
  *-----------------------------------------------------------------*/

 /*=
=Description:
 
 The GENSCAT macro produces generalized scatterplot matrix for
 all pairs of variables, which may be categorical or numeric
 (Friendly, 1999).  For pairs consisting of two numeric variables, an 
 ordinary scatterplot is produced; pairs of two categorical variables 
 are shown by a mosaic plot; pairs of one categorical and one numeric 
 variable are shown by boxplots.

=Usage:

 The GENSCAT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%genscat(data=test, var=X1 X2 Group Sex, group=gp);
 
==Parameters:

* DATA=       Name of the data set to be plotted. [Default: DATA=_LAST_]

* VAR=        List of the variables to be plotted. You can use any of
              the standard SAS abbreviations. [Default: VAR=_NUMERIC_]

* NAMES=      Alternative variable names used to label the diagonal
              cells

* TYPES=      List of variable types, corresponding to the names in VAR=
              used to override the default treatment of numeric variables:
              a list of C and N.

* GROUP=      Grouping variable (determines plot symbols in scatterplots)

* INTERP=     Plot interpolation method [Default: INTERP=NONE]

* HSYM=       Height of plot symbols

* HTITLE=     Height of variable name titles for diag panels

* PLOTOPT=    Additional plot options

* SYMBOLS=    List of symbols used for observations in scatterplots.
              [Default: SYMBOLS=%STR(CIRCLE SQUARE + : $ = X _ Y)]

* COLORS=     List of colors used in scatterplots and boxplots.
              [Default: COLORS=BLUE RED GREEN BROWN BLACK YELLOW ORANGE PURPLE]

* ANNO=       Annotate diagonal or off-diag plot. At present, ANNO=BOX adds a boxplot
              to diagonal plots, ANNO=ELLIPSE adds a data ellipse to off-diagonal
              plots for pairs of numeric variables. [Default: ANNO=NONE]

* GTEMP=      Temporary graphics catalog.  For repeated use within
              the SAS Session Manager, need to start a new catalog,
				  and kill it at the end.  [Default: GTEMP=GTEMP]

* KILL=       Delete GTEMP when done [Default: KILL=Y]

* GOUT=       Name for output graphics catalog [Default: GOUT=GSEG]

* NAME=       Graph name in output graphics catalog [Default: GOUT=GENSCAT]

==Dependencies:

 GENSCAT requires the following macros:

 gdispla
 gensym
 boxaxis
 contour
 table
 mosaic
 boxplot
 panels

=References:

 Friendly, M. ``Extending Mosaic Displays: Marginal, Conditional, and
 Partial Views of Categorical Data'', JCGS, 8:373-395, 1999.
 L<http://www.datavis.ca/papers/drew/drew.pdf>

 =*/

%macro genscat(
	data =_LAST_,          /* data set to be plotted             */
	var  =_NUMERIC_,       /* variables to be plotted - can be   */
								  /* a list or X1-X4 or VARA--VARB      */
	names=,                /* Alternative variable names         */
	types=,                /* Variable types: list of C and N    */
	group=,                /* grouping variable (plot symbol)    */
	interp=none,           /* plot interpolation method          */
	hsym=,                 /* height of plot symbols             */
	htitle=,               /* height of variable name titles     */
	plotopt=,              /* additional plot options            */
	symbols=%str(circle square + : $ = X _ Y),
	colors=BLUE RED GREEN BROWN BLACK YELLOW ORANGE PURPLE,
	anno=NONE,             /* annotate diag or off-diag plot     */
	gtemp=gtemp,           /* temporary graphics catalog         */
	kill=Y,                /* delete gtemp when done             */
	gout=GSEG,             /* graphic catalog for plot matrix    */
    name=genscat
    );
 
 options nonotes;
%local i j nvar;
%let anno=%upcase(&anno);
%if %upcase(&data)=_LAST_ %then %let data = &syslast;

%*-- For Session Manager, need to start a new graphic catalog;
%*-- Use a temporary graphics catalog to create the individual plots;

%let nvar = %words(&var);
 
*-- Parse variables list;
 data _null_;
 set &data (obs=1) nobs=nobs;
 	call symput('nobs', trim(left(put(nobs,8.))) );
    %if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 
       %* find the number of variables in the list and
         convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        if _vname_ ne "&group" then do;
           nvar + 1;
	        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
        end;
     end;
     call symput( 'VAR', trim(_vlist_) );
   %end;

     * default symbol height, if hsym not specified;
   ht = scan('1.4 2.3 3 3.7 4.2 4.5 5 5.3 5.4 5.5 5.6 5.7',&nvar,' ');
   call symput('HT',trim(left(put(ht,3.1))));
 RUN;
%if &nvar < 2 or &nvar > 12 %then %do;
   %put Cannot do a scatterplot matrix for &nvar variables ;
   %goto DONE;
   %end;
 
 /*----------------------------------------------------*
  | Determine type of each variable                    |
  *----------------------------------------------------*/
	data _tmp_;
		set &data;
		stop;
	%*** use summary to reorder the variables in order of var list;
	proc summary data=_tmp_(firstobs=1 obs=1);
		id &var;
		output out=_tmp1_(drop=_TYPE_ _FREQ_);
		
	proc contents data=_tmp1_(keep=&var) 
		noprint out=_vars_(keep=name type label format npos);run;
	%*** sort by position of variables in the list;
	proc sort data=_vars_;
	   by npos;
	data _null_;
		set _vars_(rename=(type=typ));
		if typ=1 then type='N';
			else type='C';
		%*-- Store variable info in macro variables: name1, type1, etc;
		call symput("name"||left(put(_n_,5.)),trim(name));
      call symput("type"||left(put(_n_,5.)),trim(type));
      call symput("fmt"||left(put(_n_,5.)),trim(format));
	run;
 

 /*----------------------------------------------------*
  | Determine grouping variable and plotting symbol(s) |
  *----------------------------------------------------*/
%if &group = %str() %then %do;
   %let NGROUPS=1;
   %let plotsym=1;      /* SYMBOL for data panels  */
   %let plotnam=2;      /* for variable name panel */
   %end;
%else %do;
   %let plotsym=&group;
   *-- How many levels of group variable? --;
   proc freq data = &data;
      tables &group / noprint out=_DATA_;
   data _null_;
      set _LAST_(obs=1) nobs=ngroups;
      call symput( 'NGROUPS', put(ngroups,3.) );
    run;
    %let plotnam=%eval(&ngroups+1);
%end;
 
%if &hsym = %str() %then %let h=&ht;
                   %else %let h=&hsym;
%gdispla(OFF);
 
title h=0.01 ' ';
%let annoopt=;
%let plotnum=0;    * number of plots made;
%let replay = ;    * replay list;

%if %length(&htitle)=0 %then %let htitle=2*&nvar;
 
%do i = 1 %to &nvar;                   /* rows */
   %let vi = %scan(&var , &i );
	%let ni=;
	%if %length(&names)>0
		%then %let ni = %scan(&names , &i );
	%let ti = %upcase(%scan(&types , &i ));
	%if %length(&ti)=0
		%then %let ti = &&type&i;
		%else &let ti = %substr(&ti,1,1);
	%let fi = &&fmt&i;
	%if &ti=N %then %do;
   proc means noprint data=&data;
      var &vi;
      output out=minmax min=min max=max;
	%end;
 
	%gensym(n=&ngroups, h=&h, interp=&interp, symbols=&symbols, colors=&colors);
	SYMBOL&plotnam v=none i=none;

   %do j = 1 %to &nvar;                /* cols */
      %let vj = %scan(&var , &j );
		%let tj = %upcase(%scan(&types , &j ));
		%if %length(&tj)=0
			%then %let tj = &&type&j;
			%else &let tj = %substr(&tj,1,1);
		%let fj = &&fmt&j;

      %let plotnum = %eval(&plotnum+1);
      %let replay  = &replay &plotnum:&plotnum ;
      %put plot &plotnum: &vi vs. &vj (&ti, &tj);
 
      %if &i = &j %then %do;           /*---- diagonal panel ----*/
         data title;
            length text function $8;
            xsys = '1'; ysys = '1';
            x = 50; y = 50;
            text = "&vi";
				%if %length(&ni)>0 %then %str(text = "&ni";);
            size = &htitle;
            function = 'LABEL';  output;
				
			%if &ti=N %then %do;
            set minmax;
            x = 6; y = 6; position = '6';
            text = left(put(min, best6.));
            size = min(&htitle/2,5);
            output;
 
            x = 95; y = 95; position = '4';
            text = trim(put(max, best6.));
            size = min(&htitle/2,5);
            output;
 
				%if %index(&anno,BOX) %then %do;
				%boxaxis(data=&data, var=&vi, baxis=y, oaxis=x, out=_anno_,
					pos=50, boxwidth=10, hsym=&h);
				data title;
					set title _anno_;
				%end;
			%end;

         proc gplot data = &data gout=&gtemp;
            plot &vi * &vi = &plotnam
            / frame anno=title vaxis=axis1 haxis=axis1
				  name="scat&i" des="GENMAT title &vi";
         axis1 label=none value=none major=none
               minor=none offset=(2);
         run; quit;
      %end;
 
      %else %do;                     /*---- off-diagonal panel ----*/

			%if &ti.&tj=NN %then %do;   /*- both variables numeric -*/
				%let inanno=;
				%if %index(&anno,ELLIPSE) %then %do;
				%contour(data=&data, x=&vj, y=&vi, group=&group, annoadd=NONE,
						out=_anno_, colors=&colors, plot=NO, points=30, line=1);
				%let annoopt=anno=_anno_;
				%let inanno=_anno_;
				%end;
				
				proc gplot data = &data gout=&gtemp;
					plot &vi * &vj = &plotsym / &annoopt
						frame nolegend vaxis=axis1 haxis=axis1 &plotopt
						name="scat&i.&j" des="GENSCAT plot &vi, &vj";
				axis1 label=none value=none major=none minor=none offset=(2);
				run; quit;
			%end;  /* &ti.&tj=NN */

			%else %if &ti.&tj=CC %then %do;  /*- both variables character -*/
				%table(data=&data, var=&vi &vj, out=_table_);
				%mosaic(data=_table_, vorder=&vj &vi, plots=2, 
					vlabels=0, htext=&h, gout=&gtemp, name=scat&i.&j);
			%end;

			%else %if &ti.&tj=NC %then %do;
         axis1 label=none value=none major=none
               minor=none offset=(2);
				%boxplot(data=&data, var=&vi,
					class=&vj, classfmt=&fj,
					cfill=&colors,varlab=., name=scat&i.&j,
					gout=&gtemp, print=, vaxis=axis1);
			%end;

			%else %if &ti.&tj=CN %then %do;
         axis1 label=none value=none major=none
               minor=none offset=(2);
				%*--  these should be horizontal boxplots; 
				%boxplot(data=&data, var=&vj,
					class=&vi, classfmt=&fi,
					cfill=&colors,varlab=., name=scat&i.&j,
					gout=&gtemp, print=, vaxis=axis1);
			%end;
      %end;                     /*---- off-diagonal panel ----*/
 
   %end; /* cols */
%end;    /* rows */
 
%gdispla(ON);
 
%if &nvar>0 %then %do;
	%let first = %eval(1 - &nvar*&nvar);
   %panels(rows=&nvar, cols=&nvar, order=down, first=&first, last=0,
		gin=&gtemp, name=&name);
	%end;

%if &kill=Y %then %do;
  proc catalog kill catalog=&gtemp et=grseg;
run; quit;
%end;

%DONE:
  options notes;
  goptions reset=symbol;

%mend;
