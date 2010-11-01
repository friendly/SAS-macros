 /*--------------------------------------------------------------*
  *    Name: coplot.sas                                          *
  *   Title: Construct a conditioning plot - plots of y * x | z  *
  *          conditioned on the value of z.                      *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/coplot.html     *
  *                                                              *
  * %coplot(data=, x=, y=, given=);                              *
  *--------------------------------------------------------------*
  *   Procs: freq means gplot sort sql summary print             *
  * Macdefs: coplot gensym slice                                 *
  * Macrefs: gensym gdispla gask slice panels                    *
  * -------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  27 May 1996 12:58:26                               *
  * Revised:  30 May 2004 09:47:12                               *
  * Version:  1.4                                                *
  * 1.2  inlined %slice; added given strips to plots             *
  *      Handle a character GIVEN variable                       *
  * 1.3  Fixed bug in call to panels when coplot was called more *
  *      than once in a job or session. Fixed V7+ bug with lcase *
  *      variable names.                                         *
  *      Added ANNO=ELLIPSE to add a data ellipse in each plot   *
  *      Added ANNO=LOWESS to add a lowess smooth in each plot   *
  *      Added GTEMP=
  *      Fixed bug with upper/lower in proc sql, v8+             *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The COPLOT macro plots a collection of XY plots, for subsets of a
 data set conditioned by a GIVEN= variable.  These help to show
 how the relation between X and Y varies with the given variable.

=Usage:

 The COPLOT macro is defined with keyword parameters.  The X=, Y=,
 and GIVEN=  parameters are required.  Either or both of the ROWS=
 and COLS= parameters may be used to arrange the collection of
 plots in a rectangular grid.  
 
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%coplot(x=weight, y=mpg, given=Origin);
 
==Parameters:

* DATA=       Name of the data set to be plotted [Default: DATA=_LAST_]

* X=          Horizontal variable in each plot

* Y=          Vertical variable in each plot

* GIVEN=      The name of the slicing variable.  May be a numeric
              or character variable.  If numeric, it it treated as
				  continuous.

* GROUP=      Grouping variable (determines the plot symbol/color)

* SLICES=     Number of slices of the GIVEN= variable.  Ignored if
              the given variable is character. [Default: SLICES=4]

* OVERLAP=    Allowed overlap between slices.  Ignored if
              the given variable is character. [Default: OVERLAP=0.25]

* ROWS=       Number of rows in the panel plot.  If neither ROWS= nor
              COLS= are specified, both values are calculated from the
				  SLICES= value to give a square-ish display.  If only one
				  of ROWS= or COLS= is specified, the other value is calculated.

* COLS=       Number of columns in the panel plot.

* INTERP=     Plot interpolation method in each panel. E.g.,
              INTERP=RL draws a linear regression line.
				  [Default: INTERP=NONE]

* ANNO=       Additional annotation(s) to each plot.  ANNO=ELLIPSE
              provides a 50% data ellipse.  ANNO=LOWESS includes a
				  smoothed lowess curve.  In the present version, there
				  is no control over the details (parameters) used
				  to fit the data ellipse or lowess smooth.
				  [Default: ANNO=]

* HSYM=       Height of plot symbols and value labels in the plots.

* GPANEL=     Height of the given panel, in % of the plot size. 
              If GPANEL=0, the given panel is suppressed. [Default: GPANEL=0]

* SYMBOLS=    List of symbols to use for for groups defined by the GROUP=
              variable. [Default: SYMBOLS=%STR(CIRCLE SQUARE + : $ = X _ Y)]

* COLORS=     List of colors to use for for groups.
              [Default: COLORS=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE]

* GOUT=       Name of the output graphics catalog. [Default: GOUT=GSEG]

=Dependencies:

 Requires panels.sas, gdispla.sas and gask.sas

=Limitations:

 The handling of a given panel is ugly (hence the default GPANEL=0).
 Labeling of the X,Y axes is only slightly less ugly, but a 'feature'
 of the fact that each panel is graphed separately, rather than all
 together.
  
 =*/
 
*include macros(panels);
*include macros(gask);
%macro coplot(
	data =_LAST_,          /* data set to be plotted                  */
	x=,                    /* horizontal variable in each plot        */
	y=,                    /* vertical variable in each plot          */
	given=,                /* slicing variable                        */
	group=,                /* grouping variable (plot symbol)         */
	slices=4,              /* number of slices of the given var       */
	overlap=.25,           /* allowed overlap between slices          */
	rows=,                 /* number of rows in the panel plot        */
	cols=,                 /* number of cols in the panel plot        */
	interp=none,           /* plot interpolation method in each panel */
	anno=,
	hsym=,                 /* height of plot symbols                  */
	gpanel=0,              /* height of given panel, in % (0=none)    */
	symbols=%str(circle dot square triangle diamond hash + : $ = X _ Y),  
	                       /* symbols for groups */
	colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE, /* colors */
   gtemp=gtemp,           /* temporary graphics catalog         */
   kill=Y,                /* delete gtemp when done?            */

	gout=GSEG);            /* graphic catalog for plot matrix         */

	%*-- Reset required global options;
	%if &sysver >= 7 %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
*	   options nonotes;
		%end;

%let abort=0;
%if %length(&x)=0 | %length(&y)=0 | %length(&given)=0
   %then %do;
      %put ERROR: The X=, Y= and GIVEN= parameters must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%let anno=%upcase(&anno);

 /*----------------------------------------------------*
  | Determine grouping variable and plotting symbol(s) |
  *----------------------------------------------------*/
%if %length(&group) = 0 %then %do;
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

%slice(data=&data, var=&given,
       slices=&slices, overlap=&overlap, out=_expand_,outs=_slices_);
%if &sltype=CHAR %then %let slices=&nslice;

data _null_;
	%if %length(&rows)=0 or "&rows"="0" %then %do;
		%if %length(&cols)=0 or "&cols"="0" %then %do;
			rows = floor(sqrt(&slices));
			cols = ceil(sqrt(&slices));
		   call symput('rows', put(rows,2.));
		   call symput('cols', put(cols,2.));
			%end;
		%else %do;
			rows = ceil(&slices/&cols);
		   call symput('rows', put(rows,2.));
			%end;
		%end;
	%else %do;
		cols = ceil(&slices/&rows);
		call symput('cols', put(cols,2.));
		%end;
run;
%put COPLOT: rows=&rows  cols=&cols groups=&ngroups slices=&slices;

 data _null_;
     %* default symbol height, if hsym not specified;
	%if %length(&hsym)=0 %then %do;
   	nv = max(&rows, &cols);
*  	ht = scan('1.4 2.8 2.3 3 3.7 4.2 4.5 5 5.3 5.4 5.5',nv,' ');
   	ht = 1.0*nv;
		%end;
	%else %do;
		ht = &hsym;
		%end;
   htitle = 1.5*ht;
   call symput('ht',put(ht,4.1));
   call symput('htitle',put(htitle,4.1));
	*put ht=;
 run;

%if &hsym = %str() %then %let h=&ht;
                   %else %let h=&hsym;
%gensym(n=&ngroups, h=&h, i=&interp, symbols=&symbols, colors=&colors);

**-- Find nice scale numbers for all plots;
%put COPLOT: Finding nice scale values for all plots...;
proc means noprint data=&data;
   var &x &y;
	where (&x is not missing) & (&y is not missing);
   output out=_minmax_ min=xmin ymin max=xmax ymax;
run;

data _null_;
   set _minmax_;
   array mn(2) xmin ymin;
   array mx(2) xmax ymax;
   array by(2)  xby  yby;
   do i = 1 to 2;
      r = mx(i)-mn(i);
      hinc= r/3;
      pow = 10**floor( log10(hinc) );
      nice=1000;
      do inc = 1, 2, 2.5, 4, 5;
         ut = inc * pow;
         if abs(hinc-ut) < nice then do;
            nice = abs(hinc-ut);
            best = ut;
         end;
      end;
      by(i)=best;
      mn(i) = by(i) * floor(mn(i)/by(i));
      mx(i) = by(i) * ceil (mx(i)/by(i));
   end;
   put xmin= xmax= xby=
       ymin= ymax= yby=;
   call symput('xmin', put(xmin,best8.));
   call symput('xmax', put(xmax,best8.));
   call symput('xby' , put(xby, best8.));
   call symput('ymin', put(ymin,best8.));
   call symput('ymax', put(ymax,best8.));
   call symput('yby' , put(yby, best8.));
	run;

title h=0.01 ' ';
%put COPLOT: Plotting  &slices  slices...;
%gdispla(OFF);
%let replay = ;    * replay list;
%do i = 1 %to &slices;
   %let row = %eval(1 + ((&i-1)/&cols));
   %let col = %eval(&i - (&row-1) * &cols);
   %* put slice &i row= &row col= &col;
   %let replay  = &replay &i:&i ;
   %* put plotting slice &i ;

	%let inanno=;
	%if %index(&anno,ELLIPSE) %then %do;
	%contour(data=_expand_, x=&x, y=&y, group=&group, annoadd=NONE,
			out=_ellip_, colors=&colors, plot=NO, where=slice=&i,
			pvalue=0.50);
	%let inanno=_ellip_;
	%end;
	%if %index(&anno,LOWESS) %then %do;
	%lowess(data=_expand_, x=&x, y=&y, subset=slice=&i, p=1, outanno=_anno2_,
			colors=&colors, gplot=NO, pplot=NO,
			silent=YES, step=0, in=&inanno);
	%let inanno=_anno2_;
	%end;


data _strip_;
   set _slices_;
   where (slice=&i);
	drop lower upper lopct uppct;
	length text $12 function $8;
   retain xsys '1' ysys '1' style 'solid';
	
	color='yellow'; line=0;
   x=lopct; y=100;   function='MOVE   '; output;
   x=uppct; y=96;    function='BAR    '; output;

	color='grayb0';  line=1;
	y = 96;
	x =  0;    function='MOVE   '; output;
	x =100;    function='DRAW   '; output;

	style=''; color='black';
	x=mean(of lopct uppct); 
	y=98; hsys='1'; size=4;
	function='label';
	%if &sltype=NUM 
		%then %str(text="&given";);
		%else %str(text=lower;);
	output;
	
	%if &row=1 %then %do;
		xsys='1'; ysys='1';
		x = 50;   y=1; text="&x"; position='B'; output;
		%end;
	%if &col=1 %then %do;
		xsys='1'; ysys='1';
		x = 0;   y=50; text="&y"; position='E'; angle=90; output;
		%end;
	*proc print;

	%if %length(&inanno) > 0 %then %do;
	data _strip_;
		set _strip_ &inanno;
		%end;

   proc gplot data = _expand_ anno=_strip_ gout=&gtemp;
      plot &y * &x = &plotsym
      / frame nolegend vaxis=axis1 haxis=axis2 vm=0 hm=0
		  name="coplot&i"  des="Coplot of &y * &x (&i)";
      where (slice=&i);
      axis1 order=(&ymin to &ymax by &yby)
         label=none
         %if &col>1 %then value=none ;
                    %else value=(h=&h) ;
   /*    value=none  */
         offset=(6pct,6pct);
      axis2 order=(&xmin to &xmax by &xby)
         label=none
         %if &row>1 %then value=none ;
                    %else value=(h=&h) ;
         offset=(5);
   run; quit;
%end;    /* slices */

%if &gpanel > 0 %then %do;
%gask;   *-- get hpos, vpos;
	data _boxes_;
		set _slices_ end=last;
		retain ysys '2' line 0;
		xsys = '2';
		if lower=upper then h=.6;
			else h=.3;
		x=lower; y=slice-h;   function='MOVE   '; output;
		x=upper; y=slice+h;   function='BAR    '; output;
		if last then do;
			xsys = '1'; line=3;
			%do ln = 2 %to &rows;
			y = (&ln - 1)*&cols + .5;
			x =  0;  function='MOVE    '; output;
			x =100;  function='DRAW    '; output;
			%end;
		end;

%gdispla(ON);
	%let sl1 = %eval(&slices + 1);
	%let sl1 = %eval((&rows * &cols) + 1);
	%* &i has already been incremented;
	%let replay = &replay &sl1:&i;
	goptions vsize=1.8in vpos=50 hpos=180;
	title h=&htitle "Given:  &given";
	proc gplot data=_boxes_ gout=&gtemp;
		plot slice * lower = 1
				slice * upper = 1
				/ overlay frame anno=boxes  vaxis=axis1 haxis=axis2;
		symbol1 i=none v=none;
		axis1 order=(0 to &sl1)
			label=none value=none minor=none major=none;
		axis2 label=none value=(h=&ht);
	run; quit;
	%* -- restore goptions;
	goptions vsize=&vsize vpos=&vpos hpos=&hpos;
%end;

%gdispla(ON);
%*-- For multiple runs in a given session, must use first= rather
     than replay=;
%let first = %eval(1 - &slices);
%* put replay= &replay;
%panels(rows=&rows, cols=&cols, plots=&slices, top=&gpanel,
        /* replay=&replay, */ gin=&gtemp, gout=&gout,
		  first=&first, last=0);

%if &kill=Y %then %do;
  proc catalog kill catalog=&gtemp et=grseg;
run; quit;
%end;
%DONE:
	%*-- Restore global options;
	%if &sysver >= 7 %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;


 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
%macro gensym(n=1, h=1.5, i=none,
              symbols=%str(- + : $ = X _ Y),
              colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE);
    %*-- note: only 8 symbols & colors are defined;
    %*--  if more than 8 groups symbols and colors are recycled;
  %local chr col k;
  %do k=1 %to &n ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0 
	      %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0 
	      %then %let colors = &colors &colors;
     %let chr =%scan(&symbols, &k,%str( ));
     %let col =%scan(&colors, &k, %str( ));
     symbol&k h=&h v=&chr c=&col i=&i;
  %end;
  %let k=%eval(&n+1);
     symbol&k v=none;
%mend;

/*-----------------------------------------------------------------*
  Title:   Divide a variable into slices.  

  If the variable is character, each distinct value is a slice.
  Otherwise, the range of the variable is divided into SLICE=
  ranges, allowing each successive pair to overlap by a fraction,
  OVERLAP=.
 *-----------------------------------------------------------------*/
%macro slice(
	data=,          /* name of input data set */
	var=,           /* name of the variable on which to slice */
	slices=4,       /* number of slices (if VAR is numeric) */
	overlap=.25,    /* overlap between adjacent slices (numeric) */
	out=_expand_,   /* name of output data set */
	outs=_slices_   /* name of output slice data set */
	);

%global sltype nslice;
proc sort data=&data;
   by &var;

proc contents data=&data out=_work_ noprint;
%let sltype=;
%let nslice=;
data _null_;
	set _work_;
	if upcase(name)=upcase("&VAR")
		then if type=1 then call symput('sltype', 'NUM');
							else call symput('sltype', 'CHAR');
run;
%*put sltype = &sltype;

%if &sltype=CHAR %then %do;
	*-- Find number of levels of &var and percents;
	proc freq data=&data;
	   tables &var / noprint out=&outs;
	
	data &outs;
		set &outs end=last;
		drop count percent;
		slice+1;
		lower = &var;  upper=&var;
		lopct+(lag(percent));
		if lopct=. then lopct=0;
		uppct+percent;
		if last then do;
			call symput('nslice', put(slice,2.));
			if slice ^= &slices then
				put 'NOTE:  Using ' slice " slices for the discrete variable &var";
			end;

	data &out;
		merge &data &outs(keep=&var slice lower upper);
		by &var;
	%end;    /* sltype=CHAR */

%else %do;
data &outs;
   keep slice lowerx upperx;
   f = &overlap;           *-- allowed overlap;
   k = &slices;            *-- number of intervals;

   r = n / (k*(1-f) + f);  *-- target number in each;

   do slice = 1 to k;
      values = (slice-1)*(1-f)*r;
      index1 = round( 1 + values);
      index2 = round( r + values);
      set &data point=index1 nobs=n; lowerx=&var;
      set &data point=index2;        upperx=&var;
      output;
      end;
   stop;      *-- STOP is required with POINT=;

/*-------------------------------------------------------*
 | Expand the dataset to include duplicate copies of the |
 | observations which occur in two slices.               |
 *-------------------------------------------------------*/
proc sql;
   create table &out as
      select &outs..* , &data..*
      from &outs , &data
      where (lowerx <= &var) &  (&var<= upperx)
      ;
   quit;

proc summary data=&outs;
	var lowerx upperx;
	output out=_minmax_ min=lomin upmin max=lomax upmax;

data &outs;
	set &outs;
	if _n_=1 then set _minmax_;
	drop _type_ _freq_ lomin upmin lomax upmax;
	lopct = 100*(lower-lomin)/(upmax-lomin);
	uppct = 100*(upper-lomin)/(upmax-lomin);
run;
%end;    /* sltype=NUM */
*proc print data=&outs;
%mend;
