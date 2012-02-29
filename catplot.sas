/*-------------------------------------------------------------------*
 *    Name: catplot.sas                                              *
 *   Title: Plot observed and predicted logits for logit models      *
 *          fit by PROC CATMOD.                                      *
       Doc: http://www.datavis.ca/sasmac/catplot.html             
 *                                                                   *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@YorkU.CA>         *
 * Created:  9 May 1991 12:20:09         Copyright (c) 1992          *
 * Revised:  01 Oct 2009 15:04:38                                    *
 * Version:  1.4-2                                                   *
 *  1.4   Fixed validvarname for V7+                                 *
 * Requires: %gensym                                                 *
 *                                                                   *
 * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
 *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The CATPLOT macro is designed to plot observed and/or predicted
 values for logit models fit by the CATMOD procedure.  The macro uses
 the output data set produced with the OUT= option on the RESPONSE
 statement.  This data set normally contains both logit values
 (_TYPE_='FUNCTION') and probability values (_TYPE_='PROB').  Either
 set may be plotted, as specified by the TYPE= parameter.

 The horizontal variable may be character (XC=) or numeric (X=).
 A separate curve is drawn for each value of the CLASS= variable,
 connecting predicted values, with optional standard error bars,
 and separate plots are drawn for each value of the BYVAR= variable.
 
=Usage:

 The catplot macro is called with keyword parameters. Either the
 X= or the XC= parameters are required. Use the CLASS= parameter
 to give multiple curves in each plot for the levels of the CLASS
 variable. Use the BYVAR= parameter to give multiple plots for the
 levels of the BYVAR variable. The arguments may be listed within
 parentheses in any order, separated by commas.  For example:

	proc catmod;
		direct husinc;
		response / out=logits;
		model labour = husinc children; 
	%catplot(data=logits, x=husinc, y=_pred_, class=labor, byvar=children);
 
==Parameters:

* DATA=	 The name of the SAS dataset to be plotted, which must be
			 an output data set from PROC CATMOD.  If DATA= is not
			 specified, the most recently created data set is used.

* X=		 Name of a numeric factor variable to be used as the horizontal
			 variable in plots.  Use the XC= parameter to specify a
			 character variable.  You must specify either the X= or XC=
			 variable.

* XC=		 Name of a character factor variable used as the horizontal
			 variable in plots.

* Y=		 Name of the ordinate variable.  Y=_PRED_ plots the predicted
			 value; Y=_OBS_ plots the observed value.  The default is
			 Y=_OBS_, but the predicted values are also drawn, connected
			 by lines. [Default: Y=_OBS_]

* CLASS=  The name of a factor variable, used to define separate curves
			 which are plotted for each level of this variable.

* BYVAR=	 Name of one or more factor variables to be used to define 
			 multiple panels in plots.  

* BYFMT=  Name of a SAS format used to format the value of BYVARs
			 for display in one panel of the plot(s). [Default: BYFMT=$16.]

* TYPE=   The type of observations to be plotted.  TYPE=FUNCTION (the
			 default) gives plots of the logit value; TYPE=PROB gives
			 plots of the probability value. [Default: TYPE=FUNCTION]

* Z=      Standard error multiple for confidence intervals around
			 predicted values, e.g., Z=1.96 gives 95% CI. To suppress error
			 bars, use Z=0.  The default is Z=1, giving 67% CI.

* CLFMT=	 Name of a SAS format used to format the value the CLASS=
			 variable for display in each panel of the plot(s).

* CLSIDE=	Specifies whether the values of the CLASS= variable should
			 be labelled by annotation in the plot or by a legend.  If
			 CLSIDE=LEFT or CLSIDE=FIRST, CLASS= values are written at the
			 left side of each curve.  If CLSIDE=RIGHT or CLSIDE=LAST,
			 CLASS= values are written at the right side of each curve.
			 If CLSIDE=NONE, or if a LEGEND= legend is specified, the 
			 CLASS= values appear in the legend.  You should
			 then define a LEGEND statment and use the LEGEND= parameter.
			 [Default: CLSIDE=LAST]

* XFMT=	 Name of a SAS format used to format the values of the horizontal
			 variable.
			
* POSFMT=   Format to translate the value of the CLASS variable to a 
          SAS/GRAPH annotate position.  This will almost always be a
			 user-specified format created with PROC FORMAT.

* ANNO=	 Name of an additional input annotate data set

* SYMBOLS=	List of SAS/GRAPH symbols for the levels of the CLASS= variable.  
			 The specified symbols are reused cyclically if the number of 
			 distinct values of the \texttt{CLASS=} variable exceeds the 
			 number of symbols. [Default: SYMBOLS=CIRCLE SQUARE TRIANGLE]

* COLORS=	List of SAS/GRAPH colors for the levels of the CLASS= variable.
          The specified colors are reused cyclically if the number of 
			 distinct values of the \texttt{CLASS=} variable exceeds the 
			 number of colors. [Default: COLORS=BLACK RED BLUE GREEN]

* LINES=	 List of SAS/GRAPH line styles for the levels of the CLASS= 
          variable.  The specified line styles are reused cyclically if the 
			 number of distinct values of the \texttt{CLASS=} variable
			 exceeds the number of line styles. [Default: LINES=1 20 41 21 7 14 33 12]

* VAXIS=	 Axis statement for custom response axis, e.g., VAXIS=AXIS1.
          [Default: VAXIS=AXIS1]

* HAXIS=	 Axis statement for custom horizontal axis, e.g., HAXIS=AXIS2
          [Default: HAXIS=AXIS2]

* LEGEND=	Legend statement for custom CLASS legend, e.g., LEGEND=LEGEND1

* PLOC=   For multiple plots (with a BYVAR), PLOC defines the X,Y position
          of the panel label, in graph percentage units. [Default: PLOC=5 95]

* PRINT=  Print summarized input data set? [Default: PRINT=NO]

* NAME=   Name of graphic catalog entry. [Default: NANME=CATPLOT]

 =*/
 
%macro catplot(
    data=_last_,  /* OUT= data set from PROC CATMOD                 */
    x=,           /* horizontal value for plot (NUMERIC)            */
    xc=,          /* horizontal value for plot (CHAR)               */
    y=_obs_,      /* ordinate for plotted points (_PRED_ or _OBS_)  */
	 ylab=,        /* ordinate label                                 */
    class=,       /* variable for curves within each plot           */
    byvar=,       /* one plot for each level of by variable(s)      */
    byfmt=$16.,   /* format for by variable                         */
	 type=FUNCTION,/* type of obs. plotted: FUNCTION or PROB         */
    z=1,          /* std. error multiple for confidence intervals   */
                  /* e.g., z=1.96 gives 95% CI. No error bars: z=0  */
    anno=,        /* additional input annotate data set             */
    clfmt=,       /* how to format values of class variable         */
    clside=last,  /* side for labels of class var (FIRST|LAST|NONE) */
    xfmt=,        /* format for X variable                          */
    posfmt=,      /* format to translate class var to position      */
    vaxis=axis1,  /* axis statement for logit axis                  */
    haxis=axis2,  /* axis statement for horizontal axis             */
    legend=,      /* legend statement for custom CLASS legend       */
    colors=BLACK RED BLUE GREEN,   /* colors for class levels       */
	 symbols=circle square triangle,  /* symbols for class levels    */
    lines=1 20 41 21 7 14 33 12,     /* line styles for class levels */
	 ploc=5 95,  /* location of panel variable label               */
	 print=NO,     /* print summarized input data set?               */
	 name=catplot
    );
 
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=V6;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let type=%upcase(&type);
%let print=%upcase(&print);
%let legend=%upcase(&legend);
%let clside=%upcase(&clside);
%if &clside=LEFT %then %let clside=FIRST;
%if &clside=RIGHT %then %let clside=LAST;

%let abort=0;

%if &x ^= %str() %then %do;
   %let px = &x;
	%let ax = x;
   %end;
%else %do;
   %if &xc = %str() %then %do;
       %put CATPLOT: Either X= or XC= variable must be specified;
		 %let abort=1;
       %goto DONE;
       %end;
   %let px = &xc;
	%let ax = xc;
   %end;
 
%*-- Find the last by-variable;
%if %length(&byvar) > 0 %then %do;
   %let _byvars=;
   %let _bylast=;
   %let n=1;
   %let token=%qupcase(%qscan(&byvar,&n,%str( )));

   %do %while(&token^=);
      %if %index(&token,-) %then
         %put WARNING: Abbreviated BY list &token.  Specify by= individually.;
      %else %do;
         %let token=%unquote(&token);
         %let _byvars=&_byvars &token;
         %let _bylast=&token;
      %end;
      %let n=%eval(&n+1);
      %let token=%qupcase(%scan(&byvar,&n,%str( )));
   %end;
%let nby = %eval(&n-1);
%if %index(&byfmt,%str(.))=0 %then %let byfmt = &byfmt..;
%end;  /* %if &byvar */

 %*-- Select logit (_type_='FUNCTION'), or probability (_type_='PROB') obs. ;
/*
data _pred_;
   set &data;
   drop  _type_ ;
   if _type_="&type";
	%if &type=PROB %then %do;
	label _obs_ = 'Observed probability'
		_pred_ = 'Predicted probability';
		%end;
	%else %do;
	label _obs_ = 'Observed logit'
		_pred_ = 'Predicted logit';
		%end;
*/

 %*-- Average over any other factors not given in &byvar or &class;
proc summary data=&data nway;
   class &byvar &class &px;
   var _pred_ _obs_ _seobs_ _sepred_ _resid_;
	where (_type_="&type");
   output out=_pred_(drop=_type_) mean=;

proc sort;
   by &byvar &class &px;

%if %substr(&print,1,1)=Y %then %do;
proc print data=_pred_;
   id &byvar &class &px;
   var _obs_ _seobs_ _pred_ _sepred_ _resid_;
   format _obs_ _pred_ 8.3 _seobs_ _sepred_ _resid_ 8.4;
%end;

proc contents data=&data out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

data _null_;
   set _work_(keep=name type format);
	%if %length(&clfmt)=0 %then %do;
	if upcase(name) = upcase("&class") then do;
		if format=' ' then do;
			if type = 2
				then format='$16.';
				else format='best.';
			end;
		if index(format,'.')=0 then format=trim(format)||'.';
		call symput('clfmt', format);
		*put name= format=;
		end;
	%end;
 
%let plx = %scan(&ploc,1);
%let ply = %scan(&ploc,2);
%if %length(&posfmt) 
	%then %if %index(&posfmt,%str(.))=0 %then %let posfmt = &posfmt..;
data _anno_;
   set _pred_;
   by &byvar &class;
   length function color $8 text $100;
   retain cl 0;
   drop _seobs_ _sepred_ _resid_ cl;
   %if &byvar ^= %str() %then %do;
		%*-- Label for byvar(s) in this plot;
      goptions hby=0;
      if first.&_bylast then do;
         xsys='1'; ysys='1';
			x = &plx; y=&ply; 
			position='6';
			%if &nby=1 %then %do;
				text = put(&byvar,&byfmt);
				%end;
			%else %do;
				text=' ';
				%do i=1 %to &nby;
					text = trim(text) || %scan(&byvar, &i) || ' ';
					%end;
				%end;
         function = 'LABEL'; output;
         end;
      if first.&_bylast then cl=0;
      %end;
 
   xsys = '2'; ysys='2';
   %*-- Set X or XC variable ;
	&ax = &px;
 
	*-- Index for line/color;
	%if &class = %str()
		%then %do; cl=1; %end;
   	%else %do; if first.&class then cl+1; %end;
   line=input(scan("&lines", cl),5.);
   color = scan("&colors",cl);

	%if (&clside=FIRST or &clside=LAST) & %length(&legend)=0 %then %do;
		if &clside..&class then do;
			y=_pred_;
			%if %length(&clfmt)
				%then %str(text = put(&class,&clfmt););
				%else %str(text = trim(left(&class)););
			*-- Use a null char to move label a bit;
			%if %upcase(&clside) = LAST %then %do;
				position = '6';  text = '00'x || ' ' || text;
				%end;
			%else %do;
				position='4';  text = trim(text) || '00'x;
				%end;
			%if &posfmt ^= %str() %then %do;
			position = put(&class,&posfmt);
			%end;
			function = 'LABEL'; output;
			end;
		%end;

	%if &class = %str()
		%then %do; if _n_=1 then do; %end;
   	%else %do; if first.&class then do; %end;
      y = _pred_; function='MOVE'; output;
      end;
   else do;
      y = _pred_; function='DRAW'; output;
      end;
 
    %if &z > 0 %then %do;
    %*-- plot value +- &z * std error;
    line = 33;
    y = _pred_ + &z * _sepred_ ; function='MOVE'; output;
    y = _pred_ ;                 function='DRAW'; output;
    y = _pred_ - &z * _sepred_ ; function='DRAW'; output;
    y = _pred_ ;                 function='MOVE'; output;
    %end;
 
%if &anno ^= %str() %then %do;
   data _anno_;
      set _anno_ &anno;
   %end;
*proc print data=_anno_;
 
%if &class = %str()
	%then %do;
		%let sym = 1;
		symbol1 i=none v=%scan(&symbols,1) h=1.8 c=%scan(&colors,1);
	%end;
	%else %do;
		%let sym = &class;
		%if %length(&symbols) %then %do;
		*-- How many levels of class variable? --;
		proc freq data = _pred_;
			tables &class / noprint out=_levels_;
		data _null_;
			set _levels_(obs=1) nobs=ngroups;
			call symput( 'NGROUPS', put(ngroups,3.) );
			run;
		%gensym(n=&ngroups, interp=none, symbols=&symbols, colors=&colors);
		%end;
	%end;

%if %length(&legend) %then %let legend=legend=&legend;
%else %if &legend=NONE | &clside ^= NONE %then %let legend=nolegend;


proc gplot data=_pred_;
   plot &y * &px = &sym
        / anno=_anno_ frame
			 &legend
          haxis=&haxis hminor=0
          vaxis=&vaxis vminor=1 name="&name"
			 des="catplot of &data";
   %if &byvar ^= %str() %then %do;
      by &byvar;
      %end;
   %if &xfmt ^= %str() %then %do;
      format &px &xfmt;
      %end;
   %if &ylab ^= %str() %then %do;
      label &y="&ylab";
      %end;
run; quit;

  *-- Clean up datasets no longer needed;
proc datasets nofs nolist nowarn library=work memtype=(data);
    delete _work_ ;
	run; quit;

%done:
%if &abort %then %put ERROR: The CATPLOT macro ended abnormally.;

   goptions hby=;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend catplot;
