/*-------------------------------------------------------------------*
 *    Name: meanplot.sas                                             *
 *   Title: Macro to plot means for a factorial design               *
       Doc: http://www.datavis.ca/sasmac/meanplot.html         
 *                                                                   *
 * The horizontal variable may be character (XC=) or numeric (XVAR=).*
 * A separate curve is drawn for each value of the CVAR= variable,   *
 * with optional standard error bars, and separate plots are drawn   *
 * for each value of the PANELS= variable                            *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@YorkU.CA>         *
 * Created:   2 Apr 1996 12:20:09         Copyright (c) 1996         *
 * Revised:  08 May 2009 08:36:21                                    *
 * Version:  1.7-1                                                   *
 *  - added WHERE=  to select input observations                     *
 *  - added OPTIONS= to pass options to PROC SUMMARY                 *
 *  - added ADJUST= to adjust error bars for multiple comparisons    *
 *    (geometric mean n used when sample size unequal)               *
 *  - fixed unequal n bugs / fixed name case for V7                  *
 *  - added INTERP= option, OUT= (means), HSYM= (symbol ht)          *
 * 1.5 Added CFMT= parameter for curve labels                        *
 *  - added PMEAN= to suppress panel for mean over PANELS= variable  *
 *  - fixed some small bugs                                          *
 *  - added XFMT to format/reorder the horizonal variable            *
 * 1.6 Added ability to plot multiple responses in one call          *
 *  - added %GSKIP when &nv>1                                        *
 *  - added check for missing pfmt                                   *
 * 1.7 Replaced inline gensym with v 1.4
 _  -  Inlined words macro
 *  -  Changed default to PMEAN=NO                                   *
 *-------------------------------------------------------------------*/

 /*=
=Description:
 -----------

 The meanplot macro produces 1-way, 2-way, or 3-way plots of means for
 a factorial design with any number of factor variables.  One-way plots
 show main effect means (and optional standard error bars) for each level
 of the factor variable.  Two-way plots show the means for two factors
 as a set of curves for one variable, plotted against the other variable.
 Three-way means are displayed as a collection of two-way panels, optionally
 including one additional panel showing the means collapsed over the panel
 variable.  Either or both lineprinter (PROC PLOT) plots and high-res
 (PROC GPLOT) plots may be drawn.

 For a 3-factor design, if the factors are A, B, C, the macro, by default,
 uses variable A as the horizontal(XVAR =) variable in the plots, plots 
 separate curves for each value of variable B, and produces separate panels
 for each level of variable C, as well as one additional panel for the average
 of means over variable C.  You can obtain different views of the means
 by reordering the CLASS= variables, or assigning variables to particular
 roles with the XVAR=, CVAR=, and PANELS= arguments.

=Usage:

 To plot the means for a factorial design, specify the name of the
 RESPONSE= variable, and the CLASS= factor variables.  All other
 options have default values.

 The arguments may be listed within parentheses in any order, separated
 by commas. For example:

   %meanplot(data=recall, response=score, class=Group Gender Order)

 To combine the panels in a single plot, use the %panels macro immediately
 after %meanplot, e.g.,:
	%panels(rows=1, cols=4, equate=Y)
 (or cut/paste)

==Parameters:

DATA=		The name of the SAS dataset to be plotted.  If DATA= is not
			specified, the most recently created data set is used.

RESPONSE=   Name of the response (dependent) variable(s) in the input data
			set.  Must be numeric.

VAR=        Synonym for RESPONSE=

CLASS=      The names of 1-3 factor variables.
	
FREQ=       Frequency variable. Each observation is treated as if it
			were repeated as many times as the value of the FREQ=
			variable. Observations with a FREQ= value less than or
			equal to 1 are omitted from the calculation. Fractional values 
			are not allowed.

XVAR=		Name of the factor variable to be used as the horizontal
			variable in plots.  If XVAR= is not specified, the first
			CLASS= variable is used.

XFMT=		Format for the XVAR= variable.  
			

CVAR=		Name of the factor variable to be used as the curve
			variable in plots.  If CVAR= is not specified, the second
			CLASS= variable is used in 2-way and 3-way plots.

CFMT=		Format for the CVAR= variable.  

PANELS=	    Name of the factor variable to be used to define multiple
			panels in plots.  If PANELS= is not specified, the third
			CLASS= variable is used in 3-way plots.

PFMT=		Format for the PANELS= variable.  The default is BEST. if
			the panels variable is numeric, and $16. otherwise.

CMEAN=	    Specifies whether an additional curve, representing the
			average over the levels of the CVAR= variable, is added
			to each panel.

Z=			Std. error multiple for confidence intervals or error
			bars drawn in the GPLOT versions of the plots.  The default,
			Z=1 shows one standard error for each mean plotted. Use
			Z=1.96 for individual 95% CI, or Z=0 for plots without error
			bars.

ADJUST=     Specifies whether to adjust error bars for multiple comparisons.
            In this case, error bars will overlap iff a given pair of means
            do not differ significantly.
            ADJUST=T or LSD provide standard t-value error bars, unadjusted
            for multiple comparisons;  ADJUST=BON provides Bonferroni adjusted
            error bars for all pairwise comparisons;  ADJUST=TUKEY or HSD
            provides Tukey-test adjustments for all pairwise comparisons..

ALPHA=     Specifies the error rate for comparisons made with the
	       ADJUST= option.  Default: 0.5
	
OPTIONS=   Specifies options for PROC SUMMARY.  If you use OPTIONS=NWAY
           only the n-way means will be plotted.

PPLOT=     Specifies whether line printer plots are to be done. Default: NO

GPLOT=     Specifies whether high-res plots are to be done. Default: YES

PRINT=     Specifies whether to print the means. Default: NO

OUT=       Name of output data set containing means and std errors.

Options for high-res PROC GPLOT plots:

ANNO=	   Name of an additional input annotate data set

SYMBOLS=   List of SAS/GRAPH symbols for the levels of the CVAR=variable.  
		   There should be as many symbols as there are distinct values of 
		   the CVAR=variable.

COLORS=	   List of SAS/GRAPH colors (for the GPLOT version) for the levels
           of the CVAR=variable.  There should be as many colors as there
           are distinct values of the CVAR=variable.

LINES=	   List of SAS/GRAPH line styles (for the GPLOT version) for the
			levels of the CVAR=variable.  There should be as many lines as 
			there are distinct values of the CVAR=variable.

INTERP=    Interpolation option for the points along each curve, e.g.,
           INTERP=RL (regression line) or INTERP=JOIN.

HAXIS=	   Axis statement for custom horizontal axis, e.g., HAXIS=AXIS2

VAXIS=	   Axis statement for custom response axis, e.g., VAXIS=AXIS1.  If
			no axis statement is defined, the program uses
			AXIS1 LABEL=(a=90).

LEGEND=	   Legend statement for custom CVAR legend, e.g., LEGEND=LEGEND1
           If no legend is specified, the program positions the legend
		   inside the plot frame, using: 
		   LEGEND1  POSITION=(BOTTOM CENTER INSIDE) OFFSET=(0,1) 
				MODE=SHARE FRAME;

PLOC=      Location of the panel variable label, in screen percents.
           [Default: PLOC=5 95]              

GOUT=		Name of output graphics catalog.

NAME=       Basename of the graphic catalog entries [Default: NAME=MPLOT]

=*Features*
---------

o  The program does not specify the heights or fonts for any labels
   in the plots.   You should use the GOPTIONS HTEXT= FTEXT= options
	to define these.
	
o  Variables are labeled in the plots using their variable label
   if a label has been defined in the input data set; otherwise,
	the variable name is used as the variable label.
	
o  Plots for the mean of the curve variable are labeled according to
   the convention of PROC SUMMARY, where a missing value (. for numeric
	and ' ' for character variables) represents the fact that that
	variable has been averaged over.  Thus, a panel which represents
	the means of factors A and C averaged over factor B will be labeled
	'B = .' or 'B ='.

 --------------------------------------------------------------------=*/
 
%macro meanplot(
   data=_last_,  /* name of the input dataset                      */
   response=,    /* name of response variable in the input dataset */
   var=,         /* synonym for response=                          */
   class=,       /* class (factor) variables                       */
   freq=,        /* name of a frequency variable for finding means */
   where=,       /* where-clause to subset observations            */
   xvar=,        /* horizontal value for plot [NUMERIC|CHAR]       */
   xfmt=,        /* format for horizontal variable                 */
   cvar=,        /* variable for curves within each plot           */
   cfmt=,        /* format for curve variable                      */
   panels=,      /* variable defining the panels of multiple plots */
   pfmt=,        /* format for panel variable                      */
   cmean=NO,     /* add curve for average over the cvar?           */
   pmean=NO,    /* add panel for average over the panels var?     */
   z=1,          /* std. error multiple for confidence intervals   */
                 /* e.g., z=1.96 gives 95% CI. No error bars: z=0  */
   options=,     /* options for PROC SUMMARY: NWAY, etc            */
   adjust=,      /* error bar adjustment for multiple comparisons  */
   alpha=.05,
   out=_means_,  /* name of output data set (means and SE)         */
   interp=join,  /* interpolation for points along each curve      */
   pplot=NO,
   gplot=YES,
   print=NO,     /* print the means?                               */
   anno=,        /* additional input annotate data set             */

   /* symbols, colors, and line styles for the curves in each plot */
   symbols=%str(circle square $ : triangle = X _ Y),
   hsym=1.5,
   colors=BLACK RED BLUE GREEN BROWN ORANGE PURPLE YELLOW,
   lines=1 20 41 21 7 14 33 12,

   haxis=,       /* axis statement for horizontal axis             */
   vaxis=,       /* axis statement for response axis               */
   legend=,      /* legend statement for custom CVAR legend        */
   ploc = 5 95,  /* location of panel variable label               */
   gout=,        /* name of graphic catalog                        */
   name=mplot    /* basename of graphic catalog entries            */
	);

%let abort=0;
%if (%length(&response) = 0) %then %do;
	%if %length(&var)>0 %then %let response=&var;
	%end;

%if %length(&response)=0 | %length(&class)=0
	%then %do;
		%put ERROR: The RESPONSE= and CLASS= parameters must be specified;
      %let abort=1;
		%goto DONE;
	%end;

%let cmean=%substr(%upcase(&cmean),1,1);
%let pmean=%substr(%upcase(&pmean),1,1);

options nonotes;
data _null_;
	length class $200.;
	%*-- make &data reusable if _LAST_ was specified;
	if upcase(symget('data')) eq '_LAST_'
		then call symput('data', symget('syslast'));
	class = symget('class');
	if symget('xvar') eq ' ' 
		then call symput('xvar', trim(scan(class,1,' ')));
	if symget('cvar') eq ' ' 
		then call symput('cvar', trim(scan(class,2,' ')));
	if symget('panels') eq ' ' 
		then call symput('panels', trim(scan(class,3,' ')));
	call symput('abort', put(_error_ ne 0, 1.));
	run;
%* put xvar=&xvar cvar= &cvar panels= &panels;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
			
%let xc=;
%if &gout^=%str()  %then %let gout=GOUT=&gout;

proc contents data=&data out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

data _null_;
   set _work_(keep=name type label);
	*-- set a missing macro variable for char or numeric variables;
	if upcase(name) = upcase("&xvar") then do;
		if type = 2 then do;
			miss="' '"; call symput('xc', "&xvar");
			end;
		else miss='.';
		call symput('xmiss', miss);
	end;
	else if upcase(name) = upcase("&cvar") then do;
		if type = 2 then miss="' '";
						else miss='.';
		call symput('cmiss', miss);
	end;
	%if &panels ^= %str() %then %do;
	else if upcase(name) = upcase("&panels") then do;
		if type = 2 then do;
			miss="' '"; 
			if "&pfmt" = "best." or "&pfmt" = " " then call symput('pfmt', "$16.");
			end;
		else miss='.';
		call symput('pmiss', miss);
		call symput('plabel', label);
	end;
	%end;
run;
%* put pmiss= &pmiss plabel=&plabel;

/*
%let where = (&xvar ^= &xmiss);
%if %length(&cvar)>0 
	and %upcase(&cmean) ^= Y
		%then %let where = (&xvar ^= &xmiss and &cvar ^= &cmiss);;
*/

%let where = (&xvar is not missing);
%if %length(&cvar)>0 and &cmean ^= Y
		%then %let where = (&xvar is not missing) and (&cvar is not missing);;
%if %length(&panels)>0 and &pmean ^= Y
		%then %let where = &where and (&panels is not missing);;
%*put cvar=&cvar where= &where;

%let nv = %words(&response);
%do i=1 %to &nv;
%let resp = %scan(&response, &i, %str( ));
%put MEANPLOT: Plotting &resp;
proc summary data=&data &options;
   class &class;
   var   &resp;
	%if %length(&where) %then
		where (&where)%str(;);
		
	%if &freq ^= %str() %then %do;
	freq &freq;
	%end;
   output out=&out  mean=  stderr=_se_;
	run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%if %length(&adjust) %then %do;
%let adjust=%upcase(&adjust);
%*-- Fit full n-way model to get MSE;
proc glm data=&data outstat=_stat_ noprint;
   class &class;
   model &resp = &xvar
   %if %length(&cvar)   %then  | &cvar ;
   %if %length(&panels) %then  | &panels ;
        ;
%*-- extract MSE and dfe;
data _null_;
     set _stat_;
     where (_source_='ERROR');
     mse = ss / df;
     call symput('MSE', trim(left(put(mse,12.4))));
     call symput('DFE', trim(left(put(df,12.))));
run;

%end;  /* if &adjust */

* Find number of levels of each effect;
proc freq data=&out %if %sysevalf(&sysver >6.10) %then noprint;;
   tables _type_ / noprint out=_levels_;

data &out;
   merge &out _levels_(keep=_type_ count) end=eof;
   by _type_;
   rename count=levels;

   *-- determine if sample sizes are equal in all margins;
   retain equaln 1 nobs;
   drop nobs equaln;
   if first._type_ then nobs=_freq_;
   equaln = equaln & nobs=_freq_;
   if eof then call symput('equaln', put(equaln,1.));
run;

*-- Find geometric mean of sample sizes for each effect;
data _gmean_;
   set &out;
   by _type_;
   if first._type_ then _gm_ = 0;
   gm + (log(_freq_));
   if last._type_ then do;
      _gm_ = exp(_gm_ / levels);
      output;
   end;

%if &equaln %then %do;
   %let n=_freq_;
%end;
%else %do;
   %if %length(&adjust) %then %put NOTE:  Using geometric mean sample sizes for error bars;
   %*-- Set n to geometric mean, and merge with means;
   %let n=_gm_;
   data &out;
      merge &out _gmean_(keep=_type_ _gm_);
      by _type_;
%end;

%if %sysevalf(&sysver  < 6.09) %then %do;
	%if &adjust=HSD or &adjust=TUKEY %then %do;
	%let adjust=BON;
	%put WARNING: Tukey studentized range adjustment is not available in Version &sysver.;
    %put WARNING:   Using ADJUST=BON;
	%end;
%end;
		
data &out;
	set &out;
   %*-- Determine type of error bars;
   %if %length(&adjust)=0 %then %do;
      _bar_ = &z * _se_;
   %end;
   %else %do;
      %if &adjust=LSD or &adjust=T %then %do;
         q = tinv(1-(&alpha/2), &dfe);
         _bar_   = sqrt(2 * &mse / &n) * q / 2;
      %end;
      %if &adjust=BON %then %do;
			if levels > 1
         	then q = levels * (levels-1)/2;  *-- # pairwise comparisons;
				else q = levels;
         q = tinv(1-(&alpha/(2*q)), &dfe);
         _bar_   = sqrt(2 * &mse / &n) * q / 2;
      %end;
      %else %if &adjust=HSD or &adjust=TUKEY %then %do;
         if levels > 1 then do;
            q   = probmc('range', .,  1-&alpha, &dfe, levels);
            _bar_ = sqrt(&mse / &n) * q / 2;
         end;
      %end;
   %end;

%if %substr(%upcase(&print),1,1)=Y %then %do;
proc print data=&out;
   id _type_; by _type_;
%end;

proc sort data=&out;
	where &where;
   by &panels &cvar _type_;

%*-- Line printer plots?;
%if %upcase(&pplot) = YES %then %do;
	%if &cvar = %str()
		%then %let sym = '*';
		%else %let sym = &cvar;
	proc plot data=&out uniform;
		%if &panels ^= %str() %then %do;
			by &panels;
		%end;
		%if %sysevalf(&sysver  > 6.07) %then %do;
		plot &resp * &xvar $ &sym;
		%end;
		%else %do;
		plot &resp * &xvar = &sym;
		%end;
		%if %length(&xfmt) %then %do;
			format &xvar &xfmt;
			%end;
		%if %length(&cfmt) %then %do;
			format &cvar &cfmt;
			%end;
%end;	/* pplot */

%*-- GPLOT plots?;
%if %substr(%upcase(&gplot),1,1) = Y %then %do;
goptions hby=0;
%let plx = %scan(&ploc,1);
%let ply = %scan(&ploc,2);
data _bars_;
   set &out;  
   by &panels &cvar;
   length function color $8 text $30;
   retain cl 0;
	drop cl mean _se_;	

   mean = &resp;
	%if &panels ^= %str() %then %do;
	if first.&panels then do;
		xsys='1'; ysys='1';
		x = &plx; y=&ply; 
		position='6';
		text = "&plabel";
		if text=' ' then text="&panels";
		if &panels = &pmiss
			then text = trim(text)||' (mean)';
			else text = trim(text)||' = ' || compress(put(&panels,&pfmt));
		function = 'LABEL'; output;
		cl=0;
		end;
	%end;
	
	xsys='2';  ysys= '2';
	*-- Index for line/color;
	%if &cvar = %str()
		%then %do; cl=1; %end;
   	%else %do; if first.&cvar then cl+1; %end;
   line=input(scan("&lines", cl),5.);
   color = scan("&colors",cl);

   %*-- Set X or XC variable ;
   %if &xc ^= %str() %then %do;
      xc = &xc;
      %end;
   %else %do;
      x = &xvar;
      %end;
/*
	%if &cvar = %str()
		%then %do; if _n_=1 then do; %end;
   	%else %do; if first.&cvar then do; %end;
      y = mean; function='MOVE'; output;
      end;
   else do;
      y = mean; function='DRAW'; output;
      end;
*/
   y = mean;            function = 'MOVE'; output;
	y = mean + _bar_  ;  function = 'MOVE'; output;
	y = mean          ;  function = 'DRAW'; output;
	y = mean - _bar_  ;  function = 'DRAW'; output;
	y = mean          ;  function = 'MOVE'; output;
run;

%if &anno ^= %str() %then %do;
   data _bars_;
      set _bars_ &anno;
		%if &panels ^= %str() %then %do;
		by &panels;
		%end;
   %end;

%if &i=1 %then %do; 
	*-- Create default axes/legend if none were specified;
	%if &vaxis = %str() %then %do;
	   axis1  label=(a=90);
		%let vaxis=vaxis=axis1;
		%end;
	%else %let vaxis=vaxis=&vaxis;

	%if &haxis = %str() %then %do;
	   axis2  offset=(5);
		%let haxis=haxis=axis2;
		%end;
	%else %let haxis=haxis=&haxis;

	%if &legend = %str() %then %do;
	   legend1  position=(bottom center inside) offset=(0,1) mode=share frame;
		%let legend=legend=legend1;
		%end;
	%else %let legend=legend=&legend;

	%if &cvar = %str()
		%then %do;
			%let sym = 1;
			symbol1 i=&interp v=%scan(&symbols,1) h=&hsym, c=%scan(&colors,1);
		%end;
		%else %do;
			%let sym = &cvar;
			*-- How many levels of cvar variable? --;
			proc freq data = &out;
				tables &cvar / noprint out=_levels_;
			data _null_;
				set _levels_(obs=1) nobs=ngroups;
				call symput( 'NGROUPS', put(ngroups,3.) );
				run;
			%gensym(n=&ngroups, interp=&interp, symbols=&symbols, h=&hsym,
			colors=&colors, line=&lines);
		%end;
	%end;
	
proc gplot data=&out uniform &gout;
*   where (&xvar ^= &xmiss);
	%if &panels ^= %str() %then %do;
   by &panels;
	%end;
   plot &resp * &xvar = &sym / 
		frame anno=_bars_ &vaxis &haxis &legend hm=1 /* vm=1 */
		name="&name" des="meanplot of &data (&class)";
		%if %length(&xfmt) %then %do;
			format &xvar &xfmt;
			%end;
	%if %length(&cfmt) %then %do;
		format &cvar &cfmt;
		%end;
	%if %length(&pfmt) %then %do;
		format &panels &pfmt;
		%end;
run; quit;

*proc contents data=_bars_;
goptions hby=;
%end;	/* gplot */

%if &nv>1 %then %gskip;
%end;   /* do i=1 %to &nv */

%*-- Clean up;
proc datasets nolist nowarn library=work;
	delete _work_ _bars_  _levels_
    %if %length(&adjust) %then
           _stat_ ;
    ;
    quit;

%done:
%if &abort %then %put ERROR: The MEANPLOT macro ended abnormally.;
options notes;
%mend;

 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
%macro gensym(
   n=1,
   start=1, 
   h=1.5,
   interp=none,
   line=1,
   symbols=%str(dot circle square triangle  $  X _ Y),
   colors=BLACK RED GREEN BLUE BROWN ORANGE PURPLE YELLOW,
   ci=,
   font=,
   width=1,
   repeat=1,
   label=
   );

*options mprint symbolgen;
    %*--  symbols, colors, line styles, and interp are recycled as needed;
  %local chr col int lin k cic;
  %do k=&start %to %eval(&n + &start -1) ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0 
	      %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0 
	      %then %let colors = &colors &colors;
     %if %length(%scan(&interp, &k, %str( ))) = 0 
	      %then %let interp = &interp &interp;
     %if %length(%scan(&line,   &k, %str( ))) = 0 
	      %then %let line = &line &line;
     %if %length(%scan(&width,   &k, %str( ))) = 0 
	      %then %let width = &width &width;
     %if %length(&ci) 	%then %do;
         %if %length(%scan(&ci,  &k, %str( ))) = 0 
	           %then %let ci = &ci &ci;
	     %end;

	 %let chr =%scan(&symbols,&k, %str( ));
     %let col =%scan(&colors, &k, %str( ));
     %let int =%scan(&interp, &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     %let wid =%scan(&width,  &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));

	  %if &k=99 %then %let repeat=999;
     symbol&k
      %if %length(&font) 	  %then font=&font;
	  %if %length(&label) 	%then pointlabel=%str( (h=1 "#&label") );
	  height=&h value=&chr color=&col i=&int l=&lin w=&wid r=&repeat
	  %if %length(&cic) 	%then %str(ci=&cic);
	  ;
	%if &k=99 %then %goto done;
  %end;
*options nomprint nosymbolgen;
%done:
%mend;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(%quote(&word)^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;
