 /*-------------------------------------------------------------------*
  *    Name: scatmat.sas                                              *
  *   Title: scatterplot matrix - all pairwise plots for n variables  *
        Doc: http://www.datavis.ca/sasmac/scatmat.html            
  *   Usage: %scatmat(data=, var=, group=);                           *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:   4 Oct 1989 11:07:50                                    *
  * Revised:  07 Oct 2011 16:32:31                                    *
  * Version:  1.8-2                                                   *
  * Requires: %gdispla %lowess %ellipses %boxaxis %gensym %rug        *
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *-------------------------------------------------------------------*/
 /**
 =ChangeLog:
       - Added anno=ELLIPSE to draw data ellipse on off-diag plots     
       - Added anno=LOWESS to draw lowess curve on off-diag plots      
       - Added anno=BOX to draw boxplots on diag panels                
       - Added PLOTOPT= for additional plot options                    
       - Cleaned up temporary gseg library                             
       - Added NAMES= to use different var labels than their names     
       - Increased max # of variables to 12                            
       - Fixed problem with symbols being reset                        
    1.6  Replaced old %tdef macro with new, less kludgy one            
    1.6a Changed anno=BOX to draw horizontal boxplots                  
         Added anno=RUG to do rug plots in diagonal panels             
    1.6b No longer use internal %gensym                                
       - Increased max # of variables to 15                            
    1.6c Use CI=&colors on symbols for interpolated lines              
    1.7  Added NAME= to name the catalog entry                         
         Replaced CONTOUR with ELLIPSES                                
         Fixed careless syntax error; clean up temp datasets           
    1.8  Added PVALUE to control size for ELLIPSES
         Fixed problem with long variable names (VALIDVARNAME=V7)
		 Added IWIDTH= for interpolated lines              

 **/

 /*=
=Description:

 The SCATMAT macro produces a flexible scatterplot matrix, showing all
 pairwise plots of n variables specified in the VAR= parameter.
 
 If a classification variable is specified with the GROUP=
 parameter, the value(s) of that variable determine the shape and color
 of the plotting symbol.  In addition, various interpolation options
 (INTERP=) and annotation options (ANNO=) may be used to add graphic
 summaries to each pairwise plot.

=Usage:

 The SCATMAT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%scatmat(data=auto, var=price mpg weight, group=origin);
 
==Parameters:

* DATA=        Name of the data set to be plotted [Default: DATA=_LAST_]

* VAR=         List of the variables to be plotted. This can be
               a blank-separated list, or any of the standard SAS
			   abbreviations like VAR=X1-X4 or VAR=VARA--VARB.
			   The macro imposes a restriction of 15 on the number
			   of variables.
			   [Default: VAR=_NUMERIC_]

* NAMES=      Alternative variable names, used for labels in the diagonal
              panels.

* GROUP=      Grouping or class variable, used to determine plot symbols
              and plot interpolations

* INTERP=     Plot interpolation method used in each off-diagonal plot.
              For example, INTERP=RL draws regression lines.
			  [Default: INTERP=NONE]

* HSYM=       Height of the plot symbols in the off-diagonal panels.

* HTITLE=     Height of variable name labels used in the diagonal panels.
              If not specified, the program uses HTITLE=2*NV to try to
			  preserve readability.

* PLOTOPT=    Additional plot options passed to PROC GPLOT on the PLOT
              statement.

* SYMBOLS=    Symbols used for points.  If there are more groups than
              symbols, the available symbols are recycled as needed.
			  Use SYMBOLS=NONE to suppress point symbols.
			  [Default: SYMBOLS=CIRCLE SQUARE + : $ = X _ Y]

* COLORS=     Colors used for points.  If there are more groups than
              colors, the available colors are recycled as needed.
			  [Default: COLORS=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE]

* ANNO=       You can specify one or more keywords to add additional graphic
              information to the diagonal or off-diagonal panels.
			  ANNO=ELLIPSE adds data ellipses to off-diagonal plots.
			  ANNO=LOWESS adds smoothed loess curves.
			  ANNO=BOX adds a marginal boxplot in the diagonal panels.
			  ANNO=RUG adds a marginal rugplot in the diagonal panels.
              [Default: ANNO=NONE]

* PVALUE      PVALUE for ELLIPSES.

* GTEMP=      Temporary graphics catalog used to record the individual
              plots as they are produced.  This should not be the same
			  as the GOUT= catalog.  [Default: GTEMP=GTEMP]

* KILL=       Delete gtemp when done? [Default: KILL=Y]

* NAME=       Name for the graphic catalog entry.  [Default: NAME=SCATMAT]

* GOUT=       Name of graphic catalog for the final scatterplot matrix 
              [Default: GOUT=GSEG]
                
==Dependencies:
 
 Depending on options selected, the SCATMAT macro calls several
 other macros not included here.  It is assumed that these are stored
 in an autocall library.  If not, you'll have to %include each one
 you use.
 
   %gdispla  - device-independent DISPLAY/NODISPLAY control [always]
   %gensym   - generate symbol statements    [always]
   %lowess   - smoothed lowess curves        [ANNO incl. LOWESS]
   %ellipses  - data ellipses                 [ANNO incl. ELLIPSE]
   %boxaxis  - boxplot for diagonal panels   [ANNO incl. BOX]
   %rug      - rug plot for diagonal panels  [ANNO incl. RUG]
 
 These are all available from http://euclid.psych.yorku.ca/ftp/sas
 (though in different subdirectories), or via the documentation pages,
 http://datavis.ca/sasmac/
 
  
 =*/

%macro scatmat(
   data =_LAST_,          /* data set to be plotted             */
   var  =_NUMERIC_,       /* variables to be plotted - can be   */
                          /* a list or X1-X4 or VARA--VARB      */
   names=,                /* Alternative variable names         */
   group=,                /* grouping variable (plot symbol)    */
   interp=none,           /* plot interpolation method          */
   iwidth=2,
   hsym=,                 /* height of plot symbols             */
   htitle=,               /* height of variable name titles     */
   plotopt=,              /* additional plot options            */
   symbols=circle square + : $ = X _ Y,
   colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE,
   anno=NONE,             /* annotate diag or off-diag plot     */
   pvalue=0.68,
   gtemp=gtemp,           /* temporary graphics catalog         */
   kill=Y,                /* delete gtemp when done?            */
   name=scatmat,          /* name for graphic catalog entry     */
   gout=GSEG              /* graphic catalog for plot matrix    */
   );
  

    %*-- Reset required global options;
    %if %sysevalf(&sysver  >= 7) %then %do;
        %local o1 o2;
        %let o1 = %sysfunc(getoption(notes));
        %let o2 = %sysfunc(getoption(validvarname,keyword));
        options nonotes validvarname=V7;
        %end;
    %else %do;
       options nonotes;
        %end;

%let anno=%upcase(&anno);
%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%local me; %let me=SCATMAT;

%*-- For Session Manager, need to start a new graphic catalog;
%*-- Use a temporary graphics catalog to create the individual plots;
%*let gtemp = gtemp;
 
*-- Parse variables list;
 data _null_;
 set &data (obs=1) nobs=nobs;
 	call symput('nobs', trim(left(put(nobs,8.))) );
    %if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 
       %* find the number of variables in the list and
         convert shorthand variable list to long form;
     length _vname_ $ 32 _vlist_ $ 200;
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
   %else %do;
     * find the number of variables in the list;
     nvar = n(of &var) + nmiss(of &var);
   %end;
   call symput('NVAR',trim(left(put(nvar,2.))));
     * default symbol height, if hsym not specified;
   ht = scan('1.4 2.3 3 3.7 4.2 4.5 5 5.3 5.4 5.5 5.6 5.7 5.75 5.8 5.85',nvar,' ');
   call symput('HT',trim(left(put(ht,3.1))));
 RUN;
%if &nvar < 2 or &nvar > 15 %then %do;
   %put ERROR: (&me) Cannot do a scatterplot matrix for &nvar variables ;
   %goto DONE;
   %end;
 
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

%if &ngroups=1 %then %do;
	*-- set colors and other characteristics for single group displays;
	%end;
 
%if &hsym = %str() %then %let h=&ht;
                   %else %let h=&hsym;
%gensym(n=&ngroups, h=&h, interp=&interp, symbols=&symbols, colors=&colors, width=2);
SYMBOL&plotnam v=none i=none;

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
   proc means noprint data=&data;
      var &vi;
      output out=minmax min=min max=max;
 
   %do j = 1 %to &nvar;                /* cols */
      %let vj = %scan(&var , &j );
      %let plotnum = %eval(&plotnum+1);
      %let replay  = &replay &plotnum:&plotnum ;
      %put &me: plot &plotnum: &vi vs. &vj ;
 
      %if &i = &j %then %do;           /*---- diagonal panel ----*/
         data title;
            length text $12 function $8;
            set minmax;
            xsys = '1'; ysys = '1';
            x = 50; 
			%if %index(&anno,BOX) %then %do;
				y = 70;
				%end;
			%else %do;
				y = 50;
				%end;
            text = "&vi";
				%if %length(&ni)>0 %then %str(text = "&ni";);
            size = &htitle;
            function = 'LABEL';  output;
 
            x = 6; y = 6; position = '6';
            text = left(put(min, best6.));
            size = min(&htitle/2,5);
            output;
 
            x = 95; y = 95; position = '4';
            text = trim(put(max, best6.));
            size = min(&htitle/2,5);
            output;
 
 			%if %index(&anno,BOX) %then %do;
				%if %index(&anno,BOXV)  
			 %then %boxaxis(data=&data, var=&vi, baxis=y, oaxis=x, out=_anno_,
				pos=40, boxwidth=10, hsym=&h);
			 %else %boxaxis(data=&data, var=&vi, baxis=x, oaxis=y, out=_anno_,
				pos=40, boxwidth=10, hsym=&h);

			data title;
				set title _anno_;
			%end;

 			%if %index(&anno,RUG) %then %do;
				%rug(data=&data, var=&vi, on=x, at=10, ht=4, out=_rug_);
			data title;
				set title _rug_;
				%end;

         proc gplot data = &data gout=&gtemp;
            plot &vi * &vi = &plotnam
            / frame anno=title vaxis=axis1 haxis=axis1
				  name="scat&i.&i" des="SCATMAT title &vi";
         axis1 label=none value=none major=none
               minor=none offset=(2);
         run; quit;
      %end;
 
      %else %do;                     /*---- off-diagonal panel ----*/

			%let inanno=;
			%if %index(&anno,ELLIPSE) %then %do;
			%ellipses(data=&data, x=&vj, y=&vi, group=&group, annoadd=NONE,
					out=_anno_, colors=&colors, plot=NO, points=30, line=1, pvalue=&pvalue);
			%let annoopt=anno=_anno_;
			%let inanno=_anno_;
			%gensym(n=&ngroups, h=&h, interp=&interp, symbols=&symbols, colors=&colors, width=&iwidth, ci=&colors);
			SYMBOL&plotnam v=none i=none;
			%end;
			
			%if %index(&anno,LOWESS) %then %do;
			%*-- should allow user to choose smoothing parameter;
			%lowess(data=&data, x=&vj, y=&vi, p=1, outanno=_anno2_,
					colors=&colors, gplot=NO, pplot=NO, f=,
					silent=YES, step=0, in=&inanno);
			%let annoopt=anno=_anno2_;
			%let inanno=_anno2_;
			%gensym(n=&ngroups, h=&h, interp=&interp, symbols=&symbols, colors=&colors, width=2);
			SYMBOL&plotnam v=none i=none;
			%end;
			
         proc gplot data = &data gout=&gtemp;
            plot &vi * &vj = &plotsym / &annoopt
              frame nolegend vaxis=axis1 haxis=axis1 &plotopt
				  name="scat&i.&j" des="SCATMAT plot &vi, &vj";
         axis1 label=none value=none major=none minor=none offset=(2);
         run; quit;
      %end;
 
   %end; /* cols */
%end;    /* rows */
 
%gdispla(ON);
 
%macro tdef(nv);
%* ---------------------------------------------------------------;
%* Generate a TDEF statement for a scatterplot matrix             ;
%* Start with (1,1) panel in upper left, and copy it across & down;
%* This version uses %sysfunc and %sysevalf to obtain non-integer
   %s, so it will no longer work with SAS < V7;
%* ---------------------------------------------------------------;
%local i j panl panl1 lx ly;
 
   TDEF scat&nv DES="scatterplot matrix &nv x &nv"
   %let panl=0;
   %let size = %sysfunc(round(100/&nv,0.01));
   %let shift = &size;
   %let lx = &size;
   %let ly = %sysevalf(100-&size);
   %do i = 1 %to &nv;
   %do j = 1 %to &nv;
       %let panl  = %eval(&panl + 1);
       %if &j=1 %then
          %do;
             %if &i=1 %then %do;      %* (1,1) panel;
               &panl/
                ULX=0  ULY=100   URX=&lx URY=100
                LLX=0  LLY=&ly   LRX=&lx LRY=&ly
                %end;
             %else
                %do;                  %* (i,1) panel;
                   %let panl1 = %eval(&panl - &nv );
               &panl/ copy= &panl1 xlatey= -&shift
                %end;
          %end;
       %else
          %do;
               %let panl1 = %eval(&panl - 1);
               &panl/ copy= &panl1 xlatex= &shift
          %end;
   %end;
   %end;
     %str(;);      %* end the TDEF statement;
%mend;
 
%put &me: Generating a &nvar x &nvar template to replay the plots.;
proc greplay igout=&gtemp
              gout=&gout  nofs
             template=scat&nvar
             tc=templt ;
   %tdef(&nvar); 
   TREPLAY &replay name="&name" des="SCATMAT of &data";
run;
%if %substr(%upcase(&kill),1,1)=Y %then %do;
  proc catalog kill catalog=&gtemp et=grseg;
run; quit;
%end;

  *-- Clean up datasets no longer needed;
proc datasets nofs nolist library=work memtype=(data);
    delete minmax title;
	run; quit;


%DONE:
  goptions reset=symbol;
    %*-- Restore global options;
    %if %sysevalf(&sysver  >= 7) %then %do;
        options &o1 &o2;
        %end;
    %else %do;
       options notes;
        %end;

%mend;
 
 
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
/*
%macro gensym(n=1, h=1.5, i=none,
              symbols=%str(- + : $ = X _ Y),
              colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE);
    %*--  if more than 8 groups, symbols and colors are recycled;
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
%mend gensym;
*/
