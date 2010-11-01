 /*-------------------------------------------------------------------*
  *    Name: dotplot.sas                                              *
  *   Title: Macro for dot charts                                     *
        Doc: http://www.datavis.ca/sasmac/dotplot.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  14 May 1989 09:12:26                                    *
  * Revised:  11 Dec 2008 09:51:00                                    *
  * Version:  1.5-3                                                   *
  *  - Added WHERE= parameter                                         *
  *  - Added XLOGBASE= parameter for log axis                         *
  *  - Added SYMBOL=                                                  *
  * 1.3                                                               *
  *  - Added rudimentary BY= processing                               *
  *  - Added ability to plot multiple XVAR (same scale)               *
  *  - Allow to suppress YLABEL= (specify null string)                *
  *  - Replaced %makefmt by use of proc format                        *
  * 1.4                                                               *
  *  - Allow errbar=lower upper as well as single error bar length    *
  *  - Allow VAXIS= and HAXIS to completely specify axes              *
  *  - Fixed nasty bug with proc format                               *
  *  - Added internal documentation                                   *
  *  - Inlined words macro                                            *
  * 1.5                                                               *
  *  - Added ANNO= option                                             *
  *  - Fixed bugs with VAXIS and HAXIS                                *
  *  - Added GPCOLOR= and GPHT= options                               *
  *  - Added PLOT= option
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The DOTPLOT macro produces grouped and ungrouped dot charts, with options
 for sorting and error bars.

=Usage:

 The DOTPLOT macro is defined with keyword parameters. The XVAR= and
 YVAR= parameters must be supplied.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%dotplot(data=auto, xvar=mpg, yvar=model, group=origin);
 
==Parameters:

* DATA=       Name of the input data set [Default: DATA=_LAST_]

* XVAR=       Horizontal variable (response)

* YVAR=       Vertical variable (observation label) for
                    the dot chart.  This should specify a character
                    variable.  At most 16 characters of the value
                    are used for the label.

* YSORTBY=    How to sort observations.The default,
                    YSORTBY=&XVAR, indicates that observations are
                    sorted in ascending order of the response
                    variable.  [Default: YSORTBY=&XVAR]

* GROUP=      Vertical grouping variable

* CLASS=      Synonym for GROUP=

* WHERE=      WHERE clause to subset the data

* BY=         BY variable (not completely implemented)

* GPFMT=      Format for printing group variable
                value (include the '.' at the end)

* GPCOLOR=    Color for printing the group variable value

* GPHT=       Hieght (in character units) for group variable value

* CONNECT=    Specifies how to draw horizontal lines for
	      each observation.  Valid values are ZERO, DOT, AXIS, or
			  NONE.  The default, CONNECT=DOT, draws
		    a dotted line from the Y axis to the point.
		    CONNECT=ZERO draws a line from an X value of 0
		    to the point.  CONNECT=AXIS draws a line from the
		    Y axis to the plot frame at the maximum X value.
		    CONNECT=NONE does not draw a line for the observation.
            [Default: CONNECT=DOT]

* SYMBOL=     Symbol(s) for observation [Default: SYMBOL=DOT CIRCLE SQUARE]

* SYMHT=      Height of observation symbols. [Default: SYMHT=1.5]

* COLOR=      Symbol colors [Default: COLOR=BLUE]

* DLINE=      Style of horizontal lines [Default: DLINE=34]

* DCOLOR=     Color of horizontal lines [Default: DCOLOR=BLACK]

* ERRBAR=     Name of an input variable giving length of error bar
              for each observation, or names of two variables giving
			  lower/upper values.

* ECOLOR=     Color for error bars [Default: DCOLOR=RED]

* ANNO=       Name of an optional input ANNOTATE data set

* VAXIS=      Vertical axis stmt (overrides internal choices and
              other parameters below).

* HAXIS=      Horizontal axis stmt (overrides internal choices and
              other parameters below.)

* YLABEL=     Label for y variable.  If not specified,
                    the vertical axis is labelled with the name of
                    the YVAR= variable [Default: YLABEL=&YVAR]

* YLENGTH=    Length of vertical axis 

* XORDER=     Plotting range of response. Specify
              XORDER in the form XORDER = low TO high BY step.

* XREF=       Specifies the horizontal values at which
                    reference lines are drawn for the response
                    variable.  If not specified, no reference lines
                    are drawn.

* XLOGBASE=    Use to plot the response on log scale, e.g.,
               XLOGBASE=10.

* XLOGSTYL=    Style for log scale [Default: XLOGSTYL=EXPAND]

* PLOT=       YES to produce the plot, otherwise suppress it

* NAME=       DOTPLOT

==Usage notes:

 DOTPLOT plots each observation in a row of the graphics output
 area.  Therefore the VPOS= graphics option should specify a
 sufficient number of vertical character cells.  The value for VPOS=
 should be

    VPOS >= number of observations + number of groups + 8

 =*/

%macro dotplot(
       data=_LAST_,          /* input data set                         */
       xvar=,                /* horizontal variable (response)         */
       yvar=,                /* vertical variable (observation label)  */
       ysortby=&xvar,        /* how to sort observations               */
       group=,               /* vertical grouping variable             */
       class=,               /* synonym for group=                     */
       where=,               /* WHERE clause to subset data            */
       by=,                  /* BY variable (not well implemented)     */
       gpfmt=,               /* format for printing group variable     */
                             /* value (include the . at the end)       */
       gpcolor=black,
	   gpht=1.5,
       connect=DOT,          /* draw lines to ZERO, DOT, AXIS, or NONE */
       symbol=dot circle square,   /* symbol(s) for observation        */
       symht=1.5,            /* Height of observation symbols          */
       color=black red blue, /* symbol colors                          */
       dline=34,             /* style of horizontal lines              */
       dcolor=BLACK,         /* color of horizontal lines              */
       errbar=,              /* variable giving length of error bar    */
                             /* for each observation, or lower/upper   */
       ecolor=RED,           /* color for error bars                   */
       anno=,                /* name of input annotate data set        */
	   /* Axis control parameters */
	   vaxis=,               /* Vertical axis stmt (override internal) */
	   haxis=,               /* Horiz. axis stmt (override internal)   */
       ylabel=&yvar,         /* label for y variable                   */
	   ylength=,             /* Length of vertical axis                */
       xorder=,              /* plotting range of response             */
       xref=,                /* reference lines for response variable  */
       xlogbase=,            /* Use to plot response on log scale      */
       xlogstyl=expand,      /* Style for log scale                    */
	   plot=YES,
       name=DOTPLOT);        /* Name for graphic catalog entry         */

%let abort=0; 
%if &yvar= %str() %then %do;
   %put ERROR: You must specify a YVAR= variable;
   %let abort=0; 
   %goto ENDDOT;
   %end;

%if &xvar= %str() %then %do;
   %put ERROR: You must specify XVAR= variable(s);
   %let abort=0; 
   %goto ENDDOT;
   %end;

%if %bquote(%upcase(&data)) = _LAST_ %then %let data = &syslast;

%let connect=%upcase(&connect);
%let plot=%substr(%upcase(&plot),1,1);
%*if &ylabel = %str() %then %let ylabel=%upcase(&yvar);

%* global nobs vref;
options nonotes;
 /*--------------------------------------------------*
  | Sort observations in the desired order on Y axis |
  *--------------------------------------------------*/

%let group = %scan(&group &class,1,%str( ));    %*-- handle synonym;
%if (%length(&group.&ysortby)>0) AND &ysortby ^= NONE %then %do;
proc sort data=&data;
   by &by &group &ysortby;
%end;
 
 /*-----------------------------------------------------*
  | Add Sort_Key variable and construct macro variables |
  *-----------------------------------------------------*/
data _dot_dat;
  set &data;
  %if &group = %str() %then %do;
     %let group= _GROUP_;
     _group_ = 1;
  %end;
  %if %length(&where)>0 %then %do;
     where (&where);
  %end;
run;


*-- add index number as sort_key for sorting and format;
data _dot_dat;
	set _dot_dat;
	sort_key + 1;

options notes;
data _fmt_;
	set _dot_dat (rename=(sort_key=start &yvar=label));
	end=start;
	retain fmtname '_yname_';
	*-- Avoid contamination from input variables!;
	keep start end label fmtname;
   run;
proc format cntlin=_fmt_;
run;

*-- Find the breaks for groups;
%local vref nobs;
%let vref=;
data _null_;
  set _dot_dat end=eof;
  length vref $60;
  retain vref ; drop vref;
  by &group;

  if _n_=1 then vref='';
  if last.&group & ^eof then do;
     vref = trim(vref) || put(sort_key+.5, 5.1);
     end;
  if eof then do;
     call symput('nobs', put(sort_key, 4.));
     call symput('vref', trim(vref));
     end;
run;

 
%if &nobs=0 %then %do;
   %put DOTPLOT: Data set &data has no observations;
   %goto ENDDOT;
   %end;
*makefmt(&nobs);
 
%local i;
%let nv = %words(&xvar, root=_v_);
 /*---------------------------------------------------*
  | Annotate data set to draw horizontal dotted lines |
  *---------------------------------------------------*/
data _dots_;
   set _dot_dat (keep=&xvar &yvar &by &group sort_key);
      by &by &group;
   length function $ 8 text $ 20;
   text = ' ';
   %if &connect = ZERO
       %then %str(xsys = '2';) ;
       %else %str(xsys = '1';) ;
   ysys = '2';
   line = &dline;
   color = "&dcolor";
   y  = sort_key;
   x = 0;
   function ='MOVE'; output;
 
   function ='DRAW';
   %if &connect = DOT | &connect = ZERO
       %then %do;
          xsys = '2';
			 %if &nv>1 
          	%then %str(x = max(of &xvar);); 
          	%else %str(x = &xvar;); 
          output;
       %end;
       %else %if &connect = AXIS
          %then %do;
             xsys = '1';
             x = 100;  output;
             /*
          function='POINT';
          do x = 0 to 100 by 2;
             output;
             end;
          %end;
		  	*/
          %end;
 
   %if &group ^= _GROUP_ %then %do;
      if first.&group then do;
         xsys = '1';
         x = 98; size=&gpht;
         function = 'LABEL';
         color="&gpcolor";
         position = 'A';
         %if &gpfmt ^= %str()
            %then %str(text = put(&group, &gpfmt ) ;) ;
            %else %str(text = &group ;) ;
         output;
      end;
   %end;
 
%if &errbar ^= %str() %then %do;
%let e1 = %scan(&errbar,1,%str( ));
%let e2 = %scan(&errbar,2,%str( ));
data _err_;
   set _dot_dat;
   xsys = '2'; ysys = '2';
   length comment $12 color function $8;
   y = sort_key;
   comment = 'error bars';
   color = "&ecolor";
   %do i=1 %to &nv;
    %if %length(&e2)=0 %then %do;       %*-- &errbar is bar half-length;
       x = &&_v_&i - &errbar; ;
       function = 'MOVE ';   output;
       text = '|';
       function = 'LABEL';   output;
       x = &&_v_&i ;
       function = 'DRAW ';   output;
       x = &&_v_&i + &errbar ;
       function = 'DRAW ';   output;
       function = 'LABEL';   output;
	%end;
    %else %do;                         %*-- &errbar is lower, upper;
       x = &e1; ;
       function = 'MOVE ';   output;
       text = '|';
       function = 'LABEL';   output;
       x = &&_v_&i ;
       function = 'DRAW ';   output;
       x = &e2 ;
       function = 'DRAW ';   output;
       function = 'LABEL';   output;
	%end;
   %end;
data _dots_;
   set _dots_ _err_ &anno;
%end;

%if &anno ^= %str() %then %do;
data _dots_;
   set _dots_ &anno;
%end;

%gensym(n=&nv, symbols=&symbol, colors=&color, h=&symht);

 /*-----------------------------------------------*
  | Draw the dot plot, plotting formatted Y vs. X |
  *-----------------------------------------------*/
%if %length(&vaxis)=0 %then %do;
	%let vaxis=axis98;
	axis98
           %if %length(&by)=0 %then
	           %str(order=(1 to &nobs by 1));
		   %if %length(&ylength) %then
		    	%str(length=&ylength);
            major=none value=(j=r)
			%if %bquote(&ylabel) ^= NONE %then  label=(a=90 r=0) ;
		    %else label=none ;
			  offset=(2);
/* 	axis98 &vax;
	%put DOTPLOT: Using AXIS98 &vax;
 */
 	%end;

%if %length(&haxis)=0 %then %do;
	%let haxis=axis99;
   axis99  %if %length(&xorder)>0 %then order=(&xorder) ;
     %if &xlogbase ^= %str()
          %then %do;
               logbase = &xlogbase
               %if &xlogstyl ^= %str()
                    %then %do;
                         %let xlogstyl = %upcase(&xlogstyl);
                         %if &xlogstyl ^= POWER & &xlogstyl ^= EXPAND
                            %then %do;
                             %put NOTE: XLOGSTYL must be "EXPAND" or "POWER".;
                             %put NOTE: "EXPAND" was used.;
                             %let xlogstyl = EXPAND;
                             %end;
                    logstyle = &xlogstyl
                    %end;
               %end;
           offset=(2);
	%end;

%if &plot=Y %then %do;
proc gplot data= _dot_dat /*&GOUT */ ;
   plot 
    %do i=1 %to &nv;
        sort_key * &&_v_&i = &i
        %end;
        /vaxis=&vaxis vminor=0
         haxis=&haxis frame
         name="&name"
         %if &nv>1 %then overlay;
			des="dotplot of &data (&xvar)" 
     %if %length(&vref)  %then    vref=&vref ;
     %if %length(&xref)  %then    href=&xref lhref=21 chref=red ;
         annotate=_dots_;
	%if %length(&by)>0 %then %do;
		by &by;
		%end;
	%if %length(&ylabel) %then %do;
	   label   sort_key="&ylabel";
		%end;
		
   format  sort_key _yname_.;
   run; quit;
%end;

%enddot:
%let nobs=;
%let vref=;
options notes;
%mend dotplot;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
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
%* put words() = %words();
%* put words = %words(A B C,root=W);
%* put W1= &W1 W2= &W2 W3= &W3 ;
%* put words = %words(AA BB CC);
