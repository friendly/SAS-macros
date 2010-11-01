 /*-------------------------------------------------------------------*
  *    Name: bars.sas                                                 *
  *   Title: Create an annotate data set to draw error bars           *
        Doc: http://www.datavis.ca/sasmac/bars.html                
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 23 Nov 1997 11:32                                        *
  * Revised: 23 Mar 2004 11:22:26                                     *
  * Version: 1.2                                                      *
  * 1.2  Fixed bug with CLASS= not uppercase                          *
  *      Added run; Added SUBSET=                                     *
  *      Fixed an upcase() bug                                        *             
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The BARS macro creates an Annotate data set to draw error bars
 in a plot.  The error bars may be drawn for a response variable 
 displayed on the Y axis or on the X axis.  The other (CLASS=)
 variable may be character or numeric.  

=Usage:

 The BARS macro is called with keyword parameters.  The VAR= and CLASS=
 variables must be specified.  The length of the error bars should be
 specified with either the BARLEN= parameter or the LOWER= and UPPER=
 parameters.

 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%bars(class=age, var=logodds, lower=lower, upper=upper);
	proc gplot data=mydata;
		plot logodds * age / anno=_bars_;
	 
==Parameters:

* DATA=       Name of input data set [Default: DATA=_LAST_]

* VAR=        Name of the response variable, to be plotted on the
              axis given by the BAXIS= parameter.

* CLASS=      Name of the independent variable, plotted on the
              other axis.

* CVAR=       Name of a curve variable, when PROC GPLOT is used with
              the statement PLOT &VAR * &CLASS = &CVAR.

* BY=         Name of a BY variable for multiple plots, when PROC GPLOT
              is used with the statement BY &BY;.

* BAXIS=      One of X or Y, indicating the axis along which error bars
              are drawn [Default: BAXIS=Y]

* BARLEN=     A numeric variable or constant giving the error bar length,
              for example, when the input data set contains a standard
              error variable or multiple thereof.  If BARLEN= is given,
              the LOWER= and UPPER= values are ignored, and error bars
              are drawn at the values &VAR +- &Z * &BARLEN.

* Z=          A numeric value giving the multiplier of the BARLEN=
              value used to determine the lower and upper error bar
              values.

* LOWER=      A numeric variable or constant giving the lower error bar value.
              Use the LOWER= and UPPER= parameters if the error bars are
              non-symmetric or if the lower and upper values are contained
              as separate variables in the input data set.

* UPPER=      A numeric variable or constant giving the upper error bar value.

* TYPE=       Type of error bars to be drawn: one of UPPER, LOWER, or BOTH
              and possibly one of ALT or ALTBY.  TYPE=LOWER draws only the
              lower error bars; TYPE=UPPER draws only the upper error bars;
              TYPE=BOTH draws both upper and lower error bars.  Use
              TYPE=ALT BOTH to have the error bars alternate (lower, upper)
              over observations in the input data set;  use
              TYPE=ALTBY BOTH to have the error bars alternate over values
              of the BY= variable. [Default: TYPE=BOTH]

* SUBSET=     A SAS expression, using any data set variables, used to select
              the observations for which error bars are drawn, when the
              expression evaluates to a non-zero value. [Default: SUBSET=1]

* SYMBOL=     The plotting symbol, drawn at (&CLASS, &var).  If not specified,
              no symbols are drawn.

* COLOR=      Color for lines and symbols, a character constant (enclosed
              in quotes), or variable name [Default: COLOR='BLACK']

* LINE=       The Annotate line style used for error bars [Default: LINE=1]

* SIZE=       Size of symbols and thickness of lines [Default: SIZE=1]

* BARWIDTH=   The width of error bar tops, in data units [Default:
              BARWIDTH=.5]

* OUT=        Name of the output data set, to be used as an Annotate
              data set with PROC GPLOT [Default: OUT=_BARS_]
                

 =*/
 

%macro bars(
	data=_LAST_,   /* name of input data set                       */
	var=,          /* name of response variable                    */
	class=,        /* name of independent variable                 */
	cvar=,         /* name of a curve variable                     */
	by=,           /* name of a BY variable, for multiple curves   */
	baxis=y,       /* axis along which error bars are drawn        */
	barlen=,       /* variable or constant giving error bar length */
	z=1,           /* barlen multiplier                            */
	lower=,        /* or, var or constant giving lower bar value   */
	upper=,        /* + var or constant giving upper bar value     */
	type=both,	   /* type of bars: UPPER, LOWER, BOTH, ALT, ALTBY */
	subset=1,      /* if-expression to subset the error bars       */
	symbol=,       /* plotting symbol, placed at (&class, &var)    */
	color='black', /* color for lines and symbols, 'const' or var  */
	line=1,        /* line style for error bars                    */
	size=1,        /* thickness of lines, size of symbols          */
	barwidth=.5,   /* width of bar tops                            */
	out=_bars_     /* name of output data set                      */
	);

options nonotes; 
%let abort=0;
%if &var=%str() | &class=%str()
   %then %do;
      %put ERROR: The VAR= and CLASS= parameters must be specified;
      %let abort=1;
      %goto DONE;
   %end;
%let class=%upcase(&class);

%if %upcase(&baxis) = Y
	%then %let oaxis = X;
	%else %let oaxis = Y;

%let type = %upcase(&type);
%let alt1=1; %let alt2=1;
%if %index(&type,ALTBY)  %then %do;
	%let alt1=mod(nby,2)=1;
	%let alt2=mod(nby,2)=0;
	%end;
%else %if %index(&type,ALT) %then %do;
	%let alt1=mod(_n_,2)=1;
	%let alt2=mod(_n_,2)=0;
	%end;

%let ay = &baxis;
%let ax = &oaxis;
%let gp = &class;

%if %length(&barlen) %then %do;
	%let lower = &var - &z * &barlen;
	%let upper = &var + &z * &barlen;
	%end;

/* determine if class variable is char or num */
proc contents data=&data out=_work_ noprint;
data _null_;
	set _work_;
	where (upcase(name)="&CLASS");
	if type=1 then call symput('gtype', 'NUM');
			else call symput('gtype', 'CHAR');
run; 
%if "&gtype" = "CHAR" %then %let ax = &ax.c;

proc sort data=&data out=_work_;
	by &by &cvar &class;

data &out;
   length function $8 color $8;
   retain xsys ysys '2';
   set _work_ /*(keep=&var &class &by &upper &lower &barlen) */;
	by &by &cvar &class;
	%if %length(&cvar) %then %do;
		if first.&cvar then ncv+1;
		%end;
	%if %length(&by) %then %do;
		if first.&by then nby+1;
		%end;

	if not (&subset) then return;
   color = &color;
   line  = &line;
   size  = &size;
	
   &ax = &class;   *-- sets x or xc (or y or yc);
   &ay= &var              ; function='MOVE'; output;
	%if %length(&symbol) %then %do;
   	text="&symbol"      ; function='SYMBOL'; output;
		%end;

	%if %index(&type,UPPER)=0 %then %do;  /* includes type=BOTH */
		if (&alt1) then do;
		&ay= &lower            ; function='DRAW'; output;
		%if "&gtype" = "NUM" %then %do;
		&ax = &gp - &barwidth  ; function='MOVE'; output;
		&ax = &gp + &barwidth  ; function='DRAW'; output;
		&ax = &gp              ; function='MOVE'; output;
		%end;
		end;
	%end;
	 
   &ax = &class;   *-- sets x or xc (or y or yc);
   &ay= &var              ; function='MOVE'; output;
	%if %index(&type,LOWER)=0 %then %do;
		if (&alt2) then do;
		&ay= &upper            ; function='DRAW'; output;
		%if "&gtype" = "NUM" %then %do;
		&ax = &gp - &barwidth  ; function='MOVE'; output;
		&ax = &gp + &barwidth  ; function='DRAW'; output;
		%end;
		end;
	%end;
	run;
%done:
options notes; 
%mend;
