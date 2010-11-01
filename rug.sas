 /*--------------------------------------------------------------*
  *    Name: rug.sas                                             *
  *   Title: Create annotations for a rug plot along an axis     *
        Doc: http://www.datavis.ca/sasmac/rug.html         
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 26 May 2004 09:33:55                                *
  * Revised: 10 Apr 2008 15:12:07                                *
  * Version: 1.1-1                                               *
  *    - Added when='A' to anno code                             * 
  * 1.1  Added SEP=                                              *
  *      Corrected jitter to center                            
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The RUG macro creates an annotate data set to draw 'rug lines'
 along an axis in a plot indicating the univariate distribution
 of a variable.

=Usage:

 The RUG macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%rug(data=foo, var=X1, at=0.05, out=rug1);
	proc gplot data=foo;
	   plot Y * X1 / anno=rug1 ... ;
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        Name of the variable for rug plot

* BY=         Grouping variable: if specified, rugs are produced for each
              level of the BY= variable

* CLASS=      Synonym for BY=

* ON=         Axis on which to display the VAR values [Default: ON=X]

* AT=         Location of bottom rug lines on axis, in percent coordinates.
             [Default: AT=0.5]

* HT=         Height of rug lines, in percent coordinates. [Default: HT=2]

* SEP=        Separation between successive rug lines, in percent coordinates. 
              [Default: SEP=0.5]

* COLOR=      Color(s) of rug lines.  A different color is used for each
              level of the CLASS= variable. [Default: COLOR=BLACK BLUE RED GREEN]

* JITTER=     Amount of random jitter applied to the VAR= values.  This is
              useful when the VAR= values are discrete, and would otherwise
			  overplot. [Default: JITTER=0]

* COPY=       The names of any variables to be copied to output dataset

* IN=         The name of an optional input annotate data set.  If
              specified, the IN= data set is concatenated with the
			  OUT= data set.

* OUT=        Name for output data set [Default: OUT=_RUG_]
                

 =*/
%macro rug(
    data=_last_,   /* name of input data set                     */
    var=,          /* name of variable for rug plot              */
    by=,           /* grouping variable                          */
    class=,         /*    "         "                             */
    on=x,          /* axis on which to display the VAR values    */
    at=0.5,        /* location of bottom rug lines on axis  (%)  */
    ht=2,          /* height of rug lines                        */
	sep=0.5,       /* separation between successive rug lines    */
    color=black blue red green,   /* colors of rug lines         */
    jitter=0,      /* jitter applied to the VAR values           */
    copy=,          /* vars copied to output dataset             */
    in=,           /* input annotate data set                    */
    out=_rug_      /* name for output data set */
    );

%if %length(&class) & %length(&by)=0 %then %do;
	%let by=&class;
	%end;

%if %length(&by) %then %do;
    proc sort data=&data;
    	by &by;
    %end;
%let on=%lowcase(&on);
%if &on=x %then %do;
    %let ax=x;
    %let ay=y;
    %end;
  %else %do;
    %let ax=y;
    %let ay=x;
    %end;
 
 data &out;
    set &data;
    keep &var xsys ysys x y color function when
    	 &by
         &copy
		;
    length function color $8;
    &ax.sys ='2';
    &ay.sys ='1';
    retain &ay.0 &at i 1 when 'A';
    %if %length(&by) %then %do;
    	by &by;
	%end;
    color = scan("&color", i);
    &ax = &var + &jitter * (uniform(0.)-0.5);
    &ay = &ay.0;
    function = 'move';  output;
    &ay = &ay.0 + &ht;
    function = 'draw';  output;

    %if %length(&by) %then %do;
	if last.&by then do;
	    &ay.0 = &ay.0 + &ht + &sep;
	    i+1;
	    end;
	%end;

%if %length(&in) %then %do;
   data &out;
      set &in &out;
      %if %length(&by) %then %do;
         by &by;
         %end;
   %end;


 %mend;
 
