 /*--------------------------------------------------------------*
  *    Name: lines.sas                                           *
  *   Title: Create an Annotate dataset to draw lines in a plot  *
        Doc: http://www.datavis.ca/sasmac/lines.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  3 Jan 2000                                         *
  * Revised: 18 Jan 2006 11:17:38                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The LINES macro creates an annotate data set to draw lines in a 2D or 3D
 scatterplot.  This is useful when you need to plot two variables (e.g,
 observed, predicted) against a common X, with separate curves for the
 levels of a class variable.  In PROC GPLOT, for example, you cannot do

    proc gplot;
       plot (obs fit) * X = group;

 However, you can add the fitted lines to a plot of obs*X:

   %lines(x=X, y=fit, class=group, out=_lines_);
    proc gplot;
       plot obs * X = group / anno=_lines_;

 If no CLASS= variable is specified, one set of lines is drawn to connect
 the X,Y[,Z] points in their order in the input data set.  If a CLASS= variable
 is specified, one set of such lines is drawn for each level of the CLASS=
 variable.

=Usage:

 The LINES macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%lines();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* X=          The name of the X variable for the plot

* Y=          The name of the Y variable for the plot

* Z=          The name of the Z variable for a G3D plot (optional)

* BY=         BY variable(s), used to produce multiple plots for each level
              of the BY= variables.

* CLASS=      CLASS variable, used to produce multiple lines within a given
              plot.

* SYS=        XSYS & YSYS value [Default: SYS=2]

* COLOR=      Colors used for the levels of the CLASS= variable.
              [Default: COLOR=BLUE RED GREEN BLACK PURPLE YELLOW BROWN ORANGE]

* LINE=       List of line styles used for the levels of the CLASS= variable.
              [Default: LINE=1 3 5 8 20 22 25 34]

* SIZE=       Line thickness [Default: SIZE=1]

* SUBSET=     Expression to select points [Default: SUBSET=1]

* COPY=       List of variables copied to output dataset

* IN=         Optional input annotate data set, concatenated to the output
              data set

* OUT=        The name of the output data set [Default: OUT=_LINES_]
                

 =*/

%macro lines(
   data=_LAST_,
   x=,             /* X variable for scatterplot       */
   y=,             /* Y variable for scatterplot       */
   z=,             /* Z variable for G3D (optional)    */
   by=,            /* BY variable(s) (mult plots)      */
   class=,         /* CLASS variable (mult curves)     */
   sys=2,          /* XSYS & YSYS value                */
   color=BLUE RED GREEN BLACK PURPLE YELLOW BROWN ORANGE,    
	                /* line color(s)                    */
   line=1 3 5 8 20 22 25 34,
	                /* line style(s)                    */
   size=1,         /* line thickness                   */
   subset=1,       /* expression to select points      */
   copy=,          /* vars copied to output dataset    */
   in=,            /* input annotate data set          */
   out=_lines_     /* annotate data set produced       */
   );

options nonotes; 
%if %length(&by) or %length(&class) %then %do;
	proc sort data=&data;
	by &by &class;
	%end;
run;

options notes;
data &out;
   set &data;
	%if %length(&by) or %length(&class) %then %do;
		by &by &class;
		%end;
   keep x y function
		%if %length(&line) %then  line ;
		%if %length(&size) %then  size ;
        color &by &class &copy xsys ysys ;
   length function color $8 ;
   xsys = "&sys"; ysys = "&sys";
   x = &x;
   y = &y;
   %if &z ^= %str() %then %do;
          zsys = "&sys"; keep z zsys;
          z = &z;
       %end;

	%if %length(&class) %then %do;
		if first.&class then _class_+1;
		%end;
	%else %str(_class_ = 1;);
	
	if _n_=1
		%if %length(&class) %then 
		or first.&class ;
		then function = 'move';
		else function = 'draw';

   color=scan("&color", _class_);

	%if %length(&line) %then %do;
		line=input(scan("&line", _class_), 3.);
		%end;
	%if %length(&size)   %then %str(size=&size;);
   if (&subset);
run;
%if %length(&in) %then %do;
	data &out;
		set &in &out;
		%if %length(&by) %then %do;
			by &by;
			%end;
	%end;

%mend;
