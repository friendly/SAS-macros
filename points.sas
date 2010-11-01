 /*-------------------------------------------------------------------*
  *    Name: points.sas                                               *
  *   Title: Create an Annotate dataset to draw points in a plot      *
        Doc: http://www.datavis.ca/sasmac/points.html              
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly         <friendly@yorku.ca>            *
  * Created:  12 Nov 1998 10:26:09                                    *
  * Revised:  12 May 2006 10:04:10                                    *
  * Version:  1.1                                                     *
  *  - Added BY= parameter, IN=                                       *
  *  - Added separate XSYS, YSYS, ZSYS and HSYS parameters            *
  *  - Added WHEN=                                                    *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:

 The POINTS macro creates an annotate data set to draw point symbols
 in a 2D or 3D scatterplot.  This is useful when you need to plot two
 variables (e.g, observed, predicted) against a common X, with separate
 curves for the levels of a class variable.  In PROC GPLOT, for example,
 you cannot do

    proc gplot;
       plot (obs fit) * X = group;

 However, you can add the OBS points to a plot of fit*X:

   %points(x=X, y=obs);
    proc gplot;
       plot fit * X = group / anno=_pts_;

=Usage:

 The POINTS macro is called with keyword parameters.  The X= and Y=
 parameters are required.  For a plot with PROC G3D, you must also
 give the Z= variable.
 The arguments may be listed within parentheses in any order, separated
 by commas. 
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* X=          The name of the X variable for the scatterplot

* Y=          The name of the Y variable for the scatterplot

* Z=          The name of the Z variable for a 3D scatterplot

* BY=         The name(s) of any BY variable(s) to be used for multiple
              plots.

* CLASS=      The name of a class variable, to be used with PROC GPLOT
              in the PLOT statement for multiple curves, in the form
				  
				     plot Y * X = CLASS;

* SYS=        Specifies the Annotate XSYS & YSYS value [Default: SYS=2]
* XSYS=       Specifies the Annotate XSYS  [Default: XSYS=&SYS]
* YSYS=       Specifies the Annotate YSYS  [Default: YSYS=&SYS]
* ZSYS=       Specifies the Annotate ZSYS  [Default: ZSYS=&SYS]
* HSYS=       Specifies the Annotate HSYS value for symbol height

* COLOR=      Point color(s): the name of a dataset character variable,
              or an expression which evaluates to a SAS/GRAPH color, or
              string constant enclosed in quotes. [Default: COLOR='BLACK']

* SYMBOL=     Point symbol(s): the name of a dataset character variable,
              or an expression which evaluates to a SAS/GRAPH color, or
              string constant enclosed in quotes.  [Default: SYMBOL='DOT']

* SIZE=       The size of the symbol (in GUNIT units).  If not specified,
              the global graphics option HTEXT value is used.

* FONT=       Font for symbol(s): the name of a dataset character variable,
              or an expression which evaluates to a SAS/GRAPH color, or
              string constant enclosed in quotes.  Use for special
				  symbols, e.g., FONT='MARKER'.  If not specified, the
				  standard symbol font is used.

* WHEN=       When to draw the points: A(fter) or B(efore)

* SUBSET=     An expression (which may involve any dataset variables) to
              select points.  A point will be plotted if the expression 
				  evaluates to non-zero for the current observation.
              [Default: SUBSET=1]

* COPY=       The names of any variables to be copied to output dataset

* IN=         The name of an optional input annotate data set(s).  If
              specified, the IN= data set is concatenated with the
				  OUT= data set.

* OUT=        Name of the annotate data set produced. [Default: OUT=_PTS_]
                

 =*/

%macro points(
   data=_LAST_,
   x=,             /* X variable for scatterplot       */
   y=,             /* Y variable for scatterplot       */
   z=,             /* Z variable for G3D (optional)    */
   by=,            /* BY variable(s) (mult plots)      */
   class=,         /* CLASS variable (mult curves)     */
   sys=2,          /* XSYS & YSYS value                */
   xsys=,
   ysys=,
   zsys=,
   hsys=,
   color='BLACK',  /* symbol color (quote if const)    */
   symbol='dot',   /* plot symbol                      */
   size=,          /* size of symbol                   */
   font=,          /* font for symbol                  */
   when=A,         /* when to draw points: A or B      */
   subset=1,       /* expression to select points      */
   copy=,          /* vars copied to output dataset    */
   in=,            /* input annotate data set          */
   out=_pts_       /* annotate data set produced       */
   );

options nonotes; 
%if %length(&by) or %length(&class) %then %do;
	proc sort data=&data;
	by &by &class;
	%end;
run;

options notes;
%if %length(&xsys)=0 %then %let xsys=&sys;
%if %length(&ysys)=0 %then %let ysys=&sys;
%if %length(&zsys)=0 %then %let zsys=&sys;

data &out;
   set &data;
	%if %length(&by) or %length(&class) %then %do;
		by &by &class;
		%end;
   keep x y function text when
		%if %length(&size) %then  size ;
        color &by &class &copy xsys ysys ;
   length function $8 text  color $ 8 when $1;
   xsys = "&xsys"; ysys = "&ysys"; function='SYMBOL'; when="&when";
   x = &x;
   y = &y;
   %if &z ^= %str() %then %do;
          zsys = "&zsys"; keep z zsys;
          z = &z;
       %end;
   %if %length(&hsys) %then %do;
          hsys = "&hsys"; keep hzsys;
          z = &z;
       %end;

	%if %length(&class) %then %do;
		if first.&class then _class_+1;
		%end;
	%else %str(_class_ = 1;);

	%if %length(&size)   %then %str(size=&size;);

   color=&color;
	text=&symbol;
   %if &font ^= %str() %then %do;
      keep style;
      style = &font;
      %end;

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
