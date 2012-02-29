 /*------------------------------------------------------------------*
  *    Name: label.sas                                               *
  *   Title: Create an Annotate dataset to label observations        *
  *      in a scatterplot                                            *
        Doc: http://www.datavis.ca/sasmac/label.html              
  *------------------------------------------------------------------*
  *  Author:  Michael Friendly         <friendly@yorku.ca>           *
  * Created:  15 May 1991 12:27:15                                   *
  * Revised:  11 Nov 2008 09:10:01                                   *
  * Version:  1.8                                                    *
  *  - Added BY= parameter                                           *
  *  - Added POS=- to position up/down wrt mean y                    *
  *  - Added copy= parameter; fixed bug with subset & pos            *
  *  - Added angle= and rotate=, IN= (then fixed keep= bug)          *
  *  - Allow separate XSYS, YSYS, ZSYS                               *
  *  - Added WHEN=, fixed error with rotate                          *
  * 1.8 Fixed problem with "&pos" (thx: Einar Rodland)               *
  *                                                                  *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)   *         
  *------------------------------------------------------------------*/
 /*=
=Description:
 
 The LABEL macro creates an Annotate data set used to label
 observations in a 2D (PROC GPLOT) or 3D (PROC G3D) scatterplot.
 The points which are labeled may be selected by an arbitrary
 logical expression from those in the input dataset. The macro
 offers flexible ways to position the text label relative to either
 the data point or the center of the plot. The resulting Annotate
 data set would then be used with the ANNO= option of PROC GPLOT
 or PROC G3D.


=Usage:

 Values must be supplied for the X=, Y= and TEXT= parameters.  For a PROC
 G3D plot, supply a value for the Z= parameter as well.
 The label macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%label(x=age, y=response, text=name);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* X=          The name of the (numeric) X variable for the scatterplot,
              a numeric constant, or data step expression, in SYS= coordinates.

* Y=          The name of the (numeric) Y variable for the scatterplot
              or a numeric constant, in SYS= coordinates.

* Z=          The name of the (numeric) Z variable for a 3D scatterplot
              or a numeric constant, in SYS= coordinates.

* BY=         The name(s) of any BY variable(s) to be used for multiple
              plots.

* XOFF=       An X-offset for the text label. You may specify a 
              numeric constant
              (XOFF=-1) in data units, or the name of a variable in the
				  input data set. Positive values move the label
              to the right relative to the point; negative values move it
				  to the left.


* YOFF=       A Y-offset for the text label. Positive values move the label
              towards larger Y values.


* ZOFF=       A Z-offset for the text label, for a 3D plot.

* TEXT=       The text used to label each point. TEXT= may be
	           specified as a variable in the data set or a SAS
				  expression involving dataset variables (e.g.,
				  TEXT=SCAN(MODEL,1)) and/or string 
				  constants.  If you supply an expression, use the 
				  C<%str()> macro function,  e.g.,
				  C<TEXT=%str(trim(name || '-' || place))> to protect special
				  characters.

* LEN=        Length of the TEXT variable [Default: LEN=16]

* POS=        Specifies the position of the label relative to the
              data point. 	The POS= value can be a character constant 
				  (one of the characters in "123456789ABCDEF<+>", as used 
				  by the Annotate POSITION variable), an expression involving 
				  dataset variables which evaluates to one of these characters
				  (e.g., POS=SCAN('9 1 3', _NUMBER_)) or one of the special
				  characters, "/", "|", or "-".  The special position values 
				  cause the point label to be out-justified (moved outward 
				  toward the edges of the plot relative to the data point.)
				  by comparing the coordinates of the pointto the mean of 
				  X and Y (/), or to the mean of X only (|), or to the
				  mean of Y only (-).
	
* SYS=        Specifies the Annotate XSYS, YSYS (and ZSYS) values,
              as a list of up to 3 numeric values [Default: SYS=2]

* COLOR=      Label color (the name of a dataset character variable or a
              string constant enclosed in quotes. [Default: COLOR='BLACK']

* SIZE=       The size of label (in whatever units are given by the
              GUNIT goption). There is no 
              default, which means that the labels inherit the 
              global HTEXT setting.

* FONT=       The name of the font used for the label.  There is no 
              default, which means that the labels inherit the 
              global FTEXT setting.

* ANGLE=      Baseline angle for label.

* ROTATE=     Character rotate for label

* WHEN=       When to apply label annotation? 

* SUBSET=     An expression (which may involve any dataset variables) to
              select points.  A point will be labeled if the expression 
				  evaluates to non-zero for the current observation.
              [Default: SUBSET=1]

* COPY=       The names of any variables to be copied to output dataset

* IN=         The name of an optional input annotate data set.  If
              specified, the IN= data set is concatenated with the
				  OUT= data set.

* OUT=        The name of the annotate data set produced.  [Default: OUT=_LABEL_]
                
=Example:

This example plots Weight against Price for American cars in the Auto data,
labeling the most expensive cars.

	%label(data=auto, x=price, y=weight,
				color='red', size=1.2,
				subset=origin='A' and price>10000,
				pos=1, text=scan(model,1));
	
	proc gplot data=auto(where=(origin='A'));
		plot weight * price / frame anno=_label_;
		symbol1 v='+'  i=none color=black h=1.5;

 =*/

%macro label(
   data=_LAST_,
   x=,             /* X variable for scatterplot       */
   y=,             /* Y variable for scatterplot       */
   z=,             /* Z variable for G3D (optional)    */
	cvar=,         /* name of a curve variable                     */
   by=,            /* BY variable(s) (mult plots)      */
   xoff=0,         /* X-offset for label (constant     */
   yoff=0,         /* Y-offset for label    or         */
   zoff=0,         /* Z-offset for label  variable)    */
   text=,          /* text variable or expression      */
   len=16,         /* length of text variable          */
   pos=,           /* position for label (/=out-just)  */
   sys=2,          /* XSYS & YSYS value                */
   color='BLACK',  /* label color (quote if const)     */
   size=,          /* size of label                    */
   font=,          /* font for label                   */
   angle=,         /* baseline angle for label         */
   rotate=,        /* character rotate for label       */
   when=,          /* when to apply label              */
   subset=1,       /* expression to select points      */
   copy=,          /* vars copied to output dataset    */
   in=,            /* input annotate data set          */
   out=_label_     /* annotate data set produced       */
        );

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;
 
%let cleanup=;
%* -- pos can be a constant, an expression, or / or -;
%*    if a character constant, put "" around it;
%if %index(|/-,&pos) %then
   %do;  %*-- Out-justify wrt means of x,y;
      proc summary data=&data;
         var &x &y;
         output out=_means_ mean=mx my;
	%let cleanup=_means_;
   %end;
%else %if %bquote(&pos) ^= %str() %then %do;
   %if %verify(&pos,%str(123456789ABCDEF<+>)) = 0
       %then %let pos="&pos" ;
   %end;
%else %let pos = "5";

%if %length(&by) %then %do;
   proc sort data=&data;
   by &by &cvar
      %if %datatyp(&x) = CHAR %then &x;
	  ;
   %end;
run;

%local xsys ysys zsys;
%let xsys = %scan(&sys &sys &sys,1,%str( ));
%let ysys = %scan(&sys &sys &sys,2,%str( ));
%let zsys = %scan(&sys &sys &sys,3,%str( ));

options notes;
data &out;
   set &data;
   %if %length(&by) %then %do;
      by &by &cvar
      %if %datatyp(&x) = CHAR %then &x;
	  ;
      %end;
   keep x y xsys ysys position function
      %if %length(&size) %then  size ;
      %if %length(&when) %then  when ;
      %if %length(&angle) %then angle ;
      %if %length(&rotate) %then rotate ;
        color text &by &copy;
   length function color $8 text $ &len position $1;
   xsys = "&xsys"; ysys = "&ysys"; function='LABEL';
   x = &x + &xoff ;
   y = &y + &yoff ;
   %if &z ^= %str() %then %do;
          retain zsys "&zsys"; keep z zsys;
          z = &z + &zoff;
       %end;
   %if "&text" ^= ""
      %then %do; text=&text;  %end;
      %else %do; text=left(put(_n_,5.)); %end;
   %if %length(&size)   %then %str(size=&size;);
   %if %length(&when)   %then %str(when="&when";);
   %if %length(&angle)  %then %str(angle=&angle;);
   %if %length(&rotate) %then %str(rotate=&rotate;);
   color=&color;
   %if &font ^= %str() %then %do;
      keep style;
      style = "&font";
      %end;
   %if %bquote(&pos) = %str(/) %then
      %do;
         retain mx my;
         if _n_=1 then set _means_(keep=mx my);
         if x > mx then
            if y > my then position = '3';
                      else position = '9';
            else
            if y > my then position = '1';
                      else position = '7';
      %end;
   %else %if %bquote(&pos) = %str(-) %then
      %do;
         retain mx my;
         if _n_=1 then set _means_(keep=mx my);
         if y > my then position = '2';
                   else position = '8';
      %end;
   %else %if %bquote(&pos) = %str(|) %then
      %do;
         retain mx my;
         if _n_=1 then set _means_(keep=mx my);
         if x > mx then position = '6';
                   else position = '4';
      %end;
      /* if pos has more than one character, use them cyclically */
   %else %if %qsubstr(&pos,1,1) eq %str(%") %then    /* " */
      %str(position=substr(&pos,1+mod(_n_,length(&pos)),1););
   %else %str(position = &pos;);
   if (&subset);
run;

%if %length(&in) %then %do;
   data &out;
      set &in &out;
      %if %length(&by) %then %do;
         by &by;
         %end;
   %end;

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
%if %length(&cleanup) %then %do;
options nonotes;
proc datasets nofs nolist library=work memtype=(data);
    delete &cleanup;
	 run; quit;
%end;

%done: 
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend label;
