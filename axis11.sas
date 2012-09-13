 /*--------------------------------------------------------------*
  *    Name: axis.sas                                            *
  *   Title: Create Annotate data set to draw an X or Y axis     *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/axis.html       *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 27 Jan 2006 23:45:52                                *
  * Revised: 25 May 2006 09:58:59                                *
  * Version: 1.1                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The AXIS macro creates an Annotate data set to draw an X or Y axis
 on a graph.  The values labeled on the axis can be expressed on any
 transformed scale which can be represented as a SAS expression. This
 is useful when you can't use a PLOT2 statement to draw a right vertical
 axis, or when you want to show the values on some transformed scale.
 
 Typically you will need to adjust the boundaries of the plot to allow for
 the additional axis, e.g., with the OFFSET= parameter of an AXIS statement
 or by allowing extra space at the top or right of the plot.
 
=Usage:

 The AXIS macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
        %axis();
 
==Parameters:

* ON=         X or Y: axis to be drawn.  ON=X draws a horizontal axis
              ON=Y draws a vertical axis   [Default: ON=X]

* AT=         Location on other axis (2<=AT<=98).  [Default: AT=98]

* ATUNIT=     Unit in which the AT= value is given. PCT means percent of the
              graphics data region.  Any other value is treated as meaning
              data coordinates. [Default: ATUNIT=PCT]

* VALUES=     List of tick mark values, in any of the forms that can be
              used in a DO statement, e.g.,
              
              VALUES=10, 25, 50, 100
              VALUES=10 to 100 by 20

* FUNCTION=   Function of VALUE giving the data value on the scale of the
              data that is plotted. [Default: FUNCTION=VALUE]

* HTICK=      Height of tick marks [Default: HTICK=1]

* FMT=        Format for printing value labels [Default: FMT=BEST8.]

* MINOR=      Number of minor tick marks [Default: MINOR=1]

* LABEL=      Axis label.  

* OUT=        Name of output annotate data set  [Default: OUT=AXIS] 

 =*/

%macro axis(
   out=AXIS,         /* output annotate= data set           */
   on=X,             /* X or Y: axis to be drawn            */
   at=98,            /* location on other axis (2<=at<=98)  */
   atsys=,
   values=,          /* tick mark values                    */
   function=value,   /* function of VALUE giving data value */
   htick=1,          /* height of tick marks (%)            */
   fmt=BEST8.,       /* format for printing value labels    */
   minor=1,          /* number of minor tick marks          */
   label=);          /* axis label                          */
 
%let on=%upcase(&on);
*let atunit=%upcase(&atunit);
 
%if &on = X
    %then %do;
       %let ataxis=Y;
       %if &at >= 50 %then %do;
          %let side=1;
          %let atpos=2;
          %end;
       %else %do;
          %let side=-1;
          %let atpos=8;
       %end;
    %end;
    %else %do;
       %let ataxis=X;
       %if &at >= 50 %then %do;
          %let side=1;
          %let atpos=6;
          %end;
       %else %do;
          %let side=-1;
          %let atpos=4;
       %end;
    %end;
%let AX = &on;                    /* X or Y: axis to be drawn */
%let AY = &ataxis;                /* Y or X: the other axis   */
data &out;
    length function text $ 8;
    drop value save m minor last dif;
    &AX.SYS = '1';                /* data % coordinates  */
    %if %length(&atsys) %then
       %str(&AY.SYS = "&atsys";);      /* data % coordinates for axis  */
    %else
       %str(&AY.SYS = '2';);      /* data value coordinates for axis  */
 
    &ay=&at;                      /* Draw the axis line, from 0-100%  */
    &ax=100;
    position="&atpos";
    %if &label ^= %str() %then %do;
       text = "&label";
       function='LABEL'; output;
       %end;
    function='MOVE'; output;
    &ax=0;
    function='DRAW'; output;
 
    &AX.SYS = '2';
    minor = &minor;
    do value= &values;
       &ax = &function;
       &ay = &at;
       last = lag(value);
	   *-- draw tick mark, using relative %;
/*
       function='MOVE'; output;
	   &AY.SYS = '9';
*       &ay=&at + &side * &htick;
       &ay = &side * &htick;
       function='DRAW'; output;
*/
	   *-- draw tick label;
*       &ay=&at + 1.5 * &side * &htick;
	   &AY.SYS = '1';
	   &ay = 0;
       function='MOVE'; output;
	   function='cntl2txt'; output;   *-- store position for label;

	   &AX.sys='A'; &AY.sys='A';      *-- relative cells in procedure output area; 
	   &ax=+0; &ay=-1;
       text=compress(put(value, &fmt));
       function='LABEL'; output;
 
       if minor > 0 & last^=. then do;
          save = value;
          dif = value-last;
          do m=1 to minor;
             value = last + dif * m/(minor+1);
             &ax = &function;
             &ay = &at ;
             function='MOVE'; output;
             &ay=&at + .7 * &side * &htick;
             function='DRAW'; output;
             end;
          value = save;
          end;  /* if minor */
       end;  /* do value= */
 
%mend;
 
