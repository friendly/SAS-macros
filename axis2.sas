%include goptions;
goptions vsize=8;
 
/*----------------------------------------------------------------------*
 | macro AXIS                                                           |
 |    Produces an ANNOTATE= data set to draw an X or Y axis on a graph. |
 |    The values on the axis can be expressed on any transformed scale  |
 |    which can be represented as a SAS expression.                     |
 *----------------------------------------------------------------------*/
%macro axis(out=AXIS,         /* output annotate= data set           */
            on=X,             /* X or Y: axis to be drawn            */
            at=98,            /* location on other axis (2<=at<=98)  */
            atunit=PCT,
            values=,          /* tick mark values                    */
            function=value,   /* function of VALUE giving data value */
            htick=1,          /* height of tick marks                */
            fmt=BEST8.,       /* format for printing value labels    */
            minor=1,          /* number of minor tick marks          */
            label=);          /* axis label  ** NOT IMPLEMENTED **   */
 
%let on=%upcase(&on);
%let atunit=%upcase(&atunit);
 
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
    drop value save m minor;
    &AX.SYS = '1';                /* data % coordinates  */
    %if &atunit=PCT %then %do;
       %let aysys=1;             /* data % coordinates for axis       */
       %let atsys=7;             /* data %  relative % for labels     */
       %end;
    %else %do;
       %let aysys=2;             /* data value coordinates for axis   */
       %let atsys=8;             /* data value relative for labels    */
       %end;
 
    &ay.sys="&aysys";
    &ay=&at;                      /* Draw the axis line, from 0-100%  */
    &ax=100;
    function='MOVE'; output;
    &ax=0;
    function='DRAW'; output;
 
    &AX.SYS = '2';
    position="&atpos";
    minor = &minor;
    do value= &values;
       &ay.sys="&aysys";
       &ax = &function;
       &ay = &at;
       last = lag(value);
       function='MOVE'; output;
       &ay.sys="&atsys";               /* switch to relative coords */
       &ay=      &side * &htick;
       function='DRAW'; output;
       &ay=  -1* &side * &htick;
       function='MOVE'; output;
       &ay=      1.5 * &side * &htick;
       text=compress(put(value, &fmt));
       function='LABEL'; output;
       &ay=      1.5 * &side * &htick;
       function='MOVE'; output;
 
       if minor > 0 & last^=. then do;
          save = value;
          dif = value-last;
          do m=1 to minor;
             value = last + dif * m/(minor+1);
             &ay.sys="&aysys";
             &ax = &function;
             &ay = &at ;
             function='MOVE'; output;
             &ay.sys="&atsys";
             &ay=      .7 * &side * &htick;
             function='DRAW'; output;
             end;
          value = save;
          end;  /* if minor */
       end;  /* do value= */
 
%mend axis;
 
title h=1.4 'Test of Custom %AXIS'
      h=2  a=-90 ' ';         /* allow extra space at right for axis */
 
/*-------------------------------------------------------------------*
 |  Sample data                                                      |
 |   X - considered percents, labelled on a normal probability scale |
 |   Y - plotted as square root, labelled on original scale          |
 *-------------------------------------------------------------------*/
Data xy;
   Do x=1,2,5 to 95 by 5,98, 99;
      y=x + 10*uniform(0);
      sy=sqrt(y);
      output; end;
 
%axis(on=X,out=test1,
           values=-2 to 2 by .5,
           function=round(100*probnorm(value))
           );
 
%axis(on=Y,out=test2,at=98,
           values= 10 to 100 by 10,
           function=sqrt(value));
data test;
   set test1 test2;
proc print;
proc gplot data=xy;
   plot sy * x
   / anno=test href=20 40 60 80 lhref=34
               vref=2  4  6  8  lvref=34
     vaxis=axis1;
   axis1 order=(1 to 10);
run;
 
/* Replace usual axes with transformed axes */
%axis(on=X,out=test1,at=0,
           values=-2 to 2 by .5,
           function=round(100*probnorm(value)) );
%axis(on=Y,out=test2,at=0,
           values= 10 to 100 by 10,
           function=sqrt(value));
data test;
   set test1 test2;
proc gplot data=xy;
   plot sy * x
   / anno=test href=20 40 60 80 lhref=34
               vref=2  4  6  8  lvref=34
     vaxis=axis1 haxis=axis2;
   axis1 order=(1 to 10) value=none major=none minor=none style=0;
   axis2                 value=none major=none minor=none style=0;
%gfinish;
