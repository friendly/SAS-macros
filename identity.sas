%macro identity(data,result,x,y);

 /*calculates line of identity and adds the 2 points over the full  */
 /*scope of abscissa to a SAS dataset                               */
 /*written: 13. June, 1994,  Arnold Schick University of Marburg  */

 options nonotes;

 %if &data   =  or &data   = . %then %let data   = _LAST_;
 %if &result =  or &result = . %then %let result = _NEW_;
 %if &x =  or &x = . %then %let x = x;
 %if &y =  or &y = . %then %let y = y;

 proc means data=&data noprint min max;
   var &x &y;
   output out=_MINMAX_ min=x_min y_min max=x_max y_max;
 run;

 data _NULL_;
  set _MINMAX_;
  call symput('x_min',x_min);  call symput('x_max',x_max);
  call symput('y_min',y_min);  call symput('y_max',y_max);
 run;

 data &result;
   set &data end=last;
   retain ident 1;
   keep &x &y ident;
   if last then do;
     output;
     ident=2;
     &x = .; &y = .; output;
     &x = &x_min - 0.1/(&x_max-&x_min);
     &y = &x; output;
     &x = &x_max + 0.1/(&x_max-&x_min);
     &y = &x; output;
    end;
   else output;
 run;

 options notes;

%mend identity;

*Example;
/*
data eins;
  do x=1 to 11;
     y=ranuni(0)*50;
     output;
  end;
run;

%identity(eins,zwei,x,y);

proc gplot data=zwei;
  title 'Plot with Line of Identity';
  symbol1 i=join L=2 r=1;
  symbol2 i=join L=1 r=1;
  plot y*x=ident / skipmiss;
run; quit;
title;
*/