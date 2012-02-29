 /*-------------------------------------------------------------------*
  *    Name: boxanno.sas                                              *
  *   Title: Annotate a scatter plot with univariate boxplots         *
        Doc: http://www.datavis.ca/sasmac/boxanno.html            
  *-------------------------------------------------------------------*
  * Author:   Michael Friendly            <friendly@yorku.ca>         *
  * Created:  20 Apr 1988 11:32:44                                    *
  * Revised:  03 Feb 2004 16:59:37                                    *
  * Version:  1.8                                                     *
  *   - Modified to allow baxis=MIDPOINT, for use with GCHART         *
  *   - Fixed non-display of mean point                               *
  *   - Added display of outside observations                         *
  * 1.7 Added BSYS parameter (BSYS=1 for %, =2 for data system)       *
  *     Added HSYM=, CBOX=, SYMBOLS= for greater control              *
  * 1.8 Added ID= to label outside observations                       *                   
  *  -handle special missing values (.A-.Z)                           *
  *  -cleanup temp datasets, handle notes/nonotes                     *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *-------------------------------------------------------------------*/
 
 /*------------------------------------------------------------------*
  | BOXAXIS macro - create an annotate dataset to draw a boxplot for |
  |         ONE axis in a scatterplot. Can be used with Proc GPLOT   |
  |         or Proc G3D scatterplots.                                |
  |         This macro just creates the annotate dataset. It is up to|
  |         the user to call the appropriate plot procedure.         |
  |   e.g., Proc GPLOT data= ;                                       |
  |             Plot Y * X / annotate= ... ;                         |
  *------------------------------------------------------------------*/
%macro boxaxis(
    data=_LAST_,          /* Input dataset                     */
    out=_DATA_,           /* Output ANNOTATE dataset           */
    var=,                 /* Variable to be plotted            */
	id=,                  /* ID variable for outside observations */
    baxis=x,              /* Axis on which it goes- X, Y, or Z */
    oaxis=y,              /* The other axis in the plot        */
    paxis=z,              /* The 3rd axis (ignored in GPLOT)   */
    boxwidth=4,           /* width of box in data percent      */
    cbox=BLACK,           /* color of box outline              */
	hsym=,                /* Height for outside symbols        */
	bsys=1,               /* SAS/GRAPH sys coord for BAXIS     */
	symbols=star circle dot, /* symbols for mean, outside, far */
    pos=98);              /* position of box on OAXIS 0<POS<100*/
 
%local bx by bz;             /* macro symbols for annotate x,y,z  */
%if ( &baxis = %str() or &oaxis = %str() ) %then %do;
    %put ERROR: Box (BAXIS=) and other (OAXIS=) axes must be specified;
	 %goto done;
%end;
%else %do;
   %if (&baxis=&oaxis) %then %do;
       %put ERROR: BAXIS (&BAXIS) cannot be the same as OAXIS (&OAXIS);
   %end;
%end;

%local sym1 sym2 sym3;
%let sym1 = %scan(&symbols, 1, %str( ));
%let sym2 = %scan(&symbols, 2, %str( ));
%let sym3 = %scan(&symbols, 3, %str( ));

%*-- Reset required global options;
%if %sysevalf(&sysver  >= 7) %then %do;
	%local o1;
	%let o1 = %sysfunc(getoption(notes));
	options nonotes;
	%end;
%else %do;
   options nonotes;
	%end;

 /*----------------------------------*
  | Find median & quartiles          |
  *----------------------------------*/
proc univariate data=&data noprint;
    var &var;
    output out=_quart_
           n=n q1=q1 q3=q3 median=median qrange=iqr mean=mean;
run;
 /*-----------------------------------------------*
  | Find outside & farout points                  |
  *-----------------------------------------------*/
data _plotdat_;
    set &data;
    if _n_=1 then set _quart_;
    retain q1 q3 iqr;
    keep &var &id outside;
	 if &var > .Z;
    outside=1;
    if &var < (q1-1.5*iqr) or &var > (q3+1.5*iqr)
       then outside=2;
    if &var < (q1-3.0*iqr) or &var > (q3+3.0*iqr)
       then outside=3;
run;
 /*----------------------------------------------------*
  |  Whiskers go from quartiles to most extreme values |
  |  which are *NOT* outside.                          |
  *----------------------------------------------------*/
proc univariate data=_plotdat_ noprint;
    where (outside=1);
    var &var;
    output out=_whisk_ min=lo_whisk max=hi_whisk;
run;
data _box_;
    merge _quart_ _whisk_;
*proc print data=_box_;
 /*-----------------------------------------------*
  | Annotate data set to draw boxes & whiskers    |
  *-----------------------------------------------*/
%let bx = &oaxis;
%let by = &baxis;
%let bz = &paxis;
*options notes;
data &out;
    set _box_;
    drop n lo_whisk hi_whisk q1 q3 iqr median mean
         center halfwid;
    length function text /*style*/ color $8;
    halfwid= &boxwidth / 2;
    color = "&cbox";

%if &bsys=1 %then %do;
	%if ( &pos > 50 ) %then %do;
			center= &pos - halfwid; %end;
	%else %do;
			center= &pos + halfwid; %end;
	%end;
%else %do;
	center = &pos;
	%end;

    &bx.sys = "&bsys";     /* coordinates for 'other' */
%if %upcase(&by) ^= MIDPOINT %then %do; 
    &by.sys = '2';         /* data value coordinates for box axis     */
%end;
%if ( &paxis ^= %str() ) %then %do;
    &bz.sys = '1';         /* data percentage coordinates for 3rd axis*/
    &bz     = 1  ;
%end;
call symput('center', put(center, best.));
&bx =center-halfwid       ; &by = q1;       dot=1 ; link out; * box   ;
&bx =center+halfwid       ; &by = q1;       dot=21; link out;
&bx =center+halfwid       ; &by = q3;       dot=22; link out;
&bx =center-halfwid       ; &by = q3;       dot=23; link out;
&bx =center-halfwid       ; &by = q1;       dot=24; link out; * box   ;
 
&bx =center-halfwid       ; &by = median  ; dot=3 ; link out; * median;
&bx =center+halfwid       ; &by = median  ; dot=4 ; link out;
 
&bx =center               ; &by = q1      ; dot=5 ; link out; * lo     ;
&bx =center               ; &by = lo_whisk; dot=6 ; link out; * whisker;
&bx =center               ; &by = q3      ; dot=7 ; link out; * hi     ;
&bx =center               ; &by = hi_whisk; dot=8 ; link out; * whisker;
&bx =center-halfwid/2     ; &by = lo_whisk; dot=9 ; link out;
&bx =center+halfwid/2     ; &by = lo_whisk; dot=10; link out;
&bx =center-halfwid/2     ; &by = hi_whisk; dot=11; link out;
&bx =center+halfwid/2     ; &by = hi_whisk; dot=12; link out;
&bx =center               ; &by = mean    ; dot=13; link out;
    return;
 
out:
   select;
      when (dot in (1, 3, 5, 7, 9, 11)) do;
         line = .;
         function = 'MOVE    ';          output;
      end;
      when (dot in (4, 6, 8, 10, 12, 21, 22, 23, 24)) do;
*      when (dot=4 | dot=6 | dot=8 | dot=10 | dot=12
          | dot=21| dot=22| dot=23| dot=24 ) do;
         if dot=6 | dot=8
            then line = 3;
            else line = 1;
         function = 'DRAW   ';          output;
      end;
      when (dot = 13) do;
         text = "&sym1";
         function = 'SYMBOL';        output;
      end;
      otherwise;
   end;
   return;
run;

 /*-----------------------------------------------*
  | Annotate data set to draw outside points      |
  *-----------------------------------------------*/
data _out_;
	set _plotdat_;
	drop &var;
	length text function $8;
	where (outside>1);
    &bx.sys = "&bsys";         /* coordinates for 'other' */
%if %upcase(&by) ^= MIDPOINT %then %do; 
    &by.sys = '2';         /* data value coordinates for box axis     */
%end;
%if ( &paxis ^= %str() ) %then %do;
    &bz.sys = '1';         /* data percentage coordinates for 3rd axis*/
    &bz     = 1  ;
%end;
	&bx = &center;
	&by = &var;
	style = '        ';
	function = 'SYMBOL  ';
	%if %length(&hsym) %then %do;
		size = &hsym;
		%end;
	if outside = 2
      then text = "&sym2";
      else text = "&sym3";
   %if %length(&id) %then %do;            /* if id variable,   */
      function = 'LABEL';                  /*  .. then label it */
      text = &ID;
	  position="6";
      output;

 	  %end;
		
data &out;
     set &out _out_ ;
	 run;
proc datasets lib=work memtype=data nolist nowarn;
   delete _quart_ _plotdat_ _box_ _whisk_ _out_;
   run; quit;
%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
 
 /*---------------------------------------------------------*
  | BOXANNO macro - creates annotate dataset for both X & Y |
  *---------------------------------------------------------*/
%macro boxanno(
     data=_last_,           /* Data set to be plotted  */
     xvar=,                 /* Horizontal variable     */
     yvar=,                 /* Vertical variable       */
     out=boxanno            /* Output annotate dataset */
     );
 
%boxaxis(
     data=&data, var=&xvar,
     baxis=x,    oaxis=y,    out=xanno);
 
%boxaxis(
     data=&data, var=&yvar,
     baxis=y,    oaxis=x,    out=yanno);
 /*----------------------------------------*
  |  Concatenate the two annotate datasets |
  *----------------------------------------*/
data &out;
     set xanno yanno;
%mend boxanno;
