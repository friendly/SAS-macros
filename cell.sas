 /*------------------------------------*
  | Macro to cellulate an X-Y data set |
  *------------------------------------*/
%macro CELL( data=_LAST,     /* input data set */
             out=_DATA_,
             x = X, y = Y,   /* input X, Y variables */
             xc=xc, yc=yc,   /* output X, Y variables */
             nx=15, ny=15);  /* number of bins in X & Y directions */
 
 /*-------------------------------------------------*
  | Quantize the X and Y values into discrete cells |
  *-------------------------------------------------*/
proc means data=&data noprint;
   var &x &y;
   output out=_temp_ range=rx ry;
 
data _cell_;
   if _n_=1 then set _temp_;
   set &data;
   drop rx ry;
   deltax = rx / &nx;
   deltay = ry / &nx;
   &xc = deltax * (round( &x/deltax )+.5);
   &yc = deltay * (round( &y/deltay )+.5);
 /*----------------------------------------*
  | Find number of points in each X-Y cell |
  *----------------------------------------*/
proc freq data=_cell_;
   tables &xc * &yc / noprint out=&out ;
%mend CELL;
 /*:
 :*/
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each COUNT |
  *----------------------------------------------------*/
%macro SUNSYM( data=_LAST_,ht=1.5, color=black );
%local alpha ch repeat;
%let alpha=ABCDEFGHIJKLMNOPQRSTUVWXYZ;
 
proc means data=&data noprint;
   var count;
   output out=_temp_ max=maxcount;
data _null_;
   set _temp_;
   maxcount = min( maxcount, 26 );
   call symput('HIGH',put(maxcount,2.));
run;
 
  %let repeat=;
  %do i=1 %to &HIGH %by 1;
     %let ch = &i;
     %if &i = 26 %then %let repeat = r=100;
     symbol&i h=&ht f=sun c=&color v=%substr(&alpha,&ch,1) &repeat ;
  %end;
%mend SUNSYM;
