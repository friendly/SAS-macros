/*-----------------------------------------------------------*
 * CANROTAT.SAS                                              *
 * - (Varimax-)Rotates the raw coefficients resulting from a *
 *   canonical discriminant analysis (proc candisc)          *
 *   using proc factor and computes new rotated discriminant *
 *   scores by means of proc score.                          *
 * - Rotates canonical loadings (structure coefficients) in  *
 *   a seperate step using again proc factor.                *
 * - Plots both rotated discriminant scores and centroids    *
 *   as well as seperately rotated discriminant loadings     *
 *   in one joint discriminant space via a modification      *
 *   of the CANPLOT-MACRO by Michael Friendly (1998)         *
 *-----------------------------------------------------------*/

%macro canrotat(
     data=_last_,  /* name of data set to analyze               */
     class=,       /* name of class variable                    */
     var=,         /* list of classification variables          */
     scale=4,      /* scale factor for variable vectors in plot */
     conf=.99,     /* confidence probability for canonical means*/
     out=_dscore_, /* output data set containing discrim scores */
     anno=_danno_, /* output data set containing annotations    */
     plot=YES,     /* or NO to suppress plot                    */
     rotate=YES,   /* or NO to supress rotation                 */
     haxis=,       /* AXIS statement for horizontal axis        */
     vaxis=,       /* and for vertical axis- use to equate axes */
     legend=,      /* LEGEND statement                          */
     hsym=1.2,     /* height of plot symbols                    */
          canx=can1,
          cany=can2,
     name=CANPLOT, /* name for graphic catalog entry            */
     colors=RED GREEN BLUE BLACK PURPLE YELLOW BROWN ORANGE,
     symbols=+ square star -     plus   :      $     = );

%let plot = %upcase(&PLOT);
%if %length(&var)=0 %then %do;
    %put CANPLOT:  You must specify VAR= variables list ;
    %goto DONE;
    %end;
%if %length(&class)=0 or %length(%scan(&class,2))>0 %then %do;
    %put CANPLOT:  You must specify one CLASS= variable ;
    %goto DONE;
    %end;

proc candisc data=&data
             out=&out /*short*/
             outstat=_dstat_;
   class &class;
   var &var ;

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
 *!!!!!!!!!! BEGIN OF MODIFICATION !!!!!!!!!!*
 *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

%if &rotate=YES %then %do;

DATA _dstat_;
 SET _dstat_;
 IF &class='' THEN DELETE='NO';
 IF DELETE NE 'NO' THEN DELETE;
 DROP DELETE;
RUN;

/*------------------------------*
 | Rotation of raw Coefficients *
 *------------------------------*/

DATA _canraw_ (TYPE='FACTOR');
 SET _dstat_;
 IF _TYPE_ ='RAWSCORE' THEN DELETE='NO';
 IF _TYPE_ ='N' THEN DELETE='NO';
 IF _TYPE_ ='MEAN' THEN DELETE='NO';
 IF _TYPE_='CORR' THEN DELETE='NO';
 IF DELETE NE 'NO' THEN DELETE;
 IF _TYPE_='RAWSCORE' THEN _TYPE_='PATTERN';
RUN;

PROC FACTOR
 DATA=_canraw_
 ROTATE=VARIMAX
 METHOD=PATTERN
 PREROTATE=NONE
 OUTSTAT=_patter_;
RUN;

/*--------------------------------------------*
 | Computation of rotated discriminant scores *
 *--------------------------------------------*/

DATA _patter_;
 SET _patter_;
 IF _TYPE_ = 'PATTERN' THEN _TYPE_='SCORE';
run;

PROC SCORE
 DATA=&data
 SCORE=_patter_
 OUT= &out;
 VAR &VAR;
RUN;

/*--------------------------------------------*
 | Centering of rotated discriminant scores   *
 *--------------------------------------------*/

PROC STANDARD DATA=&out MEAN=0 OUT=&out;
 VAR &canx &cany;
RUN;


/*-----------------------------------*
 | Rotation of discriminant loadings *
 *-----------------------------------*/

DATA _dstat_(type=corr);
 SET _dstat_;
RUN;

PROC FACTOR
 DATA=_dstat_
 ROTATE=VARIMAX
 METHOD=SCORE
 PREROTATE=NONE
 OUTSTAT=_dstat_
 PRINT
;
RUN;

DATA _dstat_;
 set _dstat_;
 if _type_='PATTERN' then _type_='STRUCTUR';
RUN;

%end;

/*----------------------------------------------*
 | Computation of Standard Deviations and Means *
 *----------------------------------------------*/

proc sort data=&out;
   by &class;

proc means data= &out noprint css n;
 output out=_dcss_ css=css1 css2 n=n1 n2 mean=mean1 mean2;
 var &canx &cany;
 by &class;
proc print;

data _dcss_;
 set _dcss_;
drop _TYPE_ _FREQ_;

proc means data=_dcss_ noprint sum;
 output out=_dcss_ sum=sumcss1 sumcss2 sumn1 sumn2;
proc print;

data _dcss_;
 set _dcss_;
 keep PSTD1 PSTD2;
 PSTD1= SUMCSS1/(SUMN1 - _FREQ_);
 PSTD2= SUMCSS2/(SUMN2 - _FREQ_);
proc print;

proc means data= &out mean noprint;
 var &canx &cany;
 output out=_dmean_ mean=mean1 mean2;
proc print;

run;

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
 *!!!!!!!!! END OF MODIFICATION !!!!!!!!!*
 *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

proc sort data=&out;
   by &class;
proc means noprint;
   var &canx &cany;
   by &class;
   output out=_means_ mean=&canx &cany n=n;

data &anno;
   set _means_ end=eof;
   length text color function $8;
   retain xsys '2' ysys '2';
   drop &canx &cany n;
   x = &canx;
   y = &cany;
   color=scan("&colors",_n_);
   /* mark the class mean        */
   text = 'PLUS';
   hsys = '4'; size = 4;
   function = 'SYMBOL';      output;

   /* draw confidence region */
   size = sqrt( cinv(&conf, 2, 0) / n ) ;   * radius ;
   line = 20;
   do a = 0 to 360 by 10;                   * draw a "circle" ;
      ang = a*arcos(-1)/180;                * convert to radians;
      xp=  size * cos(ang);
      yp=  size * sin(ang);
      x = xp+ &canx;                         * translate to means;
      y = yp+ &cany;
      if a=0 then FUNCTION = 'MOVE    ';
             else FUNCTION = 'DRAW    ';
      output;
   end;
   if eof then do;          * save number of groups ;
      call symput('NGP',put(_n_,best5.));
      end;


data _coeff_;
   set _dstat_;
   drop _TYPE_ ;
   if _type_ = 'STRUCTUR';
proc transpose out=_coeff_;
proc print data=_coeff_;


%if &ngp < 3 %then %do;
        %put WARNING:  CANPLOT cannot produce a plot for &NGP groups.;
        %goto done;
        %end;

data _vector_;
   set _coeff_;
   where (&canx^=.);
   length function text $8;
   retain xsys '2' ysys '2' position '6';
   x = 0 ; y = 0;
   function = 'MOVE ' ; output;
   x = &scale * &canx ;
   y = &scale * &cany ;
   function = 'DRAW ' ; output;
   text = _NAME_;
   function = 'LABEL' ; output;

data &anno;
   set &anno _vector_;

%if &plot = YES %then %do;
   %gensym(n=&ngp, h=&hsym, symbols=&symbols, colors=&colors);
   %if %length(&haxis)=0 %then %do;
      axis2 label=(h=&hsym) value=(h=&hsym);
      %let haxis=axis2;
      %end;
   %if %length(&vaxis)=0 %then %do;
      axis1 label=(h=&hsym) value=(h=&hsym);
      %let vaxis=axis1;
      %end;
   %if %length(&legend)=0 %then %do;
      legend1 label=(h=&hsym) value=(h=&hsym);
      %let legend=legend1;
      %end;

   proc gplot data=&out ;
      plot &cany * &canx = &class
           / anno=&anno frame
             vaxis=&vaxis haxis=&haxis legend=&legend
             href=0 lhref=3 vref=0 lvref=3     /* additional option */
             hm=1 vm=1 name="&name"
                                 des="canplot of &data" ;
      run; quit;
   %end;
/*proc datasets lib=work memtype=data nolist;
   delete _coeff_ _dstat_ _means_ _vector_;*/
%done:
%mend;

 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
%macro gensym(n=1, h=1.5, i=none,
              symbols=%str(- + : $ = X _ Y),
              colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE);
    %*-- note: only 8 symbols & colors are defined;
    %*--  if more than 8 groups symbols and colors are recycled;
  %local chr col k;
  %do k=1 %to &n ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0
              %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0
              %then %let colors = &colors &colors;
     %let chr =%scan(&symbols, &k,%str( ));
     %let col =%scan(&colors, &k, %str( ));
     symbol&k h=&h v=&chr c=&col i=&i;
  %end;
%mend gensym;


 /*----------------------------------------------------*
  | Example 1: Information on Resort Visits            |
  | (Malhotra (1996, p. 622))                          |
  *----------------------------------------------------*/

 options ls=90 ps=50 pageno=1 nodate;

 title "Information on Resort Visits (Malhotra (1996))";

 data resort;
  label  visit='Resort Visit'
         amount='Amount spent on family vacation'
         income='Annual Family Income'
         travel='Attitude towards travel'
         vacation='Importance Attached to Family Vacation'
         hsize='Household Size'
         age='Age of Head of Household';
  input visit $ amount $ income travel vacation hsize age;
  cards;
  1 M 50.2 5 8 3 43
  1 H 70.3 6 7 4 61
  1 H 62.9 7 5 6 52
  1 L 48.5 7 5 5 36
  1 H 52.7 6 6 4 55
  1 H 75.0 8 7 5 68
  1 M 46.2 5 3 3 62
  1 M 57.0 2 4 6 51
  1 H 64.1 7 5 4 57
  1 H 68.1 7 6 5 45
  1 H 73.4 6 7 5 44
  1 H 71.9 5 8 4 64
  1 M 56.2 1 8 6 54
  1 H 49.3 4 2 3 56
  1 H 62.0 5 6 2 58
  2 L 32.1 5 4 3 58
  2 L 36.2 4 3 2 55
  2 M 43.2 2 5 2 57
  2 M 50.4 5 2 4 37
  2 M 44.1 6 6 3 42
  2 L 38.3 6 6 2 45
  2 M 55.0 1 2 2 57
  2 L 46.1 3 5 3 51
  2 L 35.0 6 4 5 64
  2 L 37.3 2 7 4 54
  2 M 41.8 5 1 3 56
  2 M 57.0 8 3 2 36
  2 L 33.4 6 8 2 50
  2 L 37.5 3 2 3 48
  2 L 41.3 3 3 2 42
  ;
 run;


goptions ftext=zapf htitle=0.5cm htext=0.3cm;

axis order=(-4 to 4 by 1) length=7.5cm;

title2"Unrotated Discriminant Space";

%canrotat(
 data=resort,
 class=amount,
 var=income travel vacation hsize age,
 rotate=NO,
 haxis=axis, vaxis=axis,
 hsym=0.3cm
 );
run;


title2"Rotated Discriminant Space";

%canrotat(
 data=resort,
 class=amount,
 var=income travel vacation hsize age,
 haxis=axis, vaxis=axis,
 hsym=0.3cm
 );
run;

goptions reset=all;


 /*----------------------------------------------------*
  | Example 2: Business-to-Business Segmentation       |
  | (Hair et al. (1995, pp. 742-743))                  |
  *----------------------------------------------------*/

 options ls=90 ps=50 pageno=1 nodate;

 title" Business-to-Business Segmentation (Hair et al. (1995))";

 data hatco;
 input ID $ SAMPLE $ x1 x2 x3 x7 x14 $;
 label x1 = 'Delivery Speed'
       x2 = 'Price Level'
       x3 = 'Price Flexibility'
       x7 = 'Product Quality'
       x14= 'Type of Buying Situation';
 cards;
 01      A       4.1    0.6     6.9     5.2     1
 02      A       1.8    3.0     6.3     8.4     1
 05      A       6.0    0.9     9.6     4.5     3
 06      A       1.9    3.3     7.9     9.7     2
 07      A       4.6    2.4     9.5     7.6     1
 08      A       1.3    4.2     6.2     6.9     2
 11      A       2.4    1.6     8.8     5.8     1
 12      A       3.9    2.2     9.1     8.3     2
 13      A       2.8    1.4     8.1     6.6     1
 14      A       3.7    1.5     8.6     6.7     1
 15      A       4.7    1.3     9.9     6.8     3
 17      A       3.2    4.1     5.7     6.2     2
 20      A       4.7    1.3     9.9     6.8     3
 23      A       3.0    4.0     9.1     8.4     3
 24      A       2.4    1.5     6.7     7.2     1
 25      A       5.1    1.4     8.7     3.8     2
 26      A       4.6    2.1     7.9     4.7     3
 28      A       5.2    1.3     9.7     6.7     3
 29      A       3.5    2.8     9.9     5.4     3
 31      A       3.0    3.2     6.0     8.0     1
 32      A       2.8    3.8     8.9     8.2     3
 33      A       5.2    2.0     9.3     4.6     3
 36      A       1.8    3.3     7.5     7.6     1
 39      A       0.0    2.1     6.9     8.9     1
 42      A       5.9    0.9     9.6     4.5     3
 43      A       4.9    2.3     9.3     6.2     3
 45      A       2.0    2.6     6.5     8.5     1
 47      A       3.1    1.9    10.0     3.8     3
 48      A       3.4    3.9     5.6     9.1     2
 49      A       5.8    0.2     8.8     6.7     3
 50      A       5.4    2.1     8.0     5.2     3
 51      A       3.7    0.7     8.2     5.2     2
 52      A       2.6    4.8     8.2     9.0     2
 53      A       4.5    4.1     6.3     8.8     2
 54      A       2.8    2.4     6.7     9.2     1
 58      A       5.4    2.5     9.6     7.7     3
 59      A       4.3    1.8     7.6     4.4     3
 61      A       3.1    1.9     9.9     3.8     3
 65      A       1.1    2.0     7.2    10.0     1
 67      A       4.2    2.5     9.2     7.3     3
 68      A       1.6    4.5     6.4     7.1     2
 70      A       2.3    3.7     8.3     9.1     2
 71      A       3.6    5.4     5.9     8.4     2
 72      A       5.6    2.2     8.2     5.3     3
 73      A       3.6    2.2     9.9     4.9     3
 79      A       1.0    1.9     7.1     9.9     1
 80      A       4.5    1.6     8.7     6.8     3
 81      A       5.5    1.8     8.7     4.9     3
 82      A       3.4    4.6     5.5     6.3     2
 84      A       2.3    3.7     7.6     7.4     1
 86      A       2.5    3.1     7.0     9.0     1
 88      A       2.1    3.5     7.4     7.2     1
 89      A       2.9    1.2     7.3     8.0     1
 90      A       4.3    2.5     9.3     7.4     3
 92      A       4.8    1.7     7.6     5.8     2
 93      A       3.1    4.2     5.1     5.9     2
 95      A       4.0    0.5     6.7     5.0     1
 96      A       0.6    1.6     6.4     8.4     1
 97      A       6.1    0.5     9.2     7.1     3
 99      A       3.1    2.2     6.7     8.4     1
;
run;

goptions ftext=zapf htitle=0.5cm htext=0.3cm;

axis order=(-4 to 4 by 1) length=7.5cm;

title2"Unrotated Discriminant Space";

%canrotat(
 data=hatco,
 class=x14,
 var=x1 x2 x3 x7,
 rotate=NO,
 haxis=axis, vaxis=axis,
 hsym=0.3cm
 );
run;


title2"Rotated Discriminant Space";

%canrotat(
 data=hatco,
 class=x14,
 var=x1 x2 x3 x7,
 haxis=axis, vaxis=axis,
 hsym=0.3cm
 );
run;

goptions reset=all;




 /*----------------------------------------------------*

   Literature:

   Friendly, M. (1998):
        http://www.math.yorku.ca/SCS/sasmac/canplot.html

   Malhotra, N. K. (1996):
        Marketing Research, 2nd Ed., Englewood Cliffs,
        Prentice Hall.


   Hair, J. F.; R. E. Anderson; R. L. Tatham; W. C. Black (1995):
        Multivariate Data Analysis, 4th Ed., New Jearsy,
        Prentice Hall.

   Perreault, Jr., W. D.; D. N. Behrman, G. M. Armstrong (1979):
        Alternative Approaches for Interpretation of Multiple
        Discriminant Analysis in Marketing Research, Journal
        of Business Research, Vol. 7, 151 - 173.

   Watson, C. J. (1981):
        An Additional Approach for Interpretation of Multiple
        Discriminant Analysis in Business Research, Journal
        of Business Research, Vol. 9, 1 - 11.

  *----------------------------------------------------*/
