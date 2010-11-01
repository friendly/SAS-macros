 /*-------------------------------------------------------------------*
  *    Name: boxplot.sas                                              *
  *   Title: SAS/Graph Box and Whisker plot                           *
        Doc: http://www.datavis.ca/sasmac/boxplot.html            
  *                                                                   *
  * This SAS macro constructs and plots side-by-side Box and whisker  *
  * plots for ONE quantitative variable classified by ONE OR MORE     *
  * grouping (CLASS) variables. The CLASS variables may be character  *
  * or numeric.                                                       *
  *                                                                   *
  * The box for each group shows the location of the median, mean and *
  * quartiles. Whisker lines extend to the most extreme observations  *
  * which are no more than 1.5*IQR beyond the quartiles. Observations *
  * beyond the whiskers are plotted individually. [The 1.5 factor can *
  * be changed (OFACTOR=)]                                            *
  *                                                                   *
  * Optional NOTCHES are drawn to show approximate 95% confidence     *
  * intervals for each group median. Other options are provided to    *
  * connect group medians, draw box widths proportional to sample     *
  * size, and allow formatted labels for both variables.              *
  *                                                                   *
  * References:                                                       *
  *   Olmstead, A. "Box Plots using SAS/Graph Software", SAS SUGI,    *
  *     1985, 888-894.                                                *
  *   McGill, R., Tukey, J.W., & Larsen, W. "Variations of Box Plots",*
  *     American Statistician, 1978, 32, 12-16.                       *
  *-------------------------------------------------------------------*
  * Author:   Michael Friendly            <friendly@yorku.ca>         *
  * Created:  12 Apr 1988 10:19:15                                    *
  * Revised:  09 Jan 2005 13:30:49                                    *
  * Version:  2.1                                                     *
  *  - Added ability to fill the box, assign line style, fill notches *
  *    and select vertical or horizontal boxes.                       *
  *  - Added WHERE= parameter                                         *
  *  - added PRINT= parameter                                         *
  *  - added OFACTOR= parameter to control printing of outside obs.   *
  *  - added SORTBY= to arrange the classes by the order of a variable*
  *  - fixed error setting X/Yorder                                   *
  *  - allow multiple CFILL= colors, one for each class               *
  *  - added VAXIS= and HAXIS= for more precise control of axes       *
  *  - Added SBOX= line thickness                                     *
  *  - handle special missing values (.A-.Z)                          *
  *  - fixed GOUT=                                                    *
  *  - fixed problem with OFFSET calculation when ID=%str()           *
  * 2.0 Fixed problems with lowercase var names for V7+               *
  *  - fixed some local macro variables                               *
  *  - cleaned up temporary data sets                                 *
  * 2.1                                                               *
  *  - Added OLABEL=ALL|FAR to control which observations are labeled *
  *  - Added IDSYMBOL=, SYMHEIGHT= and IDHEIGHT=                      *
  * 2.2
  *   - Provide quit; for proc datasets                               *
  *   - Try to fix design flaw calculating YANGLE                     *
  *   - Added inline documentation
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *-------------------------------------------------------------------*/

 /*=
=Description:
 
 The BOXPLOT macro draws side-by-side boxplots for the groups
defined by one or more grouping (CLASS) variables in a data set.
The boxplots may be formatted horizontally or vertically,
they may be shown with "notches", indicating approximate
95% confidence intervals for difference in medians,
and the groups may be ordered in a variety of ways.

=Usage:

 The BOXPLOT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%boxplot(data=auto, var=mpg, class=origin);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* WHERE=      WHERE clause to subset the data

* CLASS=      Grouping variable(s).  The CLASS= variables
              may be character or numeric.  If the CLASS= variable
			  is a character variable, or there is more than one
			  CLASS= variable, the macro automatically constructs
			  a SAS format to label the groups

* VAR=        The name of the response variable to be plotted.
			  The VAR= variable is plotted on the ordinate when ORIENT=V and on
			  the abscissa when ORIENT=H.

* ID=         A character variable to identify each observation.  
              If an ID= variable is specified,
              outside variables are labelled on the graph,
              using the first 8 characters of the value of
              the ID variable (to reduce overplotting).
              Otherwise, outside points are not labelled.

* SORTBY=     Specifies a variable or statistic keyword used to
			  order the levels of the CLASS= variables along the class axis.
			  If SORTBY= is not specified, the classes are ordered along the
			  axis in sorted order (using the format specified by CLASSFMT=
			  if that has been used).
			  Otherwise, you can specify the name of a variable in the dataset,
			  or one of the statistics,
			  _MEAN_, _MEDIAN_, N (no underscores), _Q1_, or _Q3_
			  and the classes will be ordered on the axis according to the
			  values of that statistic for each group.

* OFACTOR=    IQR multiplier for outside values [Default: OFACTOR=1.5]

* OLABEL=     Outliers to label: Either ALL or FAR. [Default: OLABEL=ALL]

* IDSYMBOL=   Symbols used for outside, and far-out observations.
              [Default: IDSYMBOL=DIAMOND SQUARE]

* MEANSYMBOL=  Symbol to use for the mean of each group. [Default: MEANSYMBOL=DOT]

* SYMHEIGHT=  Symbol height for outside, and far-out [Default: SYMHEIGHT=2 2.5]

* IDHEIGHT=   Text height for outside, and far-out [Default: IDHEIGHT=1]

* WIDTH=      Box width as proportion of the maximum.
              The default, WIDTH=0.5, means that the maximum
              box width is half the spacing between boxes.
             [Default: WIDTH=0.5]

* NOTCH=      Specifies whether or not to draw notched
              boxes.  1=draw notched boxes; 0=do not.  See also the
			  CNOTCH= parameter for a different style of notched
			  boxplots.

* CBOX=       Color of the box outline [Default: CBOX=BLACK]

* CFILL=      Color for filling the boxes.

* CNOTCH=     Color for notch fill.  If specified, the area
			  between the upper/lower notches is filled using a solid pattern and the
			  CNOTCH color.  This is often an alternative to drawing explicit
			  notches.

* LBOX=       Line style for the box outline [Default: LBOX=1]

* SBOX=       Line thickness for the box outline [Default: SBOX=1]

* ORIENT=     Box orientation: V gives vertical boxes, H gives
			  horizontal boxes.  If the labels for the CLASS= variables are longish,
			  horizontal boxes are preferred, since the labels will appear on the
			  Y axis. [Default: ORIENT=V]

* CONNECT=    Specifies the line style used to connect
              medians of adjacent groups.  If CONNECT=0, the
              medians of adjacent groups are not to be
              connected.

* F=          For a notched boxplot, the parameter F
              determines the notch depth, from the center of
              the box as a fraction of the halfwidth of each
              box.  F must be between 0 and 1; the larger the
              value, the less deep is the notch. [Default: F=0.5]

* FN=         Box width proportionality factor.  The
              default, FN=1 means all boxes are the same
              width.  If you specify FN=sqrt(n), the boxes
              width will be proportional to the square root
              of the sample size of each group.  Other
              functions of  n  are possible as well.
              [Default: FN=1]

* VARFMT=     The name of a format for the VAR= analysis variable.

* CLASSFMT=   If the CLASS variable is a character variable, or
			  there are two or more CLASS variables, the program maps the sorted
			  values of the class variable(s) into the integers 1, 2, ... 
			  levels, where  levels is the number of distinct values
			  of the class variable(s).  A format provided for CLASSFMT should
			  therefore provide labels corresponding to the numbers 1, 2, ...
			  to the number of levels.

* VARLAB=     Label for analysis variable. If not
              specifed, the analysis axis is labelled with the
              variable name.

* CLASSLAB=   Label for class variable(s) used to label the axis.

* VAXIS=      Axis statement for vertical axis.  If specified, some of the
              axis parameters below are ignored.

* HAXIS=      Axis statement for horizontal axis  If specified, some of the
              axis parameters below are ignored.

* YORDER=     Tick marks and range for the ordinate, in the
              form YORDER = low TO  high BY tick.

* XORDER=     Tick marks and range for the horizontal axis, in the
			  form XORDER = low TO high BY tick.  With ORIENT=V,
			  the horizontal axis pertains to the CLASS= variable(s);
			  with ORIENT=H, the horizontal axis pertains to the VAR=
			  analysis variable.  
			  
			  When there are very few (3 or less)
			  levels of the CLASS= variable, SAS/GRAPH often has
			  problems dealing with the annotation information and
			  labels.  Setting XORDER= (or YORDER=, with ORIENT=H) to
			  include an extra value at both ends of the range usually
			  helps cure this problem.  For example, if there are only
			  two groups, with class values 1 and 2, set XORDER=0 to
			  3 by 1.

* ANNO=       The name of an (optional) additional
              ANNOTATE data set to be used in drawing the
              plot.  This requires some knowledge of the
			  contents of the ANNOTATE data set, which you can
			  see by specifying PRINT=ANNO in a previous call.

* PRINT=      What printed output?  You may specify any one or more of
	          ANNO, OUTSIDE and STATS. [Default: PRINT=OUTSIDE]

* OUT=        The name of the output data set containing
              statistics used in drawing the boxplot.  There
              is one observation for each group.  The
              variables are N, _MEAN_, _MEDIAN_, _Q1_, _Q3_, _IQR_,
              LO_NOTCH, HI_NOTCH, LO_WHISK, HI_WHISK [Default: OUT=BOXSTAT]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=BOXPLOT]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]
                

 =*/

                           /* Description of Parameters:         */
%macro BOXPLOT(            /* -------------------------          */
   data=_LAST_,            /* Input dataset                      */
   where=,                 /* WHERE clause to subset data        */
   class=,                 /* Grouping variable(s)               */
   var=,                   /* Analysis variable                  */
   id=,                    /* Observation ID variable            */
   sortby=,                /* Order classes by var, _mean_,...   */
   ofactor=1.5,            /* IQR multiplier for outside values  */
   olabel=ALL,             /* Outliers to label: ALL | FAR       */
   idsymbol=diamond square,  /* Symbols for out, far-out          */
   meansymbol=dot,         /* Symbol to use for the mean         */
   symheight=2 2.5,        /* Symbol height for out, far-out     */
   idheight=1,             /* Text height for out, far-out       */
   width=.5,               /* Box width as proportion of maximum */
   notch=0,                /* =0|1, 1=draw notched boxes         */
   cbox=BLACK,             /* color of box outline               */
   cfill=,                 /* color for box fill                 */
   cnotch=,                /* color for notch fill               */
   lbox=1,                 /* line style for box outline         */
   sbox=1,                 /* line thickness for box outline     */
   orient=V,               /* box orientation: H or V            */
   connect=0,              /* =0 or line style to connect medians*/
   f=0.5,                  /* Notch depth, fraction of halfwidth */
   fn=1,                   /* Box width proportional to &FN      */
   varfmt=,                /* Format for analysis variable       */
   classfmt=,              /* Format for class variable(s)       */
   varlab=,                /* Label for analysis variable        */
   classlab=,              /* Label for class variable(s)        */
   vaxis=,                 /* Axis statement for vertical axis   */
   haxis=,                 /* Axis statement for horizontal axis */
   yorder=,                /* Tick marks, range for ordinate     */
   xorder=,                /* Tick marks, range for abscissa     */
   anno=,                  /* Addition to ANNOTATE set           */
   print=outside,          /* What printed output?               */
   out=boxstat,            /* Output data set: quartiles, etc.   */
   name=BOXPLOT,           /* Name for graphic catalog entry     */
   gout=
   );
 
    %*-- Reset required global options;
    %if &sysver >= 7 %then %do;
        %local o1 o2;
        %let o1 = %sysfunc(getoption(notes));
        %let o2 = %sysfunc(getoption(validvarname,keyword));
        options nonotes validvarname=upcase;
        %end;
    %else %do;
       options nonotes;
        %end;

%local x y i p1 p2 ox abort;
%let abort=0;
%if %length(&var)=0 | %length(&class)=0
   %then %do;
      %put ERROR: The VAR= and CLASS= parameters must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%if &gout^=%str()  %then %let gout=GOUT=&gout;
goptions reset=symbol;  *-- cancel prior SYMBOL stmts;

%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let _dsn_ = %upcase(&data);
%if &classlab = %str() %then %let classlab = &class;
%let class = %upcase(&class);
%let print = %upcase(&print);
%let orient= %upcase(&orient);
%let olabel= %upcase(&olabel);

%if &orient = V %then %do;
   %let x = X;
   %let y = Y;
    %let p1 = 4; %let p2=6;    %*-- outlier annotate positions;
    %let ox =.05;              %*-- outlier label offset;
   %end;
%else %do;
   %let x = Y;
   %let y = X;
    %let p1 = 8; %let p2=2;    %*-- outlier annotate positions;
    %let ox =.05;              %*-- outlier label offset;
   %end;

proc sort data=&DATA out=_sorted_;
    by &class;
     %if %length(&where)>0 %then %do;
     where (&where);
     %end;
run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%let clvars = %nvar(&class);
%let data = _sorted_;
 
 /*----------------------------------------*
  | Determine if &CLASS is char or numeric |
  *----------------------------------------*/
%let cltype=;
%let yangle=a=90 r=0;
proc contents data=_sorted_ out=_work_ noprint;
data _null_;
     length label2 $40;
     set _work_;
     if upcase(name)=upcase("&CLASS")
        then if type=1 then call symput('CLTYPE', 'NUM');
                       else call symput('CLTYPE', 'CHAR');
 
     *-- find length of variable label and set y label angle --;
     %if &orient=V %then %do;
        %if %length(&varlab) > 0 %then
           %str( label2 = "&varlab"; );
        %else
           %str( if upcase(name)="&VAR" then label2=label; );
     %end;
     %else %do;
        %if %length(&classlab) > 0 %then
           %str( label2 = "&classlab"; );
        %else
           %str( if name="&CLASS" then label2=label; );
     %end;
    /* if length(label2) >6 
        then */ call symput('YANGLE','a=90 r=0');
run;              /* Run required here */
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%let needfmt=0; 
 /*----------------------------------------------------------------*
  |  If there are more than one class variables or class variable  |
  |  is CHAR, create a numeric class variable, XCLASS. XCLASS      |
  |  numbers the groups from 1,...number-of-groups. The macro      |
  |  creates a format to associate proper group labels with the    |
  |  XCLASS value unless CLASSFMT= is supplied.                    |
  *----------------------------------------------------------------*/
%if ( &cltype=CHAR or &clvars > 1 ) %then %do;
    %let needfmt=1;
   %let lclass = %scan( &CLASS, &clvars );
   data _sorted_;
      set _sorted_;
      by &class;
      if (first.&lclass) then xclass + 1;
      %if &cltype=CHAR and &clvars=1 and &classfmt=%str() %then
         %do;
         %end;
 
   run;
   %let KLASS = xclass;
   run;
%end;
%else %let KLASS = &CLASS;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

 /*------------------------------------------------*
  | Determine number of groups & quartiles of each |
  *------------------------------------------------*/
proc means noprint data=_sorted_;
    var &KLASS;
    output out=_grsum_ min=grmin max=grmax ;
    run;
proc univariate data=_sorted_ noprint;
    by &KLASS;
    var &VAR;
    output out=_qtile_
           n=n q1=_q1_  q3=_q3_  median=_median_ qrange=_iqr_ mean=_mean_;
data _qtile_;
    set _qtile_ nobs=grps;
    By  &KLASS;
    Lo_Notch = _median_ - 1.58*_iqr_ / sqrt(N);
    Hi_Notch = _median_ + 1.58*_iqr_ / sqrt(N);
     if _n_=1 then call symput ('GRPS',   left(put(grps, 3.)) );
run;

%if &grps < 3 %then
  %put WARNING: The BOXPLOT macro does not handle the case of a CLASS= variable with one level;

data _sorted_;    * was merged;
    merge _sorted_ _qtile_;
    by &KLASS;

 /*-----------------------------------------------------*
  |  If sorting, do it now and redefine XCLASS          |
  *-----------------------------------------------------*/
%if %length(&sortby)>0 %then %do;
    %let needfmt=1;
    %let svars = %nvar(&sortby);
    proc sort data=_sorted_;
        by &sortby;
   %let lsort = %scan( &sortby, &svars, %str( ) );
   data _sorted_;
      set _sorted_;
      by &sortby;
      if (first.&lsort) then sclass + 1;
   run;
   %let KLASS = sclass;
   run;

    *-- now, must recreate _qtile_, with new sclass variable;
    data _qtile_;
        set _sorted_(keep= &KLASS n _mean_ _q1_ _q3_ _median_ _iqr_ lo_notch hi_notch &sortby);
        by &sortby;
      if (first.&lsort) then output;
    
    *proc print;
%end;
 
%if &needfmt %then %do;
    data _sorted_;
        set _sorted_;
        by &KLASS;
        if first.&KLASS then do;
            %let cval = %scan(&class,1,%str( ));
            %do i=2 %to &clvars;
                %let cval = &cval || %scan(&class,&i,%str( ));
                %end;
            %if &clvars>1 %then %put Class variables-- cval=&cval;
            call symput('val'||left(put( &klass, 2. )), trim(&cval) );
            end;
    run;    
*proc print;
    %makefmt(&grps, _cfmt_);

    %let &x.order=1 to &grps by 1;
%end;
%put orient: &orient xorder: &xorder yorder: &yorder yangle: &yangle;

 /*-----------------------------------------------*
  | Find outside & farout points                  |
  *-----------------------------------------------*/
data plotdat;
    set _sorted_;
    keep &KLASS &VAR &ID outside &sortby;
     %if &needfmt %then %do;
        format &KLASS _cfmt_.;
        %end;
    if &var > .Z;
    outside=1;
    if &var < (_q1_   -&ofactor*_iqr_) or &var > (_q3_ +&ofactor*_iqr_)
       then outside=2;
    if &var < (_q1_ -2*&ofactor*_iqr_) or &var > (_q3_ +2*&ofactor*_iqr_)
       then outside=3;
    run;

data _out_;
   set plotdat;
   if outside > 1 ;
proc sort data=_out_;
   by &KLASS &VAR ;

%if %index(&print,OUTSIDE)>0 %then %do;
proc print data=_out_;
   id &ID &KLASS;
   title3 "Outside Observations in Data Set &_DSN_ ";
    run;
%end;

 /*-----------------------------------------------------*
  |  If connnecting group medians, find them and append |
  *-----------------------------------------------------*/
%if ( &connect ) %then %do;
   data connect;
      set _qtile_(keep=&KLASS _median_
                    rename=(_median_=&VAR));
      outside=0;
   proc append base=plotdat
               data=connect;
   run;
%end;
 
 /*----------------------------------------------------*
  |  Whiskers go from quartiles to most extreme values |
  |  which are *NOT* outside.                          |
  *----------------------------------------------------*/
data _in_;
   set plotdat;
   if outside = 1;            /* select inside points */

proc univariate data=_in_ noprint;
   by &KLASS;
   var &VAR;                  /* find min and max     */
   output out=_whisk_ min=lo_whisk max=hi_whisk;
run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

data &out;
    merge _qtile_ _whisk_ end=lastobs;
    by &KLASS;
    retain halfmax 1e23 fnmax -1e23;
    drop span halfmax fnmax offset grps;
 
    span = dif ( &KLASS );            /* x(k+1) - x(k) */
    if (_n_ > 1 )
       then halfmax = min( halfmax, span/2);
    fnmax = max( fnmax, &FN );
 
   if ( lastobs ) then do;
      if _n_=1 then halfmax=.5;
      call symput ('HALFMAX', left(put(halfmax,best.)) );
      put ' Maximum possible halfwidth is: ' halfmax /;
      call symput ('FNMAX',  left(put(fnmax,best.)) );
      grps=_n_;
        call symput ('GRPS',   left(put(grps, 3.)) );
        %if &orient=V %then %do;
            offset=max(5, 35-5*grps); 
            %if &ID = %str() %then
                %str(offset=max(10,offset););
            %end;
        %else %str(offset=2;);
      
      call symput('OFFSET',left(put(offset,2.)) );
      put ' Number of groups: ' grps  'offset=' offset ;
   end;
    run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
 
%if %index(&print,STATS)>0 %then %do;
proc print ;
   id &KLASS;
   title3 'BOXPLOT: Quartiles, notches and whisker values';
run;
%end;

 /*-----------------------------------------------*
  | Annotate data set to draw boxes & whiskers    |
  *-----------------------------------------------*/
%if %length(&cbox)=0 %then %let cbox = BLACK;
%let wbl = .9;   %*-- length of whisker bar line;
data _dots_;
    set &out;
     by &KLASS;
    retain halfmax &HALFMAX  k ;
    drop k halfmax halfwid hi_notch lo_notch _iqr_ _median_ _mean_ _q1_  _q3_ ;
    drop grmin grmax nwidth sy  _type_ _freq_;
    if ( _n_ = 1) then do;
       set _grsum_;
       K = &WIDTH * HalfMax;
    end;
    halfwid = K * &FN / &FNMax ;
    length function text color style $8 comment $15;
    XSYS = '2'; YSYS = '2';
 
   /*    Produce connect-the-dots X, Y pairs */
   nwidth = (1-&NOTCH*&F)*halfwid;
%if %length( &cnotch )>0 %then %do;
   &x = &KLASS - .8*nwidth       ; &y= Lo_Notch ; dot = 24; link out;
   &x = &KLASS + .8*nwidth       ; &y= Hi_Notch ; dot = 25; link out;
%end;

   &x = &KLASS                   ; &y= Lo_Whisk ; dot =  1; link out;
   &x = &KLASS                   ; &y= _q1_     ; dot =  2; link out;
   &x = &KLASS - halfwid         ; &y= _q1_     ; dot =  3; link out;
 
%if ( &notch ) %then %do;
   &x = &KLASS - halfwid         ; &y= Lo_Notch ; dot =  4; link out;
   &x = &KLASS - (1-&F)*halfwid  ; &y= _median_ ; dot =  5; link out;
   &x = &KLASS - halfwid         ; &y= Hi_Notch ; dot =  6; link out;
%end;
   &x = &KLASS - halfwid         ; &y= _q3_     ; dot =  7; link out;
   &x = &KLASS                   ; &y= _q3_     ; dot =  8; link out;
   &x = &KLASS                   ; &y= Hi_Whisk ; dot =  9; link out;
   &x = &KLASS                   ; &y= _q3_     ; dot = 10; link out;
   &x = &KLASS + halfwid         ; &y= _q3_     ; dot = 11; link out;
%if ( &notch ) %then %do;
   &x = &KLASS + halfwid         ; &y= Hi_Notch ; dot = 12; link out;
%end;
   &x = &KLASS + nwidth          ; &y= _median_ ; dot = 13; link out;
   &x = &KLASS - nwidth          ; &y= _median_ ; dot = 14; link out;
   &x = &KLASS + nwidth          ; &y= _median_ ; dot = 15; link out;
%if ( &notch ) %then %do;
   &x = &KLASS + halfwid         ; &y= Lo_Notch ; dot = 16; link out;
%end;
   &x = &KLASS + halfwid         ; &y= _q1_     ; dot = 17; link out;
   &x = &KLASS                   ; &y= _q1_     ; dot = 18; link out;
 
   &x = &KLASS - &wbl*halfwid    ; &y= Lo_Whisk ; dot = 19; link out;
   &x = &KLASS - &wbl*halfwid    ; &y= Hi_Whisk ; dot = 19; link out;
   &x = &KLASS                   ; &y= _mean_   ; dot = 20; link out;

   return;
 
out:
    Select;
       when ( dot=1 ) do;       comment='Low whisker';
          function = 'MOVE';                   output;
          %if %length(&cfill)>0 %then %do;
             if scan("&cfill",&KLASS)^=' '
                then color = trim(scan("&cfill",&KLASS));
                else color = "&cfill";
            *put &KLASS= color=;
          style = 'SOLID';
          %end;
          line = &lbox;         comment='Start box';
             size = &sbox;
          function = 'POLY';                   output;
          End;
       when ( 1< dot <=18) do;  comment='Box outline';
          color = "&cbox";
          function = 'POLYCONT';               output;
          End;
       when ( dot=19) do;       comment='Whisker ends';
          function = 'MOVE';                   output;
          function = 'PUSH';                   output;
          &x = &x + 2*&wbl*halfwid ;
          function = 'DRAW';                   output;
          &y.SYS = '7';
             sy = &y;
          &y = - sign(&y-_median_);
          function = 'DRAW';                   output;
          function = 'POP ';                   output;

          &x = &KLASS - &wbl* halfwid;
          &y = - sign(sy-_median_);
          function = 'DRAW';                   output;

          &y.SYS = '2';
          End;
       when ( dot=20) do;       comment='mean';
          function = 'MOVE';                   output;
          function = 'SYMBOL'; style='       ';
          TEXT="&meansymbol"; size=1.7;               output;
          End;
       when ( dot=24) do;
          function = 'MOVE';                   output;
          End;
       when ( dot=25) do;       comment='Notch fill';
          line=3; style='SOLID';
          color = "&cnotch";
          function = 'BAR ';                   output;
          style = ' ';
          End;
          Otherwise ;
    End;
    Return;
run;
 
 /*-----------------------------------------------------*
  |  Annotate data set to plot and label outside points |
  *-----------------------------------------------------*/
%let s1=%scan(&idsymbol &idsymbol,1);
%let s2=%scan(&idsymbol &idsymbol,2);
%let h1=%scan(&symheight &symheight,1,%str( ));
%let h2=%scan(&symheight &symheight,2,%str( ));

data _label_;
   set _out_;                          /* contains outliers only */
   by &KLASS;
   keep xsys ysys x y function text style position size;
   length text $12 function style $8;
   xsys = '2'; ysys = '2';
   &y = &VAR;
   &x = &KLASS ;
   function = 'SYMBOL';                     /* draw the point   */
   style = ' ';
   position = ' ';
   if outside=2
      then do;  text="&s1"; size=&h1; end;
      else do;  text="&s2"; size=&h2; end;
   output;
   if &olabel=FAR & outside<3 then return;
   %if %length(&id) %then %do;            /* if id variable,   */
      if first.&KLASS then out=0;
      out+1;
      function = 'LABEL';                  /*  .. then label it */
      text = &ID;
      size=&idheight;
*     style='SIMPLEX';
      &x = &KLASS;
      if mod(out,2)=1                      /* on alternating sides*/
         then do; &x=&x -&ox; position="&p1";  end;
         else do; &x=&x +&ox; position="&p2";  end;
      output;
   %end;
data _dots_;
   set _label_ _dots_ &anno ;
%if %index(&print,ANNO)>0 %then %do;
proc print data=_dots_;
   title3 'BOXPLOT: Annotate Data Set';
run;
%end;

options notes;
 /*--------------------------------------*
  | Symbols for connecting group medians |
  *--------------------------------------*/
%if &connect ^= 0  %then %do;
  symbol1 C=BLACK V=NONE I=JOIN L=&connect r=1; /* connected medians   */
  symbol2 C=BLACK V=NONE R=3;                 /* rest done by annotate */
%end;
%else %do;
  symbol1 C=BLACK V=NONE R=3 i=none;          /* all done by annotate  */
%end;
title3;

proc gplot data=plotdat &GOUT ;
   %if &orient = V %then %do;
        %if %length(&vaxis)=0 %then %let vaxis=axis98;
        %if %length(&haxis)=0 %then %let haxis=axis99;
      plot &VAR * &KLASS = outside /
            vaxis=&vaxis haxis=&haxis hm=0
      %end;
   %else %do;
        %if %length(&vaxis)=0 %then %let vaxis=axis99;
        %if %length(&haxis)=0 %then %let haxis=axis98;
      plot &KLASS * &VAR = outside /
            vaxis=&vaxis haxis=&haxis vm=0
      %end;
         frame nolegend name="&name"
            des="Boxplot of &var in &_dsn_" 
         annotate=_dots_;

  axis98 minor=(number=1)
  %if %length(&yorder) > 0 %then
       order=(&yorder);
       label =(&yangle);

  axis99
  %if %length(&xorder) > 0 %then
       order=(&xorder);
        /* value=(h=1.2) label =(h=1.5) */ offset=(&offset pct);

  %if &varfmt ^=  %str() %then %do; format &var   &varfmt ;      %end;
  %if &classfmt^= %str() %then %do; format &KLASS &classfmt ;    %end;
  %if %length(&varlab)>0 %then %do; label  &var   = "&varlab";   %end;
  %if %length(&classlab)>0 %then %do; label  &KLASS = "&classlab"; %end;
  run; quit;
goptions reset=symbol;  *-- cancel prior SYMBOL stmts;
options nonotes;

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist library=work memtype=(data);
    delete _work_ _grsum_ /*merged*/ _in_ _whisk_ _qtile_ _label_ _sorted_ _out_ _dots_ plotdat;
     run; quit;

%done:
%if &abort %then %put ERROR: The BOXPLOT macro ended abnormally.;


    %*-- Restore global options;
    %if &sysver >= 7 %then %do;
        options &o1 &o2;
        %end;
    %else %do;
       options notes;
        %end;

%mend boxplot;
 
 /*----------------------------------*
  | Count number of &CLASS variables |
  *----------------------------------*/
%macro nvar(varlist);
   %local wvar result;
   %let result = 1;
   %let wvar = %nrbquote(%scan( &varlist, &result));
   %do %until ( &wvar= );
       %let result = %eval( &result + 1);
       %let wvar = %nrbquote(%scan( &varlist, &result));
   %end;
   %eval( &result - 1)
%mend nvar;

 /*-----------------------------------------*
  |  Macro to generate a format of the form |
  |    1 ="&val1"  2="&val2" ...            |
  |  for observation labels on the y axis.  |
  *-----------------------------------------*/
%macro makefmt(nval, fname);
  %local i ;
  proc format;
       value &fname
    %do i=1 %to &nval ;
       &i = "&&val&i"
       %end;
        OTHER = ' ' %str(;)
    run;
%mend makefmt;
