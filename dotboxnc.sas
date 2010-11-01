 /*-------------------------------------------------------------------*
  *    Name: DOTBOXNC.SAS                                             *
  *   Title: graphically compare two groups using dot histograms      *
  *          annotated with normal density curves and boxplots        *
  *-------------------------------------------------------------------*
  * Author:   Christian Sieder    <sieder@bionorica.neumarkt.de>      *
  *           Bionorica Arzneimittel GmbH                             *
  * Created:  27 May 1998                                             *
  * Revised:  not yet                                                 *
  * Version:  1.1                                                     *
  *                                                                   *
  *                                                                   *
  *                                                                   *
  *   Uses some code and ideas of Michael Friendly's BOXAXIS macro    *
  *      Thank you, Michael!                                          *
  *                                                                   *
  *                                                                   *
  *-------------------------------------------------------------------*/

%macro dotboxnc(
      data=_last_,          /*data set                   */
      var=,       /*Req*/   /*variable to be charted     */
      class=,     /*Req*/   /*class variable for comparison*/
      min=,       /*Req*/   /*minimum value to draw      */
      max=,       /*Req*/   /*maximum value to draw      */
      grp1=,      /*Req*/   /*value of class for group 1 (right), if text enclose in quotes*/
      grp2=,      /*Req*/   /*value of class for group 2 (left) , if text enclose in quotes*/
      grp1lab=,             /*label for group 1, must be enclosed in quotes*/
      grp2lab=,             /*label for group 2, must be enclosed in quotes*/
      axislab=,             /*label for axis, must be enclosed in quotes*/

      yres=50,              /*y-axis resolution for dot histogram*/
      symbol1 = circle,     /*symbol for grp 1, e.g.,: DOT, SQUARE, DIAMOND, STAR, + */
      symbol2 = circle,     /*symbol for grp 2, e.g.,: DOT, SQUARE, DIAMOND, STAR, + */
      symbsize=1,           /*symbol size*/
      color1 = blue,        /*color for grp 1*/
      color2 = red,         /*color for grp 2*/
      bw=10,                /*box-width, 0 < bw < 100 */
      bpos=85,              /*box-position, 0 < bpos < 100 */
      normalhi=60           /*max height of normal curve, 0 < normalhi < 100 */
                );


  *local span stpwidth;
  *let span=%sysevalf(&max-&min);
  *let stpwidth=%sysevalf(&span/&yres);
  *let bw=%sysevalf(&bw/100);
  *let bpos=%sysevalf(&bpos/100);
  *let normalhi=%sysevalf(&normalhi/100);

  /*-----error filters----------*/
  %if ( &var = %str() or &class = %str() ) %then %do;
    %put ERROR: analysis-  (VAR) and class (CLASS) variable must be specified;
    %goto done;
  %end;

  %if ( &grp1 = %str() or &grp2 = %str() ) %then %do;
    %put ERROR: values of two groups (grp1=, grp2=) must be specified;
    %goto done;
  %end;

  %if ( &min = %str() or &max = %str() ) %then %do;
    %put ERROR: minimum (MIN) and maximum (MAX) for scaling axes must be specified;
       %goto done;
  %end;

  %if &axislab = %str() %then %let axislab = "&var";
  %if &grp1lab = %str() %then %let grp1lab = &grp1;
  %if &grp2lab = %str() %then %let grp2lab = &grp2;

  /*create sorted dataset*/
  data data;
    set &data;
    if &var ne .;
    if &class in (&grp1 , &grp2);
    if &class = &grp1 then grp=1;
    else grp=2;

    keep &var &class grp;
  proc sort;by grp &var ;

  /*----------------------------------*
   | Find means + sdmedian & quartiles|
   *----------------------------------*/
  proc univariate data=data noprint;
    var &var;
    output out=quartile
    n=n q1=q1 q3=q3 median=median qrange=iqr mean=mean std=std;
    by grp;
  run;

  /*-----------------------------------------------*
   | Find outside & farout points                  |
   *-----------------------------------------------*/
  proc sql;
    create table outside as
    select a.grp,a.&class,a.&var, b.q1,b.q3,b.iqr
    from data as a, quartile as b
    where a.grp = b.grp;

  data outside;
    set outside;
    keep &var grp outside;
    if &var ^= .;
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
  proc univariate data=outside noprint;
      where (outside=1);
      var &var;
      by grp;
      output out=whisk min=lo_whisk max=hi_whisk;
  run;
  data boxfile;
      merge quartile whisk;
      by grp;


   /*-----------------------------------------------*
    | Annotate data set to draw boxes & whiskers    |
    *-----------------------------------------------*/
  data boxanno;
    set boxfile;
    retain xsys ysys hsys line color;
    length function $8 color $8 ;
    xsys='2'; ysys='2'; hsys='4';style=' ';size=1;
    if grp=1 then color="&color1";
    else do;
      color="&color2";
      grp=-1;
    end;

    /*low whisker*/
	 bw = &bw/100;
	 bpos = &bpos/100;
    if LO_WHISK lt &min then LO_WHISK=&min;
    function = 'MOVE';    x= (bpos-.25*bw)*grp;
                          y=LO_WHISK;
                          output;
    function = 'DRAW';
                          x=(bpos+.25*bw)*grp;
                          y=LO_WHISK;
                          line=1;output;

    function = 'MOVE';    x=bpos*grp;
                          y=LO_WHISK;
                          output;
    function = 'DRAW';
                          x=bpos*grp;
                          y=Q1;
                          line=1;output;

    /*draw median*/
    function = 'MOVE';    x=(bpos-.5*bw)*grp;
                          y=MEDIAN;
                          output;
    function = 'DRAW';
                          x=(bpos+.5*bw)*grp;
                          y=MEDIAN;
                          line=1;output;

    /*high whisker*/
    if HI_WHISK gt &max then HI_WHISK=&max;
    function = 'MOVE';    x= (bpos-.25*bw)*grp;
                          y=HI_WHISK;
                          output;
    function = 'DRAW';
                          x=(bpos+.25*bw)*grp;
                          y=HI_WHISK;
                          line=1;output;

    function = 'MOVE';    x=bpos*grp;
                          y=HI_WHISK;
                          output;
    function = 'DRAW';
                          x=bpos*grp;
                          y=Q3;
                          line=1;output;


    /*draw box*/
    function = 'MOVE';    x=(bpos-.5*bw)*grp;
                          y=Q3;
                          output;
    function = 'BAR';     x=(bpos+.5*bw)*grp;
                          y=Q1;
                          line =0;output;

    keep XSYS YSYS HSYS LINE COLOR FUNCTION X Y ;
  run;



  /*-----------------------------------------------*
  | Annotate data set to draw outside points      |
  *-----------------------------------------------*/
  data outanno;
    set outside end = end;
    where (outside>1);
    retain xsys ysys hsys color noor;
    length function $8 color $8;
    xsys='2'; ysys='2'; hsys='4';style=' ';size=&bw/10;
    if grp=1 then color="&color1";
    else do;
      color="&color2";
      grp=-1;
    end;
    noor=0;

    /*values out of range*/
    if ((&var lt &min) or (&var gt &max)) then do;
      delete;
      noor=noor+1;
    end;

    /*draw outside values*/
      x=&bpos*grp/100;
      y=&var;
      style = '        ';
      function = 'SYMBOL  ';
      if outside = 2
            then text='CIRCLE';*'DOT       ';
            else text=':  ';
    keep XSYS YSYS HSYS COLOR FUNCTION TEXT X Y STYLE size    ;
  run;

  /*------------------------------------------*
   | compute data for drawing normal curves   |
   *------------------------------------------*/
  data normc;
    retain mean1 std1;
    set quartile;
    if grp=1 then do;
      mean1=mean;std1=std;
    end;
    else do
      mean2=mean;std2=std;
      stdmin=min(std1,std2);
      dpoints=(&max-&min)/100;
		max = &normalhi/100;
      do step = 0 to 100;
        y=&min+step*dpoints;
        curve1=max*stdmin/std1*exp(-(y-mean1)**2/(2*std1**2));
        curve2=-max*stdmin/std2*exp(-(y-mean2)**2/(2*std2**2));
        output;
      end;
    end;
    keep y curve1 curve2;
  run;

  /*----------------------------------*
   |     categorize data values       |
   *----------------------------------*/
  data dots;
    retain lastyc nic;  /*last-y-category, number-in-category*/
    set data;
	 stpwidth = (&max-&min)/&yres;
    yc=round(&var/stpwidth,1)*stpwidth;
    if yc = lastyc then nic=nic+1;
    else nic=1;
    output;
    lastyc=yc;
  run;

  /*---------------------------------------------------------*
   |     annotate data set to draw data values as dots       |
   *---------------------------------------------------------*/
  data dotanno;
    retain xsys ysys hsys style  size  ;
    length function $8 color $8 text $15;
    xsys='2'; ysys='2'; hsys='4';style=' ';size=&symbsize;
    set dots;
    y=yc;
    style = '        ';
    function = 'SYMBOL  ';
    if grp=1 then do;
      text="&symbol1";
      color="&color1";
      x=(.6+nic-1)*size/30;
    end;
    else do;
      text="&symbol2";
      color="&color2";
      x=-(.6+nic-1)*size/30;
    end;
    keep XSYS YSYS HSYS COLOR FUNCTION TEXT X Y STYLE size    ;
  run;

  /*---------------------------------------------------------*
   |     annotate data set to label groups                   |
   *---------------------------------------------------------*/

  data labanno;
    length function color  $8;
    length text $15;
    retain function 'LABEL' xsys'2' ysys '1' position '+'size 1.4 hsys '4';
    x=.4 ; y=95;color="&color1";text=&grp1lab; output;
    x=-.4 ;y=95;color="&color2";text=&grp2lab; output;
  run;
  /*-----------------------------------------------------------*
  | concatenate annotate data sets and draw histogram/Boxplot |
  *-----------------------------------------------------------*/

  data annotate;
    set dotanno boxanno outanno labanno;
    keep XSYS YSYS HSYS COLOR FUNCTION LINE TEXT X Y Size position   ;
  run;

 * goptions  ftext=&ftext;


  /*set graphics options*/
  symbol1 interpol=join value=none width=1 color=&color1;
  symbol2 interpol=join value=none width=1 color=&color2;
  axis1 major=none minor=none label=none order=(-1 to 1) value=none length=60 pct;
  axis2  label=(angle=90 &axislab) ;
  proc gplot data=normc;
    plot y*curve1 y*curve2 /overlay haxis=axis1 vaxis=axis2 href=0 frame annotate=annotate;
  run;quit;

  %done:
%mend dotboxnc;

  /*--------------------------------------*
   |     end of macro dotboxnc            |
   *--------------------------------------*/

