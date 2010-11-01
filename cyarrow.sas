%macro cyArrow(
                           /* coordinates --------------------------- */

    x1, y1                 /* starting point coordnate -- required    */
  , x2, y2                 /* end point coordinate     -- required    */


                           /* cyArrow specific options -------------- */

  , barbAngle      =30     /* angle between barb and shaft in degrees */
  , barbLengthType ="P"    /* [F]ixed or [P]roportional to shaft len  */
  , barbLength     =0.05   /* if type=F then absolute length in hsys  */
                           /*   unit. otherwise proportion of shaft   */
                           /*   length                                */
  , barbAspectRatio=1      /* the ratio of width(x) to height(y)      */


                           /* draw function options ----------------- */

  , color          ="*"    /* color codes.                            */
  , hsys           =hsys   /* coord sys for size option               */
  , line           =1      /* line type 1...46                        */
  , size           =0.5    /* thickness of lines in hsys units        */
  , when           ="A"    /* annotate "A"fter gproc outputs          */

);

%*-- draw a simple arrow using annotate facility. This replaces     --*;
%*--   the undocumented macro %arrow()                              --*;
%*-- by chang y chung and ya huang                                  --*;
%*-- v1.0 on 2004-07-26                                             --*;
%*-- v1.5 on 2004-07-26 fixed the exit part                         --*;


%*-- helpers ---------------------------------------------------------*;

     %*-- a random prefix for "global" things ------------------------*;
     %global cyArrow;
     %let cyArrow = %sysfunc(putn(%sysfunc(int(1e8*
          %sysfunc(ranuni(0)))),z8.));

     %*-- for data step runtime error handling -----------------------*;
     %local err cond msg;
     %let err = %nrstr(
          if %unquote(&cond.) then do;
               put "&preMsg. %unquote(&msg.). &postMsg.";
               function = "comment";
               output;
               goto _&cyArrow._exit;
          end;
     );
     %local preMsg postMsg commentAndExit;
     %let preMsg = NOTE: (cyArrow);
     %let postMsg= arrow not drawn.;



%*-- macro compile time error check ----------------------------------*;

     %*-- no empty parameters. ---------------------------------------*;
     %local params param value i;
     %let params = x1 y1 x2 y2 barbAngle barbLength barbLengthType
          barbAspectRatio color hsys line size when;
     %let i = 1;
     %let param = %scan(&params.,&i); 
     %do %while (&param.^=);
          %if %superq(&param.)= %then %do;
               %put &preMsg. &param. should not be blank. &postMsg.;
               %goto exit;
          %end;
          %let i = %eval(&i. + 1);
          %let param = %scan(&params., &i.);
     %end;

%*-- data step runtime check -----------------------------------------*;

     %*-- coordinates cannot be missing ------------------------------*;
     %local i coord coords;
     %let coords = x1 y1 x2 y2;
     %do i = 1 %to 4;
          %let coord = %scan(&coords., &i.); 
          _&cyArrow._&coord. = %unquote(&&&coord..);
          %let cond = missing(_&cyArrow._&coord.);
          %let msg  = &coord. should not be missing;
          %unquote(&err.)
     %end;
   
     %*-- barbs ------------------------------------------------------*;
     _&cyArrow._barbAngle = (%unquote(&barbAngle.));
     %let cond = not (0<=_&cyArrow._barbAngle<=90);
     %let msg  = barbAngle should be between 0 to 90 degrees;
     %unquote(&err.)

     _&cyArrow._barbLengthType = upcase(trim(left(
          %unquote(&barbLengthType.))));
     %let cond = missing(_&cyArrow._barbLengthType);
     %let msg  = barbLengthType should not be missing;
     %unquote(&err.)

     _&cyArrow._barbLengthType = substr(_&cyArrow._barbLengthType,1,1);
     %let cond = not (_&cyArrow._barbLengthType in ("P" "F"));
     %let msg  = barbLengthType should be either [P]roportional
          or [F]ixed;
     %unquote(&err.)

     _&cyArrow._barbLength = (%unquote(&barbLength.));
     %let cond = not (0 <= _&cyArrow._barbLength);
     %let msg  = barbLength should not be negative;
     %unquote(&err.)

     if _&cyArrow._barbLengthType = "P" then do;
          %let cond = not (_&cyArrow._barbLength<=1.0);
          %let msg  = Proportional type barbLength should be/*
               */ between 0 and 1;
          %unquote(&err.)
     end;     

     _&cyArrow._barbAspectRatio = (%unquote(&barbAspectRatio.));
     %let cond = not (0 < _&cyArrow._barbAspectRatio);
     %let msg  = barbAspectRatio should be larger than zero;
     %unquote(&err.)


%*-- calculation for the shaft ---------------------------------------*;

     %*-- always adjust y for aspect ---------------------------------*;
     _&cyArrow._ay1 = _&cyArrow._y1 * _&cyArrow._barbAspectRatio**-1;
     _&cyArrow._ay2 = _&cyArrow._y2 * _&cyArrow._barbAspectRatio**-1;

     %*-- calculate shaft angle and length ---------------------------*;
     _&cyArrow._shaftLength = sqrt(
            (_&cyArrow._x1  - _&cyArrow._x2)**2 
          + (_&cyArrow._ay1 - _&cyArrow._ay2)**2
     );
     %*-- check ------------------------------------------------------*;
     %let cond = _&cyArrow._shaftLength <= 0;
     %let msg  = shaft length <= 0;
     %unquote(&err.)

     %*-- direction --------------------------------------------------*;
     _&cyArrow._shaftDirection = atan2(
            _&cyArrow._ay1 - _&cyArrow._ay2
          , _&cyArrow._x1  - _&cyArrow._x2
     );

%*-- calculation for the barbs ---------------------------------------*;

     %*-- angle ------------------------------------------------------*;
     _&cyArrow._barbAngle  = 
          _&cyArrow._barbAngle * constant('pi') / 180
     ;
     if _&cyArrow._barbLengthType = "P" then do;
          _&cyArrow._barbLength = 
               _&cyArrow._shaftLength * _&cyArrow._barbLength
          ;
     end;   
     %*-- check ------------------------------------------------------*;
     %let cond = _&cyArrow._barbLength <= 0;
     %let msg  = barb length <= 0;
     %unquote(&err.)


     %*-- coordinates ------------------------------------------------*;
     _&cyArrow._barbX1 = _&cyArrow._x2 + _&cyArrow._barbLength
          * cos(_&cyArrow._shaftDirection + _&cyArrow._barbAngle);
     _&cyArrow._barbY1 = _&cyArrow._y2 + _&cyArrow._barbLength 
          * sin(_&cyArrow._shaftDirection + _&cyArrow._barbAngle) 
          * _&cyArrow._barbAspectRatio;
     _&cyArrow._barbX2 = _&cyArrow._x2 + _&cyArrow._barbLength
          * cos(_&cyArrow._shaftDirection - _&cyArrow._barbAngle);
     _&cyArrow._barbY2 = _&cyArrow._y2 + _&cyArrow._barbLength 
          * sin(_&cyArrow._shaftDirection - _&cyArrow._barbAngle)
          * (_&cyArrow._barbAspectRatio);


%*-- save xlast, ylast, xlastt, ylastt and other options--------------*;
     function = "push"; 
     output; 
     _&cyArrow._color = trim(color);
     _&cyArrow._line  = 1 * line;
     _&cyArrow._size  = 1 * size;
     _&cyArrow._hsys  = trim(hsys);
     _&cyArrow._when  = trim(when);


%*-- common vars to draw function ------------------------------------*;
     color = %unquote(&color.); /* codes only */               
     hsys  = %unquote(&hsys.);  /* for size      */
     line  = %unquote(&line.);  /* 1, 2, ..., 46 */
     size  = %unquote(&size.);  /* in hsys unit  */
     when  = %unquote(&when.);  /* draw before/after the proc output */


%*-- draw "shaft" ----------------------------------------------------*;
     function = "move"; 
          x = _&cyArrow._x1; 
          y = _&cyArrow._y1;
     output;
     function = "draw"; 
          x     = _&cyArrow._x2; 
          y     = _&cyArrow._y2; 
     output; 


%*-- draw "barbs" ----------------------------------------------------*;
     %do i = 1 %to 2;
         function = "move";
             x = _&cyArrow._barbX&i.;
             y = _&cyArrow._barbY&i.;
         output;
         function = "draw";
             x     = _&cyArrow._x2; 
             y     = _&cyArrow._y2; 
         output;
     %end;


%*-- restore saved values --------------------------------------------*;
     function = "pop";
     output;
     color = _&cyArrow._color;
     line  = _&cyArrow._line;
     size  = _&cyArrow._size;
     hsys  = _&cyArrow._hsys;
     when  = _&cyArrow._when;


%*-- exits -----------------------------------------------------------*;
     %exit:;
     _&cyArrow._exit:; 
          drop _&cyArrow._:;


%mend  cyArrow;


%macro cyArrow_test;

/*-- test data set ----------------------------------------------*/
data one;
  one_x =-10; one_y =-10; output;
  one_x = 10; one_y = 10; output;
run;

data anno;

  %annomac(nomsg)
  %dclanno
  %system(2,2,2)
  %frame(CXFF0000, 1, 1)

  length text $30;
  radius = 3.5;

  %macro doCircle(ar=, labelX=, labelY=
    ,offsetX=, offsetY=, centerX=, centerY=
  );
    function="label"; 
    text="barbAspectRatio = &ar."; 
    font="simplex"; 
    x=&labelX.;
    y=&labelY.; 
    size=0.5;
    output;
    do deg = 0 to 360 by 15;
      rad = deg * constant('pi') / 180;
      xx =  &offsetX. + radius * (1/1.1429) * cos(rad);
      yy =  &offsetY. + radius * (1.1429)   * sin(rad);
      %cyArrow(&CenterX., &CenterY., xx, yy
        , barbLengthType="Fixed"
        , barbLength=0.4, barbAspectRatio=&ar.
        , color="cx0000ff", size=0.5
      ) 
    end;
  %mend doCircle;
  %doCircle(ar=1.0000, labelX=-5, labelY=9.5
    , offsetX=-4.5, offsetY= 4.5, centerX=-5, centerY= 5)
  %doCircle(ar=0.7000, labelX= 5, labelY=9.5
    , offsetX= 4.5, offsetY= 4.5, centerX= 5, centerY= 5)
  %doCircle(ar=1.1429, labelX= 5, labelY=-9.5
    , offsetX= 4.5, offsetY=-4.5, centerX= 5, centerY=-5)
  %doCircle(ar=2.0000, labelX=-5, labelY=-9.5
    , offsetX=-4.5, offsetY=-4.5, centerX=-5, centerY=-5)
run;

dm log 'graph1; clear; end;' wedit;   /* close the graph1 window */
goptions goutmode=replace; /* entire contents of catalog replaced */

/*-- aspect ratio test - gplots----------------------------------*/
%macro doGPlot(hsize=, vsize=, labelSize=);
  dm 'graph1; end;' wedit;
  filename gout "shaftAndBarb.emf";
  goptions reset=all hsize=&hsize. vsize=&vsize.
    device=emf gsfname=gout gsfmode=replace 
    targetdevice=emf goutmode=replace
  ; 
  proc gplot data=one;
    title font="Tahoma" "HSize=&hsize. and VSize=&vsize."; 
    plot one_y*one_x/ annotate=anno grid;
  run;
  quit;
  title;
  goptions reset=all;
%mend doGPlot;
%doGPlot(hsize=8 in, vsize=7 in)
%mend cyArrow_test;

%*cyArrow_test; /* uncomment to run the test macro */
