 /*-------------------------------------------------------------------*
  *    Name: SYMPLOT.SAS                                              *
  *   Title: Plots for transformations to symmetry                    *
        Doc: http://www.datavis.ca/sasmac/symplot.html            
  *                                                                   *
  * Produces any of the following plot types:                         *
  *    UPLO   - Upper distance-to-median vs. Lower distance-to-       *
  *             median                                                *
  *    MIDSPR - Mid value vs. Spread                                  *
  *    MIDZSR - Mid value vs. Squared normal quantile                 *
  *    POWER  - Centered mid value vs. Squared spread measure.        *
  *             The slope in this plot usually indicates a            *
  *             reasonable power for a tranformation to symmetry.     *
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  23 Feb 1989 19:12:23                                    *
  * Revised:  24 Apr 2005 11:36:27                                    *
  * Version:  1.1                                                     *
  *  - Fixed to handle .A-.Z                                          *
  *  - Added VAXIS= and HAXIS= parameters; Added GOUT=                *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/

 %macro symplot(
    data=_LAST_,        /* data to be analyzed                   */
    var=,               /* variable to be plotted                */
    plot=MIDSPR POWER,  /* Type of plot(s): NONE, or any of      */
                        /* UPLO, MIDSPR, MIDZSQ, or POWER        */
    trim=5 PCT,         /* # or % of extreme obs. to be trimmed  */
    out=symplot,        /* output data set                       */
	symbol=dot,         /* observation plot symbol               */
	vaxis=,             /* axis statement for vertical axis      */
	haxis=,             /* axis statement for horizontal axis    */
    name=SYMPLOT,       /* name for graphic catalog entry        */
	gout=
	);
 
%let plot = %upcase(&plot);
 
options nonotes;
data analyze;
   set &data;
   keep &var;
   if &var <= .Z then delete;
 
proc univariate data=analyze noprint;
   var &var;
   output out=stats n=nobs median=median;
 
%let pct  = %upcase(%scan(&trim,2));
 
data stats;
   set stats;
   trim = %scan(&trim,1) ;
   %if &pct = PCT %then %do;
   trim = floor( trim * nobs / 100 );
   %end;
   put 'SYMPLOT:' trim ' Observations trimmed at each extreme' ;
 
proc sort data=analyze out=sortup;
   by &var;
proc sort data=analyze out=sortdn;
   by descending &var;

/* merge x(i) and x(n+1-i)    */
data &out;
   merge sortup(rename=(&var=frombot))        /* frombot = x(i)     */
         sortdn(rename=(&var=fromtop));       /* fromtop = x(n+1-i) */
   if _n_=1 then set stats;                   /* get nobs, median   */
   depth = _n_ ;
   if depth > trim ;                          /* trim extremes      */
   zsq = ( probit((depth-.5)/nobs) )**2;
   mid = (fromtop + frombot) / 2;
   spread = fromtop - frombot;
   lower = median - frombot;
   upper = fromtop - median;
   mid2  = mid - median;
   spread2 = (lower**2 + upper**2 ) / (4*median) ;
   if _n_ > (nobs+1)/2 then stop;
   label mid =  "Mid value of &var"
         lower= 'Lower distance to median'
         upper= 'Upper distance to median'
         zsq  = 'Squared Normal Quantile'
         mid2 = "Centered Mid Value of &var"
         spread2 = 'Squared Spread'
         ;
run;
 
%if %index(&PLOT,POWER) > 0 %then %do;
   *-- Annotate POWER plot with slope and power;
proc reg data=&out outest=parms noprint ;
   model mid2 = spread2;
data label;
   set parms(keep=spread2);
   xsys='1'; ysys='1';
   length text $12 function $8;
   x = 10;   y=90;
   function = 'LABEL';
   size = 1.4;
   power = round(1-spread2, .5);
   position='6'; text = 'Slope: ' || put(spread2,f5.2);  output;
   position='9'; text = 'Power: ' || put(power,  f5.2);  output;
   %if &trim ^= 0 %then %do;
   %if &pct=PCT %then %let pct=%str( %%);
   position='3'; text = 'Trim : ' || put(%scan(&trim,1),  f3. )||"&pct";
      output;
   %end;
%end;
 
%if %length(&PLOT) > 0 &
    &PLOT ^= NONE %then %do;      /* Something to plot? */
*options mprint;

%if %length(&vaxis)=0 %then %do;
   axis1 label=(h=1.5 a=90 r=0) offset=(2);
	%let vaxis=axis1;
	%end;
%if %length(&haxis)=0 %then %do;
   axis2 label=(h=1.5);
	%let haxis=axis2;
	%end;

   %*-- Upper vs. Lower plot;
   %if %index(&PLOT,UPLO) > 0 %then %do;
proc gplot data=&out;
   plot upper * lower = 1
        upper * upper = 2
        / overlay
          vaxis=&vaxis haxis=&haxis vm=1 hm=1 name="&name"
			 des="Upper-Lower SYMPLOT of &var";
   symbol1 v=&symbol c=black;
   symbol2 v=none i=join c=red l=20;
   %end;
 
   %*-- Mid vs. Spread plot;
   %if %index(&PLOT,MIDSPR) > 0 %then %do;
proc gplot data=&out;
   plot mid   * spread = 1
        median* spread = 2
        / overlay
          vaxis=axis1 haxis=axis2 vm=1 hm=1 name="&name"
			 des="Mid-Spread SYMPLOT of &var";
   symbol1 v=&symbol    i=rl   c=black;
   symbol2 v=none i=join c=red l=20;
   %end;

   %*-- Mid vs. ZSQ    plot;
   %if %index(&PLOT,MIDZSQ) > 0 %then %do;
proc gplot data=&out;
   plot mid   * zsq    = 1
        median* zsq    = 2
        / overlay
          vaxis=axis1 haxis=axis2 vm=1 hm=1 name="&name"
			 des="Mid-ZSQ SYMPLOT of &var";

   symbol1 v=&symbol    i=rl   c=black;
   symbol2 v=none i=join c=red l=20;
	%*gskip;
   %end;

   %*-- Mid2 vs. Spread2    plot;
   %if %index(&PLOT,POWER) > 0 %then %do;
proc gplot data=&out;
   plot mid2  * spread2= 1
        / overlay vref=0 lvref=20 cvref=red anno=label
          vaxis=axis1 haxis=axis2 vm=1 hm=1 name="&name"
			 des="Power SYMPLOT of &var";

   symbol1 v=&symbol    i=rl   c=black;
   symbol2 v=none i=join c=red l=20;
   %end;
run; quit;
%end;
goptions reset=symbol;
options notes;
%mend;
