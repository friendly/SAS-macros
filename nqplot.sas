 /*-------------------------------------------------------------------*
  *    Name: NQPLOT.SAS                                               *
  *   Title: Normal quantile-comparison plots                         *
        Doc: http://www.datavis.ca/sasmac/nqplot.html             
  *                                                                   *
  *  minimal syntax: %nqplot (data=dataset,var=variable);             *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  16 Feb 1989 21:43:25                                    *
  * Revised:  07 Apr 2005 16:28:14                                    *
  * Version:  1.3                                                     *
  *  - Removed font=duplex from axis labels to allow global ftext=    *
  *  - Renamed _resid_ to _res_ to avoid conflict with proc mixed     *
  *  - Allow separate ANNO= data set for each plot                    *
  *  - Added inline documentation                                     *
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/

 /*=
=Description:
 
 The NQPLOT macro produces normal Q-Q plots for single variable, with
 optional confidence bands and a detrended version.  The
 parameters MU= and SIGMA= determine how the comparison line,
 representing a perfect fit to a normal distribution, is estimated.

=Usage:

 The NQPLOT macro is defined with keyword parameters.  The VAR= parameter
 must be specified.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%nqplot(data=baseball, var=salary);
 
==Parameters:

* DATA=       Name of the input data set [Default: DATA=_LAST_]

* VAR=        Variable to be plotted [Default: VAR=X]

* OUT=        Name of the output data set [Default: OUT=NQPLOT]

* MU=         Estimate of the mean of the reference normal distribution. 
             Specify MU=MEAN, MU=MEDIAN, or MU=numeric value
             [Default: MU=MEDIAN]

* SIGMA=      Estimate of the standard deviation of the reference normal 
              distribution: Specify SIGMA=STD, SIGMA=HSPR, or SIGMA=numeric value.
              [Default: SIGMA=HSPR]


* STDERR=     Plot std errors around curves? [Default: STDERR=YES]

* DETREND=    Plot detrended version?   If DETREND=YES the detrended version is plotted too.
			 [Default: DETREND=YES]

* LH=         Height, in character cells, for axis labels [Default: LH=1.5]

* ANNO=       Name(s) of optional input annotate data set(s).

* COLORS=     Colors for points, reference line, and confidence bands [Default: COLORS=BLACK BLUE RED]

* SYMBOL=     Plotting symbol [Default: SYMBOL=DOT]

* NAME=       Name of graphic catalog entries [Default: NAME=NQPLOT]

* GOUT=       The name of the graphics catalog

 =*/

%macro nqplot (
   data=_LAST_,    /* input data set                      */
   var=x,          /* variable to be plotted              */
   out=nqplot,     /* output data set                     */
   mu=MEDIAN,      /* est of mean of normal distribution: */
                   /*  MEAN, MEDIAN or literal value      */
   sigma=HSPR,     /* est of std deviation of normal:     */
                   /*  STD, HSPR, or literal value        */
   stderr=YES,     /* plot std errors around curves?      */
   detrend=YES,    /* plot detrended version?             */
   lh=1.5,         /* height for axis labels              */
   anno=,          /* name(s) of input annotate data set  */
   colors=black blue red, /* colors for pts, ref line, CI */
   symbol=dot,     /* plotting symbol                     */
   name=NQPLOT,    /* name of graphic catalog entries     */
   gout=);         /* name of graphic catalog             */
 
%let stderr=%upcase(&stderr);
%let sigma=%upcase(&sigma);
%let detrend=%upcase(&detrend);
%if &sigma=HSPR    %then %let sigma=hspr/1.349;
%if %length(&anno)>0 %then %do;
	%local a1 a2 anno1 anno2;
	%let anno1= %scan(&anno,1);                                                      
	%let anno2= %scan(&anno,2);
	%if %length(&anno2)=0 %then %let anno2=&anno1;
	%let anno1=annotate=&anno1;
	%let anno2=annotate=&anno2;
	%end;
%else %do;
	%let anno1=;
	%let anno2=;
	%end;
	
%if %length(&gout)>0  %then %let gout=gout=&gout;
 
options nonotes; 
data pass;
  set &data;
  _match_=1;
  if &var ne . ;                * get rid of missing data;
 
proc univariate noprint;        * find n, median and hinge-spread;
   var &var;
   output out=n1 n=nobs median=median qrange=hspr mean=mean std=std;
data n2; set n1;
   _match_=1;
 
data nqplot;
   merge pass n2;
   drop _match_;
   by _match_;
 
proc sort data=nqplot;
   by &var;
run;
 
data &out;
   set nqplot;
   drop sigma hspr nobs median std mean ;
   sigma = &sigma;
   _p_=(_n_ - .5)/nobs;                 * cumulative prob.;
   _z_=probit(_p_);                     * unit-normal Quantile;
   _se_=(sigma/((1/sqrt(2*3.1415926))*exp(-(_z_**2)/2)))
      *sqrt(_p_*(1-_p_)/nobs);          * std. error for normal quantile;
  _normal_= sigma * _z_ + &mu ;         * corresponding normal quantile;
   _res_ = &var - _normal_;           * deviation from normal;
   _lower_ = _normal_ - 2*_se_;         * +/- 2 SEs around fitted line;
   _upper_ = _normal_ + 2*_se_;
   _reslo_  = -2*_se_;                  * +/- 2 SEs ;
   _reshi_   = 2*_se_;
  label _z_='Normal Quantile'
        _res_='Deviation From Normal';
  run;
 /*-
 proc plot;
   plot &var * _z_='*'
        _normal_ * _z_='-'
        _lower_ * _z_='+'
        _upper_ * _z_='+' / overlay;       * observed and fitted values;
   plot _res_ * _z_='*'
        _reslo_ * _z_='+'
        _reshi_ * _z_='+' / overlay vref=0; * deviation from fitted line;
   run;
 -*/
%let vh=1;           *-- value height;
%if &lh >= 1.5 %then %let vh=1.5;
%if &lh >= 2.0 %then %let vh=1.8;
	%let c1= %scan(&colors &colors,1);                                                      
	%let c2= %scan(&colors &colors,2);
	%let c3= %scan(&colors &colors &c2,3);
  symbol1 v=&symbol h=1.1 i=none c=&c1 l=1;
  symbol2 v=none   i=join  c=&c2  l=3 w=3;
  symbol3 v=none   i=join  c=&c3 l=20;
  axis1  label=(a=90 r=0 h=&lh) value=(h=&vh);
  axis2  label=(h=&lh) value=(h=&vh);

proc gplot data=&out &anno1 &gout ;
  plot &var     * _z_= 1
       _normal_ * _z_= 2
  %if &stderr=YES %then %do;
       _lower_ * _z_= 3
       _upper_ * _z_= 3 %end;
       / overlay frame
         vaxis=axis1 haxis=axis2
         hminor=1 vminor=1
         name="&name" 
			des="Normal probability plot of &var";
	run; quit;
%if &detrend=YES %then %do;
  %gskip;
proc gplot data=&out &anno2 &gout ;
  plot _res_ * _z_= 1
  %if &stderr=YES %then %do;
       _reslo_ * _z_= 3
       _reshi_ * _z_= 3 %end;
       / overlay
         vaxis=axis1 haxis=axis2
         vref=0 cvref=&c2 lvref=3 frame
         hminor=1 vminor=1
         name="&name"
			des="Detrended normal probability plot of &var";
run; quit;
%end;

options notes;
%mend;
