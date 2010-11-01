 /*-------------------------------------------------------------------*
  *    Name: NORMPLOT.SAS                                             *
  *   Title: SAS macro for normal quantile-comparison plot            *
  *                [Printer plot version]                             *
  *  minimal syntax: %normplot (data=dataset,var=variable);           *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  16 Feb 1989 21:43:25                                    *
  * Revised:  4 Sep 1991 11:32:36                                     *
  * Version:  1.0                                                     *
  *                                                                   *
  *-------------------------------------------------------------------*/
%macro normplot (
        data=_LAST_,    /* input data set                      */
        var=x,          /* variable to be plotted              */
        out=normplot,   /* output data set                     */
        mu=MEDIAN,      /* est of mean of normal distribution: */
                        /*  MEAN, MEDIAN or literal value      */
        sigma=HSPR,     /* est of std deviation of normal:     */
                        /*  STD, HSPR, or literal value        */
        stderr=YES,     /* plot std errors around curves?      */
        detrend=YES,    /* plot detrended version?             */
        rankadj=.375,   /* adjust rank and sample size in      */
        nadj=.5);       /*  computing quantiles e.g.,          */
                        /*  ( i - rankadj ) / ( n - nadj )     */
 
%let stderr=%UPCASE(&stderr);
%let sigma=%UPCASE(&sigma);
%let detrend=%UPCASE(&detrend);
%if &sigma=HSPR    %then %let sigma=HSPR/1.349;
options nonotes;
data pass;
  set &data;
  _match_=1;
  if &var ne . ;                * get rid of missing data;
 
proc univariate noprint;        * find n, median and hinge-spread;
   var &var;
   output out=n1 n=n median=median qrange=hspr mean=mean std=std;
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
   drop sigma hspr n median std mean ;
   sigma = &sigma;
   _p_=(_n_ - &rankadj)/(n + &nadj);    * cumulative prob.;
   _z_=probit(_p_);                     * unit-normal Quantile;
   _se_=(sigma/((1/sqrt(2*3.1415926))*exp(-(_z_**2)/2)))
      *sqrt(_p_*(1-_p_)/n);             * std. error for normal quantile;
  _normal_= sigma * _z_ + &mu ;         * corresponding normal quantile;
   _resid_ = &var - _normal_;           * deviation from normal;
   _lower_ = _normal_ - 2*_se_;         * +/- 2 SEs around fitted line;
   _upper_ = _normal_ + 2*_se_;
   _reslo_  = -2*_se_;                  * +/- 2 SEs ;
   _reshi_   = 2*_se_;
  label _z_='Normal Quantile'
        _resid_='Deviation From Normal';
  run;
 
options notes;
 proc plot data=&out;
   plot &var * _z_='*'
        _normal_ * _z_='-'
  %if &stderr=YES %then %do;
        _lower_ * _z_='+'
        _upper_ * _z_='+'
        / overlay        /* observed and fitted values */
      %end;
      ;
%if &detrend=YES %then %do;
   plot _resid_ * _z_='*'
  %if &stderr=YES %then %do;
        _reslo_ * _z_='+'
        _reshi_ * _z_='+'
      %end;
        / overlay vref=0;  /* deviation from fitted line */
  %end;
   run;
%mend;
