 /********************************************************************

       name: jackboot
      title: Jackknife and Bootstrap Analyses
    product: stat
     system: all
    support:                             update:  21Sep95

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.


Introduction
------------

The %JACK macro does jackknife analyses for simple random samples,
computing approximate standard errors, bias-corrected estimates, and
confidence intervals assuming a normal sampling distribution.

The %BOOT macro does elementary nonparametric bootstrap analyses for
simple random samples, computing approximate standard errors,
bias-corrected estimates, and confidence intervals assuming a normal
sampling distribution. Also, for regression models, the %BOOT macro can
resample either observations or residuals.

The %BOOTCI macro computes several varieties of confidence intervals
that are suitable for sampling distributions that are not normal.

In order to use the %JACK or %BOOT macros, you need to know enough about
the SAS macro language to write simple macros yourself. See _The SAS
Guide to Macro Processing_ for information on the SAS macro language.

This document does not explain how the jackknife and bootstrap are
performed or how the various confidence intervals are computed, but does
provide some advice and caveats regarding usage. For an elementary
introduction, see Dixon in the bibliography below. There is a thorough
exposition in E&T that should be accessible to anyone who has done a
year or more of statistical study.

There is a widespread myth that bootstrapping is a magical spell to
perform valid statistical inference on _anything_. S&T dispell this myth
very effectively and very technically. For an elementary demonstration
of the dangers of bootstrapping, see the "Cautionary Example" below.


The Jackknife
-------------

The jackknife works only for statistics that are smooth functions of the
data.  Statistics that are not smooth functions of the data, such as
quantiles, may yield inconsistent jackknife estimates.  The best results
are obtained with statistics that are linear functions of the data.  For
highly nonlinear statistics, the jackknife can be inaccurate. See S&T,
chapter 2, for a detailed discussion of the validity of the jackknife.


The Bootstrap
-------------

Bootstrap estimates of standard errors are valid for many commonly-used
statistics, generally requiring no major assumptions other than simple
random sampling and finite variance. There do exist some statistics for
which the standard error estimates will fail, such as the maximum or
minimum.  The bootstrap standard error is consistent for some nonsmooth
statistics such as the median. However, the bootstrap standard error may
not be consistent even for very smooth statistics when the population
distribution has very heavy tails.  Inconsistency of the usual bootstrap
estimators can often be remedied by using a resample size m(n) that is
smaller than the sample size n, so that m(n)->infinity and m(n)/n->0 as
n->infinity.  Theoretical results on the consistency of the bootstrap
standard error are not extensive. See S&T, chapter 3, for details.

The bootstrap estimates of bias provided by the %BOOT macro are valid
under simple random sampling for many commonly-used _plug-in_
estimators.  A _plug-in_ estimator is one that uses the same formula to
compute an estimate from a sample that is used to compute a parameter
from the population.  For example, if the sample variance is computed
with a divisor of n (VARDEF=N), it is a plug-in estimate; if it is
computed with a divisor of n-1 (VARDEF=DF, the default), it is _not_ a
plug-in estimate.  R-squared is a plug-in estimator; adjusted r-squared
is not.  Estimating the bias of a non-plug-in estimators requires
special treatment; see "Bias Estimation" below. If you are using an
estimator that is known to be unbiased, use the BIASCORR=0 argument with
%BOOT.  See E&T, chapter 10, for more discussion of bootstrap estimation
of bias.

The approximate normal confidence intervals computed by the %BOOT macro
are valid if both the bias and standard error estimates are valid and
if the sampling distribution is approximately normal. For non-normal
sampling distributions, you should use the %BOOTCI macro, which
requires a much larger number of resamples for adequate approximation.
If you plan to use only %BOOT, 200 resamples will typically be
enough. If you plan to use %BOOTCI, 1000 or more resamples are likely
to be needed for a 90% confidence interval; greater confidence
levels require even more resamples. The proper use of bootstrap
confidence intervals is a matter of considerable controversy; see
S&T, chapter 4, for a review.

The %BOOT macro does balanced resampling when possible. Balanced
resampling yields more accurate approximations to the ideal bootstrap
estimators of bias and standard errors than does uniform resampling. Of
course, both balanced resampling and uniform resampling produce
approximations that converge to the same ideal bootstrap estimators as
the number of resamples goes to infinity. Balanced resampling is of
little benefit with %BOOTCI. See Hall, appendix II, for a discussion of
balanced resampling and other methods from improving the computational
efficiency of the bootstrap.


Using %JACK and %BOOT
---------------------

To use the %JACK or %BOOT macros, you must write a macro called %ANALYZE
to do the data analysis that you want to bootstrap. The %ANALYZE macro
must have two arguments:

   DATA=   the name of the input data set to analyze
   OUT=    the name of the output data set containing the statistics
           for which you want to compute bootstrap distributions.

If possible, you should write the %ANALYZE macro to use BY processing.
The BY statement must be specified via the %BYSTMT macro, which
generates a BY statement in which the list of BY variables is given by a
macro variable &BY. The &BY macro variable is not an argument to
%ANALYZE or to %BYSTMT, but is specified by a %LET statement when
needed. The %JACK and %BOOT macros run %ANALYZE once without a BY
variable and then once with the the BY variable _SAMPLE_.

If you do not use the %BYSTMT macro, the computations will be done with
a macro loop instead of with BY processing. A macro loop takes much more
computer time than BY processing but requires less disk space.

If the %ANALYZE macro uses the %BYSTMT macro, two output data sets
are created by the %JACK macro:

   JACKDATA contains the jackknife resamples. The variable _SAMPLE_
            gives the resample number, and _OBS_ gives the original
            observation number.

   JACKDIST contains the resampling distributions of the statistics
            in the OUT= data set created by the %ANALYZE macro. The
            variable _SAMPLE_ gives the resample number.

Two similar data sets are also created by the %BOOT macro when the
%BYSTMT macro is used:

   BOOTDATA contains the bootstrap resamples. The variable _SAMPLE_
            gives the resample number, and _OBS_ gives the original
            observation number.

   BOOTDIST contains the resampling distributions of the statistics
            in the OUT= data set created by the %ANALYZE macro. The
            variable _SAMPLE_ gives the resample number.

In addition, the %JACK macro creates a data set JACKSTAT and the %BOOT
macro creates a data set BOOTSTAT regardless of whether the %BYSTMT
macro is used. These data sets contain the approximate standard errors,
bias-corrrected estimates, and 95% confidence intervals assuming a
normal sampling distribution. The %BOOTCI macro creates a data set
BOOTCI containing the confidence intervals.

If the OUT= data set contains more than one observation per BY group,
you must specify a list of ID= variables when you run the %JACK or %BOOT
macros.  These ID= variables identify observations that correspond to
the same statistic in different BY groups. For many procedures, these
ID= variables would naturally be _TYPE_ and _NAME_, but those names are
_not_ allowed to be used as ID= variables--you must use the RENAME= data
set option to rename them.  (Renaming variables can be tricky.  You must
use the _old_ name with the DROP= and KEEP= data set options, but you
must use the _new_ name with the WHERE= data set option.)

Consider analyzing the correlation of the LSAT and GPA variables from
Efron and Tibshirani (1993):

   title 'Law School Data from Efron and Tibshirani, p. 19';
   data law; input lsat gpa; cards;
   576 3.39
   635 3.30
   558 2.81
   578 3.03
   666 3.44
   580 3.07
   555 3.00
   661 3.43
   651 3.36
   605 3.13
   653 3.12
   575 2.74
   545 2.76
   572 2.88
   594 2.96
   ;

The following %ANALYZE macro could be used to process all the statistics
in the OUT= data set from PROC CORR:

   %macro analyze(data=,out=);
      proc corr noprint data=&data
         out=&out(rename=(_type_=stat _name_=with));
         var lsat gpa;
         %bystmt;
      run;
   %mend;

   title2 'Jacknife Analysis';
   %jack(data=law,id=stat with)

   title2 'Bootstrap Analysis';
   %boot(data=law,id=stat with,random=123)

However, if you are interested only in the correlation, it is more
efficient to extract only the relevant observations and variables.  It
is also helpful to provide descriptive names and labels, as in this
example:

   %macro analyze(data=,out=);
      proc corr noprint data=&data out=&out;
         var lsat gpa;
         %bystmt;
      run;
      %if &syserr=0 %then %do;
         data &out;
            set &out;
            where _type_='CORR' & _name_='LSAT';
            corr=gpa;
            label corr='Correlation';
            keep corr &by;
         run;
      %end;
   %mend;

   title2 'Jacknife Analysis';
   %jack(data=law)

   title2 'Bootstrap Analysis';
   %boot(data=law,random=123)

It is advisable to make the OUT= data set as small as possible to
conserve computer time and disk space.  If you are running release 6.11
or later, you can use a WHERE= data set option on an output data set:

   title2 'Using WHERE= with an output data set--6.11 only';
   %macro analyze(data=,out=);
      proc corr noprint data=&data
         out=&out(where=(_type_='CORR' & _name_='LSAT')
                  rename=(gpa=corr)
                  keep=gpa _type_ _name_ &by);
         var lsat gpa;
         %bystmt;
      run;
   %mend;

   title3 'Jacknife Analysis';
   %jack(data=law)

   title3 'Bootstrap Analysis';
   %boot(data=law,random=123)

Unfortunately, you may not DROP any variable used in a WHERE= data set
option.


Bias Estimation
---------------

The sample correlation is a plug-in estimator and hence is suitable for
the bias estimator in %BOOT. The sample variance computed with a divisor
of n-1 is not a plug-in estimator and therefore requires special
treatment. In some procedures, you can use the VARDEF= option to obtain
a plug-in estimate of the variance. The default value of VARDEF= is DF,
which yields the usual adjustment for degrees of freedom, instead of the
plug-in estimate. For example:

   title2 'The unbiased variance estimator is not a plug-in estimator';
   proc means data=law var vardef=df;
      var lsat gpa;
   run;

The following %ANALYZE macro could be used to jackknife the unbiased
variance estimator, but the bootstrap over-corrects for the nonexistent
bias:

   title2 'Estimating the bias of the unbiased estimator of variance';
   %macro analyze(data=,out=);
      proc means noprint data=&data vardef=df;
         output out=&out(drop=_freq_ _type_) var=var_lsat var_gpa;
         var lsat gpa;
         %bystmt;
      run;
   %mend;

   title3 'The jackknife computes the correct bias of zero';
   %jack(data=law)

   title3 'The bootstrap over-corrects for bias';
   %boot(data=law,random=123)

By specifying VARDEF=N instead of VARDEF=DF, you can tell the MEANS
procedure to compute a plug-in estimate of the variance:

   title2 'Estimating the bias of the plug-in estimator of variance';
   %macro analyze(data=,out=);
      proc means noprint data=&data vardef=n;
         output out=&out(drop=_freq_ _type_) var=var_lsat var_gpa;
         var lsat gpa;
         %bystmt;
      run;
   %mend;

With the above %ANALYZE macro, %JACK yields an exact bias correction,
while the bias-corrected estimates from %BOOT are very close to the
unbiased estimates:

   title3 'Jacknife Analysis';
   %jack(data=law)

   title3 'Bootstrap Analysis';
   %boot(data=law,random=123)

If the procedure you are using supports the VARDEF= option to produce
plug-in estimates, you can use the %VARDEF macro to obtain correct
bootstrap bias estimates of the corresponding non-plug-in estimates. The
%VARDEF macro generates a VARDEF= option with a value of either N or DF
as appropriate for use with the %BOOT macro (The %JACK macro ignores the
%VARDEF macro). In the %ANALYZE macro, use %VARDEF in the procedure
statement where the VARDEF= option would be syntactically correct. For
example:

   title2 'Estimating the bias of the unbiased variance estimator';
   %macro analyze(data=,out=);
      proc means noprint data=&data %vardef;
         output out=&out(drop=_freq_ _type_) var=var_lsat var_gpa;
         var lsat gpa;
         %bystmt;
      run;
   %mend;

   title3 'Bootstrap Analysis';
   %boot(data=law,random=123)

The variance estimator using VARDEF=DF is unbiased, so the bias
correction estimated by bootstrapping is much smaller than in the
previous example, in which the biased plug-in estimator was used.


Confidence Intervals
--------------------

The normal bootstrap confidence interval computed by %BOOT or %BOOTSE is
accurate only for statistics with an approximately normal sampling
distribution.  The %BOOTCI macro provides the most commonly used types
of bootstrap confidence intervals that:

 * are suitable for statistics with nonnormal sampling distributions and

 * require only a single level of resampling.

You must run %BOOT before %BOOTCI, and it is advisable to specify at
least 1000 resamples in %BOOT for a 90% confidence interval. For a
higher level of confidence or for the BC and BCa methods, even more
resamples should be used.

The terminology for bootstrap confidence intervals is confused. The
keywords used with the %BOOTCI macro follow S&T:

   Keyword      Terms from the references
   -------      -------------------------
   PCTL or      "bootstrap percentile" in S&T;
   PERCENTILE   "percentile" in E&T;
                "other percentile" in Hall;
                "Efron's `backwards' pecentile" in Hjorth

   HYBRID       "hybrid" in S&T;
                 no term in E&T;
                "percentile" in Hall;
                "simple" in Hjorth

   T            "bootstrap-t" in S&T and E&T;
                "percentile-t" in Hall;
                "studentized" in Hjorth

   BC           "BC" in all

   BCA          "BCa" in S&T, E&T, and Hjorth; "ABC" in Hall
                (cannot be used for bootstrapping residuals in
                regression models)

There is considerable controversy concerning the use of bootstrap
confidence intervals. To fully appreciate the issues, it is important to
read S&T and Hall in addition to E&T.  Asymptotically in simple random
samples, the T and BCa methods work better than the traditional normal
approximation, while the percentile, hybrid, and BC methods have the
same accuracy as the traditional normal approximation.  In small
samples, things get much more complicated:

 * The percentile method simply uses the alpha/2 and 1-alpha/2
   percentiles of the bootstrap distribution to define the interval.
   This method performs well for quantiles and for statistics that are
   unbiased and have a symmetric sampling distribution.  For a
   statistic that is biased, the percentile method amplifies the bias.
   The main virtue of the percentile method and the closely related BC
   and BCa methods is that the intervals are equivariant under
   transformation of the parameters. One consequence of this
   equivariance is that the interval cannot extend beyond the possible
   range of values of the statistic.  In some cases, however, this
   property can be a vice--see the "Cautionary Example" below.

 * The BC method corrects the percentile interval for bias--median
   bias, not mean bias. The correction is performed by adjusting the
   percentile points to values other than alpha/2 and 1-alpha/2.  If a
   large correction is required, one of the percentile points will be
   very small; hence a very large number of resamples will be required
   to approximate the interval accurately. See the "Cautionary
   Example" below.

 * The BCa method corrects the percentile interval for bias and
   skewness. This method requires an estimate of the acceleration,
   which is related to the skewness of the sampling distribution. The
   acceleration can be estimated by jackknifing for simple random
   samples which, of course, requires extra computation. For
   bootstrapping residuals in regression models, no general method for
   estimating the acceleration is known. If the acceleration is not
   estimated accurately, the BCa interval will perform poorly.  The
   length of the BCa interval is not monotonic with respect to alpha
   (Hall, pp 134-135, 137).  For large values of the acceleration and
   large alpha, the BCa interval is excessively short.  The BCa
   interval is no better than the BC interval for nonsmooth statistics
   such as the median.

 * The HYBRID method is the reverse of the percentile method. While
   the percentile method amplifies bias, the HYBRID method
   automatically adjusts for bias and skewness. The HYBRID method
   works well if the standard error of the statistic does not depend
   on any unknown parameters; otherwise, the T method works better if
   a good estimate of the standard error is available. Of all the
   methods in %BOOTCI, the HYBRID method seems to be the least likely
   to yield spectacularly wrong results, but often suffers from low
   coverage in relatively easy cases. The HYBRID method and the
   closely related T method are not equivariant under transformation
   of the parameters.

 * The T method requires an estimate of the standard error (or a
   constant multiple thereof) of each statistic being bootstrapped.
   This requires more work from the user. If the standard errors are
   not estimated accurately, the T method may perform poorly. In
   simulation studies, T intervals are often found to be very long.
   E&T (p 160) claim that the T method is erratic and sensitive to
   outliers.

Numerous other methods exist for bootstrap confidence intervals that
require nested resampling, i.e., each resample of the original
sample is itself reresampled multiple times. Since the total number
of reresamples required is typically 25,000 or more, these methods
are extremely expensive and have not yet been implemented in the
%BOOT and %BOOTCI macros.

The following example replicates the nonparametric confidence intervals
shown in E&T, p 183. This example analyzes the variances of two
variables, A and B, while E&T analyze only A. E&T do not show the hybrid
interval, the normal ("standard") interval with bias correction, or the
jackknife interval.

   title 'Spatial Test Data from Efron and Tibshirani, pp 180 & 183';
   data spatial;
      input a b @@;
   cards;
   48 42 36 33 20 16 29 39 42 38 42 36 20 15 42 33 22 20 41 43 45 34
   14 22  6  7  0 15 33 34 28 29 34 41  4 13 32 38 24 25 47 27 41 41
   24 28 26 14 30 28 41 40
   ;

   %macro analyze(data=,out=);
      proc means noprint data=&data vardef=n;
         output out=&out(drop=_freq_ _type_) var=var_a var_b;
         var a b;
         %bystmt;
      run;
   %mend;

   title2 'Jackknife Interval with Bias Correction';
   %jack(data=spatial,alpha=.10);

   title2 'Normal ("Standard") Confidence Interval with Bias Correction';
   %boot(data=spatial,alpha=.10,samples=2000,random=123);

   title2 'Normal ("Standard") Confidence Interval without Bias Correction';
   %bootse(alpha=.10,biascorr=0);

   title2 'Efron''s Percentile Confidence Interval';
   %bootci(percentile,alpha=.10)

   title2 'Hybrid Confidence Interval';
   %bootci(hybrid,alpha=.10)

   title2 'BC Confidence Interval';
   %bootci(bc,alpha=.10)

   title2 'BCa Confidence Interval';
   %bootci(bca,alpha=.10)


   title2 'Resampling with Computation of Studentizing Statistics';
   %macro analyze(data=,out=);
      proc means noprint data=&data vardef=n;
         output out=&out(drop=_freq_ _type_)
            var=var_a var_b kurtosis=kurt_a kurt_b;
         var a b;
         %bystmt;
      run;
      data &out;
         set &out;
         stud_a=var_a*sqrt(kurt_a+2);
         stud_b=var_b*sqrt(kurt_b+2);
         drop kurt_a kurt_b;
      run;
   %mend;

   %boot(data=spatial,stat=var_a var_b,samples=2000,random=123);

   title2 'T Confidence Interval';
   %bootci(t,stat=var_a var_b,student=stud_a stud_b,alpha=.10)

If you want to compute _all_ the varieties of confidence intervals,
you can use the %ALLCI macro:

   title2 'All Jackknife and Bootstrap Confidence Intervals';
   %allci(stat=var_a var_b,student=stud_a stud_b,alpha=.10)


Bootstrapping Regression Models
-------------------------------

In regression models, there are two main ways to do bootstrap
resampling, depending on whether the predictor variables are random or
fixed.

If the predictors are random, you resample observations just as you
would for any simple random sample. This method is usually called
"bootstrapping pairs".

If the predictors are fixed, the resampling process should keep the same
values of the predictors in every resample and change only the values of
the response variable by resampling the residuals. To do this with the
%BOOT macro, you must do a preliminary analysis in which you fit the
regression model using the complete sample and create an output data set
containing residuals and predicted values; it is this output data set
that is used as input to the %BOOT macro.  You must also specify the
name of the residual variable and provide an equation for computing the
response variable from the residual and predicted values.

   title 'Cement Hardening Data from Hjorth, p 31';
   data cement;
      input x1-x4 y;
      label x1='3CaOAl2O3'
            x2='3CaOSiO2'
            x3='4CaOAl2O3Fe2O3'
            x4='2CaOSiO2';
   cards;
    7 26  6 60  78.5
    1 29 15 52  74.3
   11 56  8 20 104.3
   11 31  8 47  87.6
    7 52  6 33  95.9
   11 55  9 22 109.2
    3 71 17  6 102.7
    1 31 22 44  72.5
    2 54 18 22  93.1
   21 47  4 26 115.9
    1 40 23 34  83.8
   11 66  9 12 113.3
   10 68  8 12 109.4
   ;

   proc reg data=cement;
      model y=x1-x4;
      output out=cemout r=resid p=pred;
   run;

   %macro analyze(data=,out=);
      options nonotes;
      proc reg data=&data noprint
               outest=&out(drop=Y _IN_ _P_ _EDF_);
         model y=x1-x4/selection=rsquare start=4;
         %bystmt;
      run;
      options notes;
   %mend;

   title2 'Resampling Observations';
   title3 '(bias correction for _RMSE_ is wrong)';
   %boot(data=cement,random=123)

   title2 'Resampling Residuals';
   title3 '(bias correction for _RMSE_ is wrong)';
   %boot(data=cemout,residual=resid,equation=y=pred+resid,random=123)

Either method of resampling for regression models (observations or
residuals) can be used regardless of the form of the error distribution.
However, residuals should be resampled only if the errors are
independent and identically distributed and if the functional form of
the model is correct to within a reasonable approximation.  If these
assumptions are questionable, it is safer to resample observations.

In the above example, R-squared is a plug-in estimator, so the bias
correction is appropriate. The root mean squared error, _RMSE_, is not a
plug-in estimator, so the bias correction for _RMSE_ is wrong.
Unfortunately, the REG procedure does not support the VARDEF= option.
_RMSE_ is not _very_ biased, so you could choose to ignore the bias and
run the %BOOTSE macro to compute the standard error without a bias
correction:

   title2 'Resampling Observations';
   title3 'Without bias correction';
   %bootse(stat=_rmse_,biascorr=0)

To get the proper bias correction for _RMSE_, you have to use a DATA
step that checks the macro variable &VARDEF and unadjusts for degrees of
freedom when &VARDEF=N. You must also invoke the %VARDEF macro, but
since you don't want to generate a VARDEF= option in this case, just
assign the value returned by %VARDEF to an unused macro variable:

   %macro analyze(data=,out=);
      options nonotes;
      proc reg data=&data noprint outest=&out;
         model y=x1-x4/selection=rsquare start=4;
         %bystmt;
      run;
      %let junk=%vardef;
      data &out(drop=y _in_ _p_ _edf_);
         set &out;
         _mse_=_rmse_**2;
         %if &vardef=N %then %do;
            _mse_=_mse_*_edf_/(_edf_+_p_);
            _rmse_=sqrt(_mse_);
         %end;
         label _mse_='Mean Squared Error';
      run;
      options notes;
   %mend;

   title2 'Resampling Observations';
   %boot(data=cement,random=123)

Note that _MSE_ is an unbiased estimate, so its estimated bias is very
small. _RMSE_ is slightly biased and thus has a larger estimated bias.


A Cautionary Example
--------------------

Jackknifing and bootstrapping are no remedy for an inadequate sample
size. For nonparametric resampling methods, the sample distribution must
be reasonably close in some sense to the population distribution to
obtain accurate inferences. In parametric methods, only the estimated
parameters need be reasonably close to the population parameters to
obtain accurate inferences. The smaller the sample size, the greater the
fluctuations in the distribution of the sample. Nonparametric methods
that are sensitive to a wide variety of such fluctuations will suffer
more from small sample sizes than will parametric methods _if_ the
assumptions of the parametric methods are valid.

In this example, the purpose of the analysis is to find a 95% confidence
interval for R**2 in a linear regression with 20 observations and 10
predictors.  The predictors and response are generated from a
multivariate normal distribution, so normal-theory methods are
applicable. With real data, if the distribution were not known to be
normal, you might be tempted to use the jackknife or bootstrap on the
theory that normal approximations could not be trusted in such a small
sample size. In fact, most of the jackknife and bootstrap methods cannot
be trusted either.

This example computes a 95% confidence interval with each of the methods
available in %JACK and %BOOT using 1000 resamples. The results are
assembled into a single data set called CI for comparison. PROC CANCORR
is also used to obtain a normal-theory 95% confidence interval. Two
versions of the %ANALYZE macro are shown, one with CANCORR and one with
REG; either version can be used for the analysis.

   title 'A Cautionary Example';

   %let n=20;
   %let p=10;

   *** generate multivariate normal data with true R**2=0.1;
   data x; array x x1-x&p;
      do n=1 to &n; drop n;
         do over x; x=rannor(123); end;
         p=sum(of x1-x&p)/sqrt(&p);
         e=rannor(123);
         y=p*sqrt(.1)+e*sqrt(.9);
         output;
      end;
   run;

   *** normal-theory confidence interval for R**2;
   proc cancorr data=x smc vdep short outstat=stat; var y; with x:; run;

   *** extract confidence interval from cancorr output;
   proc transpose data=stat(where=(_type_ in ('LCLRSQ' 'UCLRSQ')))
                  out=ci(rename=(LCLRSQ=alcl UCLRSQ=aucl));
      id _type_;
      var y;
   run;
   data ci; set ci; drop _name_;
      length method $20; method='Normal theory';
   run;

   %*** macro for bootstrapping using cancorr;
   %macro analyze(data=,out=);
      options nonotes;
      proc cancorr data=&data smc vdep noprint
         outstat=&out(where=(_type_='RSQUARED') keep=_type_ y &by);
         var y; with x:;
         %bystmt;
      run;
      options notes;
   %mend;

   %*** macro for bootstrapping using reg;
   %macro analyze(data=,out=);
      options nonotes;
      proc reg data=&data noprint
               outest=&out(keep=_rsq_ &by);
         model y=x:/selection=rsquare start=&p;
         %bystmt;
      run;
      options notes;
   %mend;

   %jack(data=x)
   proc append data=jackstat(keep=alcl aucl method) base=ci; run;

   %boot(data=x,samples=1000,random=123)
   proc append data=bootstat(keep=alcl aucl method) base=ci; run;

   %bootci(Hybrid)
   proc append data=bootci(keep=alcl aucl method) base=ci; run;

   %bootci(PCTL)
   proc append data=bootci(keep=alcl aucl method) base=ci; run;

   %bootci(BC)
   proc append data=bootci(keep=alcl aucl method) base=ci; run;

   %bootci(BCa)
   proc append data=bootci(keep=alcl aucl method) base=ci; run;


   %*** macro for bootstrapping, with a data step to estimate the
        standard error of R**2;
   %macro analyze(data=,out=);

      options nonotes;
      proc reg data=&data noprint
               outest=&out(keep=_rsq_ &by);
         model y=x:/selection=rsquare start=&p;
         %bystmt;
      run;
      options notes;

      data &out; set &out(rename=(_rsq_=rsquare));
         retain n &n p %eval(&p+1); drop n p;

         * standard error of rsquare--see Kendall & Stuart (1979),
           The Advanced Theory of Statistics, Vol 2, p 363,
           London: Charles Griffin & Company Ltd. Surprisingly,
           the bootstrap t interval seems to be slightly less
           conservative when rsquare is plugged into the formula
           for the standard error than when max(0,adjrsq) is
           plugged in;

         stud=max(.01,sqrt( (n-p)/(n**2-1)/(n-1) * (1-rsquare)**2 *
                ( 2*(p-1) + 4*rsquare*((n-p)*(n-1)+4*(p-1))/(n+3) )
                  ));
      run;

   %mend;

   %boot(data=x,stat=rsquare,samples=1000,random=123)
   %bootci(t,stat=rsquare,student=stud)
   proc append data=bootci(keep=alcl aucl method) base=ci; run;

   proc print data=ci; id method; run;

The actual sampling distribution of R**2, based on 10000 simulated data
sets, looks like this:

      Frequency

      1000 +                               *
           |                             * * *
           |                           * * * *
           |                           * * * * *
       800 +                         * * * * * *
           |                         * * * * * * *
           |                       * * * * * * * *
           |                       * * * * * * * *
       600 +                       * * * * * * * * *
           |                     * * * * * * * * * *
           |                     * * * * * * * * * *
           |                   * * * * * * * * * * *
       400 +                   * * * * * * * * * * * *
           |                   * * * * * * * * * * * *
           |                 * * * * * * * * * * * * *
           |                 * * * * * * * * * * * * *
       200 +               * * * * * * * * * * * * * * *
           |             * * * * * * * * * * * * * * * *
           |             * * * * * * * * * * * * * * * * *
           |         * * * * * * * * * * * * * * * * * * * *
           ------------------------------------------------------
             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
             . . . . . . . . . . . . . . . . . . . . . . . . . .
             0 0 0 1 1 2 2 2 3 3 4 4 4 5 5 6 6 6 7 7 8 8 8 9 9 0
             0 4 8 2 6 0 4 8 2 6 0 4 8 2 6 0 4 8 2 6 0 4 8 2 6 0

The bootstrap distribution computed from the one data set in this
example is not even close to the true sampling distribution:

       Frequency

           |                                                   *
       300 +                                                   *
           |                                                   *
           |                                                   *
           |                                                   *
           |                                                   *
       200 +                                                   *
           |                                                   *
           |                                                   *
           |                                                 * *
           |                                               * * *
       100 +                                           * * * * *
           |                                         * * * * * *
           |                                       * * * * * * *
           |                                   * * * * * * * * *
           |                               * * * * * * * * * * *
           ------------------------------------------------------
             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
             . . . . . . . . . . . . . . . . . . . . . . . . . .
             0 0 0 1 1 2 2 2 3 3 4 4 4 5 5 6 6 6 7 7 8 8 8 9 9 0
             0 4 8 2 6 0 4 8 2 6 0 4 8 2 6 0 4 8 2 6 0 4 8 2 6 0

The table of confidence intervals printed by the final PROC PRINT step
is:

               METHOD                ALCL        AUCL

               Normal theory        0.00000    0.62876
               Jackknife           -0.44648    0.54393
               Bootstrap Normal     0.07400    0.51324
               Bootstrap Hybrid     0.18391    0.56566
               Bootstrap PCTL       0.61824    1.00000
               Bootstrap BC         0.51547    0.57231
               Bootstrap BCa        0.51547    0.57231
               Bootstrap t         -3.11368    0.56556

The true value of R**2 in this example is 0.10, the sample plug-in
estimate is 0.59, and the adjusted estimate is 0.14. The normal-theory
interval can be considered the "right answer".

The jackknife interval has a negative lower limit, and the upper limit
is rather low, but the interval covers the true value.

The bootstrap interval based on a normal approximation is short but does
cover the true value. However, a glance at the chart of the bootstrap
distribution shows that a normal approximation is suspect.

The bootstrap hybrid interval is even shorter and does not cover the
true value. The hybrid interval is poor because the bootstrap
distribution is less variable and far more skewed than the true sampling
distribution.

The plug-in estimate is very biased, so it is no surprise that the
bootstrap PCTL method works poorly. However, the PCTL interval lies
entirely above the plug-in estimate, a dramatic illustration of Hall's
claim that the PCTL interval is "backwards"!

The bootstrap BC interval is extremely short and is not even close to
the true value.  The lower percentile point for computing the BC
interval is .00000000010453, so billions of resamples would be required
for an accurate approximation. The lower percentile point for the BCa
interval is even smaller at 7.3099E-17, and would require an
astronomical number of resamples for an accurate approximation.

The bootstrap t interval has a wildy negative lower limit, and the upper
limit is rather low, but the interval covers the true value.

A simulation was performed by repeating the above analysis 2000 times on
randomly generated data sets. For each method, the coverage probability
(COVERAGE), the average length (LENGTH), and the positive part of the
length (POSLEN=AUCL-MAX(0,ALCL)) were computed. Among the jackknife and
bootstrap methods, the only acceptable coverage probability is for the
bootstrap t interval, which is nevertheless very poor with regard to the
length of the interval. Considering only the positive part of the
interval, the bootstrap t interval is quite good, but it works well in
this example only because we know a lower bound for the parameter and
have an analytic expression for the standard error.

      METHOD               COVERAGE      LENGTH      POSLEN
      ------------------------------------------------------
      Bootstrap BC          0.00426    0.148825    0.148825
      Bootstrap BCa         0.00426    0.148575    0.148575
      Bootstrap Hybrid      0.42997    0.418778    0.351726
      Bootstrap Normal      0.54917    0.485234    0.361531
      Bootstrap PCTL              0    0.418778    0.418778
      Bootstrap T           0.95828    4.351963    0.542228
      Jackknife             0.66050    0.561788    0.333603
      Normal theory         0.95360    0.573977    0.573977


References
----------

Dixon   Dixon, P.M. (1993), "The bootstrap and the jackknife: 
        Describing the precision of ecological indices," in Scheiner,
        S.M. and Gurevitch, J., eds., Design and Analysis of Ecological
        Experiments, New York: Chapman & Hall, pp 290-318.

E&T     Efron, B. and Tibshirani, R.J. (1993), An Introduction to the
        Bootstrap, New York: Chapman & Hall.

Hall    Hall, P. (1992), The Bootstrap and Edgeworth Expansion, New York:
        Springer-Verlag.

Hjorth  Hjorth, J.S.U. (1994), Computer Intensive Statistical Methods,
        London: Chapman & Hall.

S&T     Shao, J. and Tu, D. (1995), The Jackknife and Bootstrap, New York:
        Springer-Verlag.

 ********************************************************************/

%******************************* JACK *******************************;
%macro jack(      /* Jackknife resampling analysis */
   data=,         /* Input data set. If the data set does not support
                     direct access via the POINT= option, do NOT use
                     the %BYSTMT macro in the %ANALYZE macro. */
   stat=_numeric_,/* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     jackknife distributions. */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_. */
   biascorr=1,    /* 1 for bias correction; 0 otherwise. */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals; blank to suppress
                     confidence intervals. */
   print=1,       /* 1 to print the jackknife estimates;
                     0 otherwise. */
   chart=1        /* 1 to chart the jackknife resampling distributions;
                     0 otherwise. */
   );

   %if %bquote(&data)= %then %do;
      %put ERROR in JACK: The DATA= argument must be specified.;
      %goto exit;
   %end;

   %global _jackdat; %let _jackdat=&data;

   %global vardef;
   %let vardef=DF;

   %local jack by useby;
   %let useby=0;

   *** compute the actual values of the statistics;
   %let by=;
   %analyze(data=&data,out=JACKACT);
   %if &syserr>4 %then %goto exit;

   *** find number of observations in the input data set;
   %local nobs;
   data _null_;
      call symput('nobs',trim(left(put(_nobs,12.))));
      if 0 then set &data nobs=_nobs;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &useby %then %do;
      %jackby(data=&data,print=0);
      %if &syserr>4 %then %goto exit;

      %let by=_sample_;
      %analyze(data=JACKDATA,out=JACKDIST);
      %if &syserr>4 %then %goto exit;
   %end;

   %else %do;
      %jackslow(data=&data);
      %if &syserr>4 %then %goto exit;
   %end;

   %if &chart %then %do;
      %if %bquote(&id)^= %then %do;
         proc sort data=JACKDIST; by &id; run;
         proc chart data=JACKDIST(drop=_sample_);
            vbar &stat;
            by &id;
         run;
      %end;
      %else %do;
         proc chart data=JACKDIST(drop=_sample_);
            vbar &stat;
         run;
      %end;
   %end;

   %jackse(stat=&stat,id=&id,alpha=&alpha,biascorr=&biascorr,print=&print)

%exit:;

%mend jack;


%macro jackby( /* Jackknife resampling */
   data=&_jackdat,
   print=0
   );

   data JACKDATA/view=JACKDATA;
      do _sample_=1 to &nobs;
         do _i=1 to &nobs;
            if _i^=_sample_ then do;
               _obs_=_i;
               set &data point=_i;
               output;
            end;
         end;
      end;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=JACKDATA; id _sample_ _obs_; run;
   %end;

%exit:;
%mend jackby;


%macro jackslow( /* Uniform jackknife sampling and analysis
                    without BY processing */
   data=&_jackdat
   );

   %put %cmpres(WARNING: Jackknife analysis will be slow because the
        ANALYZE macro did not use the BYSTMT macro.);

   data JACKDIST; set JACKACT; _sample_=0; delete; run;

   options nonotes;
   %local sample;
   %do sample=1 %to &nobs;
      %put Jackknife sample &sample;
      data _TMPD_;
         drop _i;
         do _i=1 to &nobs;
            set &data;
            if _i^=&sample then output;
         end;
         stop;
      run;
      %if &syserr>4 %then %goto exit;

      %analyze(data=_TMPD_,out=_TMPS_);
      %if &syserr>4 %then %goto exit;
      data _TMPS_; set _TMPS_; _sample_=&sample; run;
      %if &syserr>4 %then %goto exit;
      proc append data=_TMPS_ base=JACKDIST; run;
      %if &syserr>4 %then %goto exit;
   %end;

%exit:;
   options notes;
%mend jackslow;


%******************************* JACKSE *******************************;
%macro jackse( /* Jackknife estimates of standard error, bias, and
                  normal confidence intervals */
   stat=,
   id=,
   alpha=.05,
   biascorr=1,
   print=1
   );

   %global _jackdat;
   %if %bquote(&_jackdat)= %then %do;
      %put ERROR in JACKSE: You must run JACK before JACKSE;
      %goto exit;
   %end;

   %if %bquote(&alpha)^= %then %do;
      *** compute confidence level;
      %local conf;
      data _null_;
         conf=100*(1-&alpha);
         call symput('conf',trim(left(put(conf,best8.))));
      run;
   %end;

   %if %bquote(&id)^= %then %do;
      *** sort the actual statistics;
      proc sort data=JACKACT;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** transpose the actual statistics in each observation;
   proc transpose data=JACKACT out=JACKACT2 prefix=value;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   proc sort data=JACKACT2;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   %if %bquote(&id)^= %then %do;
      proc sort data=JACKDIST;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** compute mean, std, min, max of resampling distribution;
   proc means data=JACKDIST(drop=_sample_) noprint vardef=n;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      output out=JACKTMP2(drop=_type_ _freq_);
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   *** transpose statistics for resampling distribution;
   proc transpose data=JACKTMP2 out=JACKTMP3;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      id _stat_;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   proc sort data=JACKTMP3;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   data JACKSTAT;
      retain &id name value jackmean
             %if &biascorr %then bias;
             stderr
             %if %bquote(&alpha)^= %then alcl;
             %if &biascorr %then biasco;
             %if %bquote(&alpha)^= %then aucl confid method;
             min max n;
      merge JACKACT2(rename=(_name_=name value1=value))
            JACKTMP3(rename=(_name_=name mean=jackmean std=stderr));
      by %if %bquote(&id)^= %then &id; name;
      %if %bquote(&alpha)^= %then %do;
         length method $20;
         retain z; drop z;
         if _n_=1 then do;
            z=probit(1-&alpha/2); put z=;
            confid=&conf;
            method='Jackknife';
         end;
      %end;
      stderr=stderr*sqrt(&nobs-1);
      %if &biascorr %then %do;
         bias=(jackmean-value)*(&nobs-1);
         biasco=value-bias;
         %if %bquote(&alpha)^= %then %do;
            alcl=biasco-z*stderr;
            aucl=biasco+z*stderr;
         %end;
      %end;
      %else %if %bquote(&alpha)^= %then %do;
         alcl=value-z*stderr;
         aucl=value+z*stderr;
      %end;
      label name  ='Name'
            value ='Observed Statistic'
            jackmean='Jackknife Mean'
            %if &biascorr %then %do;
               bias  ='Estimated Bias'
               biasco='Bias-Corrected Statistic'
            %end;
            stderr='Estimated Standard Error'
            %if %bquote(&alpha)^= %then %do;
               alcl  ='Estimated Lower Confidence Limit'
               aucl  ='Estimated Upper Confidence Limit'
               method='Method for Confidence Interval'
               confid='Confidence Level (%)'
            %end;
            min   ='Minimum Resampled Estimate'
            max   ='Maximum Resampled Estimate'
            n     ='Number of Resamples'
            ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=JACKSTAT label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%exit:;

%mend jackse;





%******************************* BOOT *******************************;
%macro boot(      /* Bootstrap resampling analysis */
   data=,         /* Input data set, not a view or a tape file. */
   samples=200,   /* Number of resamples to generate. */
   residual=,     /* Name of variable in the input data set that
                     contains residuals; may not be used with SIZE= */
   equation=,     /* Equation (in the form of an assignment statement)
                     for computing the response variable */
   size=,         /* Size of each resample; default is size of the
                     input data set. The SIZE= argument may not be
                     used with BALANCED=1 or with a nonblank value
                     for RESIDUAL= */
   balanced=,     /* 1 for balanced resampling; 0 for uniform
                     resampling. By default, balanced resampling
                     is used unless the SIZE= argument is specified,
                     in which case uniform resampling is used. */
   random=0,      /* Seed for pseudorandom numbers. */
   stat=_numeric_,/* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     bootstrap distributions. */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_ */
   biascorr=1,    /* 1 for bias correction; 0 otherwise */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals; blank to suppress normal
                     confidence intervals */
   print=1,       /* 1 to print the bootstrap estimates;
                     0 otherwise. */
   chart=1        /* 1 to chart the bootstrap resampling distributions;
                     0 otherwise. */
   );

   %if %bquote(&data)= %then %do;
      %put ERROR in BOOT: The DATA= argument must be specified.;
      %goto exit;
   %end;

   %global _bootdat; %let _bootdat=&data;

   %local by useby;
   %let useby=0;

   %global usevardf vardef;
   %let usevardf=0;

   *** compute the actual values of the statistics;
   %let vardef=DF;
   %let by=;
   %analyze(data=&data,out=_ACTUAL_);
   %if &syserr>4 %then %goto exit;

   *** compute plug-in estimates;
   %if &usevardf %then %do;
      %let vardef=N;
      %analyze(data=&data,out=_PLUGIN_);
      %let vardef=DF;
      %if &syserr>4 %then %goto exit;
   %end;

   %if &useby=0 %then %let balanced=0;

   %if %bquote(&size)^= %then %do;
      %if %bquote(&balanced)= %then %let balanced=0;
      %else %if &balanced %then %do;
         %put %cmpres(ERROR in BOOT: The SIZE= argument may not be used
              with BALANCED=1.);
         %goto exit;
      %end;
      %if %bquote(&residual)^= %then %do;
         %put %cmpres(ERROR in BOOT: The SIZE= argument may not be used
              with RESIDUAL=.);
         %goto exit;
      %end;
   %end;
   %else %if %bquote(&balanced)= %then %let balanced=1;

   *** find number of observations in the input data set;
   %global _nobs;
   data _null_;
      call symput('_nobs',trim(left(put(_nobs,12.))));
      if 0 then set &data nobs=_nobs;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &balanced %then
      %bootbal(data=&data,samples=&samples,
               random=&random,print=0);

   %else %if &useby %then
      %bootby(data=&data,samples=&samples,
              random=&random,size=&size,print=0);

   %if &syserr>4 %then %goto exit;

   %if &balanced | &useby %then %do;
      %let by=_sample_;
      %analyze(data=BOOTDATA,out=BOOTDIST);
   %end;

   %else
      %bootslow(data=&data,samples=&samples,
                random=&random,size=&size);

   %if &syserr>4 %then %goto exit;

   %if &chart %then %do;
      %if %bquote(&id)^= %then %do;
         proc sort data=BOOTDIST; by &id; run;
         proc chart data=BOOTDIST(drop=_sample_);
            vbar &stat;
            by &id;
         run;
      %end;
      %else %do;
         proc chart data=BOOTDIST(drop=_sample_);
            vbar &stat;
         run;
      %end;
   %end;

   %bootse(stat=&stat,id=&id,alpha=&alpha,biascorr=&biascorr,print=&print)

%exit:;

%mend boot;


%macro bootbal( /* Balanced bootstrap resampling */
   data=&_bootdat,
   samples=200,
   random=0,
   print=0,
   );

   * Gleason, J.R. (1988) "Algorithms for balanced bootstrap
     simulations," American Statistician, 42, 263-266;
   data BOOTDATA/view=BOOTDATA;
      %bootin;
      drop _a _cbig _ii _j _jbig _k _s;
      array _c(&_nobs) _temporary_;  /* cell counts */
      array _p(&_nobs) _temporary_;  /* pointers */
      do _j=1 to &_nobs;
         _c(_j)=&samples;
      end;
      do _j=1 to &_nobs;
         _p(_j)=_j;
      end;
      _k=&_nobs;                  /* number of nonempty cells left */
      _jbig=_k;                   /* index of largest cell */
      _cbig=&samples;             /* _cbig >= _c(_j) */
      do _sample_=1 to &samples;
         do _i=1 to &_nobs;
            do until(_s<=_c(_j));
               _j=ceil(ranuni(&random)*_k);    /* choose a cell */
               _s=ceil(ranuni(&random)*_cbig); /* accept cell? */
            end;
            _l=_p(_j);
            _obs_=_l;
            _c(_j)+-1;
* put _sample_= _i= _k= _l= @30 %do i=1 %to &_nobs; _c(&i) %end;;
            if _j=_jbig then do;
               _a=floor((&samples-_sample_-_k)/_k);
               if _cbig-_c(_j)>_a then do;
                  do _ii=1 to _k;
                     if _c(_ii)>_c(_jbig) then _jbig=_ii;
                  end;
                  _cbig=_c(_jbig);
               end;
            end;
            if _c(_j)=0 then do;
               if _jbig=_k then _jbig=_j;
               _p(_j)=_p(_k);
               _c(_j)=_c(_k);
               _k+-1;
            end;
            %bootout(_l);
         end;
      end;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTDATA; id _sample_ _obs_; run;
   %end;

%exit:;

%mend bootbal;


%macro bootby( /* Uniform bootstrap resampling */
   data=&_bootdat,
   samples=200,
   random=0,
   size=,
   print=0
   );

   %if %bquote(&size)= %then %let size=&_nobs;

   data BOOTDATA/view=BOOTDATA;
      %bootin;
      do _sample_=1 to &samples;
         do _i=1 to &size;
            _p=ceil(ranuni(&random)*&_nobs);
            _obs_=_p;
            %bootout(_p);
         end;
      end;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTDATA; id _sample_ _obs_; run;
   %end;

%exit:;
%mend bootby;


%macro bootslow( /* Uniform bootstrap resampling and analysis
                    without BY processing */
   data=&_bootdat,
   samples=20,
   random=0,
   size=
   );

   %put %cmpres(WARNING: Bootstrap analysis will be slow because the
        ANALYZE macro did not use the BYSTMT macro.);

   %if %bquote(&size)= %then %let size=&_nobs;

   data BOOTDIST; set _ACTUAL_; _sample_=0; delete; run;

   options nonotes;
   %local sample;
   %do sample=1 %to &samples;
      %put Bootstrap sample &sample;
      data _TMPD_;
         %bootin;
         do _i=1 to &size;
            _p=ceil(ranuni(%eval(&random+&sample))*&_nobs);
            %bootout(_p);
         end;
         stop;
      run;
      %if &syserr>4 %then %goto exit;

      %analyze(data=_TMPD_,out=_TMPS_);
      %if &syserr>4 %then %goto exit;
      data _TMPS_; set _TMPS_; _sample_=&sample; run;
      %if &syserr>4 %then %goto exit;
      proc append data=_TMPS_ base=BOOTDIST; run;
      %if &syserr>4 %then %goto exit;
   %end;

%exit:;
   options notes;
%mend bootslow;



%******************************* BOOTSE *******************************;
%macro bootse( /* Bootstrap estimates of standard error, bias, and
                  normal confidence intervals */
   stat=,
   id=,
   alpha=.05,
   biascorr=1,
   print=1
   );

   %global _bootdat;
   %if %bquote(&_bootdat)= %then %do;
      %put ERROR in BOOTSE: You must run BOOT before BOOTSE;
      %goto exit;
   %end;

   %if %bquote(&alpha)^= %then %do;
      *** compute confidence level;
      %local conf;
      data _null_;
         conf=100*(1-&alpha);
         call symput('conf',trim(left(put(conf,best8.))));
      run;
   %end;

   %if %bquote(&id)^= %then %do;
      *** sort the actual statistics;
      proc sort data=_ACTUAL_;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
      %if &usevardf %then %do;
         *** sort the plug-in estimates;
         proc sort data=_PLUGIN_;
            by &id;
         run;
         %if &syserr>4 %then %goto exit;
      %end;
   %end;

   *** transpose the actual statistics in each observation;
   proc transpose data=_ACTUAL_ out=_ACTTR_ prefix=value;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;
   proc sort data=_ACTTR_;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &usevardf %then %do;
      *** transpose the plug-in estimates in each observation;
      proc transpose data=_PLUGIN_ out=_PLUGTR_ prefix=value;
         %if %bquote(&stat)^= %then %do;
            var &stat;
         %end;
         %if %bquote(&id)^= %then %do;
            by &id;
         %end;
      run;
      %if &syserr>4 %then %goto exit;
      proc sort data=_PLUGTR_;
         by %if %bquote(&id)^= %then &id; _name_ ;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   %if %bquote(&id)^= %then %do;
      proc sort data=BOOTDIST;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** compute mean, std, min, max of resampling distribution;
   proc means data=BOOTDIST(drop=_sample_) noprint;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      output out=_TMP2_(drop=_type_ _freq_);
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   *** transpose statistics for resampling distribution;
   proc transpose data=_TMP2_ out=_TMP3_;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      id _stat_;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   proc sort data=_TMP3_;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   data BOOTSTAT;
      retain &id name value bootmean
             %if &biascorr %then bias;
             stderr
             %if %bquote(&alpha)^= %then alcl;
             %if &biascorr %then biasco;
             %if %bquote(&alpha)^= %then aucl confid method;
             min max n;
      merge _ACTTR_(rename=(_name_=name value1=value))
            %if &usevardf %then
               _PLUGTR_(rename=(_name_=name value1=plugin));
            _TMP3_(rename=(_name_=name mean=bootmean std=stderr));
      by %if %bquote(&id)^= %then &id; name;
      %if %bquote(&alpha)^= %then %do;
         length method $20;
         retain z; drop z;
         if _n_=1 then do;
            z=probit(1-&alpha/2); put z=;
            confid=&conf;
            method='Bootstrap Normal';
         end;
      %end;
      %if &biascorr %then %do;
         bias=bootmean-%if &usevardf %then plugin; %else value;;
         biasco=value-bias;
         %if %bquote(&alpha)^= %then %do;
            alcl=biasco-z*stderr;
            aucl=biasco+z*stderr;
         %end;
      %end;
      %else %if %bquote(&alpha)^= %then %do;
         alcl=value-z*stderr;
         aucl=value+z*stderr;
      %end;
      label name  ='Name'
            value ='Observed Statistic'
            bootmean='Bootstrap Mean'
            %if &usevardf %then %do;
               plugin='Plug-In Estimate'
            %end;
            %if &biascorr %then %do;
               bias  ='Approximate Bias'
               biasco='Bias-Corrected Statistic'
            %end;
            stderr='Approximate Standard Error'
            %if %bquote(&alpha)^= %then %do;
               alcl  ='Approximate Lower Confidence Limit'
               aucl  ='Approximate Upper Confidence Limit'
               confid='Confidence Level (%)'
               method='Method for Confidence Interval'
            %end;
            min   ='Minimum Resampled Estimate'
            max   ='Maximum Resampled Estimate'
            n     ='Number of Resamples'
            ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTSTAT label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%exit:;

%mend bootse;


%******************************* BOOTCI *******************************;
%macro bootci(    /* Bootstrap percentile-based confidence intervals.
                     Creates output data set BOOTCI. */
   method,        /* One of the following methods must be specified:
                        PERCENTILE or PCTL
                        HYBRID
                        T
                        BC
                        BCA     Requires the %JACK macro
                     */
   stat=,         /* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     bootstrap distributions. */
   student=,      /* For the T method only, numeric variables in the
                     OUT= data set created by the %ANALYZE macro that
                     contain the standard errors of the statistics for which
                     you want to compute bootstrap distributions.
                     There must be a one-to-one between the VAR=
                     variables and the STUDENT= variables */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_ */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals */
   print=1);      /* 1 to print the bootstrap confidence intervals;
                     0 otherwise. */

   %global _bootdat;
   %if %bquote(&_bootdat)= %then %do;
      %put ERROR in BOOTCI: You must run BOOT before BOOTCI;
      %goto exit;
   %end;

   *** check method;
   data _null_;
      length method $10;
      method=upcase(symget('method'));
      if method=' ' then do;
         put 'ERROR in BOOTCI: You must specify one of the methods '
             'PCTL, HYBRID, T, BC or BCa';
         abort;
      end;
      else if method='PERCENTILE' then method='PCTL';
      else if method not in ('PCTL' 'HYBRID' 'BC' 'BCA' 'T')
         then do;
         put "ERROR in BOOTCI: Unrecognized method '" method "'";
         abort;
      end;
      call symput('qmethod',method);
   run;
   %if &syserr>4 %then %goto exit;

   %if &qmethod=T %then %do;
      %if %bquote(&stat)= | %bquote(&student)= %then %do;
         data _null_;
   put 'ERROR: VAR= and STUDENT= must be specified with the T method';
         run;
         %goto exit;
      %end;
   %end;

   *** sort resampling distributions;
   %if %bquote(&id)^= %then %do;
      proc sort data=BOOTDIST;
         by &id _sample_;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** transpose resampling distributions;
   proc transpose data=BOOTDIST prefix=col
      out=BOOTTRAN(rename=(col1=value _name_=name));
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      by %if %bquote(&id)^= %then &id; _sample_;
   run;
   %if &syserr>4 %then %goto exit;

   %if &qmethod=T %then %do;
      *** transpose studentizing statistics;
      proc transpose data=BOOTDIST prefix=col
         out=BOOTSTUD(rename=(col1=student _name_=studname));
            var &student;
         by %if %bquote(&id)^= %then &id; _sample_;
      run;
      %if &syserr>4 %then %goto exit;

      data BOOTTRAN;
         merge BOOTTRAN BOOTSTUD;
         label student='Value of Studentizing Statistic'
               studname='Name of Studentizing Statistic';
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   proc sort data=BOOTTRAN;
      by
         %if %bquote(&id)^= %then &id;
         name
         %if &qmethod=BC | &qmethod=BCA %then value;
         %else %if &qmethod=T %then _sample_;
      ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &qmethod=T %then %do;
      *** transpose the actual statistics in each observation
          must get data set in unsorted order for merge;
      proc transpose data=_ACTUAL_ out=_ACTTR_ prefix=value;
         %if %bquote(&stat)^= %then %do;
            var &stat;
         %end;
         %if %bquote(&id)^= %then %do;
            by &id;
         %end;
      run;
      %if &syserr>4 %then %goto exit;

      *** transpose the actual studentizing statistics;
      proc transpose data=_ACTUAL_ prefix=col
            out=_ACTSTUD(rename=(_name_=studname col1=student));
            var &student;
         %if %bquote(&id)^= %then %do;
            by &id;
         %end;
      run;
      %if &syserr>4 %then %goto exit;

      *** merge statistics with studentizing statistics;
      data _ACT_T_;
         merge _ACTTR_ _ACTSTUD;
         label student='Value of Studentizing Statistic'
               studname='Name of Studentizing Statistic';
      run;
      %if &syserr>4 %then %goto exit;

      proc sort data=_ACT_T_;
         by %if %bquote(&id)^= %then &id; _name_ ;
      run;
      %if &syserr>4 %then %goto exit;

      data BOOTTRAN;
         merge BOOTTRAN _ACT_T_(rename=(_name_=name));
         by
            %if %bquote(&id)^= %then &id;
            name
         ;
         value=(value-value1)/student;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   %if &qmethod=BC | &qmethod=BCA %then %do;

      %if &qmethod=BCA %then %do;

         %global _jackdat;
         %if %bquote(&_jackdat)^=%bquote(&_bootdat) %then %do;
            %jack(data=&_bootdat,stat=&stat,id=&id,alpha=&alpha,
                  chart=0,print=&print);
            %if &syserr>4 %then %goto exit;
         %end;

         *** estimate acceleration for BCa;
         proc means data=JACKDIST noprint vardef=df;
            %if %bquote(&stat)^= %then %do;
               var &stat;
            %end;
            output out=JACKSKEW(drop=_type_ _freq_ _sample_) skewness=;
            %if %bquote(&id)^= %then %do;
               by &id;
            %end;
         run;
         %if &syserr>4 %then %goto exit;

         *** transpose skewness;
         proc transpose data=JACKSKEW prefix=col
            out=_ACCEL_(rename=(col1=skewness _name_=name));
            %if %bquote(&stat)^= %then %do;
               var &stat;
            %end;
            %if %bquote(&id)^= %then %do;
               by &id;
            %end;
         run;
         %if &syserr>4 %then %goto exit;

         proc sort data=_ACCEL_;
            by %if %bquote(&id)^= %then &id; name ;
         run;
         %if &syserr>4 %then %goto exit;
      %end;

      *** estimate median bias for BC;
      data _BC_;
         retain _alpha _conf;
         drop value value1;
         if _n_=1 then do;
            _alpha=&alpha;
            _conf=100*(1-_alpha);
            call symput('conf',trim(left(put(_conf,best8.))));
         end;
         merge _ACTTR_(rename=(_name_=name))
               BOOTTRAN;
         by %if %bquote(&id)^= %then &id; name;
         if first.name then do; n=0; _z0=0; end;
         n+1;
         _z0+(value<value1)+.5*(value=value1);
         if last.name then do;
            _z0=probit(_z0/n);
            output;
         end;
      run;
      %if &syserr>4 %then %goto exit;

      *** compute percentiles;
      data BOOTPCTL;
         retain _i _lo _up _nplo _jlo _glo _npup _jup _gup
                alcl aucl;
         drop _alpha _sample_ _conf _i _nplo _jlo _glo _npup _jup _gup
              value;
         merge BOOTTRAN _BC_ %if &qmethod=BCA %then _ACCEL_;;
         by %if %bquote(&id)^= %then &id; name;
         label _lo='Lower Percentile Point'
               _up='Upper Percentile Point'
               _z0='Bias Correction (Z0)';
         if first.name then do;
            %if &qmethod=BC %then %do;
               _lo=probnorm(_z0+(_z0+probit(_alpha/2)));
               _up=probnorm(_z0+(_z0+probit(1-_alpha/2)));
            %end;
            %else %if &qmethod=BCA %then %do;
               drop skewness;
               retain _accel;
               label _accel='Acceleration';
               _accel=skewness/(-6*sqrt(&_nobs))*
                      (&_nobs-2)/&_nobs/sqrt((&_nobs-1)/&_nobs);
               _i=_z0+probit(_alpha/2);
               _lo=probnorm(_z0+_i/(1-_i*_accel));
               _i=_z0+probit(1-_alpha/2);
               _up=probnorm(_z0+_i/(1-_i*_accel));
            %end;
            _nplo=min(n-.5,max(.5,fuzz(n*_lo)));
            _jlo=floor(_nplo); _glo=_nplo-_jlo;
            _npup=min(n-.5,max(.5,fuzz(n*_up)));
            _jup=floor(_npup); _gup=_npup-_jup;
            _i=0;
         end;
         _i+1;
         if _glo then do;
            if _i=_jlo+1 then alcl=value;
         end;
         else do;
            if _i=_jlo then alcl=value;
            else if _i=_jlo+1 then alcl=(alcl+value)/2;
         end;
         if _gup then do;
            if _i=_jup+1 then aucl=value;
         end;
         else do;
            if _i=_jup then aucl=value;
            else if _i=_jup+1 then aucl=(aucl+value)/2;
         end;
         if last.name then do;
            output;
         end;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   %else %do;
      %local conf pctlpts pctlpre pctlname;
      %let pctlpre=a;
      %let pctlname=lcl ucl;
      data _null_;
         _alpha=&alpha;
         _conf=100*(1-_alpha);
         call symput('conf',trim(left(put(_conf,best8.))));
         %if &qmethod=PCTL %then %do;
            _lo=_alpha/2;
            _up=1-_lo;
         %end;
         %else %if &qmethod=HYBRID | &qmethod=T %then %do;
            _up=_alpha/2;
            _lo=1-_up;
         %end;
         _lo=100*_lo;
         _up=100*_up;
         call symput('pctlpts',trim(left(put(_lo,best8.)))||' '||
                               trim(left(put(_up,best8.))));
      run;
      %if &syserr>4 %then %goto exit;

      proc univariate data=BOOTTRAN noprint pctldef=5;
         var value;
         output out=BOOTPCTL n=n
            pctlpts=&pctlpts pctlpre=&pctlpre pctlname=&pctlname;
         by %if %bquote(&id)^= %then &id; name;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   data BOOTCI;
      retain &id name value alcl aucl confid method n;
      merge
         %if &qmethod=T
            %then _ACT_T_(rename=(_name_=name value1=value));
            %else _ACTTR_(rename=(_name_=name value1=value));
         BOOTPCTL;
      by %if %bquote(&id)^= %then &id; name;
      %if &qmethod=HYBRID %then %do;
         aucl=2*value-aucl;
         alcl=2*value-alcl;
      %end;
      %else %if &qmethod=T %then %do;
         aucl=value-aucl*student;
         alcl=value-alcl*student;
      %end;
      confid=&conf;
      length method $20;
      method='Bootstrap '||symget('method');
      label name  ='Name'
            value ='Observed Statistic'
            alcl  ='Approximate Lower Confidence Limit'
            aucl  ='Approximate Upper Confidence Limit'
            confid='Confidence Level (%)'
            method='Method for Confidence Interval'
            n     ='Number of Resamples'
            ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTCI label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%exit:
%mend bootci;


%******************************* ALLCI *******************************;
%macro allci(     /* Computes all types of confidence intervals
                     available in BOOTCI. Creates output data set
                     ALLCI. */
   stat=,         /* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     bootstrap distributions. */
   student=,      /* For the T method only, numeric variables in the
                     OUT= data set created by the %ANALYZE macro that
                     contain the standard errors of the statistics for which
                     you want to compute bootstrap distributions.
                     There must be a one-to-one between the VAR=
                     variables and the STUDENT= variables */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_ */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals */
   keep=,         /* Variables to keep in the output data set
                     containing the confidence intervals; can be used
                     to avoid warnings from PROC TRANSPOSE */
   print=1);      /* 1 to print the bootstrap confidence intervals;
                     0 otherwise. */

   %if %bquote(&keep)^= %then %let keep=(keep=&keep);

   %bootci(bca,stat=&stat,id=&id,alpha=&alpha,print=0)
   data ALLCI; set bootci&keep; run;

   %bootci(bc,stat=&stat,id=&id,alpha=&alpha,print=0)
   proc append data=bootci&keep base=ALLCI force; run;

   %bootci(pctl,stat=&stat,id=&id,alpha=&alpha,print=0)
   proc append data=bootci&keep base=ALLCI force; run;

   %bootci(hybrid,stat=&stat,id=&id,alpha=&alpha,print=0)
   proc append data=bootci&keep base=ALLCI force; run;

   %if %bquote(&student)^= %then %do;
      %bootci(t,stat=&stat,id=&id,student=&student,alpha=&alpha,print=0)
      proc append data=bootci&keep base=ALLCI force; run;
   %end;

   proc append data=bootstat&keep base=ALLCI force; run;
   proc append data=jackstat&keep base=ALLCI force; run;

   %if &print %then %do;
      proc print data=ALLCI label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%mend allci;



%macro bystmt;
   %let useby=1;
   by &by;
%mend bystmt;


%macro vardef;
   %let usevardf=1;
   vardef=&vardef
%mend vardef;




%macro bootin; /* INTERNAL USE ONLY
       input an observation from the original data set */
   %if %bquote(&residual)^= %then %do;
      array _r(&_nobs) _temporary_; /* residuals */
      do _i=1 to &_nobs;
         set &data point=_i;
         _r(_i)=&residual;
      end;
   %end;
   %else %do;
      drop _i;
   %end;
%mend bootin;


%macro bootout(obs); /* INTERNAL USE ONLY
       output an observation to the resampled data set */
   %if %bquote(&residual)^= %then %do;
      set &data point=_i;
      &residual=_r(&obs);
      &equation;
   %end;
   %else %do;
      set &data point=&obs;
   %end;
   output;
%mend bootout;















