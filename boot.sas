/*
Macros for bootstrap analysis, abstracted from the jackboot macro
Documentation:  http://support.sas/com/kb/24/982.html

-- Added gskip;

*/

/*
Introduction
------------

The %BOOT macro does elementary nonparametric bootstrap analyses for
simple random samples, computing approximate standard errors,
bias-corrected estimates, and confidence intervals assuming a normal
sampling distribution. Also, for regression models, the %BOOT macro can
resample either observations or residuals.

The %BOOTCI macro computes several varieties of confidence intervals
that are suitable for sampling distributions that are not normal.

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

Using %BOOT
---------------------

To use the %BOOT macro, you must write a macro called %ANALYZE
(or something else, as specified by the ANALYSE= parameter)
to do the data analysis that you want to bootstrap. The %ANALYZE macro
must have two arguments:

   DATA=   the name of the input data set to analyze
   OUT=    the name of the output data set containing the statistics
           for which you want to compute bootstrap distributions.

For example, the following macro could be used to bootstrap a regression
analysis of Y on X

   %macro analyze(data=, out=);
      options nonotes;
    	proc reg noprint data=&data outest=&out;
			model y = x;
			&bystmt;
		run;
      options notes;
   %mend;

*/


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
   chart=0,       /* 1 to chart the bootstrap resampling distributions;
                     0 otherwise. */
   gchart=1,       /* 1 to chart the bootstrap resampling distributions;
                     0 otherwise. */
	analyze=analyze  /* name of macro to do one resampled analysis */
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
   %&analyze(data=&data,out=_ACTUAL_);
   %if &syserr>4 %then %goto exit;

   *** compute plug-in estimates;
   %if &usevardf %then %do;
      %let vardef=N;
      %&analyze(data=&data,out=_PLUGIN_);
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
%put _NOBS= &_nobs;
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
      %&analyze(data=BOOTDATA,out=BOOTDIST);
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

   %if &gchart %then %do;
      %if %bquote(&id)^= %then %do;
         proc sort data=BOOTDIST; by &id; run;
         proc univariate data=BOOTDIST(drop=_sample_) noprint;
			histogram &stat / kernel(l=1 c=mise) normal(l=3);
			inset mean std p5 p95;
            by &id;
         run;
		%gskip;
      %end;
      %else %do;
	  /*  show mean & percentiles graphically
	  */
	     proc summary data=BOOTDIST;
		 	var &stat;
			output out=_stats_ mean= p5= p95= /autoname;
		*proc print;

/*
	TODO:  Find list of &stats
	use %do over stats, adding VREF for mean, p5, p95
*/
         proc univariate data=BOOTDIST(drop=_sample_) noprint;
			histogram &stat / kernel(l=1 c=mise) normal(l=3);
			inset mean std p5 p95;
         run;
		%gskip;
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

   %global _nobs;    ** MF **;
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
/*
* put _sample_= _i= _k= _l= @30 %do i=1 %to &_nobs; _c(&i) %end;;
*/
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

      %&analyze(data=_TMPD_,out=_TMPS_);
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
   %global _nobs;
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

