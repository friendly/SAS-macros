
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

%macro bystmt;
   %let useby=1;
   by &by;
%mend bystmt;


