/************************************************************************
                            %POWER macro
                            Version 1.2

  DISCLAIMER:
    THIS INFORMATION IS PROVIDED BY SAS INSTITUTE,INC. AS A
    SERVICE TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO
    WARRANTIES, EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR
    FITNESS FOR A PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE
    MATERIALS OR CODE CONTAINED HEREIN.

  PURPOSE:
    Calculates the following power-related measures for retrospective and
    prospective analyses (see the DETAILS section below for definitions):

       - Effect size
       - Power for an effect test
       - Adjusted power and confidence limits
       - Least significant number
       - Power of least significant number

  USAGE:
    You must first run PROC GLM to fit the desired model and use the
    OUTSTAT= option to create the input data set for the %POWER macro.
    
    Then submit this file to the SAS System to define the %POWER macro. You
    can do this either by copying this file into the SAS program editor and
    submitting it, or by putting a %INCLUDE statement in your program before
    your call to the %POWER macro. The %INCLUDE statement should specify the
    path and filename of your local copy of this file. DO NOT CHANGE ANY
    PART OF THE MACRO CODE BELOW THIS COMMENT BLOCK. 

    Once the macro is defined, you can invoke the macro as follows (see
    also the EXAMPLE section below):

         %power(DATA=outstat_data_set,
                OUT=output_data_set,
                EFFECT=effect_name,
                CALCS=calculations_to_report,
                SS=type_sums_of_squares_to_use,
                ALPHA=list_of_significance_levels,
                N=list_of_sample_sizes,
                SIGMA=list_of_standards_deviations,
                DELTA=list_of_effect_sizes)

    where the macro parameters are:

      OUTSTAT_DATA_SET (REQUIRED) is the name of the data set created by the
        OUTSTAT= option in the previous run of PROC GLM.

      OUTPUT_DATA_SET (REQUIRED) is the name of a new data set into which
        %POWER will store all calculations. In the output data set, values of
        .N indicate statistics that were not calculated and values of .U
        indicate that the macro was unable to calculate the statistic.

      EFFECT_NAME (REQUIRED) is a single effect from the MODEL statement in
        the previous run of PROC GLM. Power calculations are performed for the
        specified effect.

      CALCULATIONS_TO_REPORT (REQUIRED) is one or more of the following,
      separated by spaces:
        POWER to request that power be computed and displayed
        ADJPOW to request that adjusted power be computed and displayed
        POWCI to request that a confidence interval for adjusted power be
          computed and displayed
        LSN to request that the least significant number be computed
          and displayed.
      
      TYPE_SUMS_OF_SQUARES_TO_USE in the power calculations should be one of
        these values: ss1, ss2, ss3, or ss4.  The default is ss3.  These sums
        of squares must have been computed by the previous run of PROC GLM.

      LIST_OF_SIGNIFICANCE_LEVELS is a list of values separated by spaces. The
        default value of 0.05.

      LIST_OF_SAMPLE_SIZES is a list of values separated by spaces. The
        default value is the observed sample size. Any values you specify are
        used in addition to the default.
        
      LIST_OF_STANDARDS_DEVIATIONS is a list of values separated by spaces.
        The default value is the observed standard deviation. Any values you
        specify are used in addition to the default.
        
      LIST_OF_EFFECT_SIZES is a list of values separated by spaces. The
        default value is the observed effect size. Any values you specify are
        used in addition to the default.
      
  PRINTED OUTPUT:
    The following is the output from the %POWER macro as used in the EXAMPLE
    section below. See the DETAILS section below for descriptions of the
    displayed statistics. Note that values of N indicate statistics that
    were not calculated and values of U indicate that the macro was unable
    to calculate the statistic.

    Of the four combinations of significance level and sample size in this
    example, the greatest power for detecting a DRUG effect occurs at the
    larger significance level (0.05) and the larger sample size (60). Notice
    that the Least Significant Number (LSN) is not recomputed for sample
    sizes other than the observed size (30) since it is not a function of
    sample size. The suggests a sample size of 70 in order to detect a
    significant DRUG effect assuming the same variance and effect size.
    However, notice that the power at the suggested sample size is still
    less than 0.6 which may cause you to want a sample size larger than 70.

                        Power Calculation for effect DRUG
                              Type 3 Sums of Squares

         Type I              Root Mean
          Error    Sample      Square     Effect    Power of    Adjusted
          Rate      Size       Error       Size       Test        Power

          0.01       30         4.01      1.5116     0.17573     0.06645
          0.01       60         4.01      1.5116     0.47671     0.30237
          0.05       30         4.01      1.5116     0.39681     0.19951
          0.05       60         4.01      1.5116     0.72205     0.54998

          Confidence     Confidence       Least        Power
          Interval:      Interval:     Significant      when
         Lower Limit    Upper Limit       Number       N=LSN

             0.01         0.97761           70        0.57050
             0.01         0.98257            N         N
             0.05         0.98207           46        0.59037
             0.05         0.98477            N         N


  DETAILS:
    Definitions of terms that are relevant to the %POWER macro:

    Prospective power analysis: Used in the planning phase of a designed
    experiment to determine how large the sample size must be to detect
    an effect of a given size (such as the minimum difference between
    treatment effects that is of practical value).

    Retrospective power analysis: Used after the analysis of an
    experiment to determine the power of the conducted test.

    Power: Is the probability that a false null hypothesis will be rejected.
    Ideally you would design your experiment to be as powerful as possible
    at detecting hypotheses of interest. Values of power range from 0 to 1,
    where values near 0 are low power and values near 1 are high power.
    Power is a function of the sample size (N), the effect size (delta), the
    root mean square error (sigma), and the significance level (alpha). The
    power tells you how likely your experiment is to detect a given
    difference, delta, at a given significance level, alpha. Power has the
    following characteristics:
    * If the true value of the parameter is the hypothesized value, the
      power should be alpha. You do not want to reject the null hypothesis
      when it is true.
    * If the true value of the parameters is not the hypothesized value,
      you want the power to be as large as possible.
    * The power increases with the sample size. The power increases as
      variance decreases. The power increases as the true parameter gets
      farther from the hypothesized value.

    Adjusted Power: Is for retrospective power analyses. The adjusted power
    is smaller than the power, as it removes the bias associated with the
    noncentrality parameter. The noncentrality paramater is biased for
    any value other than zero.  Because power is a function of
    population quantities that are not known, the usual practice is to
    substitute sample estimates in power calculations.  If you regard
    these sample estimates as random, you can adjust them to have a
    more proper expectation.  You can also construct a confidence
    interval for this adjusted power, though it is often very wide.  The
    adjusted power and confidence interval can only be computed for your
    observed effect size, delta.

    The Least Significant Number (LSN) is the number of observations needed
    to reduce the variance of the estimates enough to achieve a significant
    result with the given values of alpha, sigma, and delta. If you need
    more data to achieve significance, the LSN helps tell you how many more.
    The LSN has the following characteristics:
    * If the LSN is less than the actual sample size N, then the
      effect is significant. This means that you have more data than
      you need to detect the significance at the given alpha level.
    * If the LSN is greater than the actual sample size N, the effect
      is not significant. In this case, if you believe that more data
      will show the same variance and structural results as the current
      sample, the LSN suggests how much data you would need to achieve
      significance.
    * If the LSN is equal to N, then the p-value is equal to the
      significance level, alpha. The test is on the border of significance.
    * Power calculated when N=LSN is always greater than or equal to 0.5.

    Power when N=LSN represents the power associated with using the N
     recommended by the LSN. 

    The noncentrality parameter, lambda, is N*delta^2/sigma^2, where N is
    the total sample size, delta is the effect size, and sigma^2 is the mean
    square error. Note that the noncentrality parameter is zero when the
    null hypothesis is true, that is, when the effect size is zero.

    The Effect Size, delta, is estimated from the data as
    sqrt[ SS(Hypothesis)/N ]. The effect size can be thought of as the
    minimum difference in means that you want to detect divided by the total
    sample size.

  LIMITATIONS:
    The %POWER macro is appropriate for fixed-effect linear models fit by
    PROC GLM only. IT IS NOT APPROPRIATE FOR PROC GLM MODELS USING THE
    RANDOM, TEST, REPEATED, OR MANOVA STATEMENTS.

    The %POWER macro does not accept a given power value as input and report
    the required sample size. Refer to the prospective power analysis
    example in the file POWEREX.SAS.

    No error checking is done.  Be careful to correctly specify the
    macro parameters.

  REFERENCES:
    Futher information on the %POWER macro, and additional references, can
    be found in the paper, "%Power: A Simple Macro for Power and Sample Size
    Calculations" available as Technical Support document TS-272 at the
    following web address:

      http://www.sas.com/service/techsup/tnote/tnote_stat.html                 

  EXAMPLE:
    Following is an example of a retrospective power analysis.  Output
    from the %POWER macro is displayed in the PRINTED OUTPUT section
    above.

    These data come from an experiment on leprosy (Snedecor & Cochran 1967).
    The pre- and post-treatment leprosy scores, X and Y respectively, are
    recorded for ten patients on each of three drugs.  The total sample
    size (N) is 30.

    The following statements examine the power for testing the DRUG effect
    on the post-treatment response (Y) in the presence of the covariate X.
    The power analysis is requested at the four combinations of sample size
    (actual (30) or 60) and significance level (0.01 or 0.05).
    
        data drugs;
          input drug $ x y @@;
          cards;
        A 11 6 A 8 0 A 5 2 A 14 8 A 19 11
        A 6 4 A 10 13 A 6 1 A 11 8 A 3 0
        B 6 0 B 6 2 B 7 3 B 8 1 B 18 18
        B 8 4 B 19 14 B 8 9 B 5 1 B 15 9
        C 16 13 C 13 10 C 11 18 C 9 5 C 21 23
        C 16 12 C 12 5 C 12 16 C 7 1 C 12 20
        ;

        proc print data=drugs;
          by drug;
          id drug;
          title 'DRUGS Data for Retrospective Power Calculations Macro';
          run;
        title;

        proc glm data=drugs outstat=drugout noprint;
          class drug;
          model y=drug x;
          run;

        proc print data=drugout;
          title 'OUTSTAT data set from GLM on DRUGS Data';
          run;
        title;

        %power (data=drugout,
                 out=drugpow,
              effect=drug,
               calcs=power adjpow powci lsn,
                  ss=ss3,
               alpha=0.01 0.05,
                   n=60);
 
************************************************************************/

%macro power(data=,
              out=,
           effect=,
            calcs=,
               ss=ss3,
            alpha=0.05,
                n=,
            sigma=,
            delta=,
            debug=);

  %if &sysver >= 7 %then %str(OPTIONS VALIDVARNAME=V6;);
  options nonotes;

  %let calcs=%upcase(&calcs);
  %let ss=%upcase(&ss);

* GET ERROR AND HYPOTHESIS DF AND
  SS FROM OUTSTAT DATA SET;
  data _null_;
    set &data(where=(_type_ in ("&ss",'ERROR')))
                end=lastobs;

    if _n_=1 then do;
      dfr=0;
      norig=0;
    end;

    norig+df;
    select(upcase(_source_));
      when ('ERROR') do;
        call symput('dfeorig',left(put(df,8.)));
        if ss gt 0 then
           call symput('sigorig',
              left(put(sqrt(ss/df),14.2)));
        else call symput('sigorig','');
      end;
      when (upcase("&effect")) do;
        dfr+df;
        call symput('dfh',left(put(df,8.)));
        call symput('ssh',left(put(ss,14.2)));
        call symput('fsamp',left(put(f,14.2)));
      end;
      otherwise do;
        dfr+df;
      end;
    end;

    if lastobs then do;
      call symput('dfr',left(put(dfr,8.)));
      call symput('norig',left(put((norig+1),8.)));
    end;
  run;

* PUT ORIGINAL DELTA INTO MACRO VARIABLE;
  data _null_;
    delta=sqrt(&ssh/&norig);
    call symput('delorig',left(put(delta,16.4)));
    stop;
  run;

* PREPEND VALUES FOR N SIGMA DELTA THAT
  OCCUR IN DATA TO USER-SPECIFIED VALUES;
  %let n=&norig &n;
  %let sigma=&sigorig &sigma;
  %let delta=&delorig &delta;
  %if &debug ne %then %do;
   %put n=&n;
   %put sigma=&sigma;
   %put delta=&delta;
   %put alpha=&alpha;
   %put dfeorig=&dfeorig;
   %put sigorig=&sigorig;
   %put dfr=&dfr;
   %put dfh=&dfh;
   %put ssh=&ssh;
   %put fsamp=&fsamp;
  %end;




* CREATE COMMA DELIMITTED LIST FOR USER-SPECIFIED
  VALUES OF ALPHA, N, SIGMA, DELTA;
  %let given=alpha n sigma delta;

  %do i=1 %to 4;
     %let current=%scan(&given,&i,%str( ));
     %comma(&&&current,&current);
  %end;

options notes;
* PERFORM CALCULATIONS;
  data &out;
    do alpha=&alpha;
      do number=&n;
         dfe=number-&dfr-1;
         do sigma=&sigma;
            do delta=&delta;

          * CALCULATE LAMBDA AND POWER;
            lambda=(number*delta**2)/(sigma**2);
            astar=1-alpha;
            fcrit=finv(astar,&dfh,dfe);
            if lambda>135 then power=1.0;
            else power=
               1-probf(fcrit,&dfh,dfe,lambda);


          * CALCULATE ADJUSTED LAMBDA AND
            CI FOR ORIGINAL DELTA;
            if delta=&delorig and
               index("&calcs",'ADJPOW')>0 then do;
               lamadj=max(0,(lambda*(&dfeorig-2)/&dfeorig)-&dfh);
               if lamadj>135 then adjpow=1.0;
               else adjpow=1-probf(fcrit,&dfh,dfe,lamadj);
            end;
            else if delta=&delorig then adjpow=.U;
            else do;
               adjpow=.N;
            end;


          * GET CI ON LAMBDA AND POWER;
            if index("&calcs",'POWCI')>0 and
               adjpow not in (.N,.U) then do;
              %if &fsamp=I %then %do;
                powlow=.U; powup=.U;
              %end;
              %else %do;
               lamlow=&dfh*(max(0,(sqrt(&fsamp)-sqrt(fcrit))))**2;
               if lamlow>135 then powlow=1.0;
               else powlow=1-probf(fcrit,&dfh,dfe,lamlow);

               lamup=&dfh*(sqrt(&fsamp)+sqrt(fcrit))**2;
               if lamup>135 then powup=1.0;
               else powup=1-probf(fcrit,&dfh,dfe,lamup);
              %end;
            end;
            else do;
               powlow=.N;
               powup=.N;
            end;

          * FIND LEAST SIGNIFICANT N;
            if number=&norig and
               index("&calcs",'LSN')>0 then do;
               niter=&dfr+2;
               lstar=(niter*delta**2)/(&dfh*sigma**2);

               do until (diff<0.0000001);
                 niter=niter+1;
                 errn=niter-&dfr-1;
                 lstar=(niter*delta**2)/(&dfh*sigma**2);
                 diff=astar-probf(lstar,&dfh,errn);
               end;

               lsn=niter;
               lsndfe=lsn-&dfr-1;
               lamblsn=(lsn*delta**2)/(sigma**2);
               astar=1-alpha;
               fcrit=finv(astar,&dfh,lsndfe);
               if lamblsn>135 then powlsn=1.0;
               else powlsn=1-probf(fcrit,&dfh,lsndfe,lamblsn);
            end;
            else do;
               lsn=.N;
               powlsn=.N;
            end;

            output;

            end; *delta;
         end;    *sigma;
      end;       *number;
    end;         *alpha;

    label  alpha='Type I Error Rate'
          number='Sample Size'
           sigma='Root Mean Square Error'
           delta='Effect Size'
           power='Power of Test'
          adjpow='Adjusted Power'
          powlow='Confidence Interval: Lower Limit'
           powup='Confidence Interval: Upper Limit'
             lsn='Least Significant Number'
          powlsn='Power when N=LSN';

    keep alpha number sigma delta
       %if %index(&calcs,POWER)>0 %then %str(power);
       %if %index(&calcs,ADJPOW)>0 %then %str(adjpow);
       %if %index(&calcs,POWCI)>0 %then %str(powlow powup);
       %if %index(&calcs,LSN)>0 %then %str(lsn powlsn);;
  run;

options nonotes;
  %let ssnum=%substr(&ss,3,1);
* PRINT RESULTS;
  proc print data=&out noobs label;
    var alpha number sigma delta
        %if %index(&calcs,POWER)>0 %then %str(power);
        %if %index(&calcs,ADJPOW)>0 %then %str(adjpow);
        %if %index(&calcs,POWCI)>0 %then %str(powlow powup);
        %if %index(&calcs,LSN)>0 %then %str(lsn powlsn);;
    title1 "Power Calculation for effect %upcase(&effect)";
    title2 "Type &ssnum Sums of Squares";
  run;
  title;
options notes;
%mend power;

* MACRO TO ADD COMMAS AFTER VALUES
  SPECIFIED FOR ALPHA, N, SIGMA, DELTA;
  %macro comma(string,varname);
    %*global &varname;
    %local count word;
    %let count=1;
    %let word=%qscan(&string,&count,%str( ));
    %let &varname=&word;
    %do %while (&word ne);
       %let count=%eval(&count+1);
       %let word=%qscan(&string,&count,%str( ));
       %if (&word ne) %then
           %let &varname=&&&varname, &word;
    %end;
    %if &debug ne %then %put &varname = &&&varname;
  %mend comma;
