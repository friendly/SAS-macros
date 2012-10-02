 /*-------------------------------------------------------------------*
  *    Name: multnorm.sas                                             *
  *   Title: Tests and Plots for Multivariate Normality               *
  *     Doc: http://datavis.ca/sasmac/multnorm.html        *
  *                                                                   *
  *  minimal syntax: %multnorm (var=variables);                       *
  *-------------------------------------------------------------------*
  *  Author:  SAS Institute                                           *
  * Revised:  24 Oct 2010 12:45:24                                    *
  * Version:  1.2-1                                                   *
  *  1.2 - Changed default PPLOT=no                                   *
  *      - Cleaned up temp datasets                                   *
  *      - made VAR=_NUMERIC_ the default
  *      - Fixed problem with long variable names (VALIDVARNAME=V7)              
  *-------------------------------------------------------------------*/
/***************************************************************************
          %MULTNORM macro:  Mardia Tests of Multivariate Normality


   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   REQUIRES:
     For multivariate normality tests:  Version 6.06 or later of SAS/IML
       Software or Version 6.09 TS450 or later (except Version 6.10) of
       SAS/ETS Software.  If neither is found, only univariate normality
       tests can be provided.

     For optional plot:  Version 6 SAS/STAT Software if SAS/IML is not
       available.  Version 6 SAS/GRAPH Software if high-resolution plot
       is desired.

   ABSTRACT:
     The %MULTNORM macro performs tests of multivariate normality based
     on multivariate skewness and kurtosis statistics (Mardia 1974).  A
     test of univariate normality is also given for each of the
     variables.  A chi-square quantile-quantile plot of the
     observations' squared Mahalanobis distances can be obtained
     allowing a visual assessment of multivariate normality.

   USAGE:
     The options and allowable values are:

        DATA=   SAS data set to be analyzed.  If the DATA= option is not
                supplied, the most recently created SAS data set is
                used.

        VAR=    REQUIRED.  The list of variables to use when testing for
                univariate and multivariate normality.  Individual
                variable names, separated by blanks, may be specified,
					 or you may use special variable lists (such as
                VAR1-VAR10, ABC--XYZ, _NUMERIC_).

        ID=     Name of ID variable for plots

        PPLOT=   Requests a chi-square quantile-quantile (Q-Q) plot of
                the squared Mahalanobis distances of observations from
                the mean vector.  PLOT=yes requests the
                plot.  PLOT=no (the default) suppresses the plot.

        GPLOT=  requests that high-resolution graphics (PROC GPLOT) be
                used when creating the plot.  You must set the graphics
                device (GOPTIONS DEVICE=) and any other graphics-related
                options before invoking the macro.  

   PRINTED OUTPUT:
     If the IML procedure is found, a single table titled "Univariate
     and Multivariate Normality Tests" is printed.  If the MODEL
     procedure is found, a table titled "Normality Test" is printed and
     previous output can be ignored.  If neither procedure is found, a
     single table titled "Univariate Normality Tests" is printed.

     For p variables listed in the VAR= option, the first p rows of the
     printed table contain univariate tests of normality, including the
     name of the test (Shapiro-Wilk or Kolmogorov), the value of the
     test statistic and the corresponding p-value.  The next two lines
     of the table contain Mardia's tests of multivariate normality,
     including the name of the test (skewness or kurtosis), the values
     of the multivariate skewness and kurtosis statistics (when IML is 
     used), the values of the test statistics and their p-values.  When 
     the MODEL procedure is used, another test of multivariate normality 
     is performed -- the Henze-Zirkler test.  This test is discussed in 
     "SAS/ETS Software, Changes and Enhancements, Release 6.11" on pages
     52-52 and in the reference below.

     For multivariate normal data, Mardia (1974) shows that the expected
     value of the multivariate skewness statistic is

            p(p+2)[(n+1)(p+1)-6]
            --------------------
                 (n+1)(n+3)

     and the expected value of the multivariate kurtosis statistic is

            p(p+2)(n-1)/(n+1)  .

     If PLOT=yes is specified, a chi-square quantile-quantile plot is
     produced which plots the squared mahalanobis distances against
     corresponding quantiles of the limiting chi-square distribution.
     If the data are distributed as multivariate normal, then the points
     should fall on a straight line with slope one and intercept zero.
     See DETAILS for more information.

   DETAILS:
     If a set of variables is distributed as multivariate normal, then
     each variable must be normally distributed.  However, when all
     individual variables are normally distributed, the set of variables
     may not be distributed as multivariate normal.  Hence, testing each
     variable only for univariate normality is not sufficient.  Mardia
     (1974) proposed tests of multivariate normality based on sample
     measures of multivariate skewness and kurtosis.

     Univariate normality is tested by either the Shapiro-Wilk W test or
     the Kolmogorov-Smirnov test.  For details on the univariate tests
     of normality, refer to "Tests for Normality" in "The UNIVARIATE
     Procedure" chapter in the SAS Procedures Guide.

     If the p-value of any of the tests is small, then multivariate
     normality can be rejected.  However, it is important to note that
     the univariate Shapiro-Wilk W test is a very powerful test and is
     capable of detecting small departures from univariate normality
     with relatively small sample sizes.  This may cause you to reject
     univariate, and therefore multivariate, normality unnecessarily if
     the tests are being done to validate the use of methods that are
     robust to small departures from normality.

     When the data's covariance matrix is singular, the macro quits and
     the following message is issued:

        ERROR: Covariance matrix is singular.

     The PRINCOMP procedure in SAS/STAT Software is required for this
     check.  If it is not found, a message is printed, nonsinularity
     is assumed and the macro attempts to perform the tests.

     For p variables and a large sample size, the squared Mahalanobis
     distances of the observations to the mean vector are distributed as
     chi-square with p degrees of freedom.  However, the sample size
     must be quite large for the chi-square distribution to obtain
     unless p is very small.  Also, this plot is sensitive to the
     presence of outliers.  So, this plot should be cautiously used as a
     rough indicator of multivariate normality.

   MISSING VALUES:
     Observations with missing values are omitted from the analysis and
     plot.

   LIMITATIONS:
     LIMITED ERROR CHECKING is done.  If the DATA= option is specified,
     be sure the named data set exists.  If DATA= is not specified, a
     data set must have been created previously in the current SAS
     session.  Be sure that the variables specified in the VAR= option
     exist on that data set.  Running PROC CONTENTS on the data set
     prior to using this macro is recommended for verifying the data set
     name and the names of variables.  Use the full words 'yes' or 'no'
     on the PLOT= and HIRES= options.

     MEMORY USAGE for the multivariate tests is quadratically related to
     the number of observations.  Moderately large data sets (1000 is
     big) may cause "Unable to allocate sufficient memory" errors.

   REFERENCES:
     Henze, N. and Zirkler, B. (1990),"A Class of Invariant Consistant 
       tests for Multivariate Normality," Commun. Statist.-Theory Meth., 
       19(10), pp. 3595-3617.
     Mardia (1974),"Applications of some measures of multivariate skewness
       and kurtosis in testing normality and robustness studies", Sankhya B,
       36, pp 115-128.
     Mardia (1975),"Assessment of Multinormality and the Robustness of
       Hotelling's T-squared Test", Applied Statistics, 1975, 24(2).
     Mardia, Kent, Bibby (1979),"Multivariate Analysis", Academic Press
     Mardia (1980),"Measures of Multivariate Skewness and Kurtosis with
       Applications", Biometrika 57(3).
     Royston, J.P. (1982), "An Extension of Shapiro and Wilk's W Test
       for Normality to Large Samples," Applied Statistics, 31.
     Shapiro, S.S. and Wilk, M.B. (1965), "An Analysis of Variance Test
       for Normality (complete samples)," Biometrika, 52.

   EXAMPLE:
     The following example is from Mardia, Kent, Bibby (1979, pg 12).
     The multivariate skewness test statistic differs due to the use in
     %MULTNORM of an alternative approximation to the chi-square
     distribution given in Mardia (1974, pg 123).


         data cork;
           input n e s w @@;
           cards;
         72 66 76 77   91 79 100 75
         60 53 66 63   56 68 47 50
         56 57 64 58   79 65 70 61
         41 29 36 38   81 80 68 58
         32 32 35 36   78 55 67 60
         30 35 34 26   46 38 37 38
         39 39 31 27   39 35 34 37
         42 43 31 25   32 30 30 32
         37 40 31 25   60 50 67 54
         33 29 27 36   35 37 48 39
         32 30 34 28   39 36 39 31
         63 45 74 63   50 34 37 40
         54 46 60 52   43 37 39 50
         47 51 52 43   48 54 57 43
         ;

         %multnorm(data=cork,var=n e s w)

****************************************************************************/

%macro multnorm (
       data=_last_ ,    /*            input data set           */
       var=_numeric_,    /* REQUIRED:  variables for test       */
                        /* May be a list e.g. var1-var10       */
       id=,
       pplot=no     ,   /*    create chi-square plot?          */
       gplot=yes        /*    high resolution plot?            */
                );

    %*-- Reset required global options;
    %if %sysevalf(&sysver  >= 7) %then %do;
        %local o1 o2;
        %let o1 = %sysfunc(getoption(notes));
        %let o2 = %sysfunc(getoption(validvarname,keyword));
        options nonotes validvarname=V7;
        %end;
    %else %do;
       options nonotes;
        %end;


%local cleanup;
%let lastds=&syslast;
%let pplot=%upcase(&pplot);
%let gplot=%upcase(&gplot);

%if %upcase(&data)=_DATA_ %then %let data=&syslast;

/* Verify that VAR= option is specified */
%if %length(&var)=0 %then %do;
    %put ERROR: You must specify some test variables in the VAR= argument;
    %goto exit;
%end;

*-- Parse variables list if it contains special lists;
%if %index(&var,-) > 0 or %upcase("&var")="_NUMERIC_" %then %do;
 data _null_;
 set &data (obs=1);
        %*  convert shorthand variable list to long form;
     length _vname_ $ 32 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
     call symput( 'VAR', trim(_vlist_) );
	  put 'NOTE: VAR= list translated to: VAR=' _vlist_;
 RUN;
%end;

/* Parse VAR= list into separate macro variables */
%let _i=1;
%do %while (%scan(&var,&_i) ne %str() );
   %let arg&_i=%scan(&var,&_i);
   %let _i=%eval(&_i+1);
%end;
%let nvar=%eval(&_i-1);

/* Remove observations with missing values */
%put NOTE: Removing observations with missing values...;
data _nomiss;
  set &data end=eof;
  if nmiss(of &var ) > 0
  		then do;
		_miss_+1;
		end;
	else do;
		output;
		end;
  if eof then do;
  		call symput('nmiss', trim(left(put(_miss_, 5.))));
  		end;
  run;
%put NOTE: ... &nmiss observations deleted.;

/* Quit if covariance matrix is singular */
%let singular=nonsingular;
%put NOTE: Checking for singularity of covariance matrix...;
proc princomp data=_nomiss outstat=_evals noprint;
  var &var ;
  run;
  %let cleanup=_evals;
%if &syserr=3000 %then %do;
  %put NOTE: PROC PRINCOMP required for singularity check.;
  %put NOTE: Covariance matrix not checked for singularity.;
  %let cleanup=;
  %goto findproc;
%end;
data _null_;
  set _evals;
  where _TYPE_='EIGENVAL';
  if round(min(of &var ),1e-8)<=0 then do;
    put 'ERROR: Covariance matrix is singular.';
    call symput('singular','singular');
  end;
  else put 'NOTE: Covariance matrix is non-singular';
  run;
%if &singular=singular %then %goto exit;

%findproc:
/* Is IML or MODEL available for analysis? */
%let mult=yes; %let multtext=%str( and Multivariate);
%put NOTE: Checking for necessary procedures...;
proc iml; quit;
%if &syserr=0 %then %do;
	%put NOTE: ... using SAS/IML;
	%goto iml;
	%end;
proc model; quit;
%if &syserr=0 and
    (%substr(&sysvlong,1,9)>=6.09.0450 and %substr(&sysvlong,3,2) ne 10)
    %then %goto model;

%put NOTE: SAS/ETS PROC MODEL with NORMAL option or SAS/IML is required;
%put %str(     ) to perform tests of multivariate normality.  Univariate;
%put %str(     ) normality tests will be done.;
%let mult=no; %let multtext=;
%goto univar;


%iml:
proc iml;
  reset;
  use _nomiss;  read all var {&var} into x;   /* input data */

  /* compute mahalanobis distances */
  n=nrow(x); p=ncol(x);
  _c=x-j(n,1)*x[:,];         /* centered variables    */
  _s=(_c`*_c)/n;               /* covariance matrix     */
  _rij=_c*inv(_s)*_c`;          /* mahalanobis angles    */

  /* get values for probability plot and output to data set */
  %if &pplot=YES or &gplot=YES %then %do;
  _d=vecdiag(_rij#(n-1)/n);   /* squared mahalanobis distances */
  _rank=ranktie(_d);            /* ranks of distances    */
  _chi=cinv((_rank-.5)/n,p);  /* chi-square quantiles  */
  _chiplot=_d||_chi;
  create _chiplot from _chiplot [colname={'distance' 'CHISQ'}];
  append from _chiplot;
  %end;

  /* Mardia tests based on multivariate skewness and kurtosis */
  _b1p=(_rij##3)[+,+]/(n##2);                    /* skewness */
  _b2p=trace(_rij##2)/n;                         /* kurtosis */
  _k=(p+1)#(n+1)#(n+3)/(n#((n+1)#(p+1)-6)); /* small sample correction */
  _b1pchi=_b1p#n#_k/6;                           /* skewness test statistic */
  _b1pdf=p#(p+1)#(p+2)/6;                      /*   and df                */
  _b2pnorm=(_b2p-p#(p+2))/sqrt(8#p#(p+2)/n); /* kurtosis test statistic */
  probb1p=1-probchi(_b1pchi,_b1pdf);             /* skewness p-value */
  probb2p=2*(1-probnorm(abs(_b2pnorm)));         /* kurtosis p-value */

  /* output results to data sets */
  names={"Mardia Skewness","Mardia Kurtosis"};
  create names from names [colname='TEST'];
  append from names;
  probs=(n||_b1p||_b1pchi||probb1p) // (n||_b2p||_b2pnorm||probb2p);
  create _values from probs [colname={'N' 'VALUE' 'STAT' 'PROB'}];
  append from probs;
quit;

data _mult;
  merge names _values;
  variable='_ALL_';
  if index(test,'Skewness')>0 then  skew=value;
  else kurt=value;
  run;
%let cleanup = &cleanup names _values _mult;

%univar:
/* get univariate test results */
proc univariate data=_nomiss noprint;
  var &var;
  output out=_skew skewness=&var ;
  output out=_kurt kurtosis=&var ;
  output out=_stat normal=&var ;
  output out=_prob  probn=&var ;
  output out=_n         n=&var ;
  run;
data _univ;
  set _stat _prob _n _skew _kurt;
  run;

proc transpose data=_univ name=variable
     out=_tuniv(rename=(col1=stat col2=prob col3=n col4=skew col5=kurt));
   var &var ;
   run;

data _both;
  length test $15.;
  set _tuniv
      %if &mult=yes %then _mult;;
  if test='' then if n<=2000 then test='Shapiro-Wilk';
                  else test='Kolmogorov';
  run;

proc print data=_both noobs split='/';
  var variable n test skew kurt   /* %if &mult=yes %then value; */
      stat prob;
  title2 "Univariate&multtext Normality Tests";
  label variable="Variable"
            test="Test"       %if &mult=yes %then
           value="Skewness &/Kurtosis";
            stat="Test/Statistic/Value"
				skew='Skewness'
				kurt='Kurtosis'
            prob="p-value";
  run;

%let cleanup = &cleanup _skew _kurt _stat _prob _n _univ _tuniv _both;
*put cleanup = &cleanup;
%goto plotstep;

%model:
/* Multivariate and Univariate tests with MODEL */
proc model data=_nomiss;
  %do _i=1 %to &nvar;
    &&arg&_i = _a;
  %end;
  fit &var / normal;
  title2 "Univariate&multtext Normality Tests";
  run;

%plotstep:
/* Create a chi-square Q-Q plot
   NOTE: Very large sample size is required for chi-square asymptotics
   unless the number of variables is very small.
*/
*put Cleaning up... _nomiss &cleanup;
proc datasets nofs nolist;
   delete _nomiss &cleanup;
	run; quit;

%if &pplot=YES or &gplot=YES %then %do;
title2 "Chi-square Q-Q plot of &data";
title2;
	%cqplot(data=&data, var=&var, nvar=&nvar, pplot=&pplot, gplot=&gplot, id=&id,
		detrend=NO);
%end;

%exit:
options _last_=&lastds;
title;
    %*-- Restore global options;
    %if %sysevalf(&sysver  >= 7) %then %do;
        options &o1 &o2;
        %end;
    %else %do;
       options notes;
        %end;


%mend;

