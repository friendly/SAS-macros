/************************************************************************

                              MAGREE macro
                                  V1.0
                                      

  DISCLAIMER:
    THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
    ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
    EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
    PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
    CONTAINED HEREIN.

  PURPOSE:
    Compute estimates and tests of agreement among multiple raters when
    responses (ratings) are on a nominal or ordinal scale. For a nominal
    or ordinal response, kappa statistics can be computed. For a
    numerically-coded, ordinal response, Kendall's coefficient of
    concordance is computed. 

  REQUIRES:
    Release 6.11 TS020 or later of Base SAS and SAS/STAT Software are
    required. %MAGREE can also be run under Release 6.09 TS450 or later on
    MVS, VMS, or CMS. 

  USAGE:
    You must first define the %MAGREE macro so that it is available for
    use. Simply submit this file to the SAS System to define the macro.
    You may then invoke the macro. The EXAMPLE section below illustrates
    how this is done. 

    The input (DATA=) data set should contain one observation per rating
    (i.e. an observation contains one rating on one subject by one rater)
    with variables containing subject and rater identifiers and a variable
    containing the rating (response). The same set of rater values must be
    used for each subject, even if subjects are rated by different raters
    (which is only valid for kappa statistics). 

    The following parameters are required:

        items=        Specifies the variable containing subject (item)
                      identifiers.  This variable may be numeric or 
                      character.

        raters=       Specifies the variable containing rater
                      identifiers.  This variable may be numeric or 
                      character.  The same rater values must be used
                      for all subjects even if subjects are rated by 
                      different raters.  Note that Kendall's statistic
                      is only valid when all subjects are rated by the
                      same raters.

        response=     Specifies the variable containing the ratings.
                      This variable may be numeric or character. If it 
                      is character, it is assumed to be nominal and
                      only kappa statistics are computed.  If it is
                      numeric, either or both statistic can be
                      computed, but note that Kendall's statistic is
                      only valid for ordinal responses.

    The following parameter is optional:

        data=         Identifies the input data set to be analyzed.
                      If not specified, the last-created data set is
                      used.
 
        stat=         Specifies which statistic to calculate.  Use
                      stat=kappa to compute kappa statistics.  Use
                      stat=kendall to compute Kendall's coefficient of
                      concordance.  Use stat=both to compute both
                      statistics.  By default, stat=both.

    
  PRINTED OUTPUT:
    Following is selected output from the EXAMPLE presented below. The
    first ten observations corresponding to the first two subjects are
    shown to illustrate the proper form of the input data set. 

                    Analysis of data from Fleiss (1981)
                       Proper form of input data set  

                                S    R    Y

                                1    1    1
                                1    2    2
                                1    3    2
                                1    4    2
                                1    5    2
                                2    1    1
                                2    2    1
                                2    3    3
                                2    4    3
                                2    5    3

    If the response variable is numeric, as it is in this example, and
    if it is also ordinal, then both kappa and Kendall's statistics are
    valid.  If this response were only nominal, then Kendall's statistic
    should be ignored or not computed by specifying the stat=kappa
    parameter.  Both the overall kappa statistic and Kendall's
    coefficient of concordance are highly significant indicating stronger
    agreement than can be expected by chance.  The kappa statistics for
    the individual response categories indicate the degree of agreement on
    each category.  In this example, agreement is strongest on category 2.

                    Analysis of data from Fleiss (1981)
                               MAGREE macro
                   Kappa statistics for nominal response

                                  Standard
               Y        Kappa       Error        Z       Prob>Z

                  1    0.29167     0.10000    2.91667    0.0018
                  2    0.67105     0.10000    6.71053    0.0001
                  3    0.34896     0.10000    3.48958    0.0002
            Overall    0.41789     0.07165    5.83220    0.0001


                    Analysis of data from Fleiss (1981) 
                               MAGREE macro
         Kendall's Coefficient of Concordance for ordinal response

               Coeff of                Num    Denom
             Concordance       F        DF      DF     Prob>F

               0.49058      3.85214    8.6     34.4    0.0021

  DETAILS:
    The methodology implemented by the %MAGREE macro is presented in
    Fleiss (1981) and Kendall (1955). See these references for details. 

    Unlike the AGREE option in PROC FREQ, weighted kappa and confidence
    intervals are not provided by this macro. 

    Note that the RATERS= variable must use the same values to identify
    the raters in all subjects even if different raters are used to rate
    the subjects, which is only valid for the kappa statistic. Both
    statistics require that the number of raters be the same across all
    subjects. 

    Ties are only a concern with Kendall's statistic and are handled by
    the macro. Tests provided for the kappa statistics and Kendall's 
    statistic are asymptotic (large sample) tests.

    Landis and Koch (1977) attempted to indicate the degree of agreement
    that exists when kappa is found to be in various ranges:

         <=0    Poor 
         0-.2   Slight 
        .2-.4   Fair 
        .4-.6   Moderate 
        .6-.8   Substantial 
        .8-1    Almost perfect 

    Values of all statistics and their p-values are available in data sets
    after execution of the macro. The kappa estimates and related
    statistics are in the data set _KAPPAS. Kendall's coefficient and
    related statistic are in the data set _W. 

  LIMITATIONS:
    Limited error checking is done. Be sure that the ITEMS=, RATERS=, and
    RESPONSE= variables are properly spelled and exist in the DATA= data
    set. You can use PROC CONTENTS to verify the variable names in a data
    set. 

    The set of values in the RATERS= variable must be the same for all
    subjects, even if different raters are used for different subjects
    (which is only valid for the kappa statistic). See "MISSING VALUES"
    below. Kendall's statistic requires that the response variable be
    numeric and ordinal. 

  MISSING VALUES:
    An error message is issued if it is determined that each rater does
    not rate each subject exactly once. The macro detects this error under
    any of the following conditions: 

     - Different rater values are found for different subjects
     - Missing values appear in the subject, rater, or response variables
     - A rater does not rate some subject(s) 
     - A subject is not rated by some rater(s)
     - All subjects do not have an equal number of ratings
     - More than one rating of a subject by a rater
 
  SEE ALSO:
    The FREQ procedure in Base SAS software can compute simple and
    weighted kappa statistics for comparing two raters.  Use the AGREE
    option on the TABLES statement.  

    The intraclass correlation coefficient can be used to test rater
    agreement (or reliability) on continuous responses. See the %INTRACC
    macro available at the following web address: 

       http://www.sas.com/service/techsup/faq/stat_macro/intmacr.html

  REFERENCES:
    Fleiss, J.L. (1981), Statistical Methods for Rates and
      Proportions, Second Edition. New York: John Wiley & Sons Inc.
    Kendall, M.G. (1955), Rank Correlation Methods, Second Edition,
      London: Charles Griffin & Co. Ltd.
    Landis, J.R. and Koch G.G. (1977), "The measurement of observer
      agreement for categorical data," Biometrics, 33, 159-174.
  
  EXAMPLE:
    This example is from Fleiss (1981, page 230, Table 13.8). Ten subjects
    (S) are each rated into one of three categories (Y) by each of five
    raters (R). 

    In the following program, change this text: 

       <path to file containing the MAGREE macro> 

    to the path and filename containing this file. 


      title "Analysis of data from Fleiss (1981)";
      data a;
      do s=1 to 10;
      do r=1 to 5;
        input y @@;
        output;
      end; end; cards;
      1 2 2 2 2 
      1 1 3 3 3
      3 3 3 3 3
      1 1 1 1 3
      1 1 1 3 3
      1 2 2 2 2
      1 1 1 1 1
      2 2 2 2 3
      1 3 3 3 3
      1 1 1 3 3
      ;
     
      proc print noobs;
        title2 'Proper form of input data set';
        run;

      proc freq;
        tables s*y;
        title2 'Summary table as shown in Fleiss, page 230';
        run;

      %include '<path to file containing the MAGREE macro>';
      %magree(data=a,
              items=s,
              raters=r,
              response=y)

************************************************************************/

%macro magree(
   data=_last_,
   items=,
   raters=,
   response=,
   stat=BOTH
   );

%let error=0;
%if %upcase(&stat) ne KAPPA and 
    %upcase(&stat) ne KENDALL and
    %upcase(&stat) ne BOTH %then %do;
  %put ERROR: STAT= must be set to KAPPA, KENDALL, or BOTH.;
  %goto exit;
%end;

options nonotes;
*title2 "MAGREE macro";

%if &data=_last_ %then %let data=&syslast;

/* Get type of response variable so can format later.
 ======================================================================*/
%let dsid=%sysfunc(open(&data));
%if &dsid %then %do;
  %let ynum=%sysfunc(varnum(&dsid,&response));
  %let ytype=%sysfunc(vartype(&dsid,&ynum));
  %let rc=%sysfunc(close(&dsid));
%end;
%else %do;
  %put WARNING: Could not check type of &response variable.;
  %put %str(         ) Continuing assuming it is numeric.;
%end;

/* Verify that all subjects have the same number of ratings.  Note
 * that the raters variable must use the same values to identify the
 * raters in all subjects even if different raters rate the subjects.
 * But the number of raters must be the same across all subjects.
 * Create response by item summary table needed to compute kappas and 
 * get numbers of subjects, raters, and response categories.
 ======================================================================*/
data _nomiss; set &data; if &response ne " "; run;

proc freq data=_nomiss noprint;
  table &items*&raters   / sparse out=_balance;
  table &response*&items / out=_ycnts(drop=percent);
  table &items           / out=_n;
  table &raters          / out=_m;
  table &response        / out=_k(drop=count percent);
  run;

data _null_;
  set _balance nobs=_nobs;
  if count ne 1 then call symput('error',1);
  run;
%if &error %then %do;
  %put ERROR: Each rater must rate each subject exactly once.;
  %goto exit;
%end;

data _null_;
  set _n nobs=n;
  set _m nobs=m;
  set _k nobs=k;
  call symput('m',left(m));
  call symput('n',left(n));
  call symput('k',left(k));
  run;


/************************  Compute kappa  ******************************/
%if %upcase(&stat)=KAPPA or %upcase(&stat)=BOTH %then %do;

/* Create coded values (1,2,3,...) for response categories to use as
 * indices. 
 ======================================================================*/
data _ycnts;
  set _ycnts; 
  by &response;
  if first.&response then _code+1;
  run;

/* Compute kappa statistics for each category and overall
 ======================================================================*/
data _kappas;
  set _ycnts end=eof;
  array kapnum {&k} knum1-knum&k;
  array catsum {&k} sum1-sum&k;
  array kapj {&k} kappa1-kappa&k;  /* V6 limit: 999 categories */
  array pb {&k} pbar1-pbar&k;
  array zkapj {&k} zkj1-zkj&k;
  array prkapj {&k} prkj1-prkj&k;
  kapnum{_code} + count*(&m - count);
  catsum{_code} + count;
  if eof then do;
    knum=0; kden=0; pqqp=0;
    nmm = &n*&m*(&m-1);
    sekapj=sqrt(2/nmm);
    do j=1 to &k;
      pb{j} = catsum{j}/(&m*&n);
      pq = pb{j}*(1-pb{j});
      kapj{j} = 1 - (kapnum{j}/(nmm*pq));
      zkapj{j} = kapj{j}/sekapj;
      prkapj{j} = 1-probnorm(zkapj{j});
      knum = knum + pq*kapj{j};
      kden = kden + pq; 
      pqqp = pqqp + pq*((1-pb{j})-pb{j});
    end;
    kappa=knum/kden;
    sekap = (sqrt(2)/(kden*sqrt(nmm)))*sqrt(kden**2-pqqp);
    zkap=kappa/sekap; prkap=1-probnorm(zkap);
    keep &response kapp stderr z prob;
    do i=1 to &k;
       set _k;
       kapp=kapj{i}; stderr=sekapj; z=zkapj{i}; prob=prkapj{i}; output;
    end;
    &response=.; kapp=kappa; stderr=sekap; z=zkap; prob=prkap;
    output;
  end;
  run;

/* Print kappa statistics
 ======================================================================*/
%let fexist=0;
data _null_;
  if ("&ytype"="N" and cexist("work.formats._yfmt.format") ne 1) or
     ("&ytype"="C" and cexist("work.formats._yfmt.formatc") ne 1) then
     call symput("fexist","1");
  run;
%if &fexist %then %do;
  proc format;
     value  %if &ytype=C %then $_yfmt;
            %else               _yfmt;  .="Overall";
  run;
%end;

proc print noobs label;
  format prob pvalue. 
         &response %if &ytype=C %then $_yfmt.; %else _yfmt.; ;
  label kapp="Kappa" stderr="Standard Error" prob="Prob>Z";
  title2 "Kappa statistics for nominal response";
  run;
%end;


/*********** Compute Kendall's Coefficient of Concordance, W ***********/
%if %upcase(&stat)=KENDALL or %upcase(&stat)=BOTH %then %do;

%if &ytype=C %then %do;
  %put %str(NOTE: Kendall%'s Coefficient of Concordance requires a);
  %put %str(     ) numeric, ordinal response.;
  %goto exit;
%end;

/* Rank the data using average ties.  If data are already ranked, this
 * won't change anything.
 ======================================================================*/
proc sort data=&data out=_sortr;
  by &raters;
  run;
proc rank data=_sortr out=_ranked;
  by &raters; 
  var &response;
  run;

/* R-square from one-way ANOVA is Kendall's W.
 ======================================================================*/
proc anova data=_ranked outstat=_anova noprint; 
  class &items; 
  model &response = &items; 
  run;

/* Compute F statistic and p-value for testing W=0.
   Ties are handled correctly.
 ======================================================================*/
data _w; 
  set _anova;
  retain SSsubj;
  if _n_=2 then do;
     w=ss/(ss+SSsubj);
     f=(&m-1)*w/(1-w);
     numdf=&n-1-2/&m;
     dendf=(&m-1)*numdf;
     prob=1-probf(f,numdf,dendf);
     keep w f numdf dendf prob; 
     output;
  end;
  SSsubj=ss;
  run;    

/* Print Kendall's coefficient of concordance and test
 ======================================================================*/
proc print noobs label;
  var w f numdf dendf prob;
  format prob pvalue.;
  label w="Coeff of Concordance" numdf="Num DF" dendf="Denom DF"
        prob="Prob>F";
  title2 "Kendall's Coefficient of Concordance for ordinal response";
  run;
%end;

%exit:
options notes;
title2;
%mend magree;
    
