/************************************************************************

                             CLASSIFY macro
                                      

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   PURPOSE:
     The %CLASSIFY macro uses the OUT= data set from the LOGISTIC or
     PROBIT procedure to produce a data set containing individual-level
     predicted probabilities as multiple variables on the original
     observations.  The output data set also contains the predicted
     response level based on a maximum-predicted-probability decision
     rule.  A threshold on the maximum predicted probability can be set
     so that an observation is not classified if no predicted
     probability exceeds the threshold.  Both binary- and
     ordinal-response variables are allowed.  The resulting data set can
     easily be used with the FREQ procedure to create a classification
     table comparing observed and predicted classifications.
                                                                      
   REQUIRES:
     Only base SAS software (Version 6 or later) is required.

   USAGE:
     You must first use the LOGISTIC or PROBIT procedure to fit a model 
     and create an OUT= data set containing predicted probabilities.
     Use the OUTPUT statement in either of these two procedures and
     specify the P= option (see the LOGISTIC or PROBIT documentation in
     the SAS/STAT User's Guide).  Note that OUT= data sets created using
     "events/trials" syntax (e.g. MODEL R/N = ...;) or using a BY 
     statement cannot be processed with the %CLASSIFY macro.  
     
     Submit this file to define the %CLASSIFY macro.  You may then 
     invoke the macro to process the output data set from LOGISTIC or 
     PROBIT.  The EXAMPLE section below shows how to invoke the macro.

     The following parameters are required when using the macro:

     data=        Name of LOGISTIC or PROBIT OUT= data set.  If not 
                  specified, the last data set created is used.
     response=    Name of response variable used in LOGISTIC or PROBIT.
     p=           Name of PREDICT= variable in OUT= data set.

     The following parameters are optional:

     threshld=0   Minimum predicted probability which must be attained
                  for an observation to be classified into a response
                  level.  The specified value must be between 0 and 1.
     predname=_predlvl
                  Names the variable in the OUT= data set containing the
                  predicted response level.  Do not use the same name as
                  given in the P= parameter.  If not specified, the
                  variable is named _PREDLVL.
     out=classout Names the output data set containing individual-level
                  predicted probabilities and the predicted response
                  level.  If not specified, the data set is named
                  CLASSOUT.
    
   PRINTED OUTPUT:
     Below is the output from the EXAMPLE.  The PSYMP variable contains
     the predicted classification.  The variables NONE, MILD, and SEVERE
     give the predicted probabilities for each of the corresponding
     response levels.  Note that they sum to 1.0 within an observation.

PREP    DOSE   SYMPTOMS    N    LDOSE    PSYMP      NONE      MILD     SEVERE

stand    10     None      33   1.00000   None     0.68267   0.14449   0.17285
stand    10     Mild       7   1.00000   None     0.68267   0.14449   0.17285
stand    10     Severe    10   1.00000   None     0.68267   0.14449   0.17285
stand    20     None      17   1.30103   Severe   0.40559   0.18494   0.40948
stand    20     Mild      13   1.30103   Severe   0.40559   0.18494   0.40948
stand    20     Severe    17   1.30103   Severe   0.40559   0.18494   0.40948
stand    30     None      14   1.47712   Severe   0.25571   0.16941   0.57488
stand    30     Mild       3   1.47712   Severe   0.25571   0.16941   0.57488
stand    30     Severe    28   1.47712   Severe   0.25571   0.16941   0.57488
stand    40     None       9   1.60206   Severe   0.17030   0.14347   0.68623
stand    40     Mild       8   1.60206   Severe   0.17030   0.14347   0.68623
stand    40     Severe    32   1.60206   Severe   0.17030   0.14347   0.68623
test     10     None      44   1.00000   None     0.85145   0.08309   0.06546
test     10     Mild       6   1.00000   None     0.85145   0.08309   0.06546
test     10     Severe     0   1.00000   None     0.85145   0.08309   0.06546
test     20     None      32   1.30103   None     0.62877   0.15833   0.21290
test     20     Mild      10   1.30103   None     0.62877   0.15833   0.21290
test     20     Severe    12   1.30103   None     0.62877   0.15833   0.21290
test     30     None      23   1.47712   None     0.46450   0.18304   0.35246
test     30     Mild       7   1.47712   None     0.46450   0.18304   0.35246
test     30     Severe    21   1.47712   None     0.46450   0.18304   0.35246
test     40     None      16   1.60206   Severe   0.34994   0.18286   0.46719
test     40     Mild       6   1.60206   Severe   0.34994   0.18286   0.46719
test     40     Severe    19   1.60206   Severe   0.34994   0.18286   0.46719

     The following table was generated by PROC FREQ to compare the 
     predicted and actual response classifications.  Using the maximum 
     predicted probability to classify observations, (132+96)/387 = 59% 
     of the observations were correctly classified.  Note, however, that 
     this will tend to be an optimistic estimate since the observations 
     being classified were also used to fit the model, resulting in 
     bias.

                          TABLE OF PSYMP BY SYMPTOMS

                 PSYMP     SYMPTOMS

                 Frequency|
                 Percent  |
                 Row Pct  |
                 Col Pct  |None    |Mild    |Severe  |  Total
                 ---------+--------+--------+--------+
                 None     |    132 |     30 |     43 |    205
                          |  34.11 |   7.75 |  11.11 |  52.97
                          |  64.39 |  14.63 |  20.98 |
                          |  70.21 |  50.00 |  30.94 |
                 ---------+--------+--------+--------+
                 Severe   |     56 |     30 |     96 |    182
                          |  14.47 |   7.75 |  24.81 |  47.03
                          |  30.77 |  16.48 |  52.75 |
                          |  29.79 |  50.00 |  69.06 |
                 ---------+--------+--------+--------+
                 Total         188       60      139      387
                             48.58    15.50    35.92   100.00

   DETAILS:
     The format of the OUT= data set produced by the LOGISTIC and PROBIT
     procedures does not lend itself well to the problem of classifying 
     observations into response levels.  This is especially so when the 
     response variable is ordinal, in which case there are k-1 output 
     observations for each input observation.  In this block of output 
     observations, the P= variable contains the first k-1 cumulative
     predicted probabilities, Pr(Y<=j), j=1,2,...,k-1.  The predicted
     probability of being in the j-th response level must be obtained by
     subtracting the predicted probability of being in the (j-1)st
     level:
        
        Pr(Y=j) = Pr(Y<=j) - Pr(Y<=j-1) ,   j=2,3,...,k

     Once these individual-level predicted probabilities are obtained, 
     you can then obtain a predicted classification of the observation 
     by classifying it into the response level with the largest 
     predicted probability.  You might want to prevent the
     classification of an observation if its largest predicted
     probability does not exceed a threshold that you specify.

     The %CLASSIFY macro does all of this for you.  The output data set 
     that it creates has as many observations as the data set that was 
     input to the LOGISTIC or PROBIT procedure.  Added to each original 
     observation are k variables containing the predicted probabilities 
     for each of the k response levels.  The names of these variables
     are constructed from the original response level names using the
     rules described in the ID Statement section of the TRANSPOSE
     procedure (See the SAS Procedures Guide).

     Any additional statistics created with the OUTPUT statement, such
     as XBETA= or STD= are included in the output data set.

   LIMITATIONS:
     Limited error checking is done.  Be sure that you correctly specify
     all options.  It is recommended that you do not process the OUT=
     data set from LOGISTIC or PROBIT in any way before using this
     macro.  In particular, do not drop the response variable or the
     predicted probability variable from the OUT= data set. 

     This macro cannot be used on an OUT= data set that was obtained 
     using:
      
        1) a BY statement.
     or
        2) "events/trials" syntax in the MODEL statement.
     
   MISSING VALUES:
     If values of the P= variable in %CLASSIFY's DATA= data set are
     missing, the individual-level predicted probabilities will be 
     missing in the OUT= data set.
      
   EXAMPLE:
     In this example (from the PROBIT chapter of the SAS/STAT User's
     Guide), the response is a three-level ordinal variable.  The 
     %CLASSIFY macro processes the OUT= data set from PROC PROBIT and 
     creates the output data set of predicted probabilities.  A
     classification table comparing predicted and actual response
     levels is created using PROC FREQ.
 
       data symptoms;
         input prep $ dose symptoms $ n;
         ldose=log10(dose);
         cards;
       stand     10      None       33
       stand     10      Mild        7
       stand     10      Severe     10
       stand     20      None       17
       stand     20      Mild       13
       stand     20      Severe     17
       stand     30      None       14
       stand     30      Mild        3
       stand     30      Severe     28
       stand     40      None        9
       stand     40      Mild        8
       stand     40      Severe     32
       test      10      None       44
       test      10      Mild        6
       test      10      Severe      0
       test      20      None       32
       test      20      Mild       10
       test      20      Severe     12
       test      30      None       23
       test      30      Mild        7
       test      30      Severe     21
       test      40      None       16
       test      40      Mild        6
       test      40      Severe     19
       ;

       proc probit order=data;
         class symptoms prep;
         model symptoms = prep ldose;
         weight n;
         output out=prbout p=pred;
         run;

       %classify( data=prbout,
                  response=symptoms,
                  p=pred,
                  predname=psymp,
                  out=prbpred)

       proc print noobs; run;
       proc freq order=data;
         weight n;
         tables psymp*symptoms;
         run;

*****************************************************************************/


%macro classify(

     /************************ REQUIRED parameters **************************/
     data=_last_  , /* Name of LOGISTIC or PROBIT OUT= data set.  If not    */
                    /*   specified, the last created data set is used.      */
     response=    , /* Name of response variable used in LOGISTIC or PROBIT.*/
     p=           , /* Name of PREDICT= variable in OUT= data set.          */

     /************************ OPTIONAL parameters **************************/
     threshld=0   , /* Minimum predicted probability which must be attained */
                    /*   for an observation to be classified into a         */
                    /*   level.  Specified value must be between 0 and 1.   */
     predname=_predlvl , 
                    /* Name of variable containing the predicted response   */
                    /*   level.  DO NOT use the same name as given in the   */
                    /*   P= parameter.                                      */
     out=classout   /* Name of final data set containing individual level   */
                    /*   predicted probabilities and the predicted response */
                    /*   level.                                             */
               );

%if &data=_last_ %then %let data=&syslast;
options nonotes ;

/******************************************************************
Some limited error checking.
*******************************************************************/
/* Verify that RESPONSE= option is specified. */
%if &response= %then %do;
    %put ERROR: Response variable must be specified in the RESPONSE= argument;
    %goto exit;
%end;

/* Verify that P= option is specified. */
%if &p= %then %do;
    %put ERROR: Predicted probability variable must be specified in the;
    %put %str(       P= argument.);
    %goto exit;
%end;

/* Verify that arguments of P= and PREDNAME= options are not the same. */
%if &p=&predname %then %do;
    %put ERROR: The predicted probability (P=) variable name must not be the;
    %put %str(       same as the predicted response (PREDNAME=) variable name.);
    %goto exit;
%end;

/******************************************************************
Store the last ordered value in &LASTLVL and the number of response
levels in &NLEV. 
*******************************************************************/
proc freq data=&data ; 
  tables &response / out=_ylvls noprint; 
proc freq data=&data ; 
  tables _level_ / out=_lvls noprint; 
  run;
data _null_;
 set _ylvls nobs=_nlev;
 if _n_=1 then call symput('nlev',left(put(_nlev,4.)));
 _test_=0;
 do _i=1 to _n;
    set _lvls nobs=_n point=_i;
    if &response=_level_ then _test_=1;
 end;
 if _test_=0 then do;
    call symput('lastlvl',&response);
    stop;
 end;
 run;

/***************************************************************** 
Identify blocks of output observations for each observation that was 
input to LOGISTIC.  
******************************************************************/
data _logout; set &data;
  %if &nlev=2 %then %str(_obs=_n_;);
  %else %str(if mod(_n_,&nlev-1)=1 then _obs+1;);
  run;

/****************************************************************** 
Output other variables to another data set for merging later.  Compute
individual-level predicted probabilities.  Obtain predicted
classification using maximum predicted probability decision rule and
satisfying the optional threshold level.
*******************************************************************/
data _logout  (keep=_obs _ilevp _level_ _maxp &predname)
     _othvars (drop=_obs &p _prevp _ilevp _level_ _maxp)
     ;        
  set _logout; by _obs; 
  retain _maxp &predname;
  _prevp=lag(&p);
  if first._obs then do;
    _prevp=0; _maxp=&p; &predname=_level_;
  end;
  _ilevp=&p-_prevp;
  if _ilevp>_maxp then do; 
    _maxp=_ilevp; &predname=_level_;
  end;
  output _logout;
  if last._obs then do;
     _level_="&lastlvl"; _ilevp=1-&p;
     if _ilevp>_maxp then do;
       _maxp=_ilevp; &predname=_level_;
     end;
     if _maxp<&threshld then &predname=" ";
     output _othvars _logout;
  end;
  run;

/******************************************************************* 
Transpose each block so that the predicted probabilities
are all on the original observation.
********************************************************************/
proc transpose data=_logout out=_tlogout(drop=_name_ _obs);
  by _obs; var _ilevp; id _level_;
  run;

/******************************************************************* 
Merge other variables from OUT= with the individual-level predicted 
probabilities.
********************************************************************/
options notes ;
data &out;
  merge _othvars _tlogout;
  run;

%exit:
%mend;

