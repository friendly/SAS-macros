/************************************************************************

                                 VARMOD
                                  V1.4
                                      

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   PURPOSE:
     Fit a model that accommodates heteroscedasticity (unequal
     variances) by iteratively fitting a normal regression model for
     the mean response and a log-linear regression model for the
     variance.  Different linear functions can be used for the mean and
     log-variance models.  The macro iteratively fits the two models
     until convergence of the likelihood function is obtained.
     Parameter estimates of both the mean and variance models are
     presented as well as a likelihood ratio test of the improvement of
     the fitted model over an equal variances model.  Output data sets 
     can be created containing parameter estimates, diagnostics and 
     predicted values.

     For Taguchi designs in which there are several measurements taken
     at each setting of the control factors, this macro can be
     particularly useful since both the mean and the variance can be
     modeled.  You can find which control factors affect the mean, which
     affect the variance and which affect both.  You can then determine
     settings of the control factors that provide optimal mean response
     and variance.

     Note that the transformation presented by Box and Cox (1964) is
     also a possible solution to heteroscedasticity.  This
     transformation can be fit using the %ADXTRANS macro provided with
     SAS/QC Software.

     This macro generalizes and enhances the %SEMIMOD macro that is 
     presented in the "Modeling Variance Homogeneity" example in the 
     PROC GENMOD documentation.
                                                                      
   REQUIRES:
     SAS/STAT Software, Release 6.08 TS415 or later.  

   USAGE:
     Submit this file to the SAS System to define the %VARMOD macro and
     make it available for use.  You may then call the macro to fit a
     model to an existing temporary or permanent data set that you
     identify with the DATA= option.  The EXAMPLE section below shows
     how to invoke the macro.

     The RESPONSE= option must be specified when using the macro to 
     identify the response variable in the DATA= data set.  Generally,
     you will also use the MMODEL= and VMODEL= options to specify the
     mean and variance models, respectively.  You can use the same
     syntax used on the right-hand side of the MODEL statement in the
     GLM procedure.  If either is omitted, an intercept-only model is
     fit.  If either model involves CLASS variables, list the variables
     in the MCLASS= and/or VCLASS= options as needed.  Several output
     data sets may be requested including data sets containing parameter
     estimates, diagnostics and predicted values.  Predicted values can
     be obtained for a separate data set using the SCORE and OUTSCORE=
     options.  There are a few additional options that may be needed
     occasionally.

     Following is a list of all available options.  Default values are 
     given after the equal sign in each case.  

     Identify input data set, response variable, and data set to score
       data=               Specify the data set to be used.  If omitted,
                           the last-created data set is used.           
       response=           **REQUIRED**.  The response variable for the
                           mean model.
       score=              Specify an optional data set of observations
                           for which predicted means and variances will 
                           be computed.  These observations do not
                           affect the model fit.  The response variable,
                           if present, is copied to the variable
                           _OBSRESP.  All explanatory variable values
                           must be nonmissing for predicted values to be
                           computed.  The OUTSCORE= option should also
                           be specified when SCORE= is specified. 

     Specify mean and variance models   
       mclass=             Specify all CLASS variables (if any) used in
                           the mean model.                              
       mmodel=             Specify all terms in the mean model using 
                           PROC GLM MODEL statement syntax.             
       vclass=             Specify all CLASS variables (if any) used in
                           the variance model.                          
       vmodel=             Specify all terms in the variance model using 
                           PROC GLM MODEL statement syntax.             
       mnoint=             If non-blank, fit a no-intercept mean model.

     Output data sets   
       outmdiag=           Specify name of output data set of predicted
                           values of the mean and diagnostics.          
       outvdiag=           Specify name of output data set of predicted
                           values of the variance and diagnostics.        
       outpred=            Specify name of output data set containing
                           the DATA= data set plus a variable _PREDM 
                           giving the predicted mean values and a 
                           variable _PREDV giving the predicted 
                           variances.                                    
       outscore=           Specify name of output data set containing
                           the SCORE= data set plus a variable _PREDM 
                           giving the predicted mean values and a 
                           variable _PREDV giving the predicted 
                           variances.  SCORE= should be specified when 
                           OUTSCORE= is specified.
       outmparm=           Specify name of output data set containing
                           parameter estimates of the mean model.       
       outvparm=           Specify name of output data set containing
                           parameter estimates of the variance model.   
       outlr=              Specify name of output data set containing
                           the likelihood ratio test for equal 
                           variances.                                   

     Control printing   
       noprint=            If non-blank, suppress all printed output.
                           Useful when you just want to create output
                           data sets.                                   
       noeqvar=            If non-blank, suppress printing equal 
                           variances mean model results.                
       noiter=             If non-blank, suppress printing iteration
                           history.                                     
       nolr=               If non-blank, suppress printing likelihood
                           ratio test for equal variances.              
       noparms=            If non-blank, suppress printing parameters 
                           of the mean and variance models.             
       mgof=               If non-blank, prints goodness of fit table 
                           for final mean model.                        
       vgof=               If non-blank, prints goodness of fit table 
                           for final variance model.                    

     Control maximum likelihood process   
       maxiter=20          Maximum number of iterations allowed.  An 
                           iteration is one fitting of both models.  
                           Default is 20.     
       converge=1e-3       Convergence is declared when the maximum 
                           change in loglikelihood is less than this. 
                           Default is 0.001.
    
   PRINTED OUTPUT:
     When the EXAMPLE (see below) is run, the following lines are
     displayed in the SAS log as the iterative process proceeds:

        VARMOD Iteration 0: Fitting mean model
        VARMOD Iteration 0: Fitting variance model
        VARMOD Iteration 0: -2*LogLikelihood = 4339.4896598
        VARMOD Iteration 1: Fitting mean model
        VARMOD Iteration 1: Fitting variance model
        VARMOD Iteration 1: -2*LogLikelihood = 3867.5374649
        VARMOD Iteration 2: Fitting mean model
        VARMOD Iteration 2: Fitting variance model
        VARMOD Iteration 2: -2*LogLikelihood = 3867.1559993
        VARMOD Iteration 3: Fitting mean model
        VARMOD Iteration 3: Fitting variance model
        VARMOD Iteration 3: -2*LogLikelihood = 3867.1559992
        VARMOD: Convergence attained.

     Following are selected sections of the printed output from the 
     EXAMPLE that are displayed after convergence.  
  
                              Iteration History

                        Iteration    -2*LogLikelihood

                            0        4339.4896597855
                            1         3867.537464924
                            2        3867.1559992645
                            3        3867.1559992075

                   Likelihood Ratio Test of Equal Variances

                              LR
                          ChiSquare    DF    Pr>Chi

                           472.334      1    0.0000

                               Final Mean Model
                       Analysis of Parameter Estimates

Parameter    LEVEL1    DF      Estimate       Std Err     ChiSquare    Pr>Chi

INTERCEPT               1       -0.6888        0.5321        1.6754    0.1955
X*P            a        1        1.0622        0.0422      632.4572    0.0001
X*P            b        1        3.0662        0.0447     4710.9793    0.0001
SCALE                   0        1.0000        0.0000         .         .

                             Final Variance Model
                       Analysis of Parameter Estimates

                                                                    Variance
 Parameter  LEVEL1  DF    Estimate     Std Err   ChiSquare  Pr>Chi    Ratio

 INTERCEPT           1      3.1693      0.0577   3013.2809  0.0001   23.7901
 P            a      1     -1.8933      0.0816    537.6759  0.0001    0.1506
 P            b      0      0.0000      0.0000       .       .         .
 SCALE               0      0.5000      0.0000       .       .         .

     See the EXAMPLE section below for background on this example.  The
     likelihood ratio test shows that the fitted model, which allows for
     variance differences, is significantly better than an equal
     variances model (p<.0001).  Note that the likelihood ratio
     chi-square value is the difference in -2*log(likelihood) values at
     the initial (equal variances) and final iterations.  The estimated
     parameters of the final mean and variance models closely match the
     true parameters that generated the data.  The variance ratio of
     23.79 for the intercept represents the estimated variance when all
     variance model parameters are zero, which corresponds to Process B
     whose true variance is 25.  The variance ratio for P represents the
     estimated ratio of process variances (A to B).  The estimated value
     of 0.15 also closely matches the true variance ratio of 0.16.
     Since this indicates that the process B variance is larger than the
     process A variance, you may prefer to use the B to A ratio which is
     simply the reciprocal, 1/0.1506 = 6.64.

   DETAILS:
     The mean and variance models fit by the %VARMOD macro are 
     respectively (the subscript i representing individual observations
     is omitted for simplicity):

        y = XB + e      and

        V(y) = exp(ZL)  or equivalently,  
        log(V(y)) = ZL  , 
    
     where y is the response, X is the model matrix for the mean model,
     B is a vector of parameter estimates of the mean model, e is a
     normal random variable with mean zero and variance V(y), Z is the
     model matrix for the variance model, and L is the vector of
     parameter estimates of the variance model.  Note that the
     explanatory variables represented by the columns of X and Z are
     often common to the two models, but need not be.

     Parameters of the models are estimated using an iterative maximum
     likelihood method which starts with an equal variances model.  A 
     likelihood ratio test of equal variances is provided using the 
     likelihood values of the equal variances model and final fitted 
     model.  Small p-values indicate that at least one of the parameters 
     in the variance model is significant and that the variances are not 
     equal.

     For each variance model parameter, a variance ratio is also
     computed.  For the intercept, this is an estimate of the overall 
     variance given that all other variance parameters are zero.  For
     main effects, these give the change in variance comparing the
     current level to the reference level (for CLASS variables) or for a
     unit increase in the variable (for non-CLASS variables).  A zero
     parameter estimate corresponds to a variance ratio of one.  Hence,
     the test of the parameter estimate is also a test of the variance
     ratio.  The variance ratio for a change in X units of the variable 
     can be computed as the variance ratio raised to the power X.

     %VARMOD does not insist that the likelihood strictly increase.  It 
     simply looks for the change in successive iterations to be less 
     than the CONVERGE= option value.

   LIMITATIONS:
     Limited error checking is done.  BE SURE THAT:

       - the DATA= data set exists and that the variables specified in
         the RESPONSE=, MCLASS=, MMODEL=, VCLASS=, and VMODEL= options
         exist on that data set
       - the SCORE= data set exists if specified
       - option names are specified as shown above
       - the variables in your input (DATA=) data set do not have any of 
         the following names:  YVAR1 PRED XBETA STD HESSWGT UPPER LOWER
         RESRAW RESCHI RESDEV _SUM _WGT _ITER _DEVOLD _RSQUARE
       - prior to invoking %VARMOD you do not have any data sets you 
         want to keep with any of the following names: _TMP _WORK _OLD
         _ITER _MNFIT _MNDIAG _MEANEST _VARFIT _VARDIAG _VARESTS _LREQV
         _SCORE

   MISSING VALUES:
     Observations with missing values for any of the explanatory
     variables in the mean or variance model are omitted from the
     analysis.  Observations with missing response, but which are 
     nonmissing on all explanatory variables, are ignored in the model 
     fitting phase, but predicted values are generated for these 
     observations in the OUTMDIAG=, OUTVDIAG=, and OUTPRED= data sets.
     However, it is generally more convenient to use the SCORE= and
     OUTSCORE= options to get predicted values for new observations.

   SEE ALSO:
     See the file VARMODEX.SAS which contains additional examples using 
     the %VARMOD macro.  See the %SEMIMOD macro in the "Modeling
     Variance Homogeneity" example in the PROC GENMOD documentation.

   REFERENCES:
     Aitkin, M. (1987), "Modelling Variance Heterogeneity in Normal 
        Regression Using GLIM", Applied Statistics, 36(3), 332-339.
      NOTE: differences between -2loglikelihood in %VARMOD and Aitkin's
      "deviance" is mostly due to his addition of log(2*pi) in the
      likelihood which is unnecessary since it is constant.

     Box, G.E.P. and Cox, D.R. (1964), "An Analysis of Transformations", 
        Journal of the Royal Statistics Society B, 26, 211-252.

     Cook, R.D. and Weisberg, S. (1983), "Diagnostics for 
        Heteroscedasticity in Regression", Biometrika, 70, 1-10.

   EXAMPLE:
     See the file VARMODEX.SAS for additional examples of the %VARMOD
     macro.  This example is a slightly expanded version of the example 
     in the GENMOD documentation.

     The data are generated from two hypothetical semiconductor
     processes.  The output, Y, of each process is affected by a
     continuous process variable, X.  Note that Process B has larger
     output variability.

       Process A:  Y = 1X + N(0,4)
       Process B:  Y = 3X + N(0,25)

     The variance ratio is 4/25 = 0.16.  A single model for the mean is:
  
       E(Y) = 1*p1*X + 3*p2*X ,
  
     where p1=1 if Process A and 0 otherwise; p2=1 if Process B and 0
     otherwise.  The variance model is:
  
       ln(var) = ln(4)*p1 + ln(25)*p2 = 1.3863*p1 + 3.2189*p2 .
  
     In a model with intercept, made identifiable by setting the last 
     parameter estimate to zero (as GENMOD and GLM do when a CLASS 
     statement is specified), the variance model becomes:
  
       ln(var) = ln(25) + [ln(4)-ln(25)]*p1 + 0*p2
               = 3.2189 + -1.8326*p1
  
     The variance ratio is then just exp(-1.8326) = 0.16 .  

     The following statements generate the data and fit the mean and
     variance models as discussed above:

        data semi;
           drop j;
           do p = 'a','b';
              do x = 10 to 15;
                 if p = 'a' then do;
                    do j = 1 to 100;
                       y = x + 2*rannor(13020238);
                       output;
                    end;
                 end;
                 else if p = 'b' then do;
                    do j = 1 to 100;
                    y = 3*x + 5*rannor(45678421);
                    output;
                    end;
                 end;
              end;
           end;
           run;

        %varmod(data=semi,
                response=y,
                mclass=p, mmodel=p*x,
                vclass=p, vmodel=p)

************************************************************************/


%macro varmod(

  /* Identify input data set, response variable and data set to score */
       data=_last_,     /* Specify the data set to be used.  If omitted,
                           the last-created data set is used.         */
       response=,       /* REQUIRED.  The response variable for the mean 
                           model.                                     */ 
       score=,          /* Specify an optional data set of observations
                           for which predicted means and variances will 
                           be computed.  These observations do not
                           affect the model fit.  The response variable,
                           if present, is copied to the variable
                           _OBSRESP.  All explanatory variable values
                           must be nonmissing for predicted values to be
                           computed.  The OUTSCORE= option should also
                           be specified when SCORE= is specified.     */

  /* Specify mean and variance models */
       mclass=,         /* Specify all CLASS variables (if any) used in
                           the mean model.                            */
       mmodel=,         /* Specify all terms in the mean model using 
                           PROC GLM MODEL statement syntax.           */
       vclass=,         /* Specify all CLASS variables (if any) used in
                           the variance model.                        */
       vmodel=,         /* Specify all terms in the variance model using 
                           PROC GLM MODEL statement syntax.           */
       mnoint=,         /* If this is non-blank, fit a no-intercept mean 
                           model.                                     */

  /* Output data sets */
       outmdiag=,       /* Specify name of output data set of predicted
                           values of the mean and diagnostics.        */
       outvdiag=,       /* Specify name of output data set of predicted
                           values of the variance and diagnostics.    */  
       outpred=,        /* Specify name of output data set containing
                           the DATA= data set plus a variable _PREDM 
                           giving the predicted mean values and a 
                           variable _PREDV giving the predicted 
                           variances.                                 */ 
       outscore=,       /* Specify name of output data set containing
                           the SCORE= data set plus a variable _PREDM 
                           giving the predicted mean values and a 
                           variable _PREDV giving the predicted 
                           variances.  SCORE= should be specified when 
                           OUTSCORE= is specified.                    */
       outmparm=,       /* Specify name of output data set containing
                           parameter estimates of the mean model.     */
       outvparm=,       /* Specify name of output data set containing
                           parameter estimates of the variance model. */
       outlr=,          /* Specify name of output data set containing
                           the likelihood ratio test for equal 
                           variances.                                 */
                           

  /* Control printing */
       noprint=,        /* If non-blank, suppress all printed output.
                           Useful when you just want to create output
                           data sets.                                 */
       noeqvar=,        /* If non-blank, suppress printing equal 
                           variances mean model results.              */
       noiter=,         /* If non-blank, suppress printing iteration
                           history.                                   */
       nolr=,           /* If non-blank, suppress printing likelihood
                           ratio test for equal variances.            */
       noparms=,        /* If non-blank, suppress printing parameters 
                           of the mean and variance models.           */
       mgof=,           /* If non-blank, prints goodness of fit table 
                           for final mean model.                      */
       vgof=,           /* If non-blank, prints goodness of fit table 
                           for final variance model.                  */

  /* Control maximum likelihood process */
       maxiter=20,      /* Maximum number of iterations allowed.  An 
                           iteration is one fitting of both models.   */
       converge=1e-3    /* Convergence is declared when the maximum 
                           change in loglikelihood is less than this. */
       );

%if &data=_last_ %then %let data=&syslast;
options nonotes nostimer;

/* Verify that RESPONSE= option is specified. */
%if &response= %then %do;
    %put ERROR: Response variable must be specified in the RESPONSE= argument.;
    %goto exit;
%end;

%if &noprint ne %then %do;
    %let noeqvar=no; %let noiter=no; %let nolr=no; %let noparms=no;
%end;

/* Add a variable to identify the SCORE= observations when concatenated 
   to the DATA= observations.  _OBSRESP contains the observed responses, 
   or is missing.  The response variable is set to missing to ensure 
   these observations do not affect the model fit.
*/
%if &score ne %then %do;
data _score;
  set &score;
  _scorobs=1; 
  _obsresp=&response;
  &response=.;
  run;
%end;

/* For 0th iteration (equal variances model fit):
   - move data to temporary file (_work)
   - set weights to 1 (_wgt)
   - initialize -2LL (_sum) and iteration (_iter) variables
*/
data _work;
  set &data 
      %if &score ne %then _score;
      ;
  _wgt = 1;
  _sum = 0;
  run;

data _tmp;
  _sum = 0; _iter=0;
  run;

%global _print_;
%let conv = 0;
%let iter = 0;


/******************************************************************** 
   Iteratively fit mean and variance models until convergence.
   0th iteration fits the equal variances model (intercept-only).
   Convergence declared if change in -2*logLikelihood < &converge in 
   fewer than &maxiter iterations.
********************************************************************/
%do %while( &conv = 0 );

%if &iter > &maxiter - 1 %then %do;
  %put NOTE: Convergence not attained in &maxiter iterations.  Terminating.;
  %goto exit;
%end;

/* Store previous iteration's -2LL for comparison with this iteration */
data _old;
  set _tmp;
  _devold = _sum;
  keep _devold;
  run;

/*************************** MEAN MODEL *****************************
  set NOSCALE so that scale is not estimated
  select OBSTATS to get residuals
  SCWGT selects dispersion parameter weights
*/
%put VARMOD Iteration &iter: Fitting mean model;
%if &noeqvar ne %then %let _print_=off;
title "Equal variances model";
proc genmod data=_work;
  scwgt _wgt;
  make 'modfit' out=_mnfit;
  make 'obstats' out=_mndiag noprint;
  make 'parmest' out=_meanest;
  %if &mclass ne %then %str(class &mclass;);
  model &response = %quote(&mmodel)
     / noscale obstats
       %if &mnoint ne %then noint;
     ;
  run;

%let _print_=off;

/* Obtain squared residuals 
   Add 1e-12 to the squared residuals to keep them off the zero 
   boundary.
*/
data _work;
  drop _sum yvar1 pred xbeta std hesswgt upper lower
       resraw reschi resdev;
  set _mndiag ;
  set _work;
  _rsquare = resraw*resraw + 1e-12;
  run;

/*********************** VARIANCE MODEL *****************************
  set scoring=100 to use Fisher scoring
  set scale = .5 for 1 df in gamma distribution
  first time thru, fit intercept-only model to get -2LL for the equal 
    variances model
*/
%put VARMOD Iteration &iter: Fitting variance model;
proc genmod data=_work;
  make 'modfit' out=_varfit;
  make 'obstats' out=_vardiag;
  make 'parmest' out=_varests;
  %if &vclass ne %then %str(class &vclass;);
  model _rsquare =   %if &iter>0 %then %quote(&vmodel);
        / dist=gamma link=log obstats noscale scale=.5 scoring=100;
  run;

/* Get DF of equal variances model */
%if &iter=0 %then %do;
  data _null_; set _varfit; 
    if _n_=1 then call symput("eqdf",df); 
    stop;
    run;
%end;

/* Compute weights for mean model 
   Compute _sum = -2*log(likelihood)
*/
data _work;
  drop xbeta yvar1 pred std hesswgt upper lower resraw reschi resdev;
  set _work;
  set _vardiag;
  _wgt = 1./pred;
  _sum + (_rsquare/pred + log(pred));   
  /* Aitkin adds log(2*pi) to the likelihood contribution above.  It
     isn't necessary since it isn't a function of the parms, but it can
     be added in by inserting the following before the last parenthesis
     above:          + log(2*arcos(-1))
  */
  run;

/* Update -2LL */
data _tmp;
  set _work nobs = _last;
  keep _sum _iter;
  if _n_ = _last;
  _iter = &iter ;
  run;

/* Update iteration history */
data _iter;
  set %if &iter>0 %then _iter;
      _tmp;
  run;

/* Check for convergence */
data _NULL_;
  set _tmp; set _old;
  put "VARMOD Iteration &iter: -2*LogLikelihood = " _sum;
  if ( abs(_sum - _devold) <= &converge ) then do;
     _conv = 1;
     put "VARMOD: Convergence attained.";
  end;
  else _conv = 0;
  call symput( 'dev', left(put(_sum,12.4)) );
  call symput( 'conv', left(put(_conv,3.)) );
  run;

%let iter=%eval( &iter+1 );

%end;  
/******************* End of model-fitting loop *********************/


/*************** Print iteration history ******************/
%if &noiter= %then %do;
title 'Iteration History';
proc print data=_iter label noobs; 
  var _iter _sum;
  format _sum best15.;
  label _iter="Iteration"
        _sum="-2*LogLikelihood";
  run;
%end;

/***************** Compute/print/output LR test ********************/
title 'Likelihood Ratio Test of Equal Variances';
data _lreqv; 
  keep lrchi lrdf lrp;
  label lrchi="LR ChiSquare"
        lrdf="DF"
        lrp="Pr>Chi";
  format lrp 6.4;
  set _iter end=_eof;
  retain eq2ll;
  if _n_=1 then do;
     eq2ll=_sum;
     set _varfit;
     retain lrdf;
     lrdf=&eqdf-df; 
  end;
  if _eof then do;
     lrchi=eq2ll-_sum;
     if lrdf>0 then lrp=1-probchi(lrchi,lrdf);
     output;
  end;
  run;

%if &nolr= %then %do;
proc print data=_lreqv label noobs;
  var lrchi lrdf lrp;
  run;
%end;

/*************** Print mean model parameter estimates **************/
title 'Final Mean Model';
%if &mgof ne %then %do;
proc print data=_mnfit noobs label; 
  where criterio ne "Log Likelihood";
  title2 "Criteria for Assessing Goodness of Fit";
  run;
%end;
%if &noparms= %then %do;
proc print data=_meanest noobs label; 
  title2 "Analysis of Parameter Estimates";
  run;
%end;

/************** Print variance model parameter estimates *************/
title 'Final Variance Model';
%if &vgof ne %then %do;
proc print data=_varfit noobs label; 
  where criterio ne "Log Likelihood";
  title2 "Criteria for Assessing Goodness of Fit";
  run;
%end;
/* Compute variance ratios */
data _varests; 
  set _varests;
  if parm ne "SCALE" and df ne 0 then
     varratio=exp(estimate);
  label varratio="Variance Ratio";
  run;
%if &noparms= %then %do;
proc print data=_varests noobs label; 
  title2 "Analysis of Parameter Estimates";
  run;
%end;

/********************** Output data sets *********************/
options notes;
/* OUTMDIAG= data set -- mean model diagnostics */
%if &outmdiag ne %then %do;
  %if &score= %then %do;
  data &outmdiag; 
    set _mndiag; 
    run;
  %end;
  %else %do;
  data &outmdiag; 
    merge _work (keep=_scorobs) 
          _mndiag;
    drop _scorobs;
    if _scorobs ne 1;
    run;
  %end;
%end;

/* OUTVDIAG= data set -- variance model diagnostics */
%if &outvdiag ne %then %do;
  %if &score= %then %do;
  data &outvdiag; 
    set _vardiag; 
    run;
  %end;
  %else %do;
  data &outvdiag; 
    merge _work (keep=_scorobs) 
          _vardiag;
    drop _scorobs;
    if _scorobs ne 1;
    run;
  %end;
%end;

/* OUTPRED= data set -- predicted mean and variance for DATA= */
%if &outpred ne %then %do;
  data &outpred;
    merge _work
          _mndiag  (keep=pred rename=(pred=_predm))
          _vardiag (keep=pred rename=(pred=_predv));
    %if &score ne %then %str(if _scorobs ne 1;);
    drop _wgt _sum _rsquare
      %if &score ne %then _scorobs _obsresp;
      ;
    run;
%end;

/* OUTSCORE= data set -- predicted mean and variance for SCORE= */
%if &outscore ne %then %do;
  %if &score= %then %do;
    %put ERROR: No SCORE= data set specified.  OUTSCORE= is ignored.;
    %goto clean;
  %end;
  data &outscore;
    merge _work
          _mndiag  (keep=pred rename=(pred=_predm))
          _vardiag (keep=pred rename=(pred=_predv));
    drop _wgt _sum _rsquare _scorobs &response;
    if _scorobs=1;
    run;
%end;

/* OUTMPARM= data set -- mean model parameter estimates */
%if &outmparm ne %then %do;
  data &outmparm;
    set _meanest;
    run;
%end;

/* OUTVPARM= data set -- variance model parameter estimates */
%if &outvparm ne %then %do;
  data &outvparm;
    set _varests;
    run;
%end;

/* OUTLR= data set -- Likelihood ratio test of equal variances */
%if &outlr ne %then %do;
  data &outlr;
    set _lreqv;
    run;
%end;
options nonotes;

/******************** Clean up temporary data sets ******************/
%clean:
proc datasets nolist;
  delete _tmp _work _old _iter _mnfit _mndiag _meanest
         _varfit _vardiag _varests _lreqv
         %if &score ne %then _score;
         ;
  quit;

title;
%exit: 
options notes stimer;
%let _print_=on;
%mend varmod;

