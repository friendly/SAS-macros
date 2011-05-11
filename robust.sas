 /*-------------------------------------------------------------------*
  * Name:   robust.sas                                                *
  * Title:  M-estimation for robust models fitting via IRLS           *
  *   Doc:  http://datavis.ca/sasmac/robust.html              *
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:   2 Dec 1996 11:34:22                                    *
  * Revised:  28 Mar 2008 13:04:05                                    *
  * Version:  1.2-1                                                   *
  *  1.2  Fixed some errors with LOGISTIC                             *
  *       Allow CLASS= with LOGISTIC for V8+                          *
  *       Added label for _weight_ in output data set                 *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The ROBUST macro uses iteratively reweighted least squares to  
 fit linear models by M-estimation.  The weights are determined  
 by the BISQUARE, HUBER, LAV or OLS function.  The fitting procedure 
 can be PROC REG, GLM or LOGISTIC 
 
 For ANOVA and regression models normally conducted with PROC REG and
 PROC GLM, this macro is superceded by PROC ROBUSTREG.  Even so, it
 provides an example of how robust methods can be added to other SAS
 procedures.            

=Usage:

 The ROBUST macro is called with keyword parameters.
 The RESPONSE= and MODEL= parameters are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%include data(icu);
	%robust(data=icu, response=died, model=age cancer uncons admit,
		proc=logistic, id=id, iter=3);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* RESPONSE=   The name of the response variable in the model

* MODEL=      The right-hand-side of the MODEL statement

* PROC=       The name of the estimation procedure to be used,
              one of REG, GLM, or LOGISTIC.  
				  [Default: PROC=LOGISTIC]

* CLASS=      The names of any CLASS variables in the MODEL (for GLM 
              or LOGISTIC in V8+)

* ID=         The names of any observation ID variables.  These are
              simply copied to the OUT= data set.

* OUT=        The name of the output data set of observation statistics.
              [Default: OUT=RESIDS]

* OUTPARM=    The name of the output data set of parameter estimates
              on the final iteration.

* FUNCTION=   Weight function, one of HUBER, LAV (least absolute value),
              BISQUARE, or OLS. [Default: FUNCTION=BISQUARE]

* TUNE=       Tuning constant for BISQUARE or HUBER.  The weighting
              function is applied to the value _RESID_ / (&TUNE * MAD)
				  where MAD is the median absolute value of the residuals.
              The default is TUNE=6 for the BISQUARE function, and 
				  TUNE=2 for the HUBER function.

* ITER=       The maximum number of iterations [Default: ITER=10]

* CONVERGE=   The maximum change in observation weights for convergence.
              The value must have a leading 0. [Default: CONVERGE=0.05]

* PRINT=      Controls printing of intermediate and final results
              [Default: PRINT=NOPRINT].

 =*/

%macro robust(
		data=_LAST_, 
		response=,        /* response variable                         */
		model=,           /* RHS of model statement                    */
		proc=REG,         /* estimation procedure: GLM, REG, LOGISTIC  */ 
		class=,           /* class variables (GLM only)                */
		id=,              /* ID variables                              */
		out=resids,       /* output observations data set              */
		outparm=,         /* output parameters data set                */
		function=bisquare, /* weight function: BISQUARE, HUBER or LAV  */
		tune=,            /* tuning constant for bisquare/huber        */
		iter=,            /* max number of iterations                  */ 
		converge=0.05, 	/* max change in weight for convergence.     */
                        /* NB: must have leading 0                   */
		print=no
		);

%let abort=0;
%let proc = %upcase(&proc);
%let doparm = %index(REG LOGISTIC,&proc) ;	%* Getting parameter estimates?;

%if %index(REG LOGISTIC,&proc)
	%then %let outparm = outest;
	%else %let outparm = outstat;


%let r=r;
%if &proc = GLM %then %let r=rstudent;
%if &proc = LOGISTIC %then %let r=resdev;

%if %length(&iter)=0 %then %do;
	%let iter=10;
	%if &proc = LOGISTIC %then %let iter=4;
	%end;
		
%let function = %upcase(&function);
%if &tune = %str() %then %do;
	%if &function = BISQUARE %then %let tune = 6;
	%else %let tune = 2;
%end;

%let print = %upcase(&print);

data resids;
	set &data;
	_weight_ = 1;
	lastwt = .;
		
%do it = 1 %to &iter;

	%let pr=noprint;
	%if &print = PRINT %then %let pr=;
	%else %if %index(&print,NOPRINT) %then %let pr=NOPRINT;
	%else %if %index(&print,&it)  %then %let pr=;
	
	%*-- Remove parmest data set from a prior run;
	%if &it=1 %then %do;
	proc datasets nolist nowarn;
		delete parmest;
	%end;
	
	%*-- Fit the model, using current weights;
	proc &proc data=resids %if &it > 1 %then (drop=_resid_ _fit_ _hat_);
		 &outparm=parms
		 &pr;
		weight _weight_;			%*-- observation weights;
		%if %length(&class)>0 & (&proc=GLM or (&proc=LOGISTIC and &sysver>=8))
		  %then %do;
			class &class;
 		  %end;
		model &response = &model;
		output out=newres &r=_resid_ p=_fit_ h=_hat_;
		title3 "Iteration &it";
	run;
	%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

	options nonotes;
	%*-- Find the median absolute residual;
	data resids;
		set newres;
		absres = abs(_resid_);
	
	%*-- Find median absolute deviation (MAD);
	proc univariate data=resids noprint;
		var absres;
		output out=sumry median=mad;
	
	%*-- Calculate new weights;
	data &out;
		set resids end=eof;
		drop w mad _maxdif_ absres lastwt;
		retain _maxdif_ 0;
		lastwt = _weight_;
		if _n_=1 then set sumry(keep=mad);
		label _weight_ ="&function weight";

		if _resid_ ^= . then do;
			%*-- scaled residual;
			w = _resid_ / (&tune * mad);
			%if &function = BISQUARE    %then %bisquare(w);
			%else %if &function = HUBER %then %huber(w);
			%else %if &function = LAV   %then %lav(w);
			%else _weight_=1;	 /* OLS */
			
			_maxdif_ = max(_maxdif_, abs(_weight_-lastwt));
		end;
		
		if eof then do;
		*	file print;
			put "NOTE: iteration &it  " _maxdif_=;
			call symput('maxdif',left(put(_maxdif_,6.4)));
			end;	
	run;
	%*if &doparm %then %do;
	data parms;
		iter = &it;
		set parms;
		_maxdif_ = input("&maxdif", best.);
	proc append base=parmest new=parms;
	run;
	%*end;
			
	%if &maxdif < &converge %then %goto fini;

%end;
%fini:;
	data parmest;
		set parmest;
	%if &doparm %then %do;
		drop _type_ 
		%if &proc=REG %then _model_ _depvar_  &response;
		;
		title3 'Iteration history and parameter estimates';
	%end;
	%else %do;
		drop _name_ prob;
		if _type_='SS1' then delete;
		title3 'Iteration history and test statistics';
	%end;		
	proc print data=parmest;
		id iter;
		run;
	
	%if %length(&outparm)>0 %then %do;
	data &outparm;
		set parmest end=eof;
		drop iter _maxdif_;
		if eof then output;
	%end;

%if %index(&print,NO)=0 %then %do;
proc print data=&out;
	%if &id ^= %str() | &class ^= %str() %then %do;
		id &class &id;
	%end;
		var &response _fit_ _weight_ _resid_ _hat_ flag;
		title3 'Residuals, fitted values and weights';
		run;
%end;
title3;
%done:
options notes;
%mend;

%macro bisquare(w);
	if abs(&w) < 1 
		then do; _weight_ = (1 - &w**2) **2;  flag=' '; end;
		else do; _weight_ = 0;                flag='*'; end;
%mend;

%macro huber(w);
	if abs(&w) < 1 
		then do; _weight_ = 1;               flag=' '; end;
		else do; _weight_ = 1/abs(&w);       flag='*'; end;
%mend;

%macro lav(w);
	_weight_ = 1/(absres +(absres=0));
%mend;

