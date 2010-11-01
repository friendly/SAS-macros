 /*-------------------------------------------------------------------*
  * Name:   bisquare.sas                                              *
  * Title:  M-estimation for robust models fitting via IRLS           *
  * Robust (bisquare) model fitting with GLM/REG/LOGISTIC.  
	Uses IRLS, with weights determined by the bisquare function        */
/*-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
 * Created:   8 Jun 1995 10:20:14                                    *
 * Version:  1.0                                                     *
 *   BISQUARE is now superceded by ROBUST.SAS                        *
 *-------------------------------------------------------------------*/

%macro bisquare(
		data=_LAST_, 
		response=,        /* response variable                         */
		model=,           /* RHS of model statement                    */
		proc=GLM,         /* estimation procedure: GLM, REG, LOGISTIC  */ 
		class=,           /* class variables (GLM only)                */
		id=,              /* id variables                              */
		out=resids,       /* output data set                           */
		tune=6,           /* tuning constant for bisquare              */
		iter=10,          /* max number of iterations                  */ 
		converge=0.05, 	/* max change in weight for convergence.     */
                        /* NB: must have leading 0                   */
		print=noprint);

%put NOTE:  The BISQUARE macro has been superceded by the ROBUST macro;
options nonotes;
%let proc = %upcase(&proc);
%let doparm = %index(REG LOGISTIC,&proc) ;	%* Getting parameter estimates?;

%let print = %upcase(&print);
data resids;
	set &data;
	_weight_ = 1;
	lastwt = .;
		
%do it = 1 %to &iter;

	%let pr=noprint;
	%if &print = PRINT %then %let pr=;
	%else %if &print = PRINT %then %let pr=NOPRINT;
	%else %if %index(&print,&it)  %then %let pr=;
	
	%*-- Remove parmest data set from a prior run;
	%if &it=1 %then %do;
	proc datasets nolist nowarn;
		delete parmest;
	%end;
	
	%*-- Fit the model, using current weights;
	proc &proc data=resids %if &it > 1 %then (drop=residual fit);
		%if &doparm %then outest=parms;
		 &pr;
		weight _weight_;			%*-- observation weights;
		%if &class ^= %str() & &proc=GLM %then %do;
			class &class;
		%end;
		model &response = &model;
		output out=newres r=residual p=fit;
		title3 "Iteration &it";
	run;
	options nonotes;
	%*-- Find the median absolute residual;
	data resids;
		set newres;
		absres = abs(residual);
	
	proc univariate data=resids noprint;
		var absres;
		output out=sumry median=mad;
	
	%*-- Calculate new weights;
	data resids;
		set resids end=eof;
		drop w mad maxdif absres lastwt;
		retain maxdif 0;
		lastwt = _weight_;
		if _n_=1 then set sumry(keep=mad);

		if residual ^= . then do;
			%*-- scaled residual;
			w = residual / (&tune * mad);
			%*-- bisquare calculation;
			if abs(w) < 1 
				then do; _weight_ = (1 - w**2) **2;  flag=' '; end;
				else do; _weight_ = 0;               flag='*'; end;
			
			maxdif = max(maxdif, abs(_weight_-lastwt));
		end;
		
		if eof then do;
		*	file print;
			put "iteration &it  " maxdif=;
			call symput('maxdif',left(put(maxdif,6.4)));
			end;	
	run;
	%if &doparm %then %do;
	data parms;
		set parms;
		maxdif = input("&maxdif", best.);
	proc append base=parmest new=parms;
	run;
	%end;
			
	%if &maxdif < &converge %then %goto fini;

%end;
options notes;
%fini:
	%if &doparm %then %do;
	data parmest;
		set parmest;
		drop _model_ _type_ _depvar_ &response;
		
	proc print data=parmest;
		title3 'Iteration history and parameter estimates';
		run;
	%end;

proc print data=resids;
	%if &id ^= %str() | &class ^= %str() %then %do;
		id &class &id;
		var &response fit _weight_ residual flag;
		title3 'Residuals, fitted values and weights';
		run;
	%end;
title3;

%mend;
