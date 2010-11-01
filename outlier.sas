/*-------------------------------------------------------------------*
  *    Name: outlier.sas                                              *
  *   Title: Robust multivariate outlier detection                    *
        Doc: http://www.datavis.ca/sasmac/outlier.html            
  *                                                                   *
  * Macro to calculate robust Mahalanobis distances for each          *
  * observation in a dataset. The results are robust in that          *
  * potential outliers do not contribute to the distance of any       *
  * other observations.                                               *
  *                                                                   *
  * The macro makes one or more passes through the data. Each         *
  * pass assigns 0 weight to observations whose DSQ value             *
  * has prob < PVALUE. The number of passes should be determined      *
  * empirically so that no new observations are trimmed on the        *
  * last pass.                                                        *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  16 Jan 1989 18:38:18                                    *
  * Revised:  01 Aug 2008 16:49:20                                    *
  * Version:  1.5-2                                                   *
  * 1.2- Added GPLOT option to produce graphic output (uses %LABEL)   *
  *      and PPLOT option to suppress proc plot version               *
  * 1.3- Added check for missing data (remove them)                   *
  *    - Added support for abbreviated VAR= lists                     *
  *    - Plot ID labels in printer plot (if &sysver>6.07)             *
  *    - Changed default pvalue to 0.05                               *
  *    - Label outliers with _CASE_ number if no ID= supplied         *
  * 1.4- Fixed bug when LAST observation contains missing values      *
  *      (thanks to Wim Aerts <wim.aerts@sbx.sas.com>)                *
  *      Fixed buglet with symbol= in PPLOT. Changed default PPLOT=N  *
  * 1.5  Modified to use %CQPLOT macro for plotting                   *
  *    - Eliminated persistent TITLE2                                 *
  *    - Added inline documentation                                   *
  *    - Prefix for principal components changed to _prin             *
  *    - Moved sorting step so that OUT= dataset is returned in order *
  *      of original data                                             *
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The OUTLIER macro calculates robust Mahalanobis distances for each
 observation in a data set.  The results are robust in that potential
 outliers do not contribute to the distance of any other observations. For
 a multivariate normal sample, the points will lie on a straight line of
 unit slope; outliers will have squared distances well above the line.
 A high-resolution plot of the distances against chi-square quantitles
 is produced, optionally using the CQPLOT macro, that provides confidence
 bands as well.

 The macro makes one or more passes through the data.  Each pass
 assigns 0 weight to observations whose DSQ value has 
 Prob  ( chi-square ) < PVALUE.  The number of
 passes should be determined empirically so that no new observations
 are trimmed on the last step.

=Usage:

 The OUTLIER macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%outlier(data=auto, id=model);
 
==Parameters:

* DATA=       Name of the input data set to analyze [Default: DATA=_LAST_]

* VAR=        List of input variables. You may use any of the standard
              SAS abbreviations, e.g., VAR=X1-X10. [Default: VAR=_NUMERIC_]

* ID=         Name of an ID variable for observations

* OUT=        Output dataset for plotting [Default: OUT=CHIPLOT]

* PVALUE=     The pvalue for the chi-square, used to determine when an
              observation is given a weight=0 [Default: PVALUE=.05]

* PASSES=     Number of passes of multivariate trimming [Default: PASSES=2]

* PRINT=      Print OUT= data set? [Default: PRINT=YES]

* PPLOT=      Produce printer plot? [Default: PPLOT=NO]

* GPLOT=      Produce graphics plot? [Default: GPLOT=YES]

* SYMBOL=     Point symbol [Default: SYMBOL=DOT]

* USECQPLT=   Use the %CQPLOT macro to plot? [Default: USECQPLT=Y]

* NAME=       Name for graphic catalog entry [Default: NAME=OUTLIER]

* GOUT=       name for graphic catalog [Default: GOUT=GSEG]
                

 =*/

%macro outlier(
   data=_LAST_,      /* Data set to analyze            */
   var=_NUMERIC_,    /* input variables                */
   id=,              /* ID variable for observations   */
   out=CHIPLOT,      /* Output dataset for plotting    */
   pvalue=.05,       /* Prob < pvalue --> weight=0     */
   passes=2,         /* Number of passes               */
   print=YES,        /* Print OUT= data set?           */
   pplot=NO,         /* Produce printer plot?          */
   gplot=YES,        /* Produce graphics plot?         */
   symbol=dot,       /* point symbol                   */
   useCQplt=Y,       /* use the %CQPLOT macro to plot? */
   name=outlier,     /* name for graphic catalog entry */
   gout=gseg         /* name for graphic catalog       */
   );

*-- Parse variables list if it contains special lists;
%let var=%upcase(&var);
%if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 data _null_;
 set &data (obs=1);
        %*  convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
	  put "NOTE: The VAR=&VAR list translates to: VAR=" _vlist_;
     call symput( 'VAR', trim(_vlist_) );
 RUN;
 %let nvar = %words(&var);
 /*
	data _null_;
		if 0 set &data;
		array _var{*} &var;;
		call symput('nvar',trim(left(put(dim(_var),12.))));
	run;
*/
%end;

%let pplot=%substr(%upcase(&pplot),1,1);
%let gplot=%substr(%upcase(&gplot),1,1);
 /*-------------------------------------------------------*
  | Add WEIGHT variable. Determine number of observations |
  | and variables, and create macro variables.            |
  | Delete missing observations.                          |
  *-------------------------------------------------------*/
data in;
   set &data end=lastobs;
	retain _nobs_ _nmiss_ 0;
	drop _nmiss_ _nobs_;
   array invar{*} &var;
   _weight_ = 1;               /* Add weight variable */
	_miss_ = nmiss(of &var);
	if _miss_>0 then
		_nmiss_+1;
	else _nobs_+1;
   if ( lastobs ) then do;
      call symput('NOBS', trim(left(put(_nobs_,8.))) );
      call symput('NVAR', left(put(dim(invar),3.)) );
		if _nmiss_>0 then 
		put 'WARNING: ' _nmiss_ 'of ' _n_ ' observations were omitted due to missing values.';
      end;
	if _miss_>0 then delete;

run;

options nonotes;
%do pass = 1 %to &PASSES;
   %if &pass=1 %then %let in=in;
               %else %let in=trimmed;
 /*--------------------------------------------------------------*
  | Transform variables to scores on principal components.       |
  | Observations with _WEIGHT_=0 are not used in the calculation,|
  | but get component scores based on the remaining observations.|
  *--------------------------------------------------------------*/
   proc princomp std noprint data=&in out=prin prefix=_prin;
      var &var;
      freq _weight_;
 
 /*-----------------------------------------------------------*
  | Calculate Mahalanobis D**2 and its probability value. For |
  | standardized principal components, D**2 is just the sum   |
  | of squares. Output potential outliers to separate dataset.|
  *-----------------------------------------------------------*/
   data out1    (keep=_pass_ _case_ &id dsq prob)
        trimmed (drop=_pass_ _case_ );
      set prin ;
      _pass_ = &pass;
      _case_ = _n_;
 
      dsq = uss(of _prin1-_prin&nvar);    /* Mahalanobis D**2 */
      prob = 1 - probchi(dsq, &nvar);
      _weight_ = (prob > &pvalue);
      output trimmed;
      if _weight_ = 0 then do;
         output out1   ;
         end;
   run;
   proc append base=outlier data=out1;
	run;
%end; /* %do passes */

   proc print data=outlier;
		id _pass_;
		by _pass_;
   title2 'Observations trimmed in calculating Mahalanobis distance';
 /*------------------------------------------*
  | Prepare for Chi-Square probability plot. |
  *------------------------------------------*/
options notes;
data &out;
   set trimmed;
   drop _prin1 - _prin&nvar;
   _weight_ = prob > &pvalue;
   expected = 2 * gaminv(_n_/(&nobs+1), (&nvar/2));
   label dsq     ='Squared Distance'
         expected='Chi-square quantile';

/*
*-- Restore the &ID variable to the OUT= dataset;
%if %length(&id) %then %do;
	data &out;
		merge in (keep=&id) &out;
	%end; 
*/
proc sort data=trimmed;
   by dsq;
	run;

%if %upcase(&print)=YES %then %do;
proc print data=&out;
   %if &id ^=%str() %then
   %str(id &id;);
	var _weight_ dsq prob &var;
   title2 'Possible multivariate outliers have _WEIGHT_=0';
   run;
%end;


%if &pplot=Y or &gplot=Y %then %do;
%if &useCQplt=Y %then %do;
	%put OUTLIER: Running CQPLOT;
	title2;
	%cqplot(data=&out, dsq=dsq, nvar=&nvar, pplot=&pplot, gplot=&gplot, id=&id,
		label=_weight_=0, detrend=NO);
	%end;

%else %do; 
   %if &pplot = Y 
	   %then %do;
	   %if &ID = %str() 
		   %then %let sym='*';
		   %else %do;
			   /* Use proc plot point labels, but just for outliers */
			   %if &sysver > 6.07
				   %then %do;
				   data &out;
					   set &out;
					   if _weight_=0 then _id_ = &id;			
				   %let sym = '*' $ _id_;
				   %end; 
			   /* Not available before 6.07 */
				   %else %let sym=&id;
			   %end;

   proc plot data=&out;
	  plot dsq      * expected = &sym
           expected * expected = '.'   /overlay hzero vzero;
   title2 'Chi-Squared probability plot for multivariate outliers';
   run;
   %end;

   %if &gplot = Y 
	   %then %do;
		   %let anno=;
		   %if %length(&id) = 0 %then %do;
			   %let id = _case_;
			   %end;
		   %label(data=&out, x=(expected-.2), y=dsq, text=&id, pos=4,
			   subset=(_weight_=0));
			   %let anno = anno=_label_;

		   title2;
		   proc gplot data=&out;
			   plot dsq      * expected = 1
					   expected * expected = 2
					   / overlay &anno hm=1 vm=1 vaxis=axis1
						 name="&name"
						 des="Outlier plot of &data";
			   axis1 label=(a=90);
			   symbol1 v=&symbol i=none c=black;
			   symbol2 v=none i=join c=red;
			   run;
	   %end;
	%end;
%end;
	
%done:
options nonotes;
proc datasets nofs nolist nowarn;
   delete outlier out1 trimmed;
	run; quit;
options notes;
title2;
%mend;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;

