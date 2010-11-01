 /*-------------------------------------------------------------------*
  *    Name: powerlog.sas                                             *
  *   Title: Power for logistic regression, quantitative predictor    *
        Doc: http://www.datavis.ca/sasmac/powerlog.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 17 Apr 1998 16:43:00                                     *
  * Revised: 02 Mar 2003 10:57:51                                     *
  * Version: 1.1                                                      *
  *                                                                   *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The POWERLOG macro calculates sample size required to achived given
 power values for a logistic regression model with one or more 
 quantitative predictors.  Results are displayed as a table of
 sample sizes required for a range of power values, and as a graph.

=Usage:

 The POWERLOG macro is called with keyword parameters.  The arguments 
 may be listed within parentheses in any order, separated by commas. 
 You must supply either an input data set containing the 
 variables P1, P2, ALPHA, POWER, and RSQ (one observation for each
 combination for which power is desired), or the macro parameters
 P1= and P2=.
 For example: 
 
 	%powerlog(p1=.08, p2=%str(.16, .24));
 
==Parameters:

* DATA=	 Specifies the name of an input data set containing the 
          variables P1, P2, ALPHA, POWER, and RSQ in all combinations
		    for which power is desired.  If an input DATA= data set is
		    specified, the program ignores values for the P1=, P2=, ALPHA=, 
		    POWER=, and RSQ= parameters.

* P1=     is the estimated probability of the event at the mean value
          of the quantitative predictor.

* P2=     is the estimated probability of the event at an X-value equal
          to the X-mean plus one standard deviation.
			 You may specify a list of values separated by commas, a
			 range of the form x TO y BY z, or a combination of these.
			 However, you must surround the P2= value with
			 %STR() if any commas appear in it.  For example,
			
				p2=.10 to .30 by .05
				p2=%str(.10, .13, .20)


* ALPHA=  is the desired Type I error probability for a *one-sided* test
          of H0: beta(x) = 0

* POWER=  is the desired power of the test.

* RSQ=    is the squared multiple correlation of the predictor with all
          other predictors.  Use RSQ=0 for a 1-predictor model.

* PLOT=   is a specification for plotting the results.  The default
          is PLOT=N * POWER=RSQ.  No plots are produced if  PLOT=  is
		    blank.

* PLOTBY= is another variable in the OUT= data set.  Separate plots are
          drawn for each level of the PLOTBY= variable.

* OUT=	 Specifies the name of the output data set

Reference:  Agresti, 'Introduction to Categorical Data Analysis', p.131

=Example:  

Modelling the relation of the probability of heart disease
on X = cholesterol.  If previous studies suggest that heart disease
occurs with P1=0.08 at the mean level of cholesterol, what is the
sample size required to detect a 50% increase (P2 = 1.5*.08 = .12),
or an 87.5% increase (P2 = 1.875*.08 = .15) in the probability of
heart disease, when cholesterol increases by one standard deviation?
	
If age is another predictor, how does sample size vary with the RSQ
between cholesterol and age?
	
   %powerlog(p1=.08, p2=%str(.12, .15), rsq=%str(.2, .4) );

=Notes:

 Internal documentation has been frozen.  See the web documentation
 at the Doc: link above for any updates.

=*/

%macro powerlog(
	data=,
	p1=,
	p2=,
	alpha=.05,
	power=.7 to .9 by .05,
	rsq=0 to .6 by .2,
	plot=N * power = rsq,
	plotby=theta,
	out=_power_
	);
	 
%if %length(&data)=0 %then %do;
	%if %length(&p1)=0 or %length(&p2) =0 %then %do;
		%put ERROR:  P1= and P2= must be specified.;
		%goto done;
	%end;

	%let data=_in_;
	data _in_;
		label p1='Pr(event) at X_mean'
				p2='Pr(event) at X_mean+std'
				alpha = 'Type I risk'
				power = 'Desired power'
				rsq   = 'R**2 (X, other Xs)';
		alpha = &alpha;
		do p1 = &p1;
			do p2 = &p2;
				do rsq = &rsq;
					do power = &power;
					output;
					end;
				end;
			end;
		end;
%end;

data &out;
	set &data;
	drop l1 l2 za zb lambda;
	label theta = 'Odds ratio'
	      lambda= 'Log(odds ratio)'
			delta = 'Delta'
			N     = 'Sample size';
	format theta 6.3 delta 6.2 n 6.1;
	l1 = p1 / (1-p1);
	l2 = p2 / (1-p2);
	theta = l2 / l1;
	lambda = log(theta);
	
	za = probit(1-alpha);
	zb = probit(power);
	
	delta = (1 + (1+lambda**2) * exp(5*lambda**2/4))
	      / (1 + exp(- lambda**2 / 4));
	
	N =  ( (za + zb * exp(- lambda**2 / 4))**2  * (1 + 2*p1*delta))
	  / (p1 * lambda**2); 
	
	N = N / (1-rsq);
/*	
proc print label;
	id p1 p2;
	by p1 p2;
*/
proc tabulate data=&out format=6.0;
	class alpha p2 rsq power;
	var n;
	table power='Power',  p2 *n=' '*f=5. 
		%if &rsq ^=0 %then %str(* rsq); 
		* sum=' ';
	title2 "One-tailed test, alpha=&alpha, p1=&p1 p2=&p2";

%if %length(&plot) %then %do;
%if %length(&plotby) %then %do;
proc sort data=&out;
	by &plotby;
%end;
proc gplot data=&out uniform;
	plot &plot / frame hminor=1 vaxis=axis1 haxis=axis2;
	%if %length(&plotby) %then %do;
		by &plotby;
	%end;

	axis1 label=(a=90);
	axis2 offset=(3);
	symbol1 v=circle   i=join l=1 c=black w=3;
	symbol2 v=dot      i=join l=3 c=red;
	symbol3 v=square   i=join l=5 c=blue;
	symbol4 v=triangle i=join l=7 c=green;
	symbol5 v=hash     i=join l=9 c=black;
	symbol6 v=diamond  i=join l=11 c=red;
	symbol7 v=star     i=join l=13 c=blue;
	format n 5.;
run; quit;
	title2;
	goptions reset=symbol;
%end;
%done:
%mend;

