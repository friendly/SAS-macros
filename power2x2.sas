 /*-------------------------------------------------------------------*
  |     Name: power2x2.sas                                            |
  |    Title: Power for testing two independent proportions           |
         Doc: http://www.datavis.ca/sasmac/power2x2.html           
  | ------------------------------------------------------------------|
  |    Procs: print tabulate sort plot gplot                          |
  |  Macdefs: power2x2                                                |
  | ------------------------------------------------------------------|
  | Original: Modified from POWER2x2.SAS by SAS Institute             |
  |   Author: Michael Friendly               <friendly@yorku.ca>      |
  |  Created: 12 May 1999 10:16:12                                    |
  |  Revised: 19 Aug 1999 09:33:12                                    |
  |  Version: 1.1                                                     |
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:

The POWER2X2 macro computes the power of a test comparing proportions
from two, equal-sized, independent samples.  Power is given for
various sizes of the total sample, or required sample size is given
for various power values, allowing you to pick the sample size that
achieves the desired power.                                                                      

=Usage:

 The POWER2X2 macro takes 9 keyword arguments.  You must supply the
 DIFF= parameter.  By default the macro computes power for a range
 of sample sizes (given by NMIN= and NMAX=).  Alternatively, you may
 specify a range of power values (given by POWER=) for which the
 required sample size is calculated.

==Parameters:

* P1=.5      Specifies an estimate of the "success" rate in one
			    group, the baseline group. [Default: P1=.50]

* DIFF=      Specifies the difference in the proportions that you 
             want to detect.  This is the specification of the 
             alternative hypothesis at which power is computed.
			    The difference MUST be specified; there is NO default.
				 You may specify a list of values separated by commas, a
				 range of the form x TO y BY z, or a combination of these.
             However, you must surround the DIFF= value with
             %STR() if any commas appear in it.  For example,

                     diff=.10 to .30 by .05
                     diff=%str(.10, .13, .20)

* ALPHA=.05  Specifies the significance level or size of the test. 
             It is a decimal value less that 1.  
             For example, ALPHA=.05 sets the probability of a Type
             1 error at 0.05.  You may specify a single value, or
				 a list of values separated by commas, or a range of the
				 form x TO y by z.  [Default: ALPHA=.05]

* POWER=     Values of power for sample size calculation.
             You may specify a list of values separated by commas, a
			    range of the form x TO y BY z, or a combination of these,
				 as in a DO statement.
             However, you must surround the POWER= value with
             %STR() if any commas appear in it.

* NMIN=10    Specifies the minimum total sample size at which power 
             will be computed.  [Default: NMIN=10]

* NMAX=200   Specifies the minimum total sample size at which power
             will be computed.  [Default: NMAX=200]
             To get power for a single total sample size, set NMIN and
             NMAX to half of the total sample size.

* PLOT=      is a specification for plotting the results, in the form
             Y * X or Y * X = Z, where X, Y, and Z may be any of the
				 variables N, DIFF, P2, POWER or OR.  No plots are produced 
				 if PLOT=  is blank.  [Default: PLOT=POWER * N=DIFF]

* PLOTBY=    is another variable in the OUT= data set.  Separate plots are
             drawn for each level of the PLOTBY= variable.

* OUT=       The name of the output data set. [Default: OUT=_POWER_]

=Example:

	  	%power2x2( p1=.6,  diff=.10 to .20 by .05,  nmin=50);

 With the settings above, the expected baseline success rate is 60%.
 Power for detecting a difference of 10-20% in the two proportions will
 be computed for a .05 level test and for sample sizes ranging from
 50 to 200.

 PRINTED OUTPUT:
 Using the settings shown, the following output is generated:


                 Power for testing two independent proportions
           Two-tailed test, alpha=.05, p1=0.6 diff=.10 to .20 by .05

                   -----------------------------------------
                   |                  |     Diff p1-p2     |
                   |                  |--------------------|
                   |                  | 0.1  | 0.15 | 0.2  |
                   |------------------+------+------+------|
                   |Total Sample Size |      |      |      |
                   |------------------|      |      |      |
                   |50                | 0.116| 0.209| 0.353|
                   |------------------+------+------+------|
                   |60                | 0.129| 0.242| 0.410|
                   |------------------+------+------+------|
                   |70                | 0.143| 0.274| 0.465|
                   |------------------+------+------+------|
                   |80                | 0.156| 0.306| 0.516|
                   |------------------+------+------+------|
                   |90                | 0.170| 0.337| 0.564|
                   |------------------+------+------+------|
                   |100               | 0.184| 0.368| 0.609|
                   |------------------+------+------+------|
                   |110               | 0.198| 0.398| 0.650|
                   |------------------+------+------+------|
                   |120               | 0.211| 0.428| 0.688|
                   |------------------+------+------+------|
                   |130               | 0.225| 0.456| 0.722|
                   |------------------+------+------+------|
                   |140               | 0.239| 0.484| 0.754|
                   |------------------+------+------+------|
                   |150               | 0.252| 0.511| 0.782|
                   |------------------+------+------+------|
                   |160               | 0.266| 0.537| 0.807|
                   |------------------+------+------+------|
                   |170               | 0.280| 0.562| 0.830|
                   |------------------+------+------+------|
                   |180               | 0.293| 0.586| 0.851|
                   |------------------+------+------+------|
                   |190               | 0.306| 0.609| 0.869|
                   |------------------+------+------+------|
                   |200               | 0.320| 0.631| 0.885|
                   -----------------------------------------

=Details:

 Hypotheses in the test are:                                          

        H0: p1 = p2                                                       
        Ha: p1 ne p2                                                      

 where p1 and p2 are the success probabilities in the two
 populations.  The Pearson chi-square statistic tests the null
 hypothesis (H0) against the alternative hypothesis (Ha) and is
 available in the FREQ procedure when the CHISQ option is specified
 on the TABLES statement.
                                                                      
 The power is the probability of rejecting H0 and is a function of
 the true difference in proportions.  Power is often computed
 assuming many different settings of the true proportions.  The type
 2 error rate (denoted beta) is the probability of accepting H0 for
 some non-zero true difference and is equal to 1-power.  The power
 and beta are computed for a range of total sample sizes at a
 particular alternative hypothesis that you specify.  It is assumed
 that the total sample size will be split equally between the two
 samples.

=References:

* Agresti, A. (1990), Categorical Data Analysis, New York: John Wiley
       & Sons, Inc.
* Agresti, A. (1996), An Introduction to Categorical Data Analysis,
	    New York: John Wiley & Sons, Inc.

 =*/

/************************************************************************

                                POWER2x2

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   REQUIRES:
     POWER2x2 requires only Version 6 base SAS Software.

   USAGE:
     POWER2x2 is a macro program.  The options and allowable values are:

   SEE ALSO:
     PWR2x2un -- Computes the power of a test comparing proportions from
                 two, unequally-sized, independent samples.

     POWERRxC -- Computes power for Pearson and Likelihood Ratio
                 Chi-square tests of independence in FREQ.  Handles any 
                 number of rows and columns in a two-way table.

************************************************************************/

%macro power2x2(
	p1 = .5,     /* Success probability in baseline group       */
	diff =,      /* difference in proportions to be detected    */ 
	alpha = .05, /* alpha-level of test                         */
	power=,      /* Values of power for sample size calculation */
	nmin = 10,   /* Minimum sample size to consider             */
	nmax = 200,  /* Maximum sample size to consider             */
	plot =power * n=diff,    /* plot request                    */
	plotby =,
	print =diff n power or,  /* variables to be printed         */    
	out=_power_
	);
	
%if %length(&diff)=0 %then %do;
	%put ERROR: The required difference in proportions must be specified;
	%goto done;
	%end;
	
data &out;

  p1=&p1;
  do alpha=&alpha;

  /********************** Compute power ************************/

	za = probit(alpha/2);
	%if %length(&power)>0 %then %do;
		do diff = &diff;
			p2 = p1 + diff;
			if (0 < p2 < 1) then do;
				or = (p2 / (1-p2)) / (p1 / (1-p1));
				do power = &power;
					zb = probit(1-power);
					n = 2 * ( (za+zb)**2 * (p1*(1-p1) + (p2*(1-p2))) ) / diff**2;
					n = round(n);
					output;
				end;
			end;
		end;
		drop za zb;
	%end;
	%else %do;    /* determine power for specified n */
		nmin=&nmin;
		nmax=&nmax;
		/* Select 'nice' range of total sample sizes */
		diforder=10**(max(floor(log10(nmax-nmin+1e-8)),1)-1);
		normlen=(nmax-nmin)/diforder;
		step=diforder*((normlen<=20)+2*(20<normlen<=40)+5*(40<normlen<=100));
		
		do n=nmin, nmin+step-mod(nmin+step,step) to nmax by step, nmax;
			do diff = &diff;
				/* power and beta computation */
				p2 = p1 + diff;
				or = (p2 / (1-p2)) / (p1 / (1-p1));
				var = 2*( p1*(1-p1) + p2*(1-p2) ) / n;
*				power=1-probnorm(-za - diff*sqrt(n/(4*p*(1-p)))) +
							probnorm( za - diff*sqrt(n/(4*p*(1-p))));
				power=1-probnorm(-za - diff/sqrt(var)) +
						  probnorm( za - diff/sqrt(var));
				output;
			end;
			if n=round(nmax) then stop;
		end;
	end; /* do &alpha */
		drop diforder normlen step nmin nmax za var;
  %end;
  
  label n='Total Sample Size'
	power='Power'
	diff='Diff p1-p2'
	or = 'Odds ratio';
  run;

%if %length(&print)>0 %then %do;
proc print data=&out noobs split=' ';
  var &print;
  %if %length(&power)=0 %then %do;
	title  "Power for testing two independent proportions";
	%end;
	%else %do;
	title  "Sample size for testing two independent proportions";
	%end;
  title2 "Total sample size to be split equally between the groups";
  title3 "Baseline p1=&p1; p1-p2=&diff; alpha=&alpha";
  run;
%end;

proc tabulate data=&out format=6.0;
	class diff n;
	var power;
	table n,  diff *power=' '*f=6.3  * sum=' ';
	title2 "Two-tailed test, alpha=&alpha, p1=&p1 diff=&diff";
run;

%if %length(&plot)>0 %then %do;
%if %length(&plotby) %then %do;
proc sort data=&out;
	by &plotby;
%end;

title2 "Baseline: p1=&p1; p1-p2=&diff; alpha=&alpha";
proc plot data=&out;
	plot &plot /box ;
	run;

proc gplot data=&out uniform;
	plot &plot / frame hminor=1 vaxis=axis1 haxis=axis2;
	%if %length(&plotby) %then %do;
		by &plotby;
	%end;

	axis1 label=(a=90);
	axis2 offset=(3);
	symbol1 v=circle   i=join l=1 c=black;
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
title;
%done:
%mend;

