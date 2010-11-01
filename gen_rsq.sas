/************************************************************************

    %GEN_RSQ macro:  Calculating the Generalized Coefficient of
                     Determination

	 Name: gen_rsq.sas
	Title: Calculate Generalized Coefficient of Determination for a GLM

    DISCLAIMER:
      THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
      ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
      EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
      PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
      CONTAINED HEREIN.

    PURPOSE:
      The %GEN_RSQ macro calculates the generalized coefficient of
      determination for a generalized linear model.

    REQUIRES:
      The %GEN_RSQ macro requires Version 6.12 or later of Base SAS software
		and SAS/STAT.

    USAGE:
      The macro input parameters are:

        NULLFIT= SAS data set that contains the MODFIT table from GENMOD for
		           the intercept only model.

       MODELFIT= SAS Data set that contains the MODFIT table from GENMOD for
		           the model of interest.

              N= The number of observations used for the analysis.  This number
				     should coincide with the number of observations in the dataset
					  that have non-missing values in the dependent and independent 
					  variables.

           DIST= Distribution of the dependent variable.  The value can be
			        any of the values that GENMOD accepts.

    LIMITATIONS:
      No error checking is done.  The macro assumes that dataset
      names are valid and exist. The user must first run the Intercept
		only model and the model of interest in GENMOD.  It also assumes that 
		these two models are run on the same data set.


    REFERENCES: Nagelkerke, N.J.D.(1991) "A Note on a General Definition of
	             the Coefficient of Determination," Biometrika 78 pp691-2.

    EXAMPLE:

data prostate;
title 'Prostate Data';
	input case age acid xray size grade nodalinv @@;
	lacd=log(acid);
datalines;
1  66  .48 0 0 0 0  2  68  .56 0 0 0 0  3  66  .50 0 0 0 0
4  56  .52 0 0 0 0  5  58  .50 0 0 0 0  6  60  .49 0 0 0 0
7  65  .46 1 0 0 0  8  60  .62 1 0 0 0  9  50  .56 0 0 1 1
10 49  .55 1 0 0 0  11 61  .62 0 0 0 0  12 58  .71 0 0 0 0
13 51  .65 0 0 0 0  14 67  .67 1 0 1 1  15 67  .47 0 0 1 0
16 51  .49 0 0 0 0  17 56  .50 0 0 1 0  18 60  .78 0 0 0 0
19 52  .83 0 0 0 0  20 56  .98 0 0 0 0  21 67  .52 0 0 0 0
22 63  .75 0 0 0 0  23 59  .99 0 0 1 1  24 64 1.87 0 0 0 0
25 61 1.36 1 0 0 1  26 56  .82 0 0 0 1  27 64  .40 0 1 1 0
28 61  .50 0 1 0 0  29 64  .50 0 1 1 0  30 63  .40 0 1 0 0
31 52  .55 0 1 1 0  32 66  .59 0 1 1 0  33 58  .48 1 1 0 1
;

proc genmod data=prostate;
	model nodalinv=lacd xray / dist=bin link=logit;
	make 'modfit' out=model;
*  ods output ModelFit=model         (for V7+);
	run;

proc genmod data=prostate;
	model nodalinv= / dist=bin link=logit;
	make 'modfit' out=nullfit;
*  ods output ModelFit=nullfit         (for V7+);
	run;


%gen_rsq(nullfit=nullfit,
	modelfit=model,
	n=33,
	dist= bin);

********************************************************************/
%macro gen_rsq(
	nullfit=  ,
	modelfit= ,
	n=        ,
	dist=,
	out=genrsq);


%if &sysver < 7
	%then %let crit=Criterio;
	%else %let crit=Criterion;

data int;
	set &nullfit ;
	if &crit = 'Deviance' then do;
		devint=value;
		output;
		end;	
	keep devint ;

data beta;
	set &modelfit;
	if &crit = 'Deviance' then do;
		devbeta=value;
		output;
		end;
	keep devbeta;

%if %upcase(&dist)= BIN or %upcase(&dist)=POI
or %upcase(&dist)= BINOMIAL or %upcase(&dist)=POISSON
or  %upcase(&dist)= B or %upcase(&dist)=P %then %do;

/*Apply the adjustment to the discrete model*/
data &out;
	merge int beta;
	diff=devbeta-devint;
	rsq=1-exp((1/&n)*diff);
	rsqmax=1-exp((-1/&n)*devint);
	adj=rsq/rsqmax;
	label rsq='R-Square'
			adj='Max-Rescaled R-Square';

proc print data=&out split='*' noobs;
title 'Calculation of the Generalized Coefficient of Determination';
	var rsq adj;

run;
%end;

%else %do;
/*Does not appy adjustment to Continuous Models*/
data &out;
	merge int beta;
	diff=devbeta-devint;
	rsq=1-exp((1/&n)*diff);
	label rsq='R-Square';

proc print data=&out split='*' noobs;
title 'Calculation of the Generalized Coefficient of Determination';
var rsq;

run;
%end;


title;
run;
%mend;
