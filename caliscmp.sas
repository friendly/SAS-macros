 /*--------------------------------------------------------------*
  *    Name: caliscmp.sas                                        *
  *   Title: Compare model fits from PROC CALIS                  *
        Doc: http://www.datavis.ca/sasmac/caliscmp.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 25 Oct 2000 08:55:11                                *
  * Revised:  8 Nov 2001 17:45:19                                *
  * Version: 1.0                                                 *
  *  Added more fit indices for V8                               *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 PROC CALIS can fit only one model at a time.  It cannot, therefore,
 provide the comparative model tests given by other SEM/CFA software,
 (e.g., AMOS), which can fit multiple models together.

 The CALISCMP macro extracts goodness of fit statistics from the
 RAM data sets produced in a series of models fit to a given data
 set using PROC CALIS.  The COMPARE= parameter allows pairs of
 nested models to be tested.

=Usage:

 For each model, use the OUTRAM= option on the PROC CALIS statement
 to same the model fit statistics in a separate data set.  For example,
 
		proc calis data=lord cov summary outram=ram1;
		lineqs ....
		proc calis data=lord cov summary outram=ram2;
		lineqs ....
		...

 The CALISCMP macro is defined with keyword parameters.  The RAM=
 parameter must specify a list of one or more OUTRAM data sets.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%caliscmp(ram=ram1 ram2 ram3 ram4,
		models=%str(H1 rho=1/H2/H3 rho=1/H4),
		compare=1 2 / 3 4 /1 3/ 2 4);

 
==Parameters:

* RAM=        List of the names of two or more RAM data sets from 
              CALIS runs, separated by spaces.

* MODELS=     List of model descriptions or labels, separated by '/',
              in the order corresponding to the RAM= list.  If not
				  specified, the models are labeled 'Model 1', 'Model 2', ...

* STATS=      List of statistics to be printed for each model.
              [Default: STATS=NPARM DF CHISQUAR P_CHISQ RMR GFI AGFI AIC
				  CAIC SBC CENTRALI PARSIMON CNHOELT]

* COMPARE=    List of pairs of models to be compared, separated by '/'.
              Each pair consists of two integers, referring to the
				  models specified in the RAM= and MODELS= lists.
				  
              Each pair should refer to two nested models, where the 
				  first is the more restrictive or more constrained.
				  For each pair, the macro calculates the difference in
				  Chi-squares, which is tested as a Chi-square on the 
				  difference in degrees of freedom.

* OUT=        Name of the outoput data set containing the fit statistics
              for all models. [Default: OUT=RAMSTATS]
                
=Example:

 The SAS/STAT User's Guide (``Intro to Structural Equations...'')
 describes four models fit to Lord's data on four vocabulary tests,
 two short tests with liberal time limits, two long, speeded tests.
 These models posit one factor for the unspeeded tests, and a second factor
 for the speeded tests. The interesting psychometric questions are
 (a) whether each pair of tests can be considered parallel, and
 (b) whether the two-factors could be collapsed into one (rho=1).
 
 After fitting these models with separate PROC CALIS calls, compare
 the fit statistics, and test for differences among pairs of models
 as follows:
 
	%caliscmp(ram=ram1 ram2 ram3 ram4,
		models=%str(H1 rho=1/H2/H3 rho=1/H4),
		compare=1 2 / 3 4 /1 3/ 2 4);

 This produces the following output:
 
                  Model Comparison Statistics from 4 RAM data sets
  
                                                         RMS
  Model      Parameters   df   Chi-Square   P>ChiSq   Residual     GFI     Adj GFI
  
  H1 rho=1        4        6     37.3412    0.00000    2.53409   0.97048   0.95081
  H2              5        5      1.9320    0.85847    0.69829   0.99849   0.99699
  H3 rho=1        8        2     36.2723    0.00000    2.43656   0.97122   0.85608
  H4              9        1      0.7033    0.40168    0.27150   0.99946   0.99458
  
                                    Schwarz                            Critical
  Model         AIC       C_AIC       BIC     Centrality   Parsimony       N
  
  H1 rho=1    25.3412    -7.5114    -1.5114     0.97614     0.97454      219.51
  H2          -8.0680   -35.4452   -30.4452     1.00237     0.83224     3714.12
  H3 rho=1    32.2723    21.3214    23.3214     0.97394     0.32509      108.04
  H4          -1.2967    -6.7722    -5.7722     1.00023     0.16659     3540.52


           Model Comparison                 ChiSq   df   p-value 
          ---------------------------------------------------------
          H1 rho=1 vs. H2                 35.4092    1   0.00000 ****
          H3 rho=1 vs. H4                 35.5690    1   0.00000 ****
          H1 rho=1 vs. H3 rho=1            1.0689    4   0.89918     
          H2 vs. H4                        1.2287    4   0.87335     
 =*/


%macro caliscmp(
	ram=,      /* list of RAM data sets from CALIS runs */
	models=,   /* list of model descriptions */
	stats=nparm df chisquar p_chisq rmr gfi agfi aic caic sbc centrali parsimon cnhoelt,    /* list of statistics to be printed for each model */
	compare=,   /* list of pairs of models to be compared */
	out=ramstats
	);

%let abort=0;
%if %length(&ram)=0 %then %do;
	%put ERROR:  The RAM= parameter must specify a list of RAM data sets;
	%let abort=1;
	%goto done;
	%end;
	
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		data _null_;
			set sashelp.vtitle(obs=2);
			if _n_=2 then call symput('title2', text);
		%end;
	%else %do;
	   options nonotes;
		%end;

*-- Combine the OUTRAM data sets, select the _TYPE_='STAT' statistics;
data _stats_;
	length model $40;
	set
	%let i=1;
	%let dsn = %scan(&ram, 1, %str( ));
	%do %while (&dsn ne );
			&dsn (in=in&i)
		%let i = %eval(&i+1);
		%let dsn = %scan(&ram, &i, %str( ));
		%end;
	%let n = %eval(&i-1);
	   ;
	%do i=1 %to &n;
		if in&i then do;
			model = "%scan(&models, &i, %str(/))";
			if model = ' ' then model = "Model &i";
			end;
		%end;	

   keep model _name_ _estim_;
   if _type_='STAT';
run;
		
proc transpose data=_stats_ out=&out(drop=_label_ _name_);
   by model notsorted;
   var _estim_;
data &out;
	set &out nobs=nmod end=eof;
	label
		model='Model'
		n='N'
		fit='Fit Criterion'
		nparm='Parameters'
		df='df'
		chisquar ='Chi-Square'
		p_chisq = 'P>ChiSq'
		rmr='RMS Residual'
		gfi='GFI'
		agfi='Adj GFI'
		chisqnul='Null model ChiSq'
		aic='AIC'
		caic='Consistent AIC'
		sbc='Schwarz BIC'
		centrali='Centrality'
		parsimon='Parsimonious NFI'
		cnhoelt='Critical N'
		ztestwh = 'Wilson-Hilferty Z'
		%if %sysevalf(&sysver  >= 7) %then %do;
			rmseaest = 'RMSEA Estimate'
			rmsealob = 'RMSEA Lower bound'
			rmseaupb = 'RMSEA Upper bound'
			compfiti = 'Comparative fit index'
			evci_est = 'ECVI Estimate'
			ecvi_lob = 'ECVI lower-90% limit'
			ecvi_upb = 'ECVI upper-90% limit'
			bb_nonor = 'Bentler-Bonet non-normed index'
			bb_normd = 'Bentler-Bonet normed fit index'
			bol_rho1 = 'Bollen normed Rho1'
			bol_del2 = 'Bollen non-normed Delta2'
			%end;
		;
	if eof then call symput('nmod', trim(left(put(nmod,3.0))));
	run;
%put nmod=&nmod;
title2 "Model Comparison Statistics from &nmod RAM data sets";
proc print label;
   id model;
   var &stats;
run;

%if %length(&compare) %then %do;
proc format;
	value stars     0 - 0.0001 = '****'
	             0.0001<-0.001 = '*** '
					 0.001 <-0.01  = '**  '
					 0.01 <-0.05   = '*   '
					 other         = '    ';
data _null_;
	set &out end=eof;
	file print;
	length comp modcmp $200 pair $20;
	array chi{&nmod} chisq1-chisq&nmod;
	array dof{&nmod} df1-df&nmod;
	array mod{&nmod} $40 model1-model&nmod;
	retain chisq1-chisq&nmod df1-df&nmod model1-model&nmod;
	chi[_n_] = chisquar;
	dof[_n_] = df;
	mod[_n_] = model;
	if eof then do;
		comp = "&compare";
		sep = ' vs. ';
		pair = scan(comp, 1, "/");
		indent = 5;
		statind = indent + 40;
*		put +indent '     ChiSq   df   p-value           Model Comparison' /
		    +indent '---------------------------------------------------------';
		put +indent ' Model Comparison ' @(statind+5) 'ChiSq   df   p-value '/
		    +indent '---------------------------------------------------------';
		do i=1 by 1 until (pair = ' ');
			m1 = input(scan(pair, 1), 4.0);
			m2 = input(scan(pair, 2), 4.0);
*			put pair= m1= m2=;
			if (m1^=. & m2^=.) then do;
				chisq = chi[m1] - chi[m2];
				ddf = dof[m1] - dof[m2];
				modcmp = trim(mod[m1]) || sep || trim(mod[m2]);
				if (chisq < 0) then do;
					chisq = - chisq;
					ddf = - ddf;
					modcmp = trim(mod[m2]) || sep || trim(mod[m1]) || ' [R]';
					end;
				p = 1 - probchi(chisq, ddf);
				stars = put(p, stars.);
*				put +indent chisq 10.4 ddf 5. p 10.5 +1 stars $4. +5 modcmp;
				put +indent modcmp @(statind) chisq 10.4 ddf 5. p 10.5 +1 stars $4.;
				end;
			pair = scan(comp, i+1, "/");
			end;
		end;
		run;
	%end;

%exit:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		title2 "&title2";
		%end;
	%else %do;
	   options notes;
		title2;
		%end;

%done:

%mend;
