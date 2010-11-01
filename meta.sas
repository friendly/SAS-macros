/*****************************************************************/
/*   NAME: meta.sas                                              */
/*  TITLE: Calculates meta-analytic indices                      */
/*         according to the Rosenthal & Rubin method             */
/*                                                               */
/* AUTHOR: Ioannis C. Dimakos                                    */
/* ORIGINAL: 24SEP95 - Presented at SUGI 21, p938-942            */
/* MODIFIED BY:  Michael Friendly (macrified) 28 Oct 1997 18:05  */
/* Revised: 31 Jul 2002 16:04:53                                 */
/*   - Fixed bug with select                                     */
/*                                                               */
/*****************************************************************/
 /*=
=Description:
 
 The meta macro calculates several meta-analytic indices from 
 summary statistics (F, t, r, z, p, chisq) for two or more 
 studies, each testing one or more hypotheses.  For each hypothesis,
 the program calculates an equivalent value of a z-statistic,
 a correlation (r), and Fisher Z transformation of r (zf).  These
 are summarized by unweighted and weighted means to give overall
 results.
 
=Usage:
 
 Prepare a data set containing one observation for each study-hypothesis
 to be included in the meta analysis.  The following information must
 be recorded:
		the sample size (n) per hypothesis,
		the type of statistic (F, t, r, z, p, chisq),
		the observed value of statistic,
		degrees of freedom (df),
		and p-value for each hypothesis in the meta-analysis.

==Parameters:

	data=_last_,   Name of the input data set
	id=,           Names of one or more variable(s) which identify study
	               & hypothesis
	n=n,           Name of the variable giving the Sample size for the
	               hypothesis test
	stat=stat,     Name of test statistic (f, t, r, z, p, chisq)
	value=value,   Name of the variable giving the value of the test statistic
	df=df,         degrees of freedom for the test statistic
	p=p,           p-value 
	out=metanal    Name of the output data set
 
 
=References:
   Rosenthal, R. (1991). Meta-analytic procedures for social research.
	   Newbury Park, CA: Sage. 
   Mullen, B. (1989). Advanced BASIC meta-analysis. Hillsdale, NJ:
		Lawrence Erlbaum Associates.  
                                                              

=Example:
%include goptions;

data studies;
	length authors $12;
	input study hyp stat $ value n df p authors &$:;
	label n='Sample size'
		hyp   ='Hypothesis'
		stat  ='Statistic used'
		value ='value of statistic'
		df    ='degrees of freedom'
		p     ='p-value';
cards;
  1  1  t   3.13   20  18 .0001   Bar & Foo 86
  1  2  t   2.03   10   8  .078   Bar & Foo 86
  2  1  r    .60   10   8  .067   Foo & Bar 89
  3  1  z   3.14   23   .   .     Foo 90
;

%meta(data=studies, id=study hyp);
=*/

%macro meta(
	data=_last_,
	id=,          /* Variable(s) which identify study & hypothesis */
	n=n,          /* Sample size for the hypothesis test           */
	stat=stat,    /* Name of test statistic (f, t, r, z, p, chisq) */
	value=value,  /* Value of the test statistic                   */
	df=df,        /* degrees of freedom for the test statistic     */
	p=p,          /* p-value */
	out=metanal
	);

data &out;
	set &data;
 /* transform initial criteria to meta-analytic criteria.  Use z
  * for significance level and r (and Fisher's z) for  effect size
  */

select; * (&stat);
 when (&stat='t')
  do;
   z=sqrt(&df*(log(1+(&value**2/&df))))*sqrt(1-(1/(2*&df)));
   r=sqrt(&value**2 /(&value**2 + &df));
	if &p=. then &p = 1 - probt(&value,&df);
  end;

 when (&stat in ('f', 'F'))
  do;
   z=sqrt(&df*(log(1+(&value/&df))))*sqrt(1-(1/(2*&df)));
   r=sqrt(&value/(&value+&df));
  end;

 when (&stat='chisq') 
  do;
   z=sqrt(&value);
   r=sqrt(&value/&n);
	if &p=. then &p = 1 - probchi(&value,&df);
  end;

 when (&stat in ('z', 'Z'))
  do;
   z=&value;
   r=sqrt(&value**2/&n);
  end;

 when (&stat='r')
  do;
   t=(&value*sqrt(&n-2))/sqrt(1-&value**2);
   z=sqrt(&df*(log(1+(t**2/&df))))*sqrt(1-(1/(2*&df)));
   r=&value;
  end;

 otherwise
  do;
   z=abs(probit(&value));
   r=sqrt(z**2/&n);
  end;

 end;
 zf=.5*(log((1+r)/(1-r)));

 label 
  &p='p-value'
  z='z-value'
  zf='Fisher Z transformation of r'
  r='Pearson r';

/*
 Calculate:
 1) product of Sample Size n (the weight) and Z-score,
 2) squared sample size (w=n**2), to be used 
    in estimating the combined significance level.
 3) Weight for Diffuse Comparison of Effect Sizes.
*/
 nz=&n*z;
 w=&n**2;
 wzf=&n-3;
run;

 /* Calculations of Combinations of Effect Sizes and Significance
  * Levels.  Calculations of Diffuse Comparisons of E.Ss and S.Ls
  * Use separate PROC MEANS to calculate the various meta-analytic
  * indices.

 Step 1. Mean Effect Size Unweighted and 
         Weighted By Sample Size
*/

proc means noprint data=&out;
   var zf;
   output out=combzf1 mean=meanzf1;
   run;

proc means noprint data=&out;
   var zf;
   weight &n;
   output out=combzf2 mean=meanzf2;
   run;

 /* Step 2. Calculate chi^2 for Diffuse Comparison of Effect Sizes.
  * Chi^2 has k-1 degrees of freedom.
  */

proc means css noprint data=&out;
   var zf;
   weight wzf;
   output out=diffzf css=csszf;
   run;

 /* Step 3. Combinations and Diffuse Comparisons of S.L Calculate
  * sums of N*Z and Squared Weights to be used for Combination of
  * S.L, chi^2(df=k-1) for Diffuse Comparison of S.L.
  */

proc means noprint data=&out;
   var nz w;
   output out=sigcomb sum=sumnz sumw;
   run;

proc means noprint data=&out;
   var z;
   output out=sigdiff css=cssz;
   run;

/*
 Step 4. Final Calculations for 
		Combined Significance Level,
		Probability of Significance Level, and
		Probability of chi^2 for Diffuse Comparison 
		of Effect Sizes.
*/
data final;
   merge combzf1 combzf2 diffzf sigcomb sigdiff;
   zcomb=sumnz/sqrt(sumw);
   probcomb=1-probnorm(zcomb);
   probz=1-probchi(cssz,_FREQ_-1);
   dfz=_FREQ_-1;
   probzf=1-probchi(csszf,_FREQ_-1);
   dfzf=_FREQ_-1;
   keep meanzf1 meanzf2 zcomb cssz csszf
        dfz dfzf probcomb probz probzf;
   label meanzf1='Mean Effect Size, Unweighted'
      meanzf2   ="Mean Effect Size, Weighted by &n"
      zcomb     ='Z, Combination of Significance Levels'
      probcomb  ='Probability for Z'
      cssz      ='x2, Diffuse Comparison of Sig. Levels'
      probz     ='Probability of x2'
      dfz       ='degrees of Freedom'
      csszf     ='x2, Diffuse Comparison of Effect Sizes'
      probzf    ='Probability of x2'
      dfzf      ='degrees of Freedom';
   run;

/*
 Presentation Step 1.
 Print Primary Statistics of Individual Studies
*/
proc print data=&out label uniform;
 %if %length(&id) %then 
     %str(id &id;) ;
 var &n &stat &value &df &p z r zf;
 title  'Meta-Analysis: Initial Statistics and Transformations';
run;

/*
 Presentation Step 2.
 Print Meta-Analytic Statistics obtained with SAS
*/
proc print data=final label uniform noobs;
 var meanzf1 meanzf2 zcomb probcomb cssz
     dfz probz csszf dfzf probzf;
 title2 'Combinations and Diffuse Comparisons';
 title3 'of Effect Sizes and Significance Levels';
 footnote;
run;

/*
 Presentation Step 3.
 Chart of Effect Sizes.  
 Use PROC CHART if PROC GCHART unsupported.
*/
proc gchart data=&out;
 vbar zf /  midpoints=0 to 1 by .2 raxis=axis1;
 axis1 label=(a=90 r=0);
 title 'Frequency Distribution of Effect Sizes';
run;

/*
 Presentation Step 4.
 Plot Effect Sizes against Sample Sizes (aka the funnel plot)
 Use PROC PLOT if PROC GPLOT unsupported
*/

proc gplot data=&out;
 plot &n*zf/ haxis = 0 to 1 by .1 hminor=1
             vaxis = axis1;
 axis1 label=(a=90 r=0);
 title 'Plot of Fisher Zf and Sample Size';
run; quit
%mend meta;


