/*****************************************************************/
/*                                                               */
/* PROGRAM NAME: HOMOVAR.SAS                                     */
/*                                                               */
/*       AUTHOR: Kristin Latour                                  */
/*                                                               */
/*      UPDATED: 11Mar92                                         */
/*      TITLE:   Various tests for homogeneity of variance       */
/*      PURPOSE: Calculates the following tests for              */
/*               homogeneity of variances:                       */
/*                                                               */
/*                     - O'Brien                                 */
/*                     - Brown-Forsythe                          */
/*                     - Levene                                  */
/*                     - Bartlett                                */
/*                     - Welch ANOVA F test                      */
/*                                                               */
/*               The macro is invoked with the following         */
/*               statement:                                      */
/*                                                               */
/*               %homovar(data=input_data_set,                   */
/*                        out=output_data_set,                   */
/*                        response=response_variable,            */
/*                        class=class_variable)                  */
/*                                                               */
/*               where INPUT_DATA_SET has variable               */
/*               RESPONSE_VARIABLE and CLASS_VARIABLE.           */
/*               RESPONSE_VARIABLE is the variable whose         */
/*               variances are to be tested for equality         */
/*               over all levels of CLASS_VARIABLE.              */
/*                                                               */
/*               All results are written to OUTPUT_DATA_SET.     */
/*                                                               */
/*        NOTES: Groups with only one observation, or all        */
/*               missing data are excluded from all analyses.    */
/*                                                               */
/*               Groups with only two observations are excluded  */
/*               from O'Brien's test.                            */
/*                                                               */
/*               The Brown-Frosythe statistic is corrected for   */
/*               artificial zeros occurring in odd sized         */
/*               samples.                                        */
/*****************************************************************/

%macro homovar(data=_last_,
			out=homovar,
			response=y,
			class=x,
			print=TESTS GROUPS,
			);
  %local i levels;

  %put HOMOVAR: data=&data response=&response class=&class out=&out;
  %let tempout=
         %substr(&out,1,%length(&data)-1);
  %*put tempout=&tempout;
	options nonotes;
	
  proc format;
    value $stat
                'R' = "O'Brien"
           'ABSMED' = "Brown-Forsythe"
           'ABSDIF' = "Levene"
         'BARTLETT' = "Bartlett"
            'WELCH' = "Welch ANOVA";
  run;

* SORT DATA FOR BY GROUP PROCESSING;
  proc sort data=&data
             out=&tempout.1;
    by &class;
  run;

* RUN UNIVARIATE TO GET STATISTICS BY GROUP;
  proc univariate data=&tempout.1 noprint;
    var &response;
    by &class;
    output out=&tempout.2
          mean=mean
           var=variance
			  std=std
        median=grpmed
             n=n;
  run;

* MERGE STATISTICS FOR ALL DATA WITH ORIGINAL
  DATA AND BY GROUP STATS. CREATE VARIABLES
  TO SUM OVER FOR HOMO VAR TESTS;
  data &tempout.4;
    merge &tempout.1
          &tempout.2 end=lastobs;
    by &class;

    if _n_=1 then grpcnt=0;
    if n <= 1 then delete;
    if first.&class then grpcnt+1;

  * for LEVINE;
    absdif=abs(&response-mean);

  * for BROWN-FORSYTHE;
    absmed=abs(&response-grpmed);

  * for OBRIEN;
    if (n-2) ne 0 then
      r=((n-1.5)*n*((&response-mean)**2)
        -((0.5)*(variance)*(n-1)))/((n-1)*(n-2));
    else r=.;

  * for BARTLETT;
    if last.&class then do;
       b4=n-1;
       b1=b4*log(variance);
       b2=b4*variance;
       b3=1/b4;
    end;
  run;

* STORE THE NUMBER OF LEVELS IN THE BY
  GROUP VARIABLE IN MACRO VARIABLE;
  data _null_;
    set &tempout.4 end=last;
    if last then
       call symput('levels',left(put(grpcnt,3.)));
  run;

* RANK |Y-MED| BY variance FOR OBRIEN. SET TIES
  EQUAL TO HIGHEST SO THAT ZERO |Y-MED| ARE
  REPLACED ONLY WHEN IT HAS A RANK OF 1;
  proc rank data=&tempout.4
             out=&tempout.5
            ties=high;
    var absmed;
    by &class;
    ranks grprank;
  run;

* SORT BY DESCENDING RANK TO PREPARE FOR LAG
  AND REPLACEMENT OF ZEROS;
  proc sort data=&tempout.5;
    by &class descending grprank;
  run;

* LAG SORTED DATA 1 OBSERVATION BY variance.
  THIS PUTS THE |Y-MED| WITH NEXT HIGHEST
  RANK WITH EACH OBSERVATION BUT THE LAST.
  NOW CHECK IF |Y-MED| IS 0 AND RANK IS 1,
  IS SO TAKE THE LAGGED VALUE;
  data &tempout.5;
    set &tempout.5;
    by &class;

    newmed=lag1(absmed);

    if first.&class then newmed=absmed;
    if absmed=0 and grprank=1 then absmed=newmed;
    else absmed=absmed;
  run;


* GET F-TEST FOR OBRIEN, BROWN-FORSYTHE, AND LEVENE;
  proc glm data=&tempout.5
        outstat=ob_bf_le noprint;
    class &class;
    model r absmed absdif=&class/ss1;
  run;

* USE SQL TO CREATE DATA SET WITH THE
  ERROR DEGREES OF FREEDOM ON THE SAME
  RECORD AS THE F RATIO, NUMERATOR DEGREES
  OF FREEDOM, AND P-VALUE.;
  proc sql;
    create table denom as
      select _name_, df
      from ob_bf_le
      where ob_bf_le._source_='ERROR';

    create table obfl as
      select ob_bf_le._name_ as test,
             ob_bf_le.f as fratio,
             ob_bf_le.df as dfnum,
                denom.df as dfden,
             ob_bf_le.prob as prob
      from ob_bf_le, denom
      where ob_bf_le._name_=denom._name_ and
            ob_bf_le._source_ ne 'ERROR';
  run;

* PROC MEANS FOR BARTLETT;
  proc means data=&tempout.4 noprint;
    where b1 ne . ;
    var b1 b2 b3 b4;
    output out=&tempout.6
           sum=sb1 sb2 sb3 sb4;
  run;

* CREATE BARTLETT TEST STATISTIC;
  data bartlett;
    set &tempout.6;
    test='BARTLETT';
    c=1+(1/(3*(&levels-1)))*(sb3-(1/sb4));
    l=sb4*log(sb2/sb4)-sb1;
    u=l/c;
    dfnum=&levels-1;
    fratio=u/dfnum;
    prob=1-probchi(u,dfnum);
    keep test fratio dfnum prob;
  run;



* COMPUTE WELCHS ANOVA FOR THE MEANS;
  data &tempout.7;
    set &tempout.2;

  * Remove groups with fewer
    than 2 observations;
    if n <= 1 then delete;

    w=n/variance;
    ystar1=w*mean;
  run;


  proc univariate data=&tempout.7 noprint;
    var w ystar1;
    output out=&tempout.8
           sum=u ys1sum;
  run;

  data &tempout.9;
    set &tempout.7;
    if _n_=1 then set &tempout.8;
    retain u ys1sum;

    ytilda=ys1sum/u;
    lambda=((1-(w/u))**2)/(n-1);
    numer=(w*((mean-ytilda)**2)/(&levels-1));
  run;

  proc univariate data=&tempout.9 noprint;
    var lambda numer;
    output out=welch sum=lamsum numsum;
  run;

  data welch;
    set welch;
    denom=(1+2*(&levels-2)*lamsum/(&levels**2-1));
    fratio=numsum/denom;
    dfnum=&levels-1;
    dfden=(&levels**2-1)/(3*lamsum);
    prob=1-probf(fratio,dfnum,dfden);
    test='WELCH';
  run;


* PUT ALL TEST STATISTISTICS INTO OUTPUT DATA SET;
  data &out;
    length testname $15;
    set obfl bartlett welch;
    testname=put(test,$stat.);
    label testname='Test Name'
            fratio='F Ratio'
             dfnum='Numerator Degrees of Freedom'
             dfden='Denominator Degrees of Freedom'
              prob='Probability of >F';
    keep testname fratio dfnum dfden prob;
  run;
	options notes;
	
* PRINT OUTPUT DATA SET;
  proc print data=&out noobs label;
    var testname fratio dfnum dfden prob;
    title2 'Homogeneity of Variances Tests';
  run;

	%if %index(&print, GROUPS) > 0 %then %do;
	proc print data=&tempout.2;
		id &class;
		var mean std variance n;
	%end;
   run;
		
* CLEAN UP WORKING DATA SETS;
  options nonotes;
  proc datasets library=work nolist nowarn;
    delete %do i=1 %to 9;
              &tempout.&i
           %end;
           ob_bf_le bartlett welch
           denom obfl;
  run;
  options notes;
title2;
%mend homovar;

