%macro misscomb(
	DATA=_LAST_,
	PARNAME=,
	REGDEP=,
	DSNAME=DSNUM);


*MACRO COMBINE(DATA=_LAST_,PARNAME=,REGDEP=,DSNAME=DSNUM);

/******************************************************************************
*******************************************************************************
Version 1.03, 10-21-1999

This is a BETA version.  USE AT YOUR OWN RISK!.  Report any errors, problems
or questions to the e-mail address below. The latest version of this macro
can be found at the web site below:

Author:  Paul D. Allison, University of Pennsylvania
         allison@ssc.upenn.edu
         http://www.ssc.upenn.edu/~allison/

MACRO COMBINE combines information from two or more analyses of multiply 
imputed data sets to produce a single set of estimates and associated 
statistics. COMBINE can read SAS data sets in two forms:

1. A SAS data set produced by the OUTEST and COVOUT options of PROCS REG, 
LOGISTIC or PHREG. In this case, the macro presumes that the multiple data sets 
are concatenated into a single SAS data set, as would be the case if the 
regression analyses were carried out with a BY statement. COMBINE expects this 
form of data if the PARNAME parameter is NOT used.  

2. A SAS data set with two variables. The first variable is the parameter 
estimate and the second is its estimated standard error. If there are N 
parameters in each analysis, the first N records correspond to the N parameters 
in the first analysis. The next N records correspond to the N parameters in the 
second analysis, and so on. COMBINE expects this form of data if the PARNAME
parameter IS used.  


The formulas for the various statistics are described in Schafer (1997)
Analysis of Incomplete Multivariate Data.  Chapman & Hall.

MACRO COMBINE requires the installation of IML (interactive matrix language).

There are four parameters:

DATA= is the name of the data set to be analyzed.  Default is the last data
   set created.

PARNAME= is a list of names of the parameters. These names do not have to 
    correspond to any names in the input data set.  This parameter should be    
    used only if the data are in form 2 described above.  

REGDEP= is the name of the dependent variable in a PROC REG analysis when the
   data set was produced by the OUTEST option.  This parameter need not be    
   specified for PROC LOGISTIC or PROC PHREG, or if PARNAME parameter is used.

DSNAME= is the name of the variable distinguishing one data set from another. 
   The default is DSNUM which is the name used by the MISS macro.  This 
   parameter is unnecessary if the PARNAME parameter is used. 


Examples of usage:

1. Form 1 data: 

proc logistic data=impute outest=a covout;
  model y = x1 x2 x3;
  by dsnum;
run;
%combine(data=a)

3. Form 2 data:

data a;
  input x se;
datalines;
.4  .20
.45  .10
.35  .15
.41  .18
;
%combine(data=a,parname=b)

*******************************************************************************
******************************************************************************/
%do i=1 %to 30;
  %let vari&i=%scan(&parname,&i);
  %if &&vari&i= %then %goto out;
%end;
%out: %let i=%eval(&i-1); ;

data _est_;
  set &data;
  drop /* _lnlike_ _link_ */ &dsname  _rmse_ &regdep;
run;


proc iml;
  %if &parname= %then %do;
  use _est_;
   read all into test;
   r=ncol(test);
   n=nrow(test)/(r+1);
   var=j(n,r,0);
  use _est_;
    read all into estim[colname=name] where(_type_='PARMS');
    read all into cov where(_type_='COV');
    do i=1 to n;
      var[i,]=vecdiag(cov[r*i-r+1:r*i,1:r])`;
    end;
  %end;
  %else %do;
    use &data;
    name={&parname};
    r=&i;
    read all into b;
    n=nrow(b)/r;
    estim=shape(b[,1],n,r);
    s=b[,2]##2;
    var=shape(s,n,r);
  %end;
  meanest=estim[+,]/n;
  meanvar=var[+,]/n;
  dev=estim-repeat(meanest,n,1);
  ssx=dev[##,];
  varest=ssx/(n-1);
  stderr=sqrt(meanvar+(1+1/n)*varest);
  t=meanest/stderr;
  ri=varest*(1+1/n)/meanvar;
  v=(n-1)*(1 + ri##-1)##2;
  prob=2*(1-probt(abs(t),v));
  mi=100*(ri+2/(v+3))/(ri+1);
  reset noname fuzz=.000001 fw=7;
  out=meanest`||stderr`||t`||v`||prob`||mi`;
  print out[rowname=name colname={'Estimate' 'Std Err' 't-ratio' 'df' 'p-val.' 
'%Miss Inf.' }];
quit;
%mend;
