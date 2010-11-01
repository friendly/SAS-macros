/**************************************************************************
***************************************************************************

Version 1.05, 2-23-2000
Revised:  6 Jun 2001 08:45:52

Report any errors, problems or questions to the e-mail address below. For the 
latest version of this macro, see the web site below.

Author:  Paul D. Allison, University of Pennsylania
         allison@ssc.upenn.edu
         http://www.ssc.upenn.edu/~allison/

Modified by: M. Friendly
	- Changed default DANUM=3
	- Handle up to 60 variables
	- Added SEED= for random number seed

MACRO MISS does

(1) maximum likelihood estimation of the mean
and covariance matrix of the multivariate normal
distribution for incomplete data using the EM algorithm.

(2) data augmentation to produce multiple data sets with
randomly imputed values for the missing data.

The estimates produced by the EM algorithm in step (1) are used as
starting values for the data augmentation in step (2).
The algorithms are modeled on those described by J.L. Schafer (1997)
Analysis of Incomplete Multivariate Data.  Chapman & Hall.

MACRO MISS requires the installation of SAS/IML (interactive matrix language).

The macro has the following parameters. The only required parameter is VAR.

DATA is the name of the data set to be analyzed.
  Default is the most recently created data set.

VAR is a list of variable names for which the parameters
  will be estimated and any missing data will be imputed. (This should
  be a list of actual names and not an alias like _NUMERIC_ or V1-V5.)
  These variables should include all variables in the model
  that will ultimately be estimated, and any other variables
  that are likely to be good predictors of the variables with
  missing values. Maximum number is 30.

OUT is the name of a data set to which the imputed data
  are written.  Each data set is identical to the input data set
  except that missing values are imputed for all variables named
  in the VAR parameter. If multiple data sets are requested
  (DANUM>1), all data sets are written into a single output file
  which includes a new variable DSNUM that distinguishes the
  different data sets (facilitating further analysis using a
  BY statement). Default is WORK.IMPUTE.

MAXITER is the maximum number of iterations for the EM
  algorithm. Default is 20.

DANUM is the number of data sets produced by data augmentation.
  Default is 0, in which case only EM results are produced.

DAITER is the number of iterations for each data set produced by
  the data augmentation algorithm.  Default is 10. The default number is set
  fairly low because these iterations can be very time consuming, especially
  with large data sets. It is advisable to set it much higher if you can
  spare the computing time. If DAITER=1, the imputed values are
  based on random draws from the residual distribution with
  parameters estimated by EM. In other words, there is no random draw
  from the posterior distribution of the parameters. If DAITER=.5, the
  imputed values are generated from the EM regression estimates without
  random draws of any kind.

PATTERNS=yes requests a list of missing data patterns and
 associated frequencies.  Default is no.

SSCPOUT is the name of a data set to which sums of squares and
  crossproducts from the EM algorithm are written. This data set
  may be used as input to any SAS procedure that will accept
  an SSCP data set (e.g., PROCS REG, FACTOR, CALIS, DISCRIM and
  CANCORR). Resulting parameter estimates will be maximum
  likelihood estimates for just-identified models (e.g. regression
  models). They will not be ML for overidentified models (as in
  most CALIS models). Moreover, the standard error estimates will
  generally be too low because they do not account for the imputation
  of the data. When using this data set for any of these
  linear model procedures, remember to use the TYPE=SSCP option
  after the data set name.

SSCPIN is the name of a data set containing sums of squares and
  crossproducts to be used as starting values for the data
  augmentation.  Typically, this will be a data set produced in
  an earlier run of MISS that used the SSCPOUT parameter. Use of
  the SSCPIN parameter suppresses the EM algorithm. Hence, there
  is no point in using it unless DANUM>0.

Cautions and notes:

1. This version of MISS produces none of the diagnostic
statistics recommended by Schafer.

2. For variables that are highly skewed, it is advisable to transform
them to an approximately normal distribution before imputing.  After
imputation, transform them back to the original metric.

3. For variables that are discrete, it is advisable to round the
imputed values after imputation.

4. A companion macro, COMBINE, takes estimates from multiply imputed
data sets and combines them into a single set of estimates and
associated statistics.

5. If there are any observations that have missing data for ALL
the variables named in the VAR parameter, MISS will still impute
values for the missing data. (The starting values for the imputations
are the means estimated by the EM algorithm.)  For some applications,
it may be desirable to delete such observations before calling
MISS. Similarly, when estimating regression models, it may be
desirable to delete observations with missing values on the dependent
variable before calling MISS.


******************************************************************
******************************************************************/

%macro miss(
	data=_last_,
	var=,         /* list of variables for imputation */
	out=impute,   /* name of output data set          */
	maxiter=20,
	danum=3,      /* number of imputations (M)        */
	daiter=10,    /* number of data augmentation iterations */
	seed=0,       /* random number seed               */
   patterns=no,  /* print missing data patterns?     */
	sscpout=,
	sscpin=);


%do i=1 %to 60;
  %let vari&i=%scan(&var,&i);
  %if &&vari&i= %then %goto out;
%end;
%out: %let i=%eval(&i-1);

data _r_;
  retain _no_ 0;
  retain _tmiss_ 0;
  set &data;
   _no_+1;
  array _y_ (*) &var;
  array _r_ (*) r1-r&i;
  sum=0;
  do j=1 to &i;
    _r_(j)=_y_(j) ne .;
    sum=sum+(1-_r_(j));
  end;
  sumd=sum>0;
  _tmiss_=_tmiss_+sum;
  drop j sumd _tmiss_;
  call symput('nobs',_no_);
  call symput('tmiss',_tmiss_);
run;

proc sort data=_r_;
	by r1-r&i;
	run;

%if %eval(&nobs)>&i and %eval(&nobs)>20+%eval(&tmiss) 
	%then %let nomiss=nomiss; 
	%else %let nomiss=;

proc corr data=_r_ cov outp=theta &nomiss noprint ;
 var &var;
run;


%if &patterns=yes %then %do;
title1 'MISSING DATA PATTERNS';
title3 "R1-R&i Correspond to the &i variables specified in the VAR parameter.";
title4 'A value of 1 indicates data present, 0 indicates data missing.';
proc freq data=_r_;
table r1
%do j=2 %to &i; *r&j %end; 
	/list nocum ;run;
title ;
%end;


/***********************************************************
 EM ALGORITHM
***********************************************************/
proc iml;
	use _r_;
	read all var {&var} into y;
	read all var ('r1':"r&i") into r;
	read all var ('_no_') into obsnum;

	use theta;
	read all var {&var} into cov where (_type_='COV');
	read all var {&var} into mean where (_type_='MEAN');

	if det(cov)=0 then cov=i(&i);
	theta1={-1}||mean;
	theta2=mean`||cov;
	theta=theta1//theta2;
	n=nrow(y);
	p=ncol(y);
	thetaf=theta;
	conv=0;
   seed = &seed;

	%if &sscpin^= %then %do;
		use &sscpin;
		read all into _t_;
		_t_=_t_/n;
		thetaem=sweep(_t_,{1});
		thetaem[,1]=-thetaem[,1];
		%goto noem;
	%end;

	do it=1 to &maxiter while(conv=0);
	t=j(p+1,p+1,0);
	do i=1 to n;
		if i=1 then temp={[&i] 9}; else temp=r[i-1,];
		if any(r[i,] ^= temp) then do;
			theta=thetaf;
			piv=loc({0}|| r[i,]);
			if nrow(piv) ^= 0 then do;
			theta=sweep(theta, piv);
			theta[,piv]=-theta[,piv];
			end;
		end;
			do j=1 to p;
				if y[i,j]=. then y[i,j]=0;
			end;
		ya = {1} || y[i,];
		yi = ya*theta;
		r1= {1} || r[i,];
		yf = r1#ya + ^r1#yi;
		im=diag(^r1);
		t = t+ yf`*yf + im*theta*im;
	end;
	t=t/n;
	theta=sweep(t,{1});
	theta[,1]=-theta[,1];
	conv=all(abs(theta-thetaf)<.0001*abs(thetaf));
	thetaf=theta;
	end;

thetaem=theta;
if conv<1 then
print,
"******ITERATION LIMIT EXCEEDED. ESTIMATES BELOW HAVE NOT CONVERGED.*******" ;
reset noname;
print "Number of iterations:" it;
%if &sscpout^= %then %do;
  t=n*t;
  _name_={'INTERCEP' &var};
  create &sscpout from t[colname={'INTERCEP' &var} rowname=_name_];
  append from t[rowname=_name_];
%end;

	mean=theta[1,2:p+1];
	sd=sqrt(vecdiag(theta[2:p+1,2:p+1]));
	simple=mean`||sd;
	nam={"Mean" "Std. Dev."};
	print, "Maximum Likelihood Estimates",, 
				simple[rowname={&var} colname=nam];
	m=diag(1/sd);
	corr=m*theta[2:p+1,2:p+1]*m;
	print "Correlation Matrix",, 
				corr[rowname={&var} colname={&var} format=5.3];

/*************************************************
 DATA AUGMENTATION:  I-STEP
*************************************************/
%noem: ;
yimp=j(n,p+2);
create work._impute_ from yimp [colname={&var dsnum _no_}];
daiter=&daiter;
if daiter=.5 then do; /* suppresses random draws from residual distribution */
  daiter=1;
  mult=0;
  end;
  else mult=1;

do dsnum=1 to &danum;
thetaf=thetaem;
do j=1 to daiter;
t=j(p+1,p+1,0);
do i=1 to n;
  if i=1 then temp={[&i] 9}; else temp=r[i-1,];
  if any(r[i,] ^= temp) then do;
    piv=loc({0}|| r[i,]);
    if nrow(piv) ^= 0 then do;
      theta=sweep(thetaf, piv);
      theta[,piv]=-theta[,piv];
    end;
   end;
  r1= {1} || r[i,];
  ya = {1} || y[i,];
  io=diag(r1);
  im=diag(^r1);
  lo=loc(r1);
  lm=loc(^r1);
  if any(r1=0) then do;
    yi = ya[1,lo]*theta[lo,lm];
    c=root(theta[lm,lm]);
    nm=nrow(c);
    z=j(1,nm,0);
    do ki=1 to nm;
      z[ki]=mult*rannor(seed);
    end;
    yi=yi+z*c;
    yiaug=j(1,p+1,0);
    yiaug[1,lm]=yi;
    yf = r1#ya + ^r1#yiaug;
    end;
  else yf=ya;
  t = t+ yf`*yf;
  yimp[i,]=yf[2:p+1]`||dsnum|| obsnum[i];
end;

/*************************************************
 DATA AUGMENTATION:  P-STEP
*************************************************/
	t=t/n;
	theta=sweep(t,{1});
	theta[,1]=-theta[,1];

	b=j(p,p,0);
	do u=1 to p;
		b[u,u]=sqrt(2*rangam(seed,(n-u)/2));
		do v=u+1 to p;
			b[u,v]=rannor(seed);
		end;
	end;

	c=root(n*theta[2:p+1,2:p+1]);
	m=inv(b`)*c;
	sigma=m`*m;

	z=j(p,1,0);
	do ir=1 to p;
		z[ir]=rannor(seed);
	end;
	mu=theta[2:p+1,1] + sqrt(1/n)*m`*z;

	theta1={-1}||mu`;
	theta2=mu||sigma;
	thetaf=theta1//theta2;

	end;

if &danum>0 then do;
 append from yimp;
end;
end;

quit;

/***************************************
Write imputed data
***************************************/
%if &danum>0 %then %do;
	proc sort data=work._r_(drop=r1-r&i);
		by _no_;
	%if &danum=1 
		%then %let dsnum=dsnum; 
		%else %let dsnum=;
	proc sort data=work._impute_(drop=&dsnum);
		by _no_;
	data &out(drop=_no_);
		merge work._r_ work._impute_;
		by _no_;
		label dsnum='Imputation';
	run;
	%if &danum>1 %then %do;
		proc sort data=&out;
			by dsnum;
		run;
		%end;
	%end;

%mend miss;
