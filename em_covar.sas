 /*--------------------------------------------------------------*
  *    Name:  em_covar.sas                                       *
  *   Title:  Estimate a covariance matrix via EM algorithm      *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Steve Gregorich                                    *
  * Created: 3/21/96                                             *
  * Revised: 10 Nov 1997 16:19:03                                *
  *Modified: 15 Jun 1999 10:25:41 (MF)                           *
  * Version: 1.0                                                 *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 EM_COVAR.SAS: A SAS/IML macro for estimating a covariance matrix 
 and mean vector via the expectation maximization (EM) algorithm.

 This program uses the EM algorithm to estimate the maximum 
 likelihood (ML) covariance matrix and mean vector in the 
 presence of missing data.  This implementation of the EM 
 algorithm or any similar ML approach assumes that the data 
 are missing completely at random (MCAR) or missing at 
 random (MAR: see Little & Rubin, 1987).  If data are not 
 MCAR and not MAR, estimated parameters will be biased.  

 However, under these conditions, EM (ML) parameter estimates 
 are typically less biased than those estimated with ad-hoc 
 procedures (e.g., pairwise or listwise deletion of missing 
 data, single regression imputation, mean substitution--e.g.,  
 Arbuckle, 1996/ Graham, Hofer, & MacKinnon, 1996/  Muthen, 
 Kaplan & Hollis, 1987/  Little & Rubin, 1989).

 This application of the EM algorithm also assumes that the
 data are multivariate normal.  However, it appears to be 
 fairly robust to departures from multivariate normality 
 (Graham, Hofer, & MacKinnon, 1996).  

 WARNING:
 The ML covariance matrix and mean vector estimated by
 em_covar.sas can be used as input data for a variety of
 statistical programs.  However, the standard errors 
 estimated by most programs will be incorrect--they should 
 be discarded.  An alternative approach to generating the 
 standard errors is the bootstrap (e.g., Efron & 
 Tibshirani, 1993 / Mooney & Duval, 1993).  A future
 version of em_covar.sas will produce bootstrapped 
 estimates of item covariance matrices and mean vectors.

 EM_COVAR REQUIRES  SAS/BASE AND SAS/IML.


 An example data set from Little & Rubin (1987) is provided at 
 the end of this file.  These data can be used to check 
 em_covar.sas.


 PLEASE LET ME KNOW IF YOU USE EM_COVAR.SAS IN YOUR RESEARCH.


 Steve Gregorich
 University of California, San Francisco
 Prevention Sciences Group and 
 Center for AIDS Prevention Studies
 74 New Montgomery
 San Francisco, CA 94105
 gregorich@psg.ucsf.edu


 HISTORY:
 -------
 INITIAL PROGRAMMING                                      3/21/96;
 MINOR ENHANCEMENTS                                       3/25/96;
 IMPROVED HANDLING OF MISSINGNESS PATTERNS                3/27/96; 
 IMPROVED HANDLING OF MISSINGNESS PATTERNS                8/20/96;
 ADDED OPTION FOR CORRELATION MATRIX OUTPUT               8/26/96;
 ADDITIONAL DOCUMENTATION                                 9/11/96;
 ENHANCED WARNING MESSAGES                                9/23/96;  


==Usage:
 
  Macro options and defaults;
-----------------------------------------------------------------*
 %em_covar (data     =, 
            var      =, 
            mat_type = cov,           ** The default matrix type
            out_file =,               ** Output text data file
            out      =&mat_type,      ** Output COV/CORR dataset
				outd     =                ** Output imputed dataset
            nomiss   =,               ** PAIRWISE deletion, default
            conv     = 0.0001,        ** Convergence default
            max_iter = 200,           ** Max iteration default
				print    =
            df       = n-1)           ** DF default

 Specification of the following macro variables is required
 ----------------------------------------------------------
 DATA     = Name the data set containing the variables to be 
            analyzed.  

 VAR      = Provide a variable list. The list may contain blank-
            separated names, or any of the short-hand forms,
				X1-X10, VARA--VARB, or _NUMERIC_.

 Specification of the following macro variables is only required
 if you wish to change the defaults
 ---------------------------------------------------------------
 MAT_TYPE = Specify whether a covariance or correlation matrix is
            estimated and output. This is suitable as input to other
				SAS procedures (e.g., PROC FACTOR, CALIS, etc)
				A covariance matrix (the default) is specified as 
				`MAT_TYPE=COV`.  A correlation matrix is specified as
				`MAT_TYPE=CORR`.

 OUT=       Specifies the name of the SAS output dataset for the
            covariance/correlation matrix.

 OUTD=      Specifies the name of the SAS output dataset for the
            imputed data.

 OUT_FILE = Specify fully qualified path for the output text data
            file containing the EM covariance matrix and vector
            of means.
				Use this option to create a seprate output text file
				suitable for input to some other program (e.g., SPSS)
				The string should not be in quotes.  The 
            default is blank, meaning that no output text file
				is created.

 NOMISS   = Specify whether initial covariance matrix should be 
            computed with listwise or pairwise deletion of missing 
            data.  Generally listwise deletion will suffice. 
            however, if every case has at least one missing value, 
            pairwise deletion must be used.  To specify listwise 
            deletion of missing data the value of the macro variable 
            should be `nomiss`.  To specify pairwise deletion of 
            missing data the value of the macro variable should be 
            blank.  The default is pairwise deletion;

 CONV     = Set the convergence criterion. The algorithm converges 
            when the maximum elementwise difference between 
            iterations is less than the value specified.  The 
            default value is 0.0001;

 MAX_ITER = Maximum allowable iterations.  The default is 200;

 DF       = Specify the degrees of freedom for variances and 
            covariances.  The default is N-1;

=REFERENCES:
 ----------
 ARBUCKLE (1996). Full information estimation in the presence
    of incomplete data.  In Marcoulides & Schumacker (Eds.),
    _Advanced structural equation modeling: Issues and 
    techniques_.  LEA.

 EFRON & TIBSHIRANI (1993). _An introduction to the bootstrap_.
    CHAPMAN HALL.

 GRAHAM, HOFER, & MacKinnon (1996). Maximizing the usefulness
    of data obtained with planned missing value patterns: an
    application of maximum likelihood procedures.  
    _Multivariate Behavioral Research_, 31, 197-238.

 Little & Rubin (1987). _Statistical analysis with missing data_. 
    WILEY.

 Little & Rubin (1989). The analysis of social science data with
    missing values. _Sociological Methods and Research_, 18,
    292-326.

 MOONEY & DUVAL (1993). _Bootstrapping: A nonparametric approach
    to statistical inference_. SAGE.

 Muthen, Kaplan, & HOLLIS (1987). On structural equation modeling
    with data that are not completely missing at random.  
    _Psychometrika_, 52, 431-462.


 =*/
 


/*-----------------------------------------------------------------*
  MACRO EM_COVAR--
  WITH DEFAULTS FOR OUT=, NOMISS=, CONV=, MAX_ITER=, & DF=;
*-----------------------------------------------------------------*/
%macro em_covar (
	var=,
	data=,
	mat_type=cov, 
   out=&mat_type,
	out_file=,
	outd=,
	copy=,  
   nomiss= ,
	print=initial final,
	conv=0.0001,  max_iter=200,  df=n-1);

%let print=%upcase(&print);
%let miss=1;
options nonotes;

*-- Parse variables list if it contains special lists;
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
     call symput( 'VAR', trim(_vlist_) );
	  put 'NOTE: VAR= list translated to: VAR=' _vlist_;
 RUN;
%end;
/*-----------------------------------------------------------------*
  Compute listwise or pairwise covariance matrix and mean vector;
  These are the parameter estimates for the first iteration of EM;
*-----------------------------------------------------------------*/
proc corr cov outp=cov noprint &nomiss; 
  var &var;

%if %index(&print,INITIAL) %then %do;
title2 'Initial covariance estimates';
proc print data=cov;
	id _type_;
	by _type_ notsorted;
%end;

data cov_lw;                   ** CREATE COVARIANCE MATRIX DATA SET;
  set cov;
  if _type_='COV';
  keep &var;

data mean_lw;                        ** CREATE MEAN VECTOR DATA SET;
  set cov;
  if _type_='MEAN';
  keep &var;
*-----------------------------------------------------------------*;



/*-----------------------------------------------------------------*
  COUNT THE NUMBER OF ANALYSIS VARIABLES;
  PUT THE RESULT IN MACRO VARIABLE `NVAR`;
*-----------------------------------------------------------------*/
data _null_; 
  set &data;

	array x1x2x3 &var;
	call symput('nvar',left(dim(x1x2x3)));
	run;



/*-----------------------------------------------------------------*
  Create a vector of `pattern` indicator variables signifying whether
  the data for each variable is present (`0`) or missing (`1`);
 
  (1) Array the analysis variables
  (2) Create and array indicator variables for each analysis variable;
  (3) Loop through each analysis variable;
  (4) If the analysis variable is missing set the pattern 
       indicator equal to one;
  (5) If the analysis variable is not missing set the pattern 
       indicator equal to zero;
  (6) Sort the data on the pattern variables.
*-----------------------------------------------------------------*/
data _dat_; 
  set &data;

  array data {&nvar} &var;                                   ** (1);
    array pattern {&nvar} p1-p&nvar;                         ** (2);

      do jj = 1 to &nvar;                                    ** (3);
        if data{jj}=. then pattern{jj}=1;                    ** (4);   ** (6);
        else               pattern{jj}=0;                    ** (5);   ** (9);
      end;                                           ** END JJ LOOP;

  keep &var p1-p&nvar;                ** THAT`S ALL THE DATA NEEDED;

proc sort;                                                   ** (6);
  by p1-p&nvar;
*-----------------------------------------------------------------*;



/*-----------------------------------------------------------------*
  CREATE SEPARATE DATA SETS;
*-----------------------------------------------------------------*/
data pattern;   ** DATA SET HOLDING THE PATTERN VARIABLES;
  set _dat_;
  keep p1-p&nvar;

data _dat_;    ** DATA SET HOLDING THE ANALYSIS VARIABLES;
  set _dat_;
  keep &var;




/*-----------------------------------------------------------------*
 IML;
 (1) read in the original data;
 (2) read in the pattern vectors;
 (3) read in the listwise or pairwise covariance matrix;
 (4) read in the listwise or pairwise mean vector;
*-----------------------------------------------------------------*/
title2 'Covariance matrix estimation';
proc iml; 
  use _dat_; 
    read all var {&var} into data;                     ** (1);
  use pattern;
    read all var _num_ into pattern;                   ** (2);
  use cov_lw;
    read all var _num_ into cov_lw;                    ** (3);
  use mean_lw;
    read all var _num_ into mean_lw;                   ** (4);
file log;
start; 
*-----------------------------------------------------------------*;
print "EM_COVAR.SAS   (revision date: 10 Nov 1997)";
*-----------------------------------------------------------------*;


/*-----------------------------------------------------------------*
  PRELIMINARIES;
*-----------------------------------------------------------------*/
nvar     = &nvar+0;        ** CALCULATE NUMBER OF ANALYSIS VARIABLES;
N        = nrow(data);     ** CALCULATE SAMPLE SIZE;
iterate  = 0;              ** INITIATE ITERATION COUNTER;
_name_   = {&var};         ** USED FOR ROW AND COLUMN LABELS;


/*-----------------------------------------------------------------*
  MISSINGNESS PATTERN AND FREQUENCY;
  (1) Check to make sure the initial covariance matrix has
      valid values for all elements.
  (2) Check to see if pairwise deletion is required for initial
      estimates;
  (2.5) check to make sure that each variable has some valid
        data;
  (3) Holds maximum number of missing values within a single case;
  (4) If any case has all missing values, stop program;
  (5) If there is no missing data, stop program;
  (6) Compute the % of missing variables in max_wc;
  (7) Compute percent of total missing values in data matrix;
  (8) `PATTERN2` HOLDS THE SEQUENTIAL MISSINGNESS PATTERN 
      NUMBER.  THIS IS CALLED `SEQ_CLASS`. A VALUE OF `1` IS 
      RESERVED FOR THE FIRST SEQUENTIAL `MISSINGNESS` CLASSIFICATION, 
      WHETHER OR NOT IT HAS ANY MISSING VALUES;
  (9) Loop from 2 to n--you know that the first row has seq_class
      equal to `1`;
  (10) If the sums of elements of adjacent pattern vectors are not
       equal the patterns must be different.  The value of seq_class
       is incremented by one.;
  (11) Given that the sums of elements of adjacent pattern vectors 
       are the same (because of `else if` after `if` in (10), if 
       the sum of the cross-products of the adjacent pattern 
       vectors does not equal the sum of the elements of the 
       pattern vectors, then the patterns must be different.  The 
       value of seq_class is incremented by one.;
  (12) Else the patterns must be the same;
  (13) If some cases have no missing data this will find the
       first case with some missing data;
  (14) If all cases have some missing data, set `start` to equal 1;
  (15) Count the number of missingness patterns;
*-----------------------------------------------------------------*/
if cov_lw[+,+]=0 then do;                                    ** (1);
  print 
  "WARNING: All elements of the initial covariance matrix are missing";
  print "Try pairwise deletion for initial estimates";
  goto jump;
end;

if min(pattern[,+]) > 0 & upcase("&nomiss")='NOMISS' then do;  ** (2);
  print "WARNING: All cases have some missing data."; 
  print "Use pairwise deletion for initial estimates.";
  goto jump;
end;

if max(pattern[+,]) = N                            then do;  ** (2.5);
  print "WARNING: At least one variable has no valid data."; 
  goto jump;
end;



max_wc   = max(pattern[,+]);                                 ** (3);

if max_wc = nvar then do;                                    ** (4);
  print "WARNING: At least one case has no valid data";      ** (4);
  goto jump;                                                 ** (4);
end;                                                   ** END IF-DO;

if max_wc = 0 then do;                                       ** (5);
  print "WARNING: No missing data detected";
  call execute('%let miss=0;');   
  goto jump;
end;                                                   ** END IF-DO;

perc_wc  = ( max_wc / nvar ) # 100;                          ** (6);
perc_tot = ( pattern[+,+] / (N * nvar) ) # 100;              ** (7);

pattern2 = j(n,1,1);                                         ** (8);

  do jj = 2 to N;                                            ** (9);

          if pattern[jj,+]^=pattern[jj-1,+] then 
             pattern2[jj,1] =pattern2[jj-1,1] + 1;           ** (10);

     else if pattern[jj,]*pattern[jj-1,]` ^=pattern[jj,+] then 
             pattern2[jj,1] =pattern2[jj-1,1] + 1;           ** (11);

     else pattern2[jj,1] =pattern2[jj-1,1] ;                 ** (12);
          if pattern2[jj-1,1] = 1 & pattern2[jj,1] = 2 then 
                                             start = jj;     ** (13);
  end;                                                   ** END JJ LOOP;




if pattern[1,+]>0 then start = 1;                            ** (14);
npattern = pattern2[N,1];                                    ** (15);

*put 'Number of Missingness Patterns:' npattern;



/*-----------------------------------------------------------------*
  (1) CREATE COV_OLD;
      An augmented covariance matrix with the mean vectors in the
      (nvar+1) row and column.  Note that the lower right cell 
      contains a `-1`.  This is as it would be if the uncorrected 
      SSCP matrix was swept on the constant--except that here the 
      degrees of freedom for the variances and covariances are equal
      to N-1 (by default) instead of N;
*-----------------------------------------------------------------*/
cov_old  = (cov_lw // mean_lw) || (mean_lw` // -1);         ** (1);



/*-----------------------------------------------------------------*
  MAIN LOOP;
  (1) Convergence is obtained when the maximum change for all 
      parameters of the estimated covariance matrix and mean 
      vector is less than the convergence criterion.  
  (2) Iteration counter;
  (3) SSCP_P holds the `penalty terms` for each imputation.  This 
      matrix is zeroed-out at the beginning of each iteration.
      Residual variance and covariance terms are added to this 
      matrix whenever missing values are estimated;
*-----------------------------------------------------------------*/
do until (converge < &conv+0);                               ** (1);
  time1=time();
  iterate = iterate + 1;                                     ** (2);

  if (iterate > &max_iter+0) then do;
    print "WARNING: Maximum iterations exceeded";
    goto output;
  end;

** This matrix holds the penalty terms to be added to the SSCP;
sscp_p = j(nvar,nvar,0);                                     ** (3);



/*-----------------------------------------------------------------*
 Compute regression parameters for all variables & impute
 missing values.
 (1) Loop through the data starting with the first row containing 
     missing data.
 (2) Initiate `SWEEP_IT`.  If SWEEP_IT equals `1` the covariance
     matrix should be swept to obtain regression parameter estimates.
 (3) `SWEEP_IT` should be set to 1 whenever the loop begins at the
     row number indicated by the variable `start`.  This can happen
     in two different cases (a) when all cases have at least one
     missing value the value of the variable `start` will equal `1`
     and the first value of `jj` will equal `1` by definition. (b)
     when some cases have no missing data, the first value of `jj`
     (and `start`) will represent a row after the first row.  Because
     the first possibility precludes the option of checking the value
     of the row _before_ the first row, the two lines of logic were
     required.
 (4) IF `SWEEP_IT` = 1 perform the following functions.
 (5) Initialize vectors to hold the column numbers for IVs AND DVs.
 (6) Build vectors of IV and DV column numbers.
 (7) get rid of leading zero in IV AND DV vectors.
 (8) Sweep the augmented covariance matrix on the pivot points.
 (9) Add the appropriate residual variances and covariances to the 
     SSCP_P matrix.  This is done by pre-multiplying the row`s
     pattern vector by its transpose and then performing element-
     wise multiplication of the swept matrix (actually, element-
     wise multiplication is performed on the swept matrix minus 
     the nvar+1 row and column which hold the intercept terms).
     This picks out the appropriate residual variances and covariances.
 (10) This loop will estimate the missing values from the regression
     parameters.  The loop uses the columns of the dv vector to
     `find` the DV variables to estimate values for.
 (11) The data values corresponding to the columns indicated in
     the IV vector are augmented with a `1` for the constant term.
     This vector is then post-multiplied by a column vector holding
     the regression parameters (the intercept term is concatenated
     to the vector of variable parameters).
*-----------------------------------------------------------------*/

do jj = start to N;                                       ** (1);
  sweep_it=0;                                             ** (2);
       if (jj = start)                            then sweep_it=1; *(3);
  else if (pattern2[jj,1] = pattern2[jj-1,1] + 1) then sweep_it=1; *(3);

  if sweep_it=1 then do;                                  ** (4);

    iv = 0;                                               ** (5);
    dv = 0;                                               ** (5);

      do kk = 1 to nvar;                                  ** (6);
              if pattern[jj,kk]=0 then iv=iv||kk;         ** (6);
         else if pattern[jj,kk]=1 then dv=dv||kk;         ** (6);
      end;                                        ** END KK LOOP;

    iv = iv[,2:ncol(iv)];                                 ** (7);
    dv = dv[,2:ncol(dv)];                                 ** (7);

    smatrix = sweep(cov_old,iv);                          ** (8);
  end;                                              ** END IF-DO;

  sscp_p = sscp_p + 
           pattern[jj,]` * pattern[jj,] #
           smatrix[1:nvar,1:nvar];                        ** (9);

  do kk = 1 to ncol(dv);                                  ** (10);
    data[jj,dv[1,kk]] = (data[jj,iv] || 1) *      
              (smatrix[iv,dv[1,kk]] // 
               smatrix[nvar+1,dv[1,kk]]);                 ** (11);

  end;                                            ** END KK LOOP;
end;                                              ** END JJ LOOP;


/*-----------------------------------------------------------------*
  Compute new covariance matrix and compare to old.
  (1) variable means.
  (2) corrected sscp matrix.
  (3) Add cumulative residual variance and covariances to sscp.
  (4) Aompute new covariance matrix.
  (5) Augment covariance matrix with mean vectors.
  (6) Compute difference between cov_old and cov.
  (7) Compute convergence as the maximum discrepancy between 
      cov_old and cov.
  (8) Update cov_old.
*-----------------------------------------------------------------*/
sum      = data[+,];
mean     = sum/n;                                             **(1);
sscp     = data` * data - sum` * sum / n;                     **(2);
sscp     = sscp + sscp_p;                                     **(3);

cov      = sscp / (&df);                                      **(4);

cov      = (cov // mean) || (mean` // -1);                    **(5);

cov_diff = cov - cov_old;                                     **(6);
converge = max(abs(cov_diff));                                **(7);

cov_old=cov;                                                  **(8);

time2 = time();
time = time2-time1;
history = history // (iterate || converge || mean);
*put 'iteration: ' iterate '    converge: ' converge '    time: ' time;

end;                                                ** END DO-UNTIL;
/*-----------------------------------------------------------------*
  END DO-UNTIL;
 *-----------------------------------------------------------------*/


output:
/*-----------------------------------------------------------------*
  OUTPUT AND PRINT;
  (1) Get rid of last column (means) of matrix `COV`;
  PRINT OUT VARIOUS RESULTS;
 *-----------------------------------------------------------------*/
cov = cov[,1:nvar];                                          ** (1);

det = det (cov[1:nvar,]);

reset noname;
%if %index(&print,HISTORY) %then %do;
hlab = {'Iter' 'Converge'} || _name_;
print 'Iteration history and variable means', history[c=hlab f=best7.];
%end;

miss = pattern[,+];
%if %index(&print,IMPUTE) %then %do;
print 'Imputed data matrix, # Missing, and Missing Pattern';
pvec = -1 + t(nvar:1);
do i=1 to nvar;
	pvec[i] = 10**pvec[i];
	end;
pat  = pattern * pvec;
print data[c=_name_]  miss[c={_MISS_} f=3.] pat[c={'Pattern'}] /* pattern[f=1.] */;
%end;

%if %length(&outd) %then %do;
	put 'WARNING:  The imputed data matrix will not have the same covariance' /
	    'matrix as that estimated by the program';
	clab = _name_ || '_miss_';
	data = data || miss;
	create &outd from data [colname=clab];
	append     from data;
%end;

file print;
put  'Total sample size                   '  n 5.0 /
     'Number of missingness patterns      '  npattern 5.0 /
	  'Maximum within-case missing values  '  max_wc 5.0 ' (' perc_wc 5.2 '%)' / 
	  'Percentage Total Missing Values:    '  perc_tot 5.2 "%" //
	  'Determinant of EM Covariance Matrix:'  det;

/*--------------------------------------------------------------*
  IF REQUESTED, COMPUTE CORRELATION MATRIX;
*--------------------------------------------------------------*/
if upcase("&mat_type")='CORR' then do;
	std = sqrt(vecdiag(cov[1:nvar,]));
	s = inv(diag(std));
	corr = s * cov[1:nvar,] * s;
	cov = corr // cov[nvar+1,];
end;

cov = cov//repeat(N,1,nvar);


*-----------------------------------------------------------------*;
* CREATE OUTPUT DATA SET;
*-----------------------------------------------------------------*;
create cov from cov [colname=_name_ rowname=_name_];
append     from cov [rowname=_name_];
*-----------------------------------------------------------------*;


jump:
finish; 
run; quit;
*-----------------------------------------------------------------*;
* END OF IML;
*-----------------------------------------------------------------*;


*-----------------------------------------------------------------*;
* OUTPUT COVARIANCE/CORRELATION MATRIX AND VECTORS OF ITEM MEANS
* AND Ns;
*
* &OUT is a SAS data set that can be used as input in SAS procs.
* By specifying a two-level name, this data set can be saved.
*
* &OUT_FILE removes the vector of Ns prior to being written as
* a text file.  Ns are stripped because sem software does not
* expect them in the data file.
*-----------------------------------------------------------------*;
data &out(type=&mat_type); 
  set cov;

     if      _name_ ^=' '   then _type_="&mat_type ";
else if      _name_  =' '   then _type_='MEAN';
     if lag1(_type_)='MEAN' then _type_='N   ';
     _type_ = upcase(_type_);

%if %index(&print,FINAL) %then %do;
title2 'Final covariance estimates';
proc print data=&out;
	id _type_;
	by _type_ notsorted;
%end;
	
%if %length(&out_file)>0 %then %do;
data _null_;
  set &out;
  if _type_^='N';
  file "&out_file" LRECL=72; 
  put (&var) (12.7);
%end;

%if %length(&outd)>0 and %length(&copy)>0 %then %do;
data &outd;
	merge &outd
	      &data(keep=&copy);
%end;
*-----------------------------------------------------------------*;
%done:
options notes;
title;
%mend em_covar;


*-----------------------------------------------------------------*;
* TESTING EM_COVAR.SAS;
*-----------------------------------------------------------------*;
  /*
*-----------------------------------------------------------------*;
* TABLE 6.4 FROM Little & Rubin (1987), PAGE 119.;
* TO DUPLICATE THEIR RESULTS, SPECIFY DF=N;
*-----------------------------------------------------------------*;
data lr;
input x1 x2 x3 x4 x5;
cards;
  7  26  6  60  78.5
  1  29 15  52  74.3
 11  56  8  20 104.3
 11  31  8  47  87.6
  7  52  6  33  95.9
 11  55  9  22 109.2
  3  71 17   . 102.7
  1  31 22   .  72.5
  2  54 18   .  93.1
  .   .  4   . 115.9
  .   . 23   .  83.8
  .   .  9   . 113.3
  .   .  8   . 109.4
;
*-----------------------------------------------------------------*;


*-----------------------------------------------------------------*;
%em_covar (data     = lr,
           nomiss   = nomiss,
           var      = x1 x2 x3 x4 x5,
           df       = n) 
*-----------------------------------------------------------------*;
  /*
*-----------------------------------------------------------------*;
* HERE`S A THE COVARIANCE MATRIX AND MEAN VECTOR THAT SHOULD
* BE PRODUCED FROM THE ABOVE L&R DATA.
*-----------------------------------------------------------------*;
  21.8255822   20.8643418  -24.9003963  -11.4734555   46.9530569
  20.8643418  238.0123567  -15.8173608 -252.0722620  195.6035775
 -24.9003963  -15.8173608   37.8698225   -9.5992167  -47.5562130
 -11.4734555 -252.0722620   -9.5992167  294.1830190 -190.5984738
  46.9530569  195.6035775  -47.5562130 -190.5984738  208.9048521
   6.6551671   49.9652567   11.7692308   27.0470899   95.4230769
*-----------------------------------------------------------------*;
  /*  */
*-----------------------------------------------------------------*;
