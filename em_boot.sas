/*=
=Description:

 EM_BOOT.SAS: A SAS/IML macro for estimating a covariance matrix 
 and mean vector via the expectation maximization (em) algorithm.

 This version can also generate bootstrap estimates of the em
 covariance matrix and mean vector.  Bootstrap estimates are
 stored in a stacked sas data file and a stacked raw output
 data file.  

 This program uses the EM algorithm to estimate the maximum 
 likelihood (ML) covariance matrix and mean vector in the 
 presence of missing data.  This implementation of the em 
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
 em_boot.sas can be used as input data for a variety of
 statistical programs.  However, the standard errors 
 estimated by most programs will be incorrect--they should 
 be discarded.  An alternative approach to generating the 
 standard errors is the bootstrap (e.g., Efron & 
 Tibshirani, 1993 / Mooney & Duval, 1993).  

 This version of em_boot.sas will produce bootstrapped estimates 
 of item covariance matrices and mean vectors.   One approach is to
 use the em covariance matrix and mean vector estimated from the 
 original data set to assess model fit and obtain parameter estimates.
 once a model is selected, a series of, say 50 to 200, bootstrap 
 em covariance matrices and mean vectors can be generated and the
 parameters of the selected model estimated from each.   The
 empirical standard deviations of the model parameters estimated 
 from the bootstrap samples can be used as estimates of the 
 respective parameter standard errors.   

 A caveat...
 The bootstrap looping algorithm used in this macro is not as 
 efficient as it could be--each bootstrap sample requires a several 
 data steps, one procedure call, and the initiation and closure of 
 proc iml.   The overhead is considerable compared to, say, a macro 
 that would call proc iml only once.   However, calling proc iml only 
 once would require sorting data (to speed EM) and appending to 
 existing raw text files from within proc iml.   This is possible,
 but awkward (to me anyway), so i chose the more expedient coding
 strategy.

 em_boot.SAS REQUIRES THE SAS/BASE AND SAS/IML MODULES.


 Example data sets are provided at the end of this file.   
 These data can be used to check em_boot.sas.


 Please let me know if you use em_boot.sas in your research.


 Steve Gregorich
 University of California, San Francisco
 Prevention Sciences Group and 
 Center for AIDS Prevention Studies
 74 New Montgomery
 San Francisco, CA 94105
 gregorich@psg.ucsf.edu


 HISTORY:
 -------
 INITIAL PROGRAMMING                                      3/21/96
 MINOR ENHANCEMENTS                                       3/25/96
 IMPROVED HANDLING OF MISSINGNESS PATTERNS                3/27/96 
 IMPROVED HANDLING OF MISSINGNESS PATTERNS                8/20/96
 ADDED OPTION FOR CORRELATION MATRIX OUTPUT               8/26/96
 ADDITIONAL DOCUMENTATION                                 9/11/96
 ENHANCED WARNING MESSAGES                                9/23/96  
 IMPROVED SAS DATA SET CREATION (M. FRIENDLY)             4/21/97
 ADDED BOOTSTRAP FACILITY                                12/19/97
 MINOR ENHANCEMENTS TO PRINTED OUTPUT & NEW EXAMPLE DATA 12/31/97
 MINOR CHANGE TO BOOTSTRAP LOOP.  NOW THE DEFAULT NUMBER 
  OF BOOTSTRAP SAMPLES IS ZERO (I.E., THE ORIGINAL DATA
  IS ANALYZED, WITH NO BOOTSTRAP SAMPLES).               12/09/98
 Modified extensively for greater convenience (MF)


==Usage:
 
  Macro options and defaults
  ---------------------------------------------------------------*
 %em_boot  (data     =_last_, 
            var      = , 
            mat_type = cov,           ** The default matrix type
            out_file =,               ** Output text data file
            out      =&mat_type,      ** Output COV/CORR dataset
				outd     =                ** Output imputed dataset
            format   = 12.7,          ** DEFAULT FORMAT FOR OUTPUT DATA
            macrolog = yes,           ** MACRO OUTPUT TO SAS LOG?   
            nomiss   =,               ** PAIRWISE DELETION, DEFAULT
            conv     = 0.0001,        ** CONVERGENCE DEFAULT
            max_iter = 200,           ** MAX ITERATION DEFAULT
            df       = n-1,           ** DENOMINATOR DEFAULT
            bs       = 0)             ** DEFAULT # OF BOOTSTRAP SAMPLES 

 SPECIFICATION OF THE FOLLOWING MACRO VARIABLES IS REQUIRED
 ----------------------------------------------------------
 VAR      = Provide a variable list. The list may contain blank-
            separated names, or any of the short-hand forms,
				X1-X10, VARA--VARB, or _NUMERIC_.

 Specification of the following macro variables is only required
 if you wish to change the defaults
 ---------------------------------------------------------------
 DATA     = Name the data set containing the variables to be 
            analyzed. 

 MAT_TYPE = Specify whether a covariance or correlation matrix is
            estimated and output. This is suitable as input to other
				SAS procedures (e.g., PROC FACTOR, CALIS, etc)
				A covariance matrix (the default) is specified as 
				`MAT_TYPE=COV`.  A correlation matrix is specified as
				`MAT_TYPE=CORR`.

 OUT=       Specifies the name of the SAS output dataset for the
            covariance/correlation matrix.  Use a two-part name
				as usual for a permanent data set.

 OUT_FILE = Specify fully qualified path for the output text data
            file containing the EM covariance matrix and vector
            of means.
				Use this option to create a seprate output text file
				suitable for input to some other program (e.g., SPSS)
				The string should not be in quotes.  The 
            default is blank, meaning that no output text file
				is created.

 FORMAT   = Specify the format for the text output data file
            specified by OUT_FILE=.   The default is 12.7.

 MACROLOG = SPECIFY WHETHER THE MACRO SENDS FULL OR ABBREVIATED 
            NOTES/MESSAGES TO THE SAS LOG FILE.  THE DEFAULT, 
            `YES` SENDS FULL MACRO NOTES/MESSAGES TO THE SAS LOG.  
            THE IS APPROPRIATE WHEN CHECKING FOR PROPER DATA INPUT 
            AND MACRO EXECUTION, WHEN NOT REQUESTING BOOTSTRAPPING,
            OR WHEN A SMALL NUMBER OF BOOTSTRAP SAMPLES ARE REQUESTED.  
            WHEN A LARGE NUMBER OF BOOTSTRAP SAMPLES ARE REQUESTED, 
            THE SAS LOG MAY BECOME FULL AND WILL NEED TO BE CLEARED 
            BEFORE EXECUTION CAN CONTINUE. IN THIS CASE IT IS ADVISED 
            THAT ABBREVIATED MACRO NOTES/MESSAGES ARE REQUESTED BY 
            SETTING `MACROLOG` EQUAL TO `NO`.

 NOMISS   = Specify whether initial covariance matrix should be 
            computed with listwise or pairwise deletion of missing 
            data.   Generally listwise deletion will suffice.  
            However, if every case has at least one missing value, 
            pairwise deletion must be used.   To specify listwise 
            deletion of missing data the value of the macro variable 
            should be `nomiss`.   To specify pairwise deletion of 
            missing data the value of the macro variable should be 
            blank.   The default is pairwise deletion.

 CONV     = Set the convergence criterion. The algorithm converges 
            when the maximum elementwise difference between 
            iterations is less than the value specified.  The 
            default value is 0.0001;

 MAX_ITER = Maximum allowable iterations.  The default is 200;

 DF       = Specify the degrees of freedom for variances and 
            covariances.  The default is N-1;

 BS       = Specify the number of bootstrap samples (each of size n) to 
            generate.   By default no bootstrap samples are generated 
            and the em covariance matrix and mean vector are estimated
            from the original data set.   The default value for `bs`
            is zero (0).   To produce stacked bootstrap em covariance 
            matrices and mean vectors, specify a positive integer 
            representing the number of bootstrap samples to generate.   
            When bootstrap samples are requested the sas dataset produced 
            (i.e., work.cov) contains the variable _sample_.   This 
            variable indexes the sequential number of the bootstrap sample.   
            This allows for by-variable processing when analyzing bootstrap 
            covariance matrices with sas procedures.           


=REFERENCES:
 ----------
 Arbuckle (1996).  Full information estimation in the presence
    of incomplete data.   In Marcoulides & Schumacker (eds.),
    _advanced structural equation modeling: issues and 
    techniques_.   LEA.

 Efron & Tibshirani (1993).  _An introduction to the bootstrap_.
    chapman hall.

 Graham, Hofer, & Mackinnon (1996).  Maximizing the usefulness
    of data obtained with planned missing value patterns: an
    application of maximum likelihood procedures.   
    _Multivariate Behavioral Research_, 31, 197-238.

 Little & Rubin (1987).  _Statistical analysis with missing data_.  
    Wiley.

 Little & Rubin (1989).  The analysis of social science data with
    missing values.  _Sociological methods and research_, 18,
    292-326.

 Mooney & Duval (1993).  _Bootstrapping: a nonparametric approach
    to statistical inference_.  Sage.

 Muthen, Kaplan, & Hollis (1987).  On structural equation modeling
    with data that are not completely missing at random.   
    _Psychometrika_, 52, 431-462.

 =*/
 
/*-----------------------------------------------------------------*
 MACRO em_boot--
 WITH DEFAULTS FOR MAT_TYPE=, OUT_FILE=, FORMAT=, MACROLOG=, 
                    NOPRINT=,  NOMISS=,   CONV=,   MAX_ITER=, 
                    DF=,       BS=

 Do not specify alternative values of macro variables here.
 Specify them when you call the macro after it is compiled.
 (See examples at the end of this file).
*-----------------------------------------------------------------*/
%macro em_boot  (var      =,  
                 data     = _last_,  
                 mat_type = cov, 
                 out_file = ,
					  out      = &mat_type,
					  outd     = ,
                 format   = 12.7, 
            /*   macrolog = yes,   */
					  print    = initial final, 
            /*   noprint  = noprint,*/
                 nomiss   = , 
                 conv     = 0.0001,  
                 max_iter = 200,  
                 df       = n-1, 
                 bs       = 0,
					  seed     = 0);
*-----------------------------------------------------------------*;

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


*-----------------------------------------------------------------*;
* LOOP ONCE FOR EACH BOOTSTRAP SAMPLE;
*-----------------------------------------------------------------*;
      %if &bs =0 %then %let bs_start=0;
%else %if &bs^=0 %then %let bs_start=1;

%do bootloop = &bs_start %to &bs;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* OPEN INPUT DATA SET;
*-----------------------------------------------------------------*;
data temp;
  set &data;
keep &var;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* CREATE BOOTSTRAP SAMPLE IF REQUESTED;
*-----------------------------------------------------------------*;
%if &bs^=0 %then %do;
  data temp;
    drop i;
    do i=1 to n;
      rnd =int(ranuni(&seed)*n)+1;
      set temp point=rnd nobs=n;
      output;
    end;
  stop;
%end;
*-----------------------------------------------------------------*;



/*-----------------------------------------------------------------*
  Compute listwise or pairwise covariance matrix and mean vector;
  These are the parameter estimates for the first iteration of EM;
  This will work on the most recently created data set--either
  the primary data set of the bootstrap data set.
*-----------------------------------------------------------------*/

proc corr cov outp=cov1 noprint &nomiss; 
  var &var;

%if %index(&print,INITIAL) %then %do;
title2 'Initial covariance estimates';
proc print data=cov1;
	id _type_;
	by _type_ notsorted;
%end;

data cov_lw;                   ** CREATE COVARIANCE MATRIX DATA SET;
  set cov1;
  if _type_='COV';
  keep &var;

data mean_lw;                        ** CREATE MEAN VECTOR DATA SET;
  set cov1;
  if _type_='MEAN';
  keep &var;


*-----------------------------------------------------------------*;
* COUNT THE NUMBER OF ANALYSIS VARIABLES;
* PUT THE RESULT IN MACRO VARIABLE `NVAR`;
*-----------------------------------------------------------------*;
data _null_; 
  set temp;

	array x1x2x3 &var;
	call symput('nvar',left(dim(x1x2x3)));


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
data temp; 
  set temp;

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



*-----------------------------------------------------------------*;
* CREATE SEPARATE DATA SETS;
*-----------------------------------------------------------------*;
data pattern;   ** DATA SET HOLDING THE PATTERN VARIABLES;
  set temp;
  keep p1-p&nvar;

data temp;    ** DATA SET HOLDING THE ANALYSIS VARIABLES;
  set temp;
  keep &var;
*-----------------------------------------------------------------*;



/*-----------------------------------------------------------------*
 IML;
 (1) read in the original data;
 (2) read in the pattern vectors;
 (3) read in the listwise or pairwise covariance matrix;
 (4) read in the listwise or pairwise mean vector;
*-----------------------------------------------------------------*/
proc iml; 
  use temp; 
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
print "em_boot.sas   (revision date: December 09, 1998)";
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* PRELIMINARIES;
*-----------------------------------------------------------------*;
nvar     = &nvar+0;        ** CALCULATE NUMBER OF ANALYSIS VARIABLES;
N        = nrow(data);     ** CALCULATE SAMPLE SIZE;
iterate  = 0;              ** INITIATE ITERATION COUNTER;
tot_time = 0;              ** HOLDS TOTAL TIME REQUIRED FOR ITERATIONS;
_name_   = {&var};         ** USED FOR ROW AND COLUMN LABELS;
*-----------------------------------------------------------------*;



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
  (8) `Pattern2` holds the sequential missingness pattern 
      number.   This is called `seq_class`.  A value of `1` is 
      reserved for the first sequential `missingness` classification, 
      whether or not it has any missing values;
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
  goto jump;                                                 ** (5);
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

*put ' ';
*put 'Number of Missingness Patterns:' npattern;

%if &bs^=0 %then %do;
  put 'Bootstrap Sample #:' &bootloop '  out of:' &bs;
%end;
%else %do;
  put 'Analysis of Original Data Set';
%end;
*-----------------------------------------------------------------*;



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
    goto jump;
  end;

** THIS MATRIX HOLDS THE PENALTY TERMS TO BE ADDED TO THE SSCP;
sscp_p = j(nvar,nvar,0);                                     ** (3);
*-----------------------------------------------------------------*;



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
*-----------------------------------------------------------------*;



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
tot_time = tot_time+time;

*put ' ';
*put 'iteration: ' iterate '    converge: ' converge '    time: ' time;
history = history // (iterate || converge || mean);

end;                                                ** END DO-UNTIL;
*-----------------------------------------------------------------*;
* END DO-UNTIL;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* OUTPUT AND PRINT;
* (1) GET RID OF LAST COLUMN (MEANS) OF MATRIX `COV`;
*
* PRINT OUT VARIOUS RESULTS;
*-----------------------------------------------------------------*;
cov = cov[,1:nvar];                                          ** (1);

det = det (cov[1:nvar,]);
eig = min(eigval(cov[1:nvar,]));

ave_time = tot_time/iterate;  ** AVERAGE TIME PER ITERATION;

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
	create outd from data [colname=clab];
	append     from data;
%end;

%if &bs^=0 %then %do;
  print "Data: %upcase(&data)        Bootstrap Sample #: &bootloop of &bs";
  %end;
%else %do;
  print "Data: %upcase(&data)        Analysis of Original Data Set";
  %end;

file print;
put  'Total sample size                    '  n 9.0 /
     'Number of variables                  '  nvar 9.0 /
     'Number of missingness patterns       '  npattern 9.0 /
	  'Maximum within-case missing values   '  max_wc 9.0 ' (' perc_wc 5.2 '%)' / 
	  'Percentage Total Missing Values:     '  perc_tot 9.2 "%" //
	  'Determinant of EM Covariance Matrix: '  det best9./
	  'Maximum eigenvalue:                  '  eig best9. //
	  'Average time per iteration (sec.):   '  ave_time 9.3 /
	  'Number of iterations:                '  iterate 9.0 /
	  'Convergence criterion:               '  converge 9.4 ;

cov = cov//repeat(N,1,nvar);
*--------------------------------------------------------------*;



*--------------------------------------------------------------*;
* IF REQUESTED, COMPUTE CORRELATION MATRIX;
*--------------------------------------------------------------*;

if upcase("&mat_type")='CORR' then do;
  do r = 2 to nvar;
    do c = 1 to (r-1);
      cov[r,c] = cov[r,c] / (cov[r,r] * cov[c,c])**.5;
      cov[c,r] = cov[r,c];
    end;
  end;
  do d = 1 to nvar;
    cov[d,d]=1;
  end;
end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* CREATE OUTPUT DATA SET;
*-----------------------------------------------------------------*;
create em from cov [colname=_name_ rowname=_name_];
append    from cov [rowname=_name_];
*-----------------------------------------------------------------*;


jump:
finish; run; quit;
*-----------------------------------------------------------------*;
* END OF IML;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* OUTPUT COVARIANCE/CORRELATION MATRIX AND VECTORS OF ITEM MEANS
* AND Ns;
*
* WORK.COV_OUT REMOVES THE VECTOR OF Ns PRIOR TO BEING WRITTEN AS
* A TEXT FILE.  Ns ARE STRIPPED BECAUSE SEM SOFTWARE DOES NOT
* EXPECT THEM IN THE DATA FILE.
*-----------------------------------------------------------------*;

** IF THE ORIGINAL SAMPLE OR THE FIRST BOOTSTRAP SAMPLE IS ANALYZED;

%if &bs_start=0 | (&bs_start=1 & &bootloop=1) %then %do;

  ** CREATE THE SAS COVARIANCE DATA SET;
  data &out(type=&mat_type); 
    set em;
       if      _name_ ^=' '   then _type_="&mat_type ";
  else if      _name_  =' '   then _type_='MEAN';
       if lag1(_type_)='MEAN' then _type_='N   ';
  _type_ = upcase(_type_);
  _sample_ = &bootloop;

	%if %index(&print,FINAL) %then %do;
	title2 'Final covariance estimates';
	proc print data=&out;
		id _type_;
		by _type_ notsorted;
	%end;
	

	** WRITE THE DATE TO AN ASCII FILE;
	%if %length(&out_file)>0 %then %do;
	data _null_;
		set &out;
		if _type_^='N';
		file "&out_file" LRECL=72; 
		put (&var) (&format);
	%end;

	%if %length(&outd) %then %do;
	data &outd;
		set outd;
	  _sample_ = &bootloop;
	%end;	
%end;


** IF THE SECOND OR HIGHER BOOTSTRAP SAMPLE IS ANALYZED;
%if (&bs_start=1 & &bootloop^=1) %then %do;

  ** CREATE THE SAS COVARIANCE DATA SET;
  data em(type=&mat_type); 
    set em;
       if      _name_ ^=' '   then _type_="&mat_type ";
  else if      _name_  =' '   then _type_='MEAN';
       if lag1(_type_)='MEAN' then _type_='N   ';
  _type_ = upcase(_type_);
  _sample_ = &bootloop;

  ** CONCATENATE ONTO THE PREVIOUSLY ESTIMATED SAS COVARIANCE DATA SETS;
  data &out (type=&mat_type);
    set &out em;

  ** CONCATENATE ONTO THE PREVIOUSLY WRITTEN ASCII FILES;
%if %length(&out_file)>0 %then %do;
  data _null_;
    set em;
    drop _sample_;
    if _type_^='N';
    file "&out_file" LRECL=72 mod; 
    put (&var) (&format);
	 %end;

	%if %length(&outd) %then %do;
	data &outd;
		set &outd outd(in=ind);
		if ind then _sample_ = &bootloop;
	%end;	
	 
%end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* END OF BOOTLOOP;
*-----------------------------------------------------------------*;
%end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
*options mprint symbolgen notes mlogic number date;
run;
%mend em_boot;
*-----------------------------------------------------------------*;
* END MACRO em_boot;
*-----------------------------------------------------------------*;





*-----------------------------------------------------------------*;
* EXAMPLES/TESTING em_boot.SAS;
*-----------------------------------------------------------------*;
  *-----------------------------------------------------------------*;
  * EXAMPLE 1:
  * TABLE 6.4 FROM LITTLE & RUBIN (1987), PAGE 118.;
  *-----------------------------------------------------------------*;
    /* (REMOVE THIS LINE TO RUN THIS EXAMPLE)  
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
  * CALL em_boot;
  * TO DUPLICATE THEIR RESULTS, SPECIFY DF=N;
  *-----------------------------------------------------------------*;
  %em_boot (data     = lr,
             var      = x1 x2 x3 x4 x5,
             nomiss   = nomiss,
             df       = n);
  *-----------------------------------------------------------------*;
  * HERE`S THE COVARIANCE MATRIX AND MEAN VECTOR THAT SHOULD
  * BE PRODUCED FROM THE ABOVE L&R DATA. 
  *-----------------------------------------------------------------*;
 /* 21.8255822   20.8643418  -24.9003963  -11.4734555   46.9530569
    20.8643418  238.0123567  -15.8173608 -252.0722620  195.6035775
   -24.9003963  -15.8173608   37.8698225   -9.5992167  -47.5562130
   -11.4734555 -252.0722620   -9.5992167  294.1830190 -190.5984738
    46.9530569  195.6035775  -47.5562130 -190.5984738  208.9048521

     6.6551671   49.9652567   11.7692308   27.0470899   95.4230769  */
  *-----------------------------------------------------------------*;


  *-----------------------------------------------------------------*;
  * EXAMPLE 2:
  * EXAM DATA DISTRIBUTED WITH JOE SCHAFER`S NORM PROGRAM FOR WINDOWS;
  *-----------------------------------------------------------------*;
    /* (REMOVE THIS LINE TO RUN THIS EXAMPLE) 
  data exam;
    input age bmi hyp chol;
  cards;
   1    .     .    .
   2 22.7     0  187
   1    .     0  187
   3    .     .    .
   1 20.4     0  113
   3    .     .  184
   1 22.5     0  118
   1 30.1     0  187
   2 22.0     0  238
   2    .     .    .
   1    .     .    .
   2    .     .    .
   3 21.7     0  206
   2 28.7     1  204
   1 29.6     0    .
   1    .     .    .
   3 27.2     1  284
   2 26.3     1  199
   1 35.3     0  218
   3 25.5     1    .
   1    .     .    .
   1 33.2     0  229
   1 27.5     0  131
   3 24.9     0    .
   2 27.4     0  186
  ;
  *-----------------------------------------------------------------*;
  * CALL em_boot;
  * TO DUPLICATE THE RESULTS OF THE `NORM` PROGRAM, SPECIFY DF=N;
  *-----------------------------------------------------------------*;
  %em_boot (data     = exam,
             var      = age bmi hyp chol,
             format   = 13.7,
             df       = n)
  *-----------------------------------------------------------------*;
  * HERE`S THE COVARIANCE MATRIX AND MEAN VECTOR THAT SHOULD
  * BE PRODUCED FROM THE ABOVE `EXAM` DATA.  
  *-----------------------------------------------------------------*;
 /*   0.6624000   -1.5923357    0.1702745   20.7951813
     -1.5923357   18.3795340   -0.0109208   51.2645321
      0.1702745   -0.0109208    0.1779341    7.4581507
     20.7951813   51.2645321    7.4581507 1961.9320191

      1.7600000   26.5225598    0.2293973  194.4468054              */
  *-----------------------------------------------------------------*;



  *-----------------------------------------------------------------*;
  * EXAMPLE 3:
  * BOOTSTRAP EM CORRELATION COEFFICIENTS;
  * FISHER (1936) IRIS DATA WITH SOME DATA MISSING COMPLETELY AT RANDOM;
  *-----------------------------------------------------------------*;
    /* (REMOVE THIS LINE TO RUN THIS EXAMPLE)  
  data iris;
    input sepallen sepalwid petallen petalwid species @@;
    cards;
  50 33 14 02 1 64 28 56 22 3 65 28 46 15 2 67 31 56 24 3
  63 28 51 15 3 46 34 14 03 1 69 31 51 23 3 62 22 45 15 2
  59 32 48 18 2 46 36 10 02 1 61 30 46 14 2 60 27 51 16 2
  65 30 52 20 3 56 25 39 11 2 65 30 55 18 3 58 27 51 19 3
  68 32 59 23 3 51 33 17 05 1 57 28 45 13 2 62 34 54 23 3
  77 38 67 22 3 63 33 47 16 2 67 33 57 25 3 76 30 66 21 3
  49 25 45 17 3 55 35 13 02 1 67 30 52 23 3 70 32 47 14 2
  64 32 45 15 2 61 28 40 13 2 48 31 16 02 1 59 30 51 18 3
  55 24 38 11 2 63 25 50 19 3 64 32 53 23 3 52 34 14 02 1
  49 36 14 01 1 54 30 45 15 2 79 38 64 20 3 44 32 13 02 1
  67 33 57 21 3 50 35 16 06 1 58 26 40 12 2 44 30 13 02 1
  77 28 67 20 3 63 27 49 18 3 47 32 16 02 1 55 26 44 12 2
  50 23 33 10 2 72 32 60 18 3 48 30 14 03 1 51 38 16 02 1
  61 30 49 18 3 48 34 19 02 1 50 30 16 02 1 50 32 12 02 1
  61 26 56 14 3 64 28 56 21 3 43 30 11 01 1 58 40 12 02 1
  51 38 19 04 1 67 31 44 14 2 62 28 48 18 3 49 30 14 02 1
  51 35 14 02 1 56 30 45 15 2 58 27 41 10 2 50 34 16 04 1
  46 32 14 02 1 60 29 45 15 2 57 26 35 10 2 57 44 15 04 1
  50 36 14 02 1 77 30 61 23 3 63 34 56 24 3 58 27 51 19 3
  57 29 42 13 2 72 30 58 16 3 54 34 15 04 1 52 41 15 01 1
  71 30 59 21 3 64 31 55 18 3 60 30 48 18 3 63 29 56 18 3
  49 24 33 10 2 56 27 42 13 2 57 30 42 12 2 55 42 14 02 1
  49 31 15 02 1 77 26 69 23 3 60 22 50 15 3 54 39 17 04 1
  66 29 46 13 2 52 27 39 14 2 60 34 45 16 2 50 34 15 02 1
  44 29 14 02 1 50 20 35 10 2 55 24 37 10 2 58 27 39 12 2
  47 32 13 02 1 46 31 15 02 1 69 32 57 23 3 62 29 43 13 2
  74 28 61 19 3 59 30 42 15 2 51 34 15 02 1 50 35 13 03 1
  56 28 49 20 3 60 22 40 10 2 73 29 63 18 3 67 25 58 18 3
  49 31 15 01 1 67 31 47 15 2 63 23 44 13 2 54 37 15 02 1
  56 30 41 13 2 63 25 49 15 2 61 28 47 12 2 64 29 43 13 2
  51 25 30 11 2 57 28 41 13 2 65 30 58 22 3 69 31 54 21 3
  54 39 13 04 1 51 35 14 03 1 72 36 61 25 3 65 32 51 20 3
  61 29 47 14 2 56 29 36 13 2 69 31 49 15 2 64 27 53 19 3
  68 30 55 21 3 55 25 40 13 2 48 34 16 02 1 48 30 14 01 1
  45 23 13 03 1 57 25 50 20 3 57 38 17 03 1 51 38 15 03 1
  55 23 40 13 2 66 30 44 14 2 68 28 48 14 2 54 34 17 02 1
  51 37 15 04 1 52 35 15 02 1 58 28 51 24 3 67 30 50 17 2
  63 33 60 25 3 53 37 15 02 1
  ;
  *---------------------------------------------------------------*;
  * IMPOSE RANDOMLY MISSING DATA;
  *---------------------------------------------------------------*;
  data iris;
    set iris;

  array miss sepallen sepalwid petallen petalwid;
    do over miss;
      if ranuni(1234567) <.10 then miss=.;
    end;
  *---------------------------------------------------------------*;
  * CALL em_boot;
  *---------------------------------------------------------------*;
  %em_boot (data     = iris,
             var      = sepallen sepalwid petallen petalwid,
             macrolog = no,
             mat_type = corr,
             bs       = 3);

  *---------------------------------------------------------------*;
    /*  */
*-----------------------------------------------------------------*;
* END OF EXAMPLES/TEST DATA SETS;
*-----------------------------------------------------------------*;

