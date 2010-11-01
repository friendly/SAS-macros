*-----------------------------------------------------------------*;
* EM_COVAR.SAS: A SAS/IML macro for estimating a covariance matrix 
* and mean vector via the expectation maximization (em) algorithm.
* this version can also generate bootstrap estimates of the em
* covariance matrix and mean vector.  Bootstrap estimates are
* stored in a stacked sas data file and a stacked raw output
* data file.  
*
* This program uses the EM algorithm to estimate the maximum 
* likelihood (ML) covariance matrix and mean vector in the 
* presence of missing data.  This implementation of the em 
* algorithm or any similar ML approach assumes that the data 
* are missing completely at random (MCAR) or missing at 
* random (MAR: see LittlE & Rubin, 1987).  If data are not 
* MCAR and not MAR, estimated parameters will be biased.  
* however, under these conditions, EM (ML) parameter estimates 
* are typically less biased than those estimated with ad-hoc 
* procedures (e.g., pairwise or listwise deletion of missing 
* data, single regression imputation, mean substitution--e.g.,  
* ARBUCKLE, 1996/ GRAHAM, HOFER, & MACKINNON, 1996/  MUTHEN, 
* KAPLAN,& HOLLIS, 1987/  LITTLE & RUBIN, 1989).
*
* This application of the em algorithm also assumes that the
* data are multivariate normal.  However, it appears to be 
* fairly robust to departures from multivariate normality 
* (GRAHAM, HOFER, & MACKINNON, 1996).  
*
* WARNING:
* THE ML COVARIANCE MATRIX AND MEAN VECTOR ESTIMATED BY
* EM_COVAR.SAS CAN BE USED AS INPUT DATA FOR A VARIETY OF
* STATISTICAL PROGRAMS.  HOWEVER, THE PARAMETER STANDARD ERRORS 
* ESTIMATED BY MOST PROGRAMS WILL BE INCORRECT--THEY SHOULD 
* BE DISCARDED.  AN ALTERNATIVE APPROACH TO GENERATING THE 
* STANDARD ERRORS IS THE BOOTSTRAP (E.G., EFRON & 
* TIBSHIRANI, 1993 / MOONEY & DUVAL, 1993).  
*
* THIS VERSION OF EM_COVAR.SAS WILL PRODUCE BOOTSTRAPPED ESTIMATES 
* OF ITEM COVARIANCE MATRICES AND MEAN VECTORS.  ONE APPROACH IS TO
* USE THE EM COVARIANCE MATRIX AND MEAN VECTOR ESTIMATED FROM THE 
* ORIGINAL DATA SET TO ASSESS MODEL FIT AND OBTAIN PARAMETER ESTIMATES.
* ONCE A MODEL IS SELECTED, A SERIES OF, SAY 50 TO 200, BOOTSTRAP 
* EM COVARIANCE MATRICES AND MEAN VECTORS CAN BE GENERATED AND THE
* PARAMETERS OF THE SELECTED MODEL ESTIMATED FROM EACH.  THE
* EMPIRICAL STANDARD DEVIATIONS OF THE MODEL PARAMETERS ESTIMATED 
* FROM THE BOOTSTRAP SAMPLES CAN BE USED AS ESTIMATES OF THE 
* RESPECTIVE PARAMETER STANDARD ERRORS.  
*
* A CAVEAT...
* THE BOOTSTRAP LOOPING ALGORITHM USED IN THIS MACRO IS NOT AS 
* EFFICIENT AS IT COULD BE--EACH BOOTSTRAP SAMPLE REQUIRES A SEVERAL 
* DATA STEPS, ONE PROCEDURE CALL, AND THE INITIATION AND CLOSURE OF 
* PROC IML.  THE OVERHEAD IS CONSIDERABLE COMPARED TO, SAY, A MACRO 
* THAT WOULD CALL PROC IML ONLY ONCE.  HOWEVER, CALLING PROC IML ONLY 
* ONCE WOULD REQUIRE SORTING DATA (TO SPEED EM) AND APPENDING TO 
* EXISTING RAW TEXT FILES FROM WITHIN PROC IML.  THIS IS POSSIBLE,
* BUT AWKWARD (TO ME ANYWAY), SO I CHOSE THE MORE EXPEDIENT CODING
* STRATEGY.
*
* EM_COVAR.SAS REQUIRES THE SAS/BASE AND SAS/IML MODULES.
*
*
* EXAMPLE DATA SETS ARE PROVIDED AT THE END OF THIS FILE.  
* THESE DATA CAN BE USED TO CHECK EM_COVAR.SAS.
*
*
* PLEASE LET ME KNOW IF YOU USE EM_COVAR.SAS IN YOUR RESEARCH.
*
*
* STEVE GREGORICH
* PREVENTION SCIENCES GROUP &
* CENTER FOR AIDS PREVENTION STUDIES
* UNIVERSITY OF CALIFORNIA, SAN FRANCISCO
* 74 NEW MONTGOMERY STREET
* SAN FRANCISCO, CA 94105
* GREGORICH@PSG.UCSF.EDU
*
*
* HISTORY:
* -------
* INITIAL PROGRAMMING                                      3/21/96;
* MINOR ENHANCEMENTS                                       3/25/96;
* IMPROVED HANDLING OF MISSINGNESS PATTERNS                3/27/96; 
* IMPROVED HANDLING OF MISSINGNESS PATTERNS                8/20/96;
* ADDED OPTION FOR CORRELATION MATRIX OUTPUT               8/26/96;
* ADDITIONAL DOCUMENTATION                                 9/11/96;
* ENHANCED WARNING MESSAGES                                9/23/96;  
* IMPROVED SAS DATA SET CREATION (M. FRIENDLY)             4/21/97;
* ADDED BOOTSTRAP FACILITY                                12/19/97;
* MINOR ENHANCEMENTS TO PRINTED OUTPUT & NEW EXAMPLE DATA 12/31/97;
* MINOR CHANGE TO BOOTSTRAP LOOP.  NOW THE DEFAULT NUMBER 
*  OF BOOTSTRAP SAMPLES IS ZERO (I.E., THE ORIGINAL DATA
*  IS ANALYZED, WITH NO BOOTSTRAP SAMPLES).               12/09/98;
*
*
* REFERENCES:
* ----------
* ARBUCKLE (1996). FULL INFORMATION ESTIMATION IN THE PRESENCE
*    OF INCOMPLETE DATA.  IN MARCOULIDES & SCHUMACKER (EDS.),
*    _ADVANCED STRUCTURAL EQUATION MODELING: ISSUES AND 
*    TECHNIQUES_.  LEA.
*
* EFRON & TIBSHIRANI (1993). _AN INTRODUCTION TO THE BOOTSTRAP_.
*    CHAPMAN HALL.
*
* GRAHAM, HOFER, & MACKINNON (1996). MAXIMIZING THE USEFULNESS
*    OF DATA OBTAINED WITH PLANNED MISSING VALUE PATTERNS: AN
*    APPLICATION OF MAXIMUM LIKELIHOOD PROCEDURES.  
*    _MULTIVARIATE BEHAVIORAL RESEARCH_, 31, 197-238.
*
* LITTLE & RUBIN (1987). _STATISTICAL ANALYSIS WITH MISSING DATA_. 
*    WILEY.
*
* LITTLE & RUBIN (1989). THE ANALYSIS OF SOCIAL SCIENCE DATA WITH
*    MISSING VALUES. _SOCIOLOGICAL METHODS AND RESEARCH_, 18,
*    292-326.
*
* MOONEY & DUVAL (1993). _BOOTSTRAPPING: A NONPARAMETRIC APPROACH
*    TO STATISTICAL INFERENCE_. SAGE.
*
* MUTHEN, KAPLAN, & HOLLIS (1987). ON STRUCTURAL EQUATION MODELING
*    WITH DATA THAT ARE NOT COMPLETELY MISSING AT RANDOM.  
*    _PSYCHOMETRIKA_, 52, 431-462.
*
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* EXPLANATION OF MACRO OPTIONS AND DEFAULTS;
*-----------------------------------------------------------------*
* %em_covar (data     =, 
*            var      =, 
*            mat_type = cov,           ** THE DEFAULT MATRIX TYPE
*            out_file = c:\test.emc,   ** THE DEFAULT OUTPUT FILE
*            format   = 12.7,          ** DEFAULT FORMAT FOR OUTPUT DATA
*            macrolog = yes,           ** MACRO OUTPUT TO SAS LOG?   
*            nomiss   =,               ** PAIRWISE DELETION, DEFAULT
*            conv     = 0.0001,        ** CONVERGENCE DEFAULT
*            max_iter = 200,           ** MAX ITERATION DEFAULT
*            df       = n-1,           ** DENOMINATOR DEFAULT
*            bs       = 0)             ** DEFAULT # OF BOOTSTRAP SAMPLES 
*
* SPECIFICATION OF THE FOLLOWING MACRO VARIABLES IS REQUIRED
* ----------------------------------------------------------
* DATA     = NAME THE DATA SET CONTAINING THE VARIABLES TO BE 
*            ANALYZED.  
* VAR      = PROVIDE A VARIABLE LIST.  NOTE THAT THE VARIABLE LIST 
*            SHOULD EXPLICITLY NAME EACH VARIABLE.  DO *NOT* USE 
*            THE DASH-CONVENTION TO SPECIFY A SEQUENCE OF VARIABLE 
*            NAMES (E.G., DO NOT USE X1-X10);
*
* SPECIFICATION OF THE FOLLOWING MACRO VARIABLES IS ONLY REQUIRED
* IF YOU WISH TO CHANGE THE DEFAULTS
* ---------------------------------------------------------------
* MAT_TYPE = SPECIFY WHETHER A COVARIANCE OR CORRELATION MATRIX IS
*            ESTIMATED AND OUTPUT. A COVARIANCE MATRIX (THE DEFAULT)
*            IS SPECIFIED AS `OUT_TYPE=COV`.  A CORRELATION MATRIX
*            IS SPECIFIED AS `OUT_TYPE=CORR`.
* OUT_FILE = SPECIFY FULLY QUALIFIED PATH FOR THE OUTPUT TEXT DATA
*            FILE CONTAINING THE EM COVARIANCE MATRIX AND VECTOR
*            OF MEANS.  THE STRING SHOULD NOT BE IN QUOTES.  THE 
*            DEFAULT IS C:\TEST.EMC.
* FORMAT   = SPECIFY THE FORMAT FOR THE TEXT OUTPUT DATA FILE
*            SPECIFIED BY OUT_FILE=.  THE DEFAULT IS 12.7.
* MACROLOG = SPECIFY WHETHER THE MACRO SENDS FULL OR ABBREVIATED 
*            NOTES/MESSAGES TO THE SAS LOG FILE.  THE DEFAULT, 
*            `YES` SENDS FULL MACRO NOTES/MESSAGES TO THE SAS LOG.  
*            THE IS APPROPRIATE WHEN CHECKING FOR PROPER DATA INPUT 
*            AND MACRO EXECUTION, WHEN NOT REQUESTING BOOTSTRAPPING,
*            OR WHEN A SMALL NUMBER OF BOOTSTRAP SAMPLES ARE REQUESTED.  
*            WHEN A LARGE NUMBER OF BOOTSTRAP SAMPLES ARE REQUESTED, 
*            THE SAS LOG MAY BECOME FULL AND WILL NEED TO BE CLEARED 
*            BEFORE EXECUTION CAN CONTINUE. IN THIS CASE IT IS ADVISED 
*            THAT ABBREVIATED MACRO NOTES/MESSAGES ARE REQUESTED BY 
*            SETTING `MACROLOG` EQUAL TO `NO`.
* NOMISS   = SPECIFY WHETHER INITIAL COVARIANCE MATRIX SHOULD BE 
*            COMPUTED WITH LISTWISE OR PAIRWISE DELETION OF MISSING 
*            DATA.  GENERALLY LISTWISE DELETION WILL SUFFICE. 
*            HOWEVER, IF EVERY CASE HAS AT LEAST ONE MISSING VALUE, 
*            PAIRWISE DELETION MUST BE USED.  TO SPECIFY LISTWISE 
*            DELETION OF MISSING DATA THE VALUE OF THE MACRO VARIABLE 
*            SHOULD BE `NOMISS`.  TO SPECIFY PAIRWISE DELETION OF 
*            MISSING DATA THE VALUE OF THE MACRO VARIABLE SHOULD BE 
*            BLANK.  THE DEFAULT IS PAIRWISE DELETION;
* CONV     = SET THE CONVERGENCE CRITERION. THE ALGORITHM CONVERGES 
*            WHEN THE MAXIMUM ELEMENTWISE DIFFERENCE BETWEEN 
*            ITERATIONS IS LESS THAN THE VALUE SPECIFIED.  THE 
*            DEFAULT VALUE IS 0.0001;
* MAX_ITER = MAXIMUM ALLOWABLE ITERATIONS.  THE DEFAULT IS 200;
* DF       = SPECIFY THE DEGREES OF FREEDOM FOR VARIANCES AND 
*            COVARIANCES.  THE DEFAULT IS N-1;
* BS       = SPECIFY THE NUMBER OF BOOTSTRAP SAMPLES (EACH OF SIZE N) TO 
*            GENERATE.  BY DEFAULT NO BOOTSTRAP SAMPLES ARE GENERATED 
*            AND THE EM COVARIANCE MATRIX AND MEAN VECTOR ARE ESTIMATED
*            FROM THE ORIGINAL DATA SET.  THE DEFAULT VALUE FOR `BS`
*            IS ZERO (0).  TO PRODUCE STACKED BOOTSTRAP EM COVARIANCE 
*            MATRICES AND MEAN VECTORS, SPECIFY A POSITIVE INTEGER 
*            REPRESENTING THE NUMBER OF BOOTSTRAP SAMPLES TO GENERATE.  
*            WHEN BOOTSTRAP SAMPLES ARE REQUESTED THE SAS DATASET PRODUCED 
*            (I.E., WORK.COV) CONTAINS THE VARIABLE _SAMPLE_.  THIS 
*            VARIABLE INDEXES THE SEQUENTIAL NUMBER OF THE BOOTSTRAP SAMPLE.  
*            THIS ALLOWS FOR BY-VARIABLE PROCESSING WHEN ANALYZING BOOTSTRAP 
*            COVARIANCE MATRICES WITH SAS PROCEDURES.          
*-----------------------------------------------------------------*;
* NOTE.  EM_COVAR CREATES A SAS DATA SET THAT IS ALWAYS NAMED
*        WORK.COV REGARDLESS OF THE OPTIONS SPECIFIED;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* MACRO EM_COVAR--
* WITH DEFAULTS FOR MAT_TYPE=, OUT_FILE=, FORMAT=, MACROLOG=, 
                    NOPRINT=,  NOMISS=,   CONV=,   MAX_ITER=, 
                    DF=,       BS=;
*
* DO NOT SPECIFY ALTERNATIVE VALUES OF MACRO VARIABLES HERE.
* SPECIFY THEM WHEN YOU CALL THE MACRO AFTER IT IS COMPILED.
* (SEE EXAMPLES AT THE END OF THIS FILE).;
*-----------------------------------------------------------------*;
%macro em_covar (var      =,  
                 data     =,  
                 mat_type = cov, 
                 out_file = c:\test.emc,  
                 format   = 12.7, 
                 macrolog = yes, 
                 noprint  = noprint,
                 nomiss   = , 
                 conv     = 0.0001,  
                 max_iter = 200,  
                 df       = n-1, 
                 bs       = 0);
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* CONTROL OUTPUT TO SAS LOG;
*-----------------------------------------------------------------*;
options mprint symbolgen notes mlogic nodate nonumber;

%if %upcase(&macrolog)=NO %then %do;
  options nomprint nosymbolgen nonotes nomlogic;
%end;

run;
*-----------------------------------------------------------------*;



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
* NOTE. THE SEED FOR THE RANDOM NUMBER GENERATOR IS NOT CONTROLED 
*       BY THE MACRO;
*-----------------------------------------------------------------*;
%if &bs^=0 %then %do;
  data temp;
    drop i;
    do i=1 to n;
      rnd =int(ranuni(0)*n)+1;
      set temp point=rnd nobs=n;
      output;
    end;
  stop;
%end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* COMPUTE LISTWISE OR PAIRWISE COVARIANCE MATRIX AND MEAN VECTOR;
* THESE ARE THE PARAMETER ESTIMATES FOR THE FIRST ITERATION OF EM;
* THIS WILL WORK ON THE MOST RECENTLY CREATED DATA SET--EITHER
* THE PRIMARY DATA SET OF THE BOOTSTRAP DATA SET;
*-----------------------------------------------------------------*;
title;
proc corr cov outp=cov1 &noprint &nomiss; 
  var &var;

data cov_lw;                   ** CREATE COVARIANCE MATRIX DATA SET;
  set cov1;
  if _type_='COV';
  keep &var;

data mean_lw;                        ** CREATE MEAN VECTOR DATA SET;
  set cov1;
  if _type_='MEAN';
  keep &var;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* COUNT THE NUMBER OF ANALYSIS VARIABLES;
* PUT THE RESULT IN MACRO VARIABLE `NVAR`;
*-----------------------------------------------------------------*;
data temp; 
  set temp;

array x1x2x3 &var;
call symput('nvar',left(dim(x1x2x3)));
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* CREATE A VECTOR OF `PATTERN` INDICATOR VARIABLES SIGNIFYING WHETHER
* THE DATA FOR EACH VARIABLE IS PRESENT (`0`) OR MISSING (`1`);
*
* (1) ARRAY THE ANALYSIS VARIABLES
* (2) CREATE AND ARRAY INDICATOR VARIABLES FOR EACH ANALYSIS VARIABLE;
* (3) LOOP THROUGH EACH ANALYSIS VARIABLE;
* (4) IF THE ANALYSIS VARIABLE IS MISSING SET THE PATTERN 
*      INDICATOR EQUAL TO ONE;
* (5) IF THE ANALYSIS VARIABLE IS NOT MISSING SET THE PATTERN 
*      INDICATOR EQUAL TO ZERO;
* (6) SORT THE DATA ON THE PATTERN VARIABLES.
*-----------------------------------------------------------------*;
data temp; 
  set temp;

  array data {&nvar} &var;                                   ** (1);
    array pattern {&nvar} p1-p&nvar;                         ** (2);

      do jj = 1 to &nvar;                                    ** (3);
        if data{jj}=. then pattern{jj}=1;                    ** (4);        ** (6);
        else               pattern{jj}=0;                    ** (5);          ** (9);
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



*-----------------------------------------------------------------*;
* IML;
* (1) READ IN THE ORIGINAL DATA;
* (2) READ IN THE PATTERN VECTORS;
* (3) READ IN THE LISTWISE OR PAIRWISE COVARIANCE MATRIX;
* (4) READ IN THE LISTWISE OR PAIRWISE MEAN VECTOR;
*-----------------------------------------------------------------*;
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
print "EM_COVAR.SAS   (revision date: December 09, 1998)";
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



*-----------------------------------------------------------------*;
* MISSINGNESS PATTERN AND FREQUENCY;
* (1) CHECK TO MAKE SURE THE INITIAL COVARIANCE MATRIX HAS
*     VALID VALUES FOR ALL ELEMENTS.
* (2) CHECK TO SEE IF PAIRWISE DELETION IS REQUIRED FOR INITIAL
*     ESTIMATES;
* (2.5) CHECK TO MAKE SURE THAT EACH VARIABLE HAS SOME VALID
*       DATA;
* (3) HOLDS MAXIMUM NUMBER OF MISSING VALUES WITHIN A SINGLE CASE;
* (4) IF ANY CASE HAS ALL MISSING VALUES, STOP PROGRAM;
* (5) IF THERE IS NO MISSING DATA, STOP PROGRAM;
* (6) COMPUTE THE % OF MISSING VARIABLES IN MAX_WC;
* (7) COMPUTE PERCENT OF TOTAL MISSING VALUES IN DATA MATRIX;
* (8) `PATTERN2` HOLDS THE SEQUENTIAL MISSINGNESS PATTERN 
*     NUMBER.  THIS IS CALLED `SEQ_CLASS`. A VALUE OF `1` IS 
*     RESERVED FOR THE FIRST SEQUENTIAL `MISSINGNESS` CLASSIFICATION, 
*     WHETHER OR NOT IT HAS ANY MISSING VALUES;
* (9) LOOP FROM 2 TO N--YOU KNOW THAT THE FIRST ROW HAS SEQ_CLASS
*     EQUAL TO `1`;
* (10) IF THE SUMS OF ELEMENTS OF ADJACENT PATTERN VECTORS ARE NOT
*      EQUAL THE PATTERNS MUST BE DIFFERENT.  THE VALUE OF SEQ_CLASS
*      IS INCREMENTED BY ONE.;
* (11) GIVEN THAT THE SUMS OF ELEMENTS OF ADJACENT PATTERN VECTORS 
*      ARE THE SAME (BECAUSE OF `ELSE IF` AFTER `IF` IN (10), IF 
*      THE SUM OF THE CROSS-PRODUCTS OF THE ADJACENT PATTERN 
*      VECTORS DOES NOT EQUAL THE SUM OF THE ELEMENTS OF THE 
*      PATTERN VECTORS, THEN THE PATTERNS MUST BE DIFFERENT.  THE 
*      VALUE OF SEQ_CLASS IS INCREMENTED BY ONE.;
* (12) ELSE THE PATTERNS MUST BE THE SAME;
* (13) IF SOME CASES HAVE NO MISSING DATA THIS WILL FIND THE
       FIRST CASE WITH SOME MISSING DATA;
* (14) IF ALL CASES HAVE SOME MISSING DATA, SET `START` TO EQUAL 1;
* (15) COUNT THE NUMBER OF MISSINGNESS PATTERNS;
*-----------------------------------------------------------------*;
if cov_lw[+,+]=0 then do;                                    ** (1);
  print 
  "WARNING: ALL ELEMENTS OF THE INITIAL COVARIANCE MATRIX ARE MISSING";
  print "TRY PAIRWISE DELETION FOR INITIAL ESTIMATES";
  goto jump;
end;

if min(pattern[,+]) > 0 & upcase("&nomiss")='NOMISS' then do;  ** (2);
  print "WARNING: ALL CASES HAVE SOME MISSING DATA."; 
  print "USE PAIRWISE DELETION FOR INITIAL ESTIMATES.";
  goto jump;
end;

if max(pattern[+,]) = N                            then do;  ** (2.5);
  print "WARNING: AT LEAST ONE VARIABLE HAS NO VALID DATA."; 
  goto jump;
end;



max_wc   = max(pattern[,+]);                                 ** (3);

if max_wc = nvar then do;                                    ** (4);
  print "WARNING: AT LEAST ONE CASE HAS NO VALID DATA";      ** (4);
  goto jump;                                                 ** (4);
end;                                                   ** END IF-DO;

if max_wc = 0 then do;                                       ** (5);
  print "WARNING: NO MISSING DATA DETECTED";                 ** (5);   
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

put ' ';
put 'Number of Missingness Patterns:' npattern;

%if &bs^=0 %then %do;
  put ' ';
  put 'Bootstrap Sample #:' &bootloop '  out of:' &bs;
%end;

%if &bs=0 %then %do;
  put ' ';
  put 'Analysis of Original Data Set';
%end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* (1) CREATE COV_OLD;
*     AN AUGMENTED COVARIANCE MATRIX WITH THE MEAN VECTORS IN THE
*     (NVAR+1) ROW AND COLUMN.  NOTE THAT THE LOWER RIGHT CELL 
*     CONTAINS A `-1`.  THIS IS AS IT WOULD BE IF THE UNCORRECTED 
*     SSCP MATRIX WAS SWEPT ON THE CONSTANT--EXCEPT THAT HERE THE 
*     DEGREES OF FREEDOM FOR THE VARIANCES AND COVARIANCES ARE EQUAL
*     TO N-1 (BY DEFAULT) INSTEAD OF N;
*-----------------------------------------------------------------*;
cov_old  = (cov_lw // mean_lw) || (mean_lw` // -1);         ** (1);
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* MAIN LOOP;
* (1) CONVERGENCE IS OBTAINED WHEN THE MAXIMUM CHANGE FOR ALL 
*     PARAMETERS OF THE ESTIMATED COVARIANCE MATRIX AND MEAN 
*     VECTOR IS LESS THAN THE CONVERGENCE CRITERION.  
* (2) ITERATION COUNTER;
* (3) SSCP_P HOLDS THE `PENALTY TERMS` FOR EACH IMPUTATION.  THIS 
      MATRIX IS ZEROED-OUT AT THE BEGINNING OF EACH ITERATION.
      RESIDUAL VARIANCE AND COVARIANCE TERMS ARE ADDED TO THIS 
      MATRIX WHENEVER MISSING VALUES ARE ESTIMATED;
*-----------------------------------------------------------------*;
do until (converge < &conv+0);                               ** (1);
  time1=time();
  iterate = iterate + 1;                                     ** (2);

  if (iterate > &max_iter+0) then do;
    print "WARNING: MAXIMUM ITERATIONS EXCEEDED";
    goto jump;
  end;

** THIS MATRIX HOLDS THE PENALTY TERMS TO BE ADDED TO THE SSCP;
sscp_p = j(nvar,nvar,0);                                     ** (3);
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* COMPUTE REGRESSION PARAMETERS FOR ALL VARIABLES & IMPUTE
* MISSING VALUES;
* (1) LOOP THROUGH THE DATA STARTING WITH THE FIRST ROW CONTAINING 
      MISSING DATA;
* (2) INITIATE `SWEEP_IT`.  IF SWEEP_IT EQUALS `1` THE COVARIANCE
      MATRIX SHOULD BE SWEPT TO OBTAIN REGRESSION PARAMETER ESTIMATES;
* (3) `SWEEP_IT` SHOULD BE SET TO 1 WHENEVER THE LOOP BEGINS AT THE
      ROW NUMBER INDICATED BY THE VARIABLE `START`.  THIS CAN HAPPEN
      IN TWO DIFFERENT CASES (A) WHEN ALL CASES HAVE AT LEAST ONE
      MISSING VALUE THE VALUE OF THE VARIABLE `START` WILL EQUAL `1`
      AND THE FIRST VALUE OF `JJ` WILL EQUAL `1` BY DEFINITION. (B)
      WHEN SOME CASES HAVE NO MISSING DATA, THE FIRST VALUE OF `JJ`
      (AND `START`) WILL REPRESENT A ROW AFTER THE FIRST ROW.  BECAUSE
      THE FIRST POSSIBILITY PRECLUDES THE OPTION OF CHECKING THE VALUE
      OF THE ROW _BEFORE_ THE FIRST ROW, THE TWO LINES OF LOGIC WERE
      REQUIRED;
* (4) IF `SWEEP_IT` EQUALS ONE PERFORM THE FOLLOWING FUNCTIONS;
* (5) INITIALIZE VECTORS TO HOLD THE COLUMN NUMBERS FOR IVs AND DVs;
* (6) BUILD VECTORS OF IV AND DV COLUMN NUMBERS;
* (7) GET RID OF LEADING ZERO IN IV AND DV VECTORS;
* (8) SWEEP THE AUGMENTED COVARIANCE MATRIX ON THE PIVOT POINTS;
* (9) ADD THE APPROPRIATE RESIDUAL VARIANCES AND COVARIANCES TO THE 
      SSCP_P MATRIX.  THIS IS DONE BY PRE-MULTIPLYING THE ROW`S
      PATTERN VECTOR BY ITS TRANSPOSE AND THEN PERFORMING ELEMENT-
      WISE MULTIPLICATION OF THE SWEPT MATRIX (ACTUALLY, ELEMENT-
      WISE MULTIPLICATION IS PERFORMED ON THE SWEPT MATRIX MINUS 
      THE NVAR+1 ROW AND COLUMN WHICH HOLD THE INTERCEPT TERMS).
      THIS PICKS OUT THE APPROPRIATE RESIDUAL VARIANCES AND COVARIANCES.
* (10) THIS LOOP WILL ESTIMATE THE MISSING VALUES FROM THE REGRESSION
       PARAMETERS.  THE LOOP USES THE COLUMNS OF THE DV VECTOR TO
       `FIND` THE DV VARIABLES TO ESTIMATE VALUES FOR.
* (11) THE DATA VALUES CORRESPONDING TO THE COLUMNS INDICATED IN
       THE IV VECTOR ARE AUGMENTED WITH A `1` FOR THE CONSTANT TERM.
       THIS VECTOR IS THEN POST-MULTIPLIED BY A COLUMN VECTOR HOLDING
       THE REGRESSION PARAMETERS (THE INTERCEPT TERM IS CONCATENATED
       TO THE VECTOR OF VARIABLE PARAMETERS).
*-----------------------------------------------------------------*;

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



*-----------------------------------------------------------------*;
* COMPUTE NEW COVARIANCE MATRIX AND COMPARE TO OLD;
* (1) VARIABLE MEANS;
* (2) CORRECTED SSCP MATRIX;
* (3) ADD CUMULATIVE RESIDUAL VARIANCE AND COVARIANCES TO SSCP;
* (4) COMPUTE NEW COVARIANCE MATRIX;
* (5) AUGMENT COVARIANCE MATRIX WITH MEAN VECTORS;
* (6) COMPUTE DIFFERENCE BETWEEN COV_OLD AND COV;
* (7) COMPUTE CONVERGENCE AS THE MAXIMUM DISCREPANCY BETWEEN 
      COV_OLD AND COV;
* (8) UPDATE COV_OLD;
*-----------------------------------------------------------------*;
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

put ' ';
put 'iteration: ' iterate '    converge: ' converge '    time: ' time;

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

%if &bs^=0 %then %do;
  print "Data: %upcase(&data)             Bootstrap Sample #: &bootloop of &bs";
%end;

%if &bs=0 %then %do;
  print "Data: %upcase(&data)        Analysis of Original Data Set";
%end;

print "Total Sample Size:" N  "     Number of Variables:" nvar;

print "Number of Missingness Patterns:"
      npattern [format=3.0];

print "Maximum: Within-Case Missing Values:" 
       max_wc  [format=3.0] 
       perc_wc [format=5.2] "%";

print "Percentage of Values Missing:" 
       perc_tot [format=5.2] "%";

print "Convergence Information:" iterate converge;

print "Average Time (in Seconds) per Iteration:" ave_time;

print "Determinant of EM Covariance Matrix:" det;

print "Minimum Eigenvalue:" eig;

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
* WORK.COV IS A SAS DATA SET THAT CAN BE USED AS INPUT IN SAS PROCS.
* BY SPECIFYING A TWO-LEVEL NAME, THIS SAS DATA SET CAN BE SAVED.
*
* WORK.COV_OUT REMOVES THE VECTOR OF Ns PRIOR TO BEING WRITTEN AS
* A TEXT FILE.  Ns ARE STRIPPED BECAUSE SEM SOFTWARE DOES NOT
* EXPECT THEM IN THE DATA FILE.
*-----------------------------------------------------------------*;

** IF THE ORIGINAL SAMPLE OR THE FIRST BOOTSTRAP SAMPLE IS ANALYZED;

%if &bs_start=0 | (&bs_start=1 & &bootloop=1) %then %do;

  ** CREATE THE SAS COVARIANCE DATA SET;
  ** IF YOU WANT TO CREATE A PERMANENT SAS DATA SET SPECIFY A TWO LEVEL
  ** NAME FOR DATA SET COV (HERE, AND ALSO SEE BELOW).;
  data cov(type=&mat_type); 
    set em;
       if      _name_ ^=' '   then _type_="&mat_type ";
  else if      _name_  =' '   then _type_='MEAN';
       if lag1(_type_)='MEAN' then _type_='N   ';
  _type_ = upcase(_type_);
  _sample_ = &bootloop;


  ** WRITE THE DATE TO AN ASCII FILE;
  data _null_;
    set cov;
    if _type_^='N';
    file "&out_file" LRECL=72; 
    put (&var) (&format);
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
  ** IF YOU WANT TO CREATE A PERMANENT SAS DATA SET, USE THE TWO-LEVEL
  ** NAME FOR DATA SET COV IN BOTH THE DATA AND SET STATEMENTS.;
  data cov (type=&mat_type);
    set cov em;

  ** CONCATENATE ONTO THE PREVIOUSLY WRITTEN ASCII FILES;
  data _null_;
    set em;
    drop _sample_;
    if _type_^='N';
    file "&out_file" LRECL=72 mod; 
    put (&var) (&format);
%end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
* END OF BOOTLOOP;
*-----------------------------------------------------------------*;
%end;
*-----------------------------------------------------------------*;



*-----------------------------------------------------------------*;
options mprint symbolgen notes mlogic number date;
run;
%mend em_covar;
*-----------------------------------------------------------------*;
* END MACRO EM_COVAR;
*-----------------------------------------------------------------*;





*-----------------------------------------------------------------*;
* EXAMPLES/TESTING EM_COVAR.SAS;
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
  * CALL EM_COVAR;
  * TO DUPLICATE THEIR RESULTS, SPECIFY DF=N;
  *-----------------------------------------------------------------*;
  %em_covar (data     = lr,
             var      = x1 x2 x3 x4 x5,
             nomiss   = nomiss,
             df       = n) 
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
  * CALL EM_COVAR;
  * TO DUPLICATE THE RESULTS OF THE `NORM` PROGRAM, SPECIFY DF=N;
  *-----------------------------------------------------------------*;
  %em_covar (data     = exam,
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
  * CALL EM_COVAR;
  *---------------------------------------------------------------*;
  %em_covar (data     = iris,
             var      = sepallen sepalwid petallen petalwid,
             macrolog = no,
             mat_type = corr,
             bs       = 3)
  *---------------------------------------------------------------*;
    /*  */
*-----------------------------------------------------------------*;
* END OF EXAMPLES/TEST DATA SETS;
*-----------------------------------------------------------------*;


------ ÿextPart_000_01BE2370.8E8283E0--
