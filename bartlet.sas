%MACRO BARTLET(DATASET,FACTOR,RESP);
%*---------------------------------------------------------;
%*  name: bartlet.sas                                      ;
%* title: Bartletts test for homogeneity of variances      ;
%*                                                         ;
%* The test allows for unequal number of observations per  ;
%* group.                                                  ;
%* Arguments:                                              ;
%*   DATASET- The name of the SAS dataset                  ;
%*   FACTOR - The name of the classification variable      ;
%*   RESP   - The name of the response variable            ;
%*---------------------------------------------------------;
proc sort data=&DATASET;  /* SORT BY THE CLASSIFICATION VAR. */
  by &FACTOR;
proc means noprint;       /* STORE THE VARIANCE & NUMBER OF */
  by &FACTOR;             /*   OBSERVATIONS FOR EACH LEVEL. */
  var &RESP;
  output out=sumry var=variance n=num;
 
data _null_;
  set sumry end = eof;
  logvar = log(variance);
  n = num-1;           /* DEGREES OF FREEDOM FOR CURRENT LEVEL */
  slogvar + logvar*n;
  totn + n;
  nvar = n*variance;
  snvar + nvar;
  a + 1;               /* NUMBER OF LEVELS                     */
  sfract + 1/n;
  if eof then do;
    m = totn*log(snvar/totn)-slogvar;
    c = 1 + (1/(3*(a-1)))*(sfract-1/totn);
    chisq = m/c;
    probchi = probchi(chisq,(a-1));
    alpha = 1-probchi;
    file print;
    df  = a-1 ;
    put 'Bartlett''s test for equality of variances in one-way design'//
      @10 "Dataset :" @25 "&DATASET" /
      @10 "Response:" @25 "&RESP" /
      @10 "Factor:"   @25 "&FACTOR" /
      @10 'Levels:'   @25 a //
      @10 'Chi-Square  =  ' chisq ' with ' df 'd.f.' /
      @10 'P-value     =  ' alpha ;
    end;
%mend;
 
