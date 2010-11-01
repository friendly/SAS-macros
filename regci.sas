/*****************************************************************************
                                                                            
                                REGCI

	Title: Regression statistics and confidence intervals for parameters
                                                                            
    DISCLAIMER:                                                             
      THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO    
      ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,         
      EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A          
      PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE    
      CONTAINED HEREIN.                                                     
                                                                            
    PURPOSE:                                                                
      REGCI is to be used for producing a SAS dataset containing test    
      statistics and confidence intervals for regression parameter          
      estimates obtained from PROC REG, with or without a BY variable.  

    REQUIRES:
      Base SAS and SAS/STAT Software.  All releases.

    USAGE:
      %regci(data=, y=, x=, by=);

    PRINTED OUTPUT:
      First, the PROC REG output for each BY-group (not shown).  If it
      is not needed, use the NOPRINT option on the PROC REG statement.
      Then, the created data set containing the statistics of interest
      is printed:

 BYVAR _NAME_    PARMEST  STDERR T_FOR_H0 P_VALUE    LOWER    UPPER   RSQ

   1   INTERCEP  113.335 13.5782  8.34684 0.0001   81.2277  145.442 0.92586
   1   RUNTIME    -2.682  0.5912 -4.53587 0.0027   -4.0797   -1.284 0.92586
   1   AGE        -0.394  0.1369 -2.87948 0.0237   -0.7181   -0.071 0.92586
   1   WEIGHT     -0.240  0.1887 -1.27298 0.2437   -0.6864    0.206 0.92586
   2   INTERCEP   80.554 12.9380  6.22618 0.0004   49.9606  111.147 0.74281
   2   RUNTIME    -2.528  0.5647 -4.47664 0.0029   -3.8636   -1.193 0.74281
   2   AGE        -0.002  0.1591 -0.01026 0.9921   -0.3777    0.374 0.74281
   2   WEIGHT     -0.086  0.0630 -1.36380 0.2149   -0.2350    0.063 0.74281
   3   INTERCEP   83.507 18.1773  4.59402 0.0059   36.7808  130.233 0.75398
   3   RUNTIME    -4.121  1.1619 -3.54653 0.0164   -7.1076   -1.134 0.75398
   3   AGE        -0.032  0.2280 -0.13893 0.8949   -0.6177    0.554 0.75398
   3   WEIGHT      0.118  0.1616  0.73261 0.4967   -0.2970    0.534 0.75398

    LIMITATION:
      Only one BY variable is allowed.

    MISSING VALUES:
      Observations with missing values are omitted from the analysis.

    SEE ALSO: 
      If you are not using a BY statement, use the program REGCI.SAS in 
      the SAS/STAT Sample Library (title:  "Extracting Anova Data from 
      the Reg Output Data Set").              
                                                                            
*****************************************************************************/  


%macro regci(data=, y=, x=, by=);

%if %length(&by)=0 %then %do;
   data _sorted_;
      set &data;
      all_=1;
      run;
   %let by=ALL_;
   %end;
%else %do;
proc sort data=&data out=_sorted_;
  by &by;
run;
%end;

%let data=_sorted_;
   /*
   First a regression line is fit using PROC REG.  The OUTEST and
   COVOUT options will include the parameter estimates as well as the
   covariance matrix of the parameter estimates in an OUTEST= data
   set called est.  */

proc reg outest=est covout;
  by &by;
  model &y=&x / sse;
  output out=resids residual=r;
run;

proc means noprint n data=resids;  /* this will be used to count the */
  by &by;                        /* number of degrees of freedom   */
  var r;                           /* for each by-group              */
  output out=count n=n_obs;
run;

/**
 In the following macro, the matrix containing the parameter
 estimates and their respective variances is manipulated so that
 each parameter estimate will be stored in a variable called
 PARMEST and its standard error will be in a variable named STDERR.
 _NAME_ will identify each parameter estimate.  T_FOR_H0 is the
 T-statistic associated with the test of the null hypothesis that
 the parameter estimate is equal to zero while the variable
 P_VALUE is the P-value associated with the test.  Note that
 this value may appear as 0.0000 when in fact this indicates that
 the value is between 0 and 0.0001.  The variables UPPER and
 LOWER will contain the boundaries on the confidence interval.
 The level of confidence will be (1-ALPHA)*100% where ALPHA is
 supplied by the user.  In this example ALPHA=0.05 so each
 interval is a 95% confidence interval.  (Note that there was
 no attempt to provide a family-wise protection level, such
 as a Bonferroni correction.)
**/

data _null_;
  set count;
  by &by;
  if first.&by then do;
    i+1;
    call symput('n_obs'||left(i),n_obs);
  end;
run;

data _null_;
  set est end=eof;
  by &by;
  if _name_ ne ' ' then do;
    n+1;
    call symput('var'||left(n),_name_);
    call symput('typ'||left(n),_type_);
  end;
  if last.&by then do;
    j+1;
    call symput('n',n);
    call symput('n_byvar',j);
    n=0;
  end;
run;

%manip;

proc print data=est2 noobs;
*  var &by _name_ parmest stderr t_for_H0 p_value lower upper rsq;
run;
%mend;

%macro manip;
data est2;
   set est;
   by &by;
   format p_value 6.4;
   alpha = 0.05;
   keep 
		%if &by ^= ALL_ %then &by ;
		_name_ parmest  stderr t_for_H0 p_value upper lower rsq;
   retain rsq;
   %do i = 1 %to &n_byvar;
       df&i = &&n_obs&i - &n;
       retain df1
       %do j=1 %to &n;
          b&j
       %end;
        ;
   %end;
   if _type_='PARMS' then do;
      rsq=_rsq_;
      k+1;
      %do i=1 %to &n;
          b&i=&&var&i;
      %end;
   end;

   if _type_='COV' then do;
      %do i=1 %to &n;
      if _name_="&&var&i" then           do;
      parmest=b&i; stderr=sqrt(&&var&i); end;
      %end;
        t_for_H0 = parmest/stderr;
        %do m = 1 %to &n_byvar;
           %if &m=1 %then %do;
              if k = 1 then do;
               p_value  = 2*probt(-abs(t_for_H0),df1);
               t = -tinv(alpha/2, df1);
                       end;
           %end;
           %else %do;
             else if k=&m then do;
                 p_value  = 2*probt(-abs(t_for_H0),df&m);
                 t = -tinv(alpha/2, df&m);
               end;
           %end;
        %end;
        upper    = parmest+t*stderr;
        lower    = parmest-t*stderr;
        output;
                                end;
     run;
%mend;

