/*-------------------------------------------------------------------*
 *    Name: FPOWER.SAS                                               *
 *   Title: Power computations for ANOVA designs                     *
 *     Doc: http://www.math.yorku.ca/SCS/sasmac/fpower.html          *
 *                                                                   *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly       <FRIENDLY@YorkU.CA>              *
 *  After : AJL Cary, Syntex Research; from: SAS SUGI, 1983          *
 * Created:  14 Mar 1990 11:16:28                                    *
 * Revised:   7 Apr 1998 17:43:00                                    *
 * Version:  1.2                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
/*--------------------------------------------------------------|
| This MACRO computes power of an F-test for main effects for   |
| one- or two-way layouts with or without repeated measures,    |
| assuming main effects are fixed.  The alternative used is a   |
| minimum power alternative. Actually, the program can be used  |
| for ANY fixed effect in ANY crossed factorial design, by      |
| designating the levels of the effect of interest as A, and    |
| the levels of all other crossed factors as B.                 |
|                                                               |
| The "effect size" is specified by DELTA. Assuming that only   |
| the extreme factor levels have NON-ZERO effects, and          |
|           T1 = GM - DELTA/2                                   |
|           Tk = GM + DELTA/2                                   |
| where DELTA is specified in units of SIGMA = SQRT(MSE)        |
|                                                               |
| The intraclass correlation (RHO) is assumed to be positive    |
| and constant across all repeated measures.                    |
|                                                               |
| The macro variable N provides:                                |
|      DO-list of sample sizes to calculate power for.          |
|                                                               |
| An output dataset named PWRTABLE is created for plotting or   |
| saving, and it contains an observation for each entry.        |
|--------------------------------------------------------------*/
 
options nonotes;
     *-------------- Define output format for small & -----------
     --------------- and large probabilities -------------------;
Proc format;
   picture PROBS  /*  .99901 - 1.0000  ='  >.999'
                     0.0000 - 0.00089 ='  <.001'  */
                          other   ='  9.999';
  
%MACRO FPOWER(
     LEVELS=,    /* Levels of factor for power calculation */
     A=,         /* Levels of factor for power calculation */
     CROSSED=1,  /* Levels of crossed factor(s) (default=1) */
     B=,         /* Levels of crossed factor B (default=1) */
     C=1,        /* Levels of crossed factor C (default=1) */
                 /* For >3 factors, make C=product of # of */
                 /*  levels of factors D, E, etc.          */
     R=1,        /* Levels of repeated measure factor      */
     ALPHA=.05,  /* Significance level of test of factor A */
     N =%str( 2 to 10 by 1, 12 to 18 by 2,  20 to 40 by 5, 50),
     DELTA=.50 to 2.5 by 0.25,  /* DO list of DELTA values */
     RHO=0,      /* Intraclass correlation for repeated    */
                 /*   measures (DO list of values)         */
     PTABLE=YES, /* Print a power table?                   */
     PLOT=NO,    /* Plot power*delta=N ?                   */
     NTABLE=NO,  /* Print a sample-size table ?            */
     OUT=PWRTABLE
	  );

%if %length(&A)=0 %then %let A=&levels; 
%if %length(&B)=0 %then %let B=&crossed; 
%if &A=%str() %then %do;
    %put ERROR : You must specify a value for LEVELS= in the macro call;
    %goto DONE;
    %end;
 
%let pfile = print;
%if %upcase(&PTABLE) ^=YES %then %do;
	%if &sysscp = WIN
		%then %str(filename ptable DUMMY 'nul:';);
		%else %str(filename ptable DUMMY '/dev/null';);
	%let pfile = ptable;
	%end;
	
data &out;;
     length A B C R N TRTDF 3;
     drop fcrit ncol nalpha;
 
     A = &A;
     B = &B;
     C = &C;
     R = &R;
 
     do ALPHA = &ALPHA;
     nalpha+1;
     file &pfile;
 
     TRTDF = (A-1);         *-- Treatment degrees of freedom--;
 
          *-- Iterate thru RHO values supplied in RHOs    --;
          *   If not repeated measures design, stop after   ;
          *   one iteration.                                ;
 
     do RHO = &RHO;                   *-- Get macro values;
          put _PAGE_;
          put @5 A 2. 'x' B 2. @;
          if C>1 then put 'x' C 2. @;
          put +2 ' layout Ha: T1=GM-Delta/2, T2=T3=...=T(k-1)'
              '=GM,  Tk=GM+Delta/2'  +3 'tested at Alpha=' ALPHA 6.3;
 
          if R>1 then put @10 'with' R 2.0
              ' Repeated measures and intraclass RHO = ' RHO 4.2 ;
          else put ;
          put @20 'DELTA (in units of sigma=Std. Dev.)'// @7 'N' @;
 
          do DELTA = &DELTA;            *-- Get macro values;
 
               put DELTA 7.3 @;
               end;
          put;
          ncol = 0;
          do DELTA = &DELTA;
             ncol = ncol+1;
             put '-------' @;
             end;
          put '-------';
 
          *-- Iterate through sample sizes --;
 
          do N = &N;             *-- Get macro values;
 
               put / N 7.0 @;
 
               *-- Compute error degrees of freedom & F crit --;
               errdf = (a * b * c * (n-1));
               fcrit = finv (1-alpha, trtdf, errdf, 0);
 
               *-- Iterate through values of DELTA --;
               do DELTA = &DELTA;
 
                    *-- Compute non-centrality parameter, & power --;
 
                    NC = N * B * C * R * DELTA**2/ (2*(1+(R-1)*RHO));
                    if NC > 140 then POWER = 1;
                    else POWER = 1 - probf(FCRIT, TRTDF, ERRDF, NC);
 
                    put POWER PROBS. @;
                    output;
                    end;    *-- Do DELTA;
                end;   *-- Do N;
 
                *-- Exit loop if not repeated measures;
                if R=1 then STOP;
            end;   *-- do RHO;
         end;  *-- do ALPHA;
   call symput(nalpha,put(nalpha,2.));
run;
   %if %upcase(&PLOT)=YES %then %do;
   proc plot data=&out;
      plot POWER * DELTA = N /vaxis = 0 to 1 by .1 vref=1;
      title2 'Power as a function of Effect Size (DELTA) and N';
      run;
   %end;
 
   %if %upcase(&NTABLE)=YES %then %do;
   data _ntable_;
      set &out;
      drop a b c r rho errdf nc trtdf;
      where (power>=.5);
      power = min(power,.9999);
*     pwr = round(power,.05);
		pwr = .1 * floor(10*power);
   proc sort;
      by alpha pwr delta n;
   *-- Find the smallest n for given power, delta;
   data _ntable_;
      set _ntable_;
      by alpha pwr delta n;
      if first.delta then output;
   /*
   proc print data=_ntable_;
      id pwr;
      by pwr;
      var delta alpha n power;
   */
   proc tabulate;
      class pwr delta;
      var n;
      table pwr='Power',
            delta*n=' '*f=5.*sum=' ' / rts=9;
      format n 3.;
      title2 "Sample size to achive a given power, alpha= &alpha";
      run;
      %end;
%DONE:
options notes;
%mend FPOWER;
*------------------ End of FPOWER -----------------------;
