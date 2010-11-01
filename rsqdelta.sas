/************************************************************************

                                RSQDELTA

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   TITLE:  Compute R-square change and F-statistics in regression
	
   PURPOSE:  
     In a regression analysis, compute the change in R-square and the
     associated F statistics and p-values as variables are added to the
     model

   REQUIRES:
     Base SAS and SAS/STAT Software.

   USAGE:
     Assign the name of your input data set to the macro variable
     DATASET.  Assign the name of your dependent variable to the macro
     variable YVAR=.  Assign the names of your independent varaibles
     (in the order in which you want them included in the model) to the
     macro variable XVAR=.

   PRINTED OUTPUT:
     The example (following the macro definition below) creates the 
     following output:

        Change in R-square & F statistics as variables are added
                        to the regression model

     OBS    _MODEL_      _RSQ_     RSQDELTA          F      PROB

      1     INTERCEP    0.00000      .         2451.73    0.00000
      2     RUNTIME     0.74338     0.74338      84.01    0.00000
      3     AGE         0.76425     0.02087       2.48    0.12666
      4     RUNPULSE    0.81109     0.04685       6.70    0.01537
      5     MAXPULSE    0.83682     0.02572       4.10    0.05330
      6     WEIGHT      0.84800     0.01118       1.84    0.18714

   DETAILS:
     PROC REG will provide the partial R-square as variables are added
     to the model, if you specify the SCORR1 option on the MODEL
     statement.  However, PROC REG does not include the partial F
     statistics and p-values and none of this is provided in an output
     data set.

     PROC REG includes the R-square statistic in the OUTEST dataset when
     the ADJRSQ option is specified on the MODEL statement.  In the
     OUTSTAT= data set, PROC GLM includes the sequential sums of squares
     (SS1) that will provide the F statistic and associated p-value.
     However, PROC GLM does not include the R-square statistic in an
     output dataset.

     By combining the information from PROC REG's OUTEST dataset and
     PROC GLM's OUTSTAT dataset and doing some DATA step programming, we
     will have a program that will compute the change in R-square as
     well as the F statistics and p-values as variables are added to a
     model.  The F statistics and p-values in the final table represent
     a partial F statistic for the general linear model testing approach
     which is defined as follows:

                  SSE(R) - SSE(F)
                  ----------------
                   df(R) - df(F)
       F   =   ______________________
        C
                      SSE(F)
                     --------
                       df(F)

     where SSE is the Error Sum of Squares and df is the error degrees
     of freedom for the full (F) or reduced (R) model.

     The rejection region is defined as 

                  F  > F                        
                   C    (alpha, df(R)-df(F), df(F))

     Note the F  statistic  is a different than an overall F test which
               C
     tests whether or not there is a regression relationship between the
     dependent variable and the set of independent variables.  Most
     regression textbooks provide a discussion of tests about the
     regression coefficients.


   REFERENCES:
     Myers, (1986), Classical and Modern Regression with Applications.
     Neter, Wasserman, Kutner (1989), Applied Linear Regression Model.
     Rawlings (1988), Applied Regression Analysis.

************************************************************************/


%macro rsqdelta(data=,yvar=,xvar=);

  %let count=1;
  %let b=%str(&xvar);
  %let z=%str( );
  %let c=%str( );
  proc reg data= &data outest=est;
    INTERCEP:model &yvar=&z/adjrsq;
  %do %until( "&c" = "" );
    %let c= %scan(&b,&count,%str( ));
    %let z=%str(&z &c);
    %let count=%eval(&count+1);
    %if "&c" ^= "" %then &c:model &yvar=&z/adjrsq;
    ;
  %end;

  %let count=1;
  %let b=%str(&xvar);
  %let z=%str( );
  %let c=%str( );
  %do %until( "&c" = "" );
    %let z=%str(&z &c);
    %let c= %scan(&b,&count,%str( ));
    proc glm data=&data outstat=out&count noprint;
           model &yvar=&z/ss1;
    run;
    %let count=%eval(&count+1);
  %end;

  %do i=1 %to %eval(&count-1);
  data out&i;
     do while ( last ^= 1);
        set out&i end=last;
     end;
     output;keep f prob;stop;
  run;
  %end;
  data g;
      retain prevrsq .;
      set est;
      if _n_=1 then rsqdelta=.;
      else rsqdelta =_rsq_ - prevrsq;
      prevrsq=_rsq_;
      keep _model_ _rsq_ rsqdelta;
  run;


  data table;
  %do i=1 %to %eval(&count-1);
    set out&i;
    output;
  %end;


  data final;
  merge g table;
  run;
  proc print data=final;
  title2 "Change in R-square & F statistics as variables are added";
*  title2 "to the regression model";
  run;
%mend;


/* An example */

options ls=72;
*include data(fitness);

/* The macro parameter xvar= contains the regressor
   variables in the order that you desire */

/*
%rsqdelta(data=fitness, yvar=oxy, xvar=runtime age runpulse maxpulse weight);
*/

