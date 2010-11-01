%macro bootreg (NMODELS=1,DATA=&SYSDSN,MODEL1=, MODEL2=,MODEL3=,MODEL4=,
    MODEL5=,MODEL=,FREQ=NIDOBS,WEIGHT=,PLOT=N,SAMPLE=100); * / STMT;
 
%GLOBAL VARS;
%IF &DATA=&SYSDSN %THEN %DO;
     %LET DATA1=%SCAN(&DATA,1);
     %LET DATA2=%SCAN(&DATA,2);
     %LET DATA=&DATA1..&DATA2;
%END;
%IF &MODEL1=%STR() %THEN %LET MODEL1=&MODEL;
%LET FREQ=%UPCASE(&FREQ);
%LET PLOT=%UPCASE(&PLOT);
%IF &PLOT=YES %THEN %LET PLOT=Y;
%LET BAD=0;
%LET Z=0;
%DO N=1 %TO &NMODELS;
     %IF ( %INDEX(&&MODEL&N,%STR(%')) ^= 1 AND
             %INDEX(&&MODEL&N,%STR(%")) ^= 1 ) %THEN %DO;
          %LET BAD=1;
          %PUT MODEL&N was entered incorrectly : &&MODEL&N ;
          %PUT MODEL statement must use one of the following forms: ;
          %PUT %str(      ) MODEL&N= 'depvars = indvars'  or  ;
          %PUT %str(      ) MODEL&N= 'depvars = indvars / NOINT' ;
     %END;
%END;
%IF &BAD=1 %THEN %GOTO EXIT;
 
     /* uses BOOTSAM macro to draw bootstrap samples.  If
        input data is already a bootstrap sample (from BOOTSAM
        for example) then BOOTSAM is not called. */
 
%IF &SAMPLE ^= 0 %THEN %BOOTSAM(DATA=&DATA,SAMPLE=&SAMPLE,FREQ=&FREQ);
%ELSE %DO;
          data bootdata;
            set &DATA;
            call symput('SAMPLE',put(j,best.));
          run;
%END;
%LET N=0;
%DO N=1 %TO &NMODELS;
     %LET M=1;
     %LET L=%LENGTH(&&MODEL&N);
            /* strips &MODELn of quotes */
     %LET MODEL&N=%SUBSTR(&&MODEL&N,2,&L-2);
 
            /* parses each model statement into independent
               and dependent variables.  &In is independent
               for each model.   */
 
     %LET START=%EVAL(%INDEX(&&MODEL&N,=) + 1);
     %LET INT&N=%STR();
     %LET INT=%INDEX(&&MODEL&N,NOINT);
     %IF &INT ^= 0 %THEN %LET INT&N=NOINT;
     %LET STOP=%INDEX(&&MODEL&N, /);
     %IF &STOP ^= 0 %THEN %DO;
          %LET MODEL&N=%SUBSTR(&&MODEL&N,1,&STOP-1);
     %END;
     %LET I&N=%SUBSTR(&&MODEL&N,&START);
 
          /* macro has ability to give normal probability plots.
              To do that later, will need temporary variables
              Rn and NORMn. */
 
     %IF &PLOT=Y %THEN %DO;
          %LET R&N=%STR() ;
          %LET NORM&N=%STR();
     %END;
 
          /* parses list of indep vars for each model to create
             master list of all indep vars for all models. */
 
     %LET BOB=%SCAN(&&I&N,&M);
     %DO %UNTIL (&BOB=%STR() );
          %IF (&N=1 AND &M=1) %THEN %LET VARS=&BOB;
          %ELSE %IF %INDEX(&VARS,&BOB)=0 %THEN %LET VARS=&VARS &BOB;
 
             /* for each model, have an Rn and NORMn for each
                indep var (e.g. R1 - R3 for 3 indep vars ). */
 
          %IF &PLOT=Y %THEN %DO;
               %LET R&N=&&R&N R&M;
               %LET NORM&N=&&NORM&N N&M;
          %END;
          %LET M=%EVAL(&M+1);
          %LET BOB=%SCAN(&&I&N,&M);
     %END;
     %IF (&PLOT=Y AND &&INT&N=%STR() ) %THEN %DO;
          %LET R&N=RI &&R&N;
          %LET NORM&N=NI &&NORM&N;
          %LET I&N=INTERCEP &&I&N;
     %END;
     %IF (&&INT&N=%STR() AND &Z=0) %THEN %DO;
          %LET VARS=INTERCEP &VARS;
          %LET Z=%EVAL(&Z+1);
     %END;
%END;
%LET N=1;
 
    /* finally run regressions and save coefficient estimates */
 
%DO N=1 %TO &NMODELS;
     %IF &N=1 %THEN %DO;
          proc reg data=bootdata noprint
            outest=estdata(keep=j _model_ _depvar_ &VARS);
     %END;
            mod&N : model &&MODEL&N  / &&INT&N ;
%END;
            freq &FREQ;
            weight &WEIGHT;
            by j;
          run;
            data estdata;
              set estdata;
              array missing &VARS;
              do over missing;
                   if missing=-1 then missing=.;
              end;
            run;
            proc sort data=estdata;
              by _model_ _depvar_;
            run;
 
      /* get means, standard deviations, etc.  I chose UNIVARIATE
         because it was the only procedure I found which gave
         percentiles in the output dataset (P5, P95). */
 
            proc univariate data=estdata noprint;
              var  &VARS;
              by _model_ _depvar_;
              output out=mean mean= &VARS;
              output out=std std= &VARS;
              output out=p5 p5=&VARS;
              output out=p95 p95=&VARS;
            run;
            data bootest;
              stat='mean'; set mean; output;
              stat='std'; set std; output;
              stat='p5'; set p5; output;
              stat='p95'; set p95; output;
            run;
            proc transpose data=bootest out=bootest(drop=_label_);
              by _model_ _depvar_;
              var &VARS;
              id stat;
            run;
            data bootest; set bootest;
              if mean=. then delete;
            run;
            proc print data=bootest;
       title1 "Bootstrap estimates for regression";
       title2 "&SAMPLE samples";
       title3 ;
       title4 "Mean, standard deviation and 90% confidence interval";
       title5 "reported below" ;
       title6 ;
            run;
 
         /* this section produces freq and normal probability plots */
 
%IF &PLOT=Y  %THEN %DO;
            data mod1
     %LET N=2;
     %IF &NMODELS GT 1 %THEN %DO N=2 %TO &NMODELS;
              mod&N
     %END;
              ;
              set estdata;
              if upcase(_model_)="MOD1" then output mod1;
     %LET N=2;
     %IF &NMODELS GT 1 %THEN %DO N=2 %TO &NMODELS;
              else if upcase(_model_)="MOD&N" then output mod&N;
     %END;
            run;
     %LET N=1;
     %DO N=1 %TO &NMODELS;
       title1 "Frequency chart for model&N: &&MODEL&N";
       title2 ;
 
        /* produces freq chart for each estimate for each model */
 
            proc chart data=mod&N;
              vbar &&I&N/ levels=50 nozeros nospace type=freq;
            run;
 
        /* standardizes each beta estimate to mean=0,var=1 */
 
            proc standard data=mod&N out=stand mean=0 std=1;
              var &&I&N;
            run;
 
        /* assigns percentile rank to each sample estimate */
 
            proc rank data=stand out=rank p;
              var &&I&N;
              ranks &&R&N;
            run;
 
        /* if estimate is normally distributed and has been
           standardized to mean=0,var=1, then the probit
           transformation of its rank should equal the estimate.
           For example, the probit of .5 is 0.  That means the
           median estimate (standardized) should also equal zero
           if the estimate is normally distributed.  */
 
 
            data rank;
              set rank;
              array rnk &&R&N;
              array norm &&NORM&N;
              do over rnk;
                   rnk=rnk/100;
                   if rnk=1 then rnk=.9999;
                   norm=probit(rnk);
              end;
            run;
            proc plot data=rank;
          %LET M=1;
          %LET VAR=%SCAN(&&I&N,&M);
          %LET NOR=%SCAN(&&NORM&N,&M);
          %DO %UNTIL (&VAR=%STR() );
            plot &VAR*&NOR='+' &NOR*&NOR='*' / overlay;
               %LET M=%EVAL(&M+1);
               %LET VAR=%SCAN(&&I&N,&M);
               %LET NOR=%SCAN(&&NORM&N,&M);
          %END;
       title1 "normal probability plot" ;
       title2 "model&N: &&MODEL&N";
       title3;
            run;
 
     %END;
%END;
       title1; run;
%EXIT: ;
 
%MEND BOOTREG;

