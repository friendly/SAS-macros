%MACRO BOOTCOR (VARS=,WITH=,WEIGHT=,FREQ=NIDOBS,DATA=&SYSDSN,
      SAMPLE=100,TYPE=P) / STMT ;
 
%IF &DATA=&SYSDSN %THEN %DO;
     %LET DATA1=%SCAN(&DATA,1);
     %LET DATA2=%SCAN(&DATA,2);
     %LET DATA=&DATA1..&DATA2;
%END;
%LET BAD1=0; %LET BAD2=0;
 
   /* the only way the VARS= statement would read in more than
      one var was if the string was enclosed in quotes.  This
      blows up the macro if they weren't.  Of course, the macro
      will blow itself up if I'm not mistaken, so this may be
      extraneous. */
 
%IF ( %INDEX(&VARS,%STR(%')) ^= 1 AND
        %INDEX(&VARS,%STR(%")) ^= 1 ) %THEN %LET BAD1=1;
%IF ( %INDEX(&WITH,%STR(%')) ^= 1 AND
        %INDEX(&WITH,%STR(%")) ^= 1 AND
        &WITH ^= %STR() ) %THEN %LET BAD2=1;
%IF (&BAD1=1 OR &BAD2=1) %THEN %DO;
     %PUT VARS or WITH statement was entered incorrectly ;
     %PUT Expecting one of the following forms: ;
     %PUT %str(      ) VARS= 'variable list'  or  ;
     %PUT %str(      ) WITH= 'variable list' ;
%END;
%IF (&BAD1=1 OR &BAD2=1) %THEN %GOTO EXIT;
 
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
 
    /*  This section parses &VARS and &WITH,
        stripping them of their quotes */
 
%LET M=1;
%LET L=%LENGTH(&VARS);
%LET VARS=%SUBSTR(&VARS,2,&L-2);
%IF ( %INDEX(&WITH,%STR(%')) ^=0 OR
        %INDEX(&WITH,%STR(%")) ^=0 ) %THEN %DO;
     %LET L=%LENGTH(&WITH);
     %LET WITH=%SUBSTR(&WITH,2,&L-2);
%END;
 
     /* this section strips other stuff of quotes in case
        the user didn't follow directions. */
 
%IF ( %INDEX(&FREQ,%STR(%')) ^=0 OR
        %INDEX(&FREQ,%STR(%")) ^=0 ) %THEN %DO;
     %LET L=%LENGTH(&FREQ);
     %LET FREQ=%SUBSTR(&FREQ,2,&L-2);
%END;
%IF ( %INDEX(&WEIGHT,%STR(%')) ^=0 OR
        %INDEX(&WEIGHT,%STR(%")) ^=0 ) %THEN %DO;
     %LET L=%LENGTH(&WEIGHT);
     %LET WEIGHT=%SUBSTR(&WEIGHT,2,&L-2);
%END;
%IF ( %INDEX(&TYPE,%STR(%')) ^=0 OR
        %INDEX(&TYPE,%STR(%")) ^=0 ) %THEN %DO;
     %LET L=%LENGTH(&TYPE);
     %LET TYPE=%SUBSTR(&TYPE,2,&L-2);
%END;
 
      /* runs the correlation for each sample, saves results in
        output dataset */
 
            proc corr data=bootdata out&TYPE=estdata nosimple noprint;
              var &VARS;
              with &WITH;
              freq &FREQ;
              weight &WEIGHT;
              by j;
            run;
 
        /* keeps only the correlation estimates */
 
            data estdata;
              set estdata;
              if _type_='corr';
            run;
            proc sort;
              by _name_;
            run;
 
      /* get means, standard deviations, etc.  I chose UNIVARIATE
         because it was the only procedure I found which gave
         percentiles in the output dataset (P5, P95). */
 
            proc univariate data=estdata noprint;
              var  &VARS;
              by _name_;
              output out=mean mean= &VARS;
              output out=std std= &VARS;
              output out=p5 p5=&VARS;
              output out=p95 p95=&VARS;
            run;
            data bootcor;
              stat='mean'; set mean; output;
              stat='std'; set std; output;
              stat='p5'; set p5; output;
              stat='p95'; set p95; output;
            run;
 
      /* flips stuff around until it's in the right order
         and looks best on printed output. */
 
 
            proc transpose data=bootcor(rename=(_name_=name))
                    out=bootcor(drop=_label_);
              var &VARS;
              id stat;
              by name;
            run;
            proc sort data=bootcor out=bootcor;
              by name _name_;
            run;
            proc transpose data=bootcor out=bootcor name=stat;
              var mean std p5 p95;
              id _name_;
              by name;
            run;
            data bootcor1 bootcor2;
              set bootcor;
              if stat='mean' then output bootcor1;
              else output bootcor2;
            run;
%IF &TYPE=K %THEN %LET TYPE=KENDALL;
%ELSE %IF &TYPE=S %THEN %LET TYPE=SPEARMAN;
%ELSE %LET TYPE=PEARSON;
            proc print data=bootcor1(drop=stat);
       title1 "bootstrap estimate of &TYPE correlation matrix";
       title2 "&SAMPLE samples";
       title3 ;
            run;
            proc print data=bootcor2;
       title1 "standard deviations and 90% confidence intervals";
       title2 "of bootstrap &TYPE correlation coefficients";
       title3 "&SAMPLE samples";
       title4 ;
            run;
       title1 ; run;
%EXIT: ;
 
%MEND BOOTCOR;
