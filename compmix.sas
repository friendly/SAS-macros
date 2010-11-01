/************************************************************************

                              COMPMIX macro
                                      

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   PURPOSE:
     Fit multiple models with PROC MIXED and compare them using ideas from
     Burnham, K.P. and Anderson, D.R. (1998).

   AUTHOR:
     Russ Wolfinger (sasrdw@unx.sas.com), August 1999.
                                                                      
   REQUIRES:
     Requires Base SAS and SAS/STAT software, Version 8 or later.

   USAGE:
     Before calling the COMPMIX macro, you must first define the macro in
     your current SAS session. You can do this either by copying this file
     into the SAS program editor and submitting it, or (as in the EXAMPLE
     below) by using a %INCLUDE statement containing the path and filename
     of this file on your system. 

     Once the macro is defined, call the macro using the desired options.
     See the section below for an example. 

     The macro has the following parameters.  

     DATA=       Specifies the SAS data set you are analyzing.

     MODELS=     Specifies the number of models you want to compare.  Prior
                 to calling COMPMIX, you must define macro variables MODEL1
                 through MODELn, where n is the number you specify.  These
                 macro variables contain PROC MIXED statements defining each
                 the models (see the Example).
 
     METHOD=     Specifies the likelihood method COMPMIX uses to compare
                 models.  The default is ML (maximum likelihood).  If you use
                 METHOD=REML, make sure all of your MODEL specifications are
                 identical.
 
     PROCOPT=    Specifies options to include in the PROC MIXED statements
                 used for all the models.

     IC=         Specifies the information criterion to use.  Valid values
                 are AICC (the default), AIC, HQIC, BIC, and CAIC.
 
     OPTIONS=    Specifies additional options. You can specify the following 
                 after the OPTIONS= argument:  
 
                 LRT        prints likelihood ratio tests of all models
                            with the first one.
 
                 PRINTALL   prints all of the PROC MIXED runs.
 
                 PLOT       plots the IC weights.
    
   PRINTED OUTPUT:
     The EXAMPLE below produces the following output as well as a high
     resolution plot.

                                  AICC Values

                                                                          Cum
Obs    Model    Parms       AICC      Diff           Odds     Weight     Weight

 1       1         4       423.1     0.0000          1.00    0.64893    0.64893
 2       3         6       425.4     2.3539          3.24    0.20002    0.84895
 3       2         8       426.8     3.7789          6.62    0.09809    0.94704
 4       4         8       428.1     5.0115         12.25    0.05296    1.00000
 5       5        20       451.0    27.9097    1149524.57    0.00000    1.00000

   EXAMPLE:
      data pr;
         input person gender$ y1-y4;
         y=y1; age=8;  output;
         y=y2; age=10; output;
         y=y3; age=12; output;
         y=y4; age=14; output;
         drop y1-y4;
         datalines; 
          1   F   21.0    20.0    21.5    23.0
          2   F   21.0    21.5    24.0    25.5
          3   F   20.5    24.0    24.5    26.0
          4   F   23.5    24.5    25.0    26.5
          5   F   21.5    23.0    22.5    23.5
          6   F   20.0    21.0    21.0    22.5
          7   F   21.5    22.5    23.0    25.0
          8   F   23.0    23.0    23.5    24.0
          9   F   20.0    21.0    22.0    21.5
         10   F   16.5    19.0    19.0    19.5
         11   F   24.5    25.0    28.0    28.0
         12   M   26.0    25.0    29.0    31.0
         13   M   21.5    22.5    23.0    26.5
         14   M   23.0    22.5    24.0    27.5
         15   M   25.5    27.5    26.5    27.0
         16   M   20.0    23.5    22.5    26.0
         17   M   24.5    25.5    27.0    28.5
         18   M   22.0    22.0    24.5    26.5
         19   M   24.0    21.5    24.5    25.5
         20   M   23.0    20.5    31.0    26.0
         21   M   27.5    28.0    31.0    31.5
         22   M   23.0    23.0    23.5    25.0
         23   M   21.5    23.5    24.0    28.0
         24   M   17.0    24.5    26.0    29.5
         25   M   22.5    25.5    25.5    26.0
         26   M   23.0    24.5    26.0    30.0
         27   M   22.0    21.5    23.5    25.0
      run;

      %let model1 = %str(
         class gender person;
         model y = gender|age;
         repeated / sub=person type=cs group=gender;
      );

      %let model2 = %str(
         class gender person;
         model y = gender|age;
         repeated / sub=person type=toep group=gender;
      );

      %let model3 = %str(
         class gender person;
         model y = gender|age;
         repeated / sub=person type=arma(1,1) group=gender;
      );

      %let model4 = %str(
         class gender person;
         model y = gender|age;
         random int age / type=un sub=person group=gender;
         repeated / type=vc sub=person group=gender;
      );

      %let model5 = %str(
         class gender person;
         model y = gender|age;
         repeated / sub=person type=un group=gender;
      );

      %include 'compmix8.sas' / nosource;

      ** Compare covariance models using reml **;
      %compmix(data=pr,
         models=5,
         method=reml,
         options=plot
      )
      run;

   REFERENCES:
     Burnham, K.P. and Anderson, D.R. (1998), Model Selection and Inference,
       A Practical Information-Theoretic Approach, New York: Springer.
 
************************************************************************/

%macro compmix(data=,
    models=,
    labels=,
    method=ml,
    procopt=,
    ic=AICC,
    options=);

 /*---initialization---*/
 %if %bquote(&data)= %then %let data=&syslast;
 %if %bquote(&models)= %then %let missing = MODELS=;
 %else %let missing =;
 %if %length(&missing) %then %do;
    %put ERROR: The COMPMIX &missing argument is not present.;
    %goto finish;
 %end;

 %let data = %qupcase(&data);
 %let method = %qupcase(&method);
 %let ic = %qupcase(&ic);
 %let options = %qupcase(&options);
 %let devdf = 0;
 %let minic = 1000000;
 %let solutionf = 0;

 /*---turn off printing---*/
 %if (not %index(&options,PRINTALL)) %then %do;
    ods exclude all;
    options nonotes nodate nonumber;
 %end;

 /*---print header---*/
 %put;
 %put %str(    ) The COMPMIX Macro;
 %put;
 %put Input Data Set        : &data;
 %put Number of Models      : &models;
 %put Estimation Method     : &method;
 %put Information Criterion : &ic;
 %put;

 /*---loop through models---*/ 
 %do i = 1 %to &models;

    %put Fitting model &i;

    %let label=%scan(&labels, &i, %str( ));
    %if &label= %then %let label=&i;
    
    /*---create symbol statements for plots---*/
    %if (%index(&options,PLOT)) %then %do;
       symbol&i color=blue font=swissl 
          value="&label" h=1.5 r=1;
    %end;

    /*---get rid of old fitting data set---*/
    proc datasets lib=work nolist;
       delete _ic;
    run;

    /*---fit the model---*/
    proc mixed data=&data %if (&method=ML) %then %do; method=ml %end;
       ic &procopt;
       &&model&i
       ods output infocrit=_ic;
       %if (%index(%qupcase(&&model&i),SOLUTION)) %then %do;
          ods output solutionf=_sf&i;
          %let solutionf = 1;
       %end;
    run;

    /*---check for convergence---*/
    %let there = no;
    data _null_; 
       set _ic; 
       call symput('there','yes'); 
    run;
    %if ("&there" = "no") %then %do;
       %put %str(   )WARNING: PROC MIXED did not converge for  
          model &i.;
    %end;

    /*---store information---*/
    %else %do;
       data _ic;
          set _ic;
          Model = &i;
          label = scan("&labels", &i, ' ');
          if label=' ' then label="&i";
          if (&ic < &minic) then call symput('minic',&ic);
          /*---compute likelihood ratio tests---*/
          n2ll = neg2loglike;
          %if (%index(&options,LRT)) %then %do;
             %if (&devdf = 0) %then %do;
                chisq = .;
                df = .;
                p = .;
             %end;
             %else %do;
                chisq = abs(n2ll - &devn2ll);
                df = abs(&devdf - parms);
                if (df > 0) then p = 1 - probchi(chisq,df);
                else p = .;
             %end;
          %end;
          keep model label parms &ic n2ll;
       run;

       %if (&i=1) %then %do;
          data _icall;
             set _ic;
             call symput('devdf',parms);
             call symput('devn2ll',n2ll);
          run;
       %end;
       %else %do;
          proc append base=_icall data=_ic;
          run;
       %end;
    %end;
 %end;

 /*---reset printing---*/
 ods select all;

 /*---compute &ic weights---*/
 data _ic;
    set _icall end=last;
    retain sum 0;
    Diff = &ic - &minic;
    if (Diff < 1e-6) then Diff = 0;
    else if (Diff > 100) then Diff = 100;
    Odds = exp(0.5*Diff);
    InvOdds = 1/Odds;
    sum = sum + InvOdds;
    if (last) then call symput('sum',sum);
 run;
 data _ic;
    set _ic;
    Weight = InvOdds / &sum;
 run;

 /*---compute model-averaged solution for fixed effects---*/
 %if (&solutionf = 1) %then %do;
    /*---save number of fixed-effects parameters and alpha value---*/
    data _null_;
       set _sf1 nobs=count end=last;
       if (alpha ne .) then call symput('alpha',alpha);
       if (last) then do;
          call symput('np',left(put(count,8.)));
       end;
    run;
    /*---compute model-averaged estimate---*/
    %do i = 1 %to &models;
       proc transpose data=_sf&i out=_sft;
          var estimate;
       run;
       %if (&i = 1) %then %do;
          data _sfta;
             set _sft;
          run;
       %end;
       %else %do;
          proc append data=_sft base=_sfta;
          run;
       %end;
    %end;
    data _icw;
       set _ic;
       keep Weight;
    run;
    data _sfta;
       merge _sfta _icw;
    run;
    proc means data=_sfta mean noprint;
       var col1-col&np;
       weight Weight;
       output out=_sftam;
    run;
    proc transpose data=_sftam out=_sfae;
       var col1-col&np;
       where _stat_ = "MEAN";
       id _stat_;
    run;
    /*---compute model-averaged standard errors---*/
    %do i = 1 %to &models;
       data _sfv;
          merge _sf&i _sfa0;
          sea = sqrt(stderr**2 + (estimate-MEAN)**2);
       run;   
       proc transpose data=_sfv out=_sft;
          var sea;
       run;
       %if (&i = 1) %then %do;
          data _sfta;
             set _sft;
          run;
       %end;
       %else %do;
          proc append data=_sft base=_sfta;
          run;
       %end;
    %end;
    data _sfta;
       merge _sfta _icw;
    run;
    proc means data=_sfta mean noprint;
       var col1-col&np;
       weight Weight;
       output out=_sftam;
    run;
    proc transpose data=_sftam out=_sfas;
       var col1-col&np;
       where _stat_ = "MEAN";
       id _stat_;
    run;
    /*---compute model-averaged dfs---*/
    %do i = 1 %to &models;
       proc transpose data=_sf&i out=_sft;
          var df;
       run;
       %if (&i = 1) %then %do;
          data _sfta;
             set _sft;
          run;
       %end;
       %else %do;
          proc append data=_sft base=_sfta;
          run;
       %end;
    %end;
    data _sfta;
       merge _sfta _icw;
    run;
    proc means data=_sfta mean noprint;
       var col1-col&np;
       weight Weight;
       output out=_sftam;
    run;
    proc transpose data=_sftam out=_sfad;
       var col1-col&np;
       where _stat_ = "MEAN";
       id _stat_;
    run;

    /*---put them all together---*/
    data _sfah;
       set _sf1;
       drop estimate stderr df tvalue probt alpha lower upper;
    run;
    data _sfae;
       set _sfae;
       Estimate = MEAN;
       keep Estimate;
    run;
    data _sfas;
       set _sfas;
       StdErr = MEAN;
       keep StdErr;
    run;
    data _sfad;
       set _sfad;
       DF = MEAN;
       keep DF;
    run;
    data _sfa;
       merge _sfah _sfae _sfas _sfad;
       if (StdErr = .) or (StdErr = 0) then do;
          tValue = .;
          Probt = .;
          Alpha = .;
          Lower = .;
          Upper = .;
       end;
       else do;
          tValue = Estimate / StdErr;
          Probt = 2*(1 - probt(abs(tvalue),df));
          Alpha = &alpha;
          tcrit = -tinv(&alpha / 2,df);
          Lower = estimate - tcrit*stderr;
          Upper = estimate + tcrit*stderr;
       end;
       drop tcrit;
    run;
 %end;

 /*---print ic weights---*/
 proc sort data=_ic;
    by descending Weight;
 run;
 data _ic;
    set _ic;
    retain CumWeight 0;
    CumWeight = CumWeight + Weight;
 run;
 title "&ic Values";
 proc print data=_ic;
    var Model Label Parms &ic Diff Odds Weight CumWeight;
 run;

 /*---print model-averaged solution for fixed effects---*/
 %if (%index(&options,SOLUTION)) %then %do;
    title "&ic Model-Averaged Solution for Fixed Effects";
    proc print data=_sfa noobs;
    run;
 %end;
 
 /*---print likelihood ratio tests---*/
 %if (%index(&options,LRT)) %then %do;
    title 'Likelihood Ratio Tests with Model 1';
    proc print data=_icall noobs;
       var m label parms n2ll chisq df p;
       format n2ll 6.1 chisq 5.1 p 6.4;
    run;
 %end;

 /*---construct plots---*/
 %if (%index(&options,PLOT)) %then %do;
    goptions hsize=6in vsize=8.5in htext=1.25 ftext=swissl;
    title "&ic Model Weights";
    proc gplot data=_ic;
       plot Weight*Parms=Model / nolegend hminor=0 vminor=1;
    run;
    quit; 
 %end;

 /*---finish up---*/
 %finish:

 options notes date number;
 title;

%mend;
