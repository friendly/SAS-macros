 /*
      Name:  compmix.sas
		Title: Fit multiple covariance structures with PROC MIXED

      COMPMIX: SAS macro to fit multiple covariance structures 
      with PROC MIXED and perform likelihood ratio tests with 
      the first model specified.  Requires Release 6.10,
      Release 6.09 third maintenance, or Release 6.08 fifth
      maintenance of SAS.  Written by Russ Wolfinger 
      (sasrdw@unx.sas.com), June 1995

      The DATA= option specifies the SAS data set you are
      analyzing.

      The MDATA= options specifies a SAS data set that you
      must create prior to calling COMPMIX.  The MDATA= data
      set must have variables LABEL and MODEL, and optionally 
      CLASS, RANDOM, and REPEATED.  These variables define the 
      mixed models you want to fit and contain strings conforming 
      to PROC MIXED syntax.  The LABEL variable must be no
      longer than 8 characters.

      The first model is considered a baseline model, for the
		purposes of calculating differences in chi-square.

      PROCOPT= allows you to specify options for the PROC MIXED 
      statement used for all of the models.  You should use 
      PROCOPT=METHOD=ML to use maximum likelihood (instead of the 
      default REML) whenever your MODEL statements are 
      changing.

      You can specify the following after the OPTIONS= 
      argument:  PRINTALL prints all of the PROC MIXED runs,
      and PLOT draws plots of AIC and BIC.
---*/ 

%macro compmix(
	data=,
	mdata=,
	procopt=,
	options=,
	out=_stats_,
	font=
	);

 /*---initialization---*/
 %if %bquote(&data)= %then %let data=&syslast;
 %if %bquote(&mdata)= %then %let missing = MDATA=;
 %else %let missing =;
 %if %length(&missing) %then %do;
    %put ERROR: The COMPMIX &missing argument is not present.;
    %goto finish;
 %end;
 %let options = %qupcase(&options);

%if %length(&font)=0 %then %do;
	%if %index(%upcase(&sysdevic),PS)>0
		%then %let font=hwpsl009;
		%else %let font=swiss;
	%end;


 %global _print_;
 %if (%index(&options,PRINTALL)) %then %do;
    %let _print_ = on;
 %end;
 %else %do;
    %let _print_ = off;
    options nonotes nodate nonumber;
 %end;

 /*---number of observations in model data set---*/
 data _null_;
    set &mdata nobs=count;
    call symput('nmod',left(put(count,8.)));
 run;
 %let devdf = 0;

 /*---loop through models---*/ 
 %do i = 1 %to &nmod;

    /*---retrieve model specs---*/
    data _null_;
		length model random repeated $200;  *-- ensure these are character vars;
       set &mdata;
       if _n_ = &i;
       call symput('label',label);
       call symput('class',class);
       call symput('model',trim(model));
       call symput('random',trim(random));
       call symput('repeated',trim(repeated));
    run;

	 %if %length(&model) %then %do;
    	%put Fitting model &i: %bquote(&label) model;
		%end;
	 %else %goto next;

    /*---create symbol statements for plots---*/
    %if (%index(&options,PLOT)) %then %do;
       symbol&i color=black f=&font
          value="&label" h=1.5 r=1;
    %end;

    /*---get rid of old fitting data set---*/
    proc datasets lib=work nolist;
       delete _fit;
    run;

    /*---fit the model---*/
    proc mixed data=&data &procopt info;
       %if (%length(&class)) %then %do;
          class &class;
       %end;
       model &model;
       %if (%length(&random)) %then %do;
          random &random;
       %end;
       %if (%length(&repeated)) %then %do;
          repeated &repeated;
       %end;
       make 'dimensions' out=_dim;
       make 'fitting' out=_fit;
    run;

    %let there = no;
    data _null_; 
       set _fit; 
       call symput('there','yes'); 
    run;
    %if ("&there" = "no") %then %do;
       %put %str(   )PROC MIXED did not converge.;
    %end;
    %else %do;
       data _dim;
          set _dim;
          if (substr(descr,1,3)='Cov') then call symput('dim',value);
       run;
   
       data _ic;
          set _fit;
          retain m label parms aic bic;
          m = &i;
          length label$ 8;
          label = symget('label');
          parms = &dim;
          if (substr(descr,1,3)='Aka') then aic=value;
          else if (substr(descr,1,3)='Sch') then bic=value;
          else if (substr(descr,1,3)='-2 ') then do;
             ll2 = value;
             %if (&devdf = 0) %then %do;
                chisq = .;
                df = .;
                p = .;
             %end;
             %else %do;
                chisq = abs(ll2 - &devll2);
                df = abs(&devdf - parms);
                if (df > 0) then p = 1 - probchi(chisq,df);
                else p = .;
             %end;
             output;
          end;
          else delete;
          drop descr value;
       run;

       %if (&i=1) %then %do;
          data &out;
             set _ic;
             call symput('devdf',parms);
             call symput('devll2',ll2);
				 label parms='No. of parameters';
          run;
       %end;
       %else %do;
          proc append base=&out data=_ic;
          run;
       %end;
    %end;
%next:;
 %end;

 proc print data=&out noobs;
    format aic bic ll2 6.1 chisq 5.1 p 6.4;
 run;

 %if (%index(&options,PLOT)) %then %do;
*    goptions hsize=6in vsize=8.5in htext=1.25 ftext=swissl;
	axis2 offset=(4);
	axis1 label=(a=90);
    proc gplot data=&out;
       plot aic*parms=m / nolegend hminor=0 vminor=1 haxis=axis2; 
       plot bic*parms=m / nolegend hminor=0 vminor=1 haxis=axis2; 
       plot aic*bic=m / nolegend hminor=0 vminor=1; 
%end;

	proc plot data=&out;
		plot aic * parms $ label
		     bic * parms $ label
			  aic * bic   $ label;

 /*---finish up---*/
 %finish:

 options notes date number;
 %let _print_ = on;

%mend;
