/*****************************************************************/
/*   Name: power.sas                                             */
/*  Title: Calculates power measures from a PROC GLM             */
/*                                                               */
/*  Author: Kristin Latour                                       */
/*                                                               */
/*  Created: 24Apr96                                             */
/*****************************************************************/
 /*=
=Description: 

    Calculates the following power-related measures for retrospective and
    prospective analyses (see the DETAILS section below for definitions):
												
   - Effect size
	- Power for an effect test      
	- Adjusted power and confidence limits
	- Least significant number      
   - Power for least significant number
												
=Usage:

 You must first run PROC GLM to fit the desired model and use the
 OUTSTAT= option to create the input data set for the %POWER macro.

 The macro is invoked with the following statement.                         
 The keyword parameters assigned in any order.
                                                
	%power(DATA=outstat_data_set,
		OUT=output_data_set,
		EFFECT=effect_name,
		CALCS=calculations_to_report,
		SS=type_sums_of_squares_to_use,
		ALPHA=list_of_significance_levels,
		N=list_of_sample_sizes,
		SIGMA=list_of_standards_deviations,
		DELTA=list_of_effect_sizes);
                                                
==Parameters:

* DATA=   is the name of the data set created by the
          OUTSTAT= option in the previous run of PROC GLM.

* OUT=    is the name of a new data set into which %POWER will store
	       all calculations. In the output data set, values of .N indicate
	       statistics that were not calculated and values of .U indicate
	       that the macro was unable to calculate the statistic.

* EFFECT= (REQUIRED) is a list of one or more effects from the MODEL 
          statement in the previous run of PROC GLM, or the keyword _ALL_.
			 Power calculations are performed for thespecified effect(s).

* CALCS=  is one or more of the following, separated by spaces:
        POWER to request that power be computed and displayed
        ADJPOW to request that adjusted power be computed and displayed
        POWCI to request that a confidence interval for adjusted power be
          computed and displayed
        LSN to request that the least significant number be computed
          and displayed.
      
* SS=   type sums of squares to use in the power calculations. Should be 
        one of these values: ss1, ss2, ss3, or ss4.  The default is ss3.  
		  These sumsof squares must have been computed by the previous run
		  of PROC GLM.

* ALPHA=  LIST OF SIGNIFICANCE LEVELS, a list of values separated by
        spaces. The default value of 0.05.

* N=    LIST OF SAMPLE SIZES is a list of values separated by spaces. The
        default value is the observed sample size. Any values you specify are
        used in addition to the default.
        
* SIGMA= LIST_OF_STANDARDS_DEVIATIONS is a list of values separated by spaces.
        The default value is the observed standard deviation. Any values you
        specify are used in addition to the default.
        
* DELTA=  LIST_OF_EFFECT_SIZES is a list of values separated by spaces. The
        default value is the observed effect size. Any values you specify are
        used in addition to the default.
      
 
 All calculations are output to OUT=. Calculations are
 done/reported for CALCS= on the EFFECT= effects from the 
 DATA= data set with the corresponding SS= type sums of squares.
													
 Alpha is assigned a default value of 0.05 when no other value is
 specified. User defined value lists for N, SIGMA, and DELTA will
 have true values from the data prepended to them.
                                                
 Output values of .N indicate statistics that were not calculated;
 values of .U indicate that the macro was unable to calculate the
 statistic.
 =*/
                                             
 
%macro power(
	data=_LAST_,
	out=_POWER_,
	effect=,
	calcs=POWER LSN ADJPOW,
	ss=ss3,
	alpha=0.05,
	n=,
	sigma=,
	delta=,
	verbose=
	);
 
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=V6;
		%end;
	%else %do;
	   options nonotes;
		%end;

  %let calcs=%upcase(&calcs);
  %let ss=%upcase(&ss);
  %let effect=%upcase(&effect);
	options nonotes;

	*-- Kill output dataset if it exists;                    /* MF */
	proc datasets nolist nowarn;
		delete &out / memtype=data;

	%if %bquote(&effect) = _ALL_ %then %do; 
   data _null_;
    set &data(where=(_type_ in ("&ss", 'CONTRAST')))    /* MF */
                end=lastobs;
	length effect $200;
	retain effect ''; 
	effect = trim(effect) || ' ' || upcase(trim(_source_));
	if lastobs then do;
		call symput('effect', trim(effect));
		put 'Note: EFFECT=_ALL_ translated to EFFECT=' effect;	
		end;
	%end;
	run;
	
	%let k=1;
  %if %bquote(&effect) = %bquote() %then %do; 
  	%let abort=1; %goto endit; %end;
  %let eff = %nrbquote(%scan(&effect,&k,%str( )));          /* MF */
  %*put Working on effect &eff;
  %do %until ( &eff= );
	   
   /* get error and hypothesis df and ss from outstat data set */
   data _null_;
    set &data(where=(_type_ in ("&ss",'ERROR','CONTRAST')))    /* MF */
                end=lastobs;
 
    if _n_=1 then do;
      dfr=0;
      norig=0;
    end;
 
    norig+df;
    _source_ = upcase(_source_);                            /* MF */
    select(_source_);
      when ('ERROR') do;
        call symput('dfeorig',left(put(df,8.)));
        if ss gt 0 then
           call symput('sigorig',
              left(put(sqrt(ss/df),14.2)));
        else call symput('sigorig','');
      end;
      when (upcase("&eff")) do;
        dfr+df;
		  found+1;
        call symput('dfh',left(put(df,8.)));
        call symput('ssh',left(put(ss,14.2)));
        call symput('fsamp',left(put(f,14.2)));
      end;
      otherwise do;
        dfr+df;
      end;
    end;
 
    if lastobs then do;
      call symput('dfr',left(put(dfr,8.)));
      call symput('norig',left(put((norig+1),8.)));
      call symput('abort', put(_error_ ne 0, 1.));
      call symput('found', put(found gt 0, 1.));
    end;
  run;

%if &found=0 %then %do;
	%put WARNING: Effect &eff was specified, but was not found in dataset &data;
	%put WARNING: (Variables in effects must be listed in the same order as in the CLASS statement);
	%goto next;
%end;
 
%if &syserr > 4 %then %let abort = 1; %if &abort %then %goto endit;

	/* put original delta into macro variable */
  data _null_;
    delta=sqrt(&ssh/&norig);
    call symput('delorig',left(put(delta,16.4)));
    stop;
  run;
 
	/* prepend values for n sigma delta that occur in data to 
	user-specified values */
  %let alp=&alpha;
  %let num=&norig &n;
  %let sig=&sigorig &sigma;
  %let del=&delorig &delta;
 
%if &verbose ^= %str() %then %do;
  %put n=&num;
  %put sigma=&sig;
  %put delta=&del;
  %put alpha=&alpha;
  %put dfeorig=&dfeorig;
  %put sigorig=&sigorig;
  %put dfr=&dfr;
  %put dfh=&dfh;
  %put ssh=&ssh;
  %put fsamp=&fsamp;
%end; 
 
 
  * create comma delimitted list for user-specified
  values of alpha, n, sigma, delta;
  %let given=alp num sig del;
 
  %do i=1 %to 4;
     %let current=%scan(&given,&i,%str( ));
     %comma(&&&current,&current);
  %end;
 
 
   /* perform calculations */
  data _one_;
   length source $16;
  	source = upcase("&eff");
    do alpha=&alp;
      do number=&num;
         dfe=number-&dfr-1;
         do sigma=&sig;
            do delta=&del;
 
          *-- CALCULATE LAMBDA AND POWER;
            lambda=(number*delta**2)/(sigma**2);
            astar=1-alpha;
            fcrit=finv(astar,&dfh,dfe);
            if lambda>135 then power=1.0;
            else power=
               1-probf(fcrit,&dfh,dfe,lambda);
 
 
          *-- CALCULATE ADJUSTED LAMBDA AND CI FOR ORIGINAL DELTA;
            if delta=&delorig and
               index("&calcs",'ADJPOW')>0 then do;
               lamadj=max(0,(lambda*(&dfeorig-2)/&dfeorig)-&dfh);
               if lamadj>135 then adjpow=1.0;
               else adjpow=1-probf(fcrit,&dfh,dfe,lamadj);
            end;
            else if delta=&delorig then adjpow=.U;
            else do;
               adjpow=.N;
            end;
 
 
          * GET CI ON LAMBDA AND POWER;
            if index("&calcs",'POWCI')>0 and
               adjpow not in (.N,.U) then do;
              %if &fsamp=I %then %do;
                powlow=.U; powup=.U;
              %end;
              %else %do;
               lamlow=&dfh*(max(0,(sqrt(&fsamp)-sqrt(fcrit))))**2;
               if lamlow>135 then powlow=1.0;
               else powlow=1-probf(fcrit,&dfh,dfe,lamlow);

               lamup=&dfh*(sqrt(&fsamp)+sqrt(fcrit))**2;
               if lamup>135 then powup=1.0;
               else powup=1-probf(fcrit,&dfh,dfe,lamup);
              %end;
            end;
            else do;
               powlow=.N;
               powup=.N;
            end;

 
 
          * FIND LEAST SIGNIFICANT N;
            if number=&norig and
               index("&calcs",'LSN')>0 then do;
               niter=&dfr+2;
               lstar=(niter*delta**2)/(&dfh*sigma**2);
 
               do until (diff<0.0000001);
                 niter=niter+1;
                 errn=niter-&dfr-1;
                 lstar=(niter*delta**2)/(&dfh*sigma**2);
                 diff=astar-probf(lstar,&dfh,errn);
               end;
 
               lsn=niter;
               lsndfe=lsn-&dfr-1;
               lamblsn=(lsn*delta**2)/(sigma**2);
               astar=1-alpha;
               fcrit=finv(astar,&dfh,lsndfe);
               if lamblsn>135 then powlsn=1.0;
               else powlsn=1-probf(fcrit,&dfh,lsndfe,lamblsn);
            end;
            else do;
               lsn=.N;
               powlsn=.N;
            end;
 
            output;
 
            end; *delta;
         end;    *sigma;
      end;       *number;
    end;         *alpha;
    label source='Source'
	        alpha='Type I Error Rate'
          number='Total Sample Size'
           sigma='Root Mean Square Error'
           delta='Effect Size'
           power='Power of Test'
          adjpow='Adjusted Power'
          powlow='Confidence Interval: Lower Limit'
           powup='Confidence Interval: Upper Limit'
             lsn='Least Significant Number'
          powlsn='Power when N=LSN';
 
    keep source alpha number sigma delta
       %if %index(&calcs,POWER)>0 %then %str(power);
       %if %index(&calcs,ADJPOW)>0 %then %str(adjpow);
       %if %index(&calcs,POWCI)>0 %then %str(powlow powup);
       %if %index(&calcs,LSN)>0 %then %str(lsn powlsn);;
  run;
 

	proc append base=&out data=_one_;
%next:
		%let k = %eval( &k + 1);
		%let eff = %nrbquote(%scan(&effect,&k,%str( )));
	%end;
		 
  %let ssnum=%substr(&ss,3,1);
* PRINT RESULTS;
  proc print data=&out noobs label;
    id source;
    var alpha number sigma delta
        %if %index(&calcs,POWER)>0 %then %str(power);
        %if %index(&calcs,ADJPOW)>0 %then %str(adjpow);
        %if %index(&calcs,POWCI)>0 %then %str(powlow powup);
        %if %index(&calcs,LSN)>0 %then %str(lsn powlsn);;
    title1 "Power Calculation for effect(s) %upcase(&effect)";
    title2 "Type &ssnum Sums of Squares";
  run;
  title;

%endit:
%if &k = 1 %then %put WARNING: No EFFECT= effect(s) were specified.;
%if &abort %then %put ERROR: The POWER macro ended abnormally.;

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
 
%mend power;
 
* MACRO TO ADD COMMAS AFTER VALUES
  SPECIFIED FOR ALPHA, N, SIGMA, DELTA;

%macro comma(string,varname);
    %*global &varname;
    %local count word;
    %let count=1;
    %let word=%qscan(&string,&count,%str( ));
    %let &varname=&word;
    %do %while (&word ne);
       %let count=%eval(&count+1);
       %let word=%qscan(&string,&count,%str( ));
       %if (&word ne) %then
           %let &varname=&&&varname, &word;
    %end;
    %put &varname = &&&varname;
%mend comma;
 
