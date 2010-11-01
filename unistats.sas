/*
Newsgroups: comp.soft-sys.sas
Date: Wed, 19 Nov 1997 03:21:00 -0500
Reply-To: "Stuerzl, Heinr.,DE,Diagnostics" <STUERZL@MSMBWMA.HOECHST.COM>
Sender: "SAS(r) Discussion" <SAS-L%VTVM1.BITNET@VTBIT.CC.VT.EDU>
From: "Stuerzl, Heinr.,DE,Diagnostics" <STUERZL@MSMBWMA.HOECHST.COM>
Subject: Tip: Macro UNISTATS
Lines: 216

In September and October 1997 Ian Whitlock and VITRAI Jozsef develoed in
SAS-L the macro UNISTATS (Subject was: mean AND median), which
calculates univariate statistics with PROC UNIVARIATE and provides a
dataset with one observation per variable, which allows an output
similar to PROC MEANS.

To use by-processing I added a further macro parameter BY. With it the
input dataset will be sorted if nessesary.
With the PRINT macro parameter (Default=y) you get an output with PROC
PRINT, which uses labels for all possible statistics of PROC UNIVARIATE.

So UNISTATS can be used in most cases instead of PROC UNIVARIATE.
It requires SAS 6.11 TS040 or higher (because of %sysfunc).
It needs the variables after VARS= to be named explicitely (no
abreviations allowed).
If you omit an output dataset WORK.STAT1 is used as default.

 Heinrich Stuerzl
 Data Management & Biostatistics
 Behring Diagnostics GmbH, Marburg, Germany
 eMail:stuerzl@msmbwma.hoechst.com

MF: 
- Added stats=_all_
  vars=x1-x12 or vars=_numeric_
  fixed bug: variable names not sorted

_________
Examples:

  data temp;
    do i=1 to 1000;
      gruppe=mod(i,2);
      x=normal(-1);
      y=normal(-1);
      z=normal(-1);
      if mod(i, 100)=0 then x=.;
      output;
    end;
  run;

  %unistats (data=work.temp, vars=i x y z, by=gruppe, out=work.stat,
             stats=n min max median mean normal probn);

* if you omit STATS or let the parameter empty, you get N MEDIAN MEAN
STD CV MIN MAX as default;
  %unistats (data=temp, vars=i x y z, by=gruppe, stats=);

* to suppress output use PRINT=n;
  %unistats (data=temp, vars=i x y z, out=work.stat, print=n);

* all supported statistics, all numeric variables;
  %unistats (data=temp, vars=_numeric_, stats=_all_);


____________________________
*/
  %macro unistats (data=&syslast, vars=, out=work.stat1, by=, print=Y,
                   stats=N MEDIAN MEAN STD CV MIN MAX) /
                   des="Univariate Statistics";

     %local i j stat hvar stdstats nstats lib mem
            N NOBS NMISS SUM MEDIAN MEAN VAR STD CV MIN MAX Q1 Q3 P1 P5
P10 P90 P95 P99
            NORMAL PROBN T PROBT MSIGN PROBM SIGNRANK PROBS
            STDMEAN USS CSS SKEWNESS KURTOSIS SUMWGT RANGE QRANGE MODE ;

  %* Unterstutzte Statistiken (werden gelabelt);
     %let stdstats = N NOBS NMISS SUM MEDIAN MEAN VAR STD CV MIN MAX Q1
Q3 P1 P5 P10 P90 P95 P99
                     NORMAL PROBN T PROBT MSIGN PROBM SIGNRANK PROBS
                     STDMEAN USS CSS SKEWNESS KURTOSIS SUMWGT RANGE
QRANGE MODE ;

  %* Label Definitions;
      %let n       = N ;
      %let nobs    = N total ;
      %let nmiss   = N missing ;
      %let sum     = Sum ;
      %let median  = Median ;
      %let mean    = Mean ;
      %let var     = Variance ;
      %let std     = Standard deviation ;
      %let cv      = %nrstr(CV % ) ;
      %let min     = Min ;
      %let max     = Max ;
      %let q1      = Lower quartile ;
      %let q3      = Upper quartile ;
      %let p1      = %nrstr(Percentile 1% ) ;
      %let p5      = %nrstr(Percentile 5% ) ;
      %let p10     = %nrstr(Percentile 10% ) ;
      %let p90     = %nrstr(Percentile 90% ) ;
      %let p95     = %nrstr(Percentile 95% ) ;
      %let p99     = %nrstr(Percentile 99% ) ;
      %let normal  = Normality ;
      %let probn   = Prob Norm ;
      %let t       = T ;
      %let probt   = Prob >|T| ;
      %let msign   = M(Sign) ;
      %let probm   = Prob >=|M| ;
      %let signrank= Sgn Rank ;
      %let probs   = Prob >=|S| ;
      %let stdmean = StdMean ;
      %let uss     = Uncorr. SSQ ;
      %let css     = Corrected SSQ ;
      %let skewness= Skewness ;
      %let kurtosis= Kurtosis ;
      %let sumwgt  = Sum of weights ;
      %let range   = Range ;
      %let qrange  = Q3-Q1 ;
      %let mode    = Mode ;


      %if &data= %then %let data = &syslast;
      %if %length(&vars)=0 %then %do;
        %put ---> ERROR: No variables;
        %goto ende;
      %end;

	options nonotes;
   %let by = %upcase(&by);
   %let vars = %upcase(&vars);
	*-- Parse variables list if it contains special lists;  /* MF */
	%if %index(%quote(&vars),-) > 0 or "&vars"="_NUMERIC_" %then %do;
		data _null_;
		set &data (obs=1);
				%*  convert shorthand variable list to long form;
			length _vname_ $ 8 _vlist_ $ 200;
			array _xx_ &vars;
			_vname_ = ' ';
			do over _xx_;
				call vname(_xx_,_vname_);
				if _vname_ ne "&by" then
				_vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
			end;
			call symput( 'vars', trim(_vlist_) );
			put 'NOTE: vars= list translated to: vars=' _vlist_;
		RUN;
	%end;

      %if &stats= %then %let stats = N MEDIAN MEAN STD CV MIN MAX ;
      %if %upcase(&stats)=_ALL_ %then %let stats=&stdstats;     /* MF */
		
      %if %qscan ( &out , 2 , . ) = %then
      %do ;
          %let lib = WORK ;
          %let mem = &out ;
      %end ;
      %else %do;
          %let lib = %qscan ( &out , 1 , . ) ;
          %let mem = %qscan ( &out , 2 , . ) ;
      %end ;

      %if %length(&by)>0 %then %do;
        proc sort data=&data;
          by &by;
        run ;
      %end;
      proc univariate data=&data normal noprint ;
         var &vars ;
         output out = __u1

            %let i = 1 ; %* Statistiken;
            %let stat = %qscan ( &stats , &i ) ;
            %do %while ( &stat ^= ) ;

               &stat=

               %let i = %sysfunc ( putn ( &i , z2. ) ) ;
               %let j = 1 ;  %* Variablen;
               %let hvar = %qscan ( &vars , &j ) ;
               %do %while ( &hvar ^= ) ;
                   %let j = %sysfunc ( putn ( &j , z3. ) ) ;
                   v&j._s&i.
                   %let j = %eval ( &j + 1 ) ;
                   %let hvar = %qscan ( &vars , &j ) ;
               %end ;
               %let i = %eval ( &i + 1 ) ;
               %let stat = %qscan ( &stats , &i ) ;
            %end ;
            %let nstats = %eval ( &i - 1 ) ;
              ;
         by &by;
      run ;
      %if &syserr=1 %then %goto ende;


      proc transpose data=__u1 out=__u2 ;
        by &by;
      run;

      proc sort data=__u2;
        by &by _name_ ;
      run ;

      data __u2 ;
         length vname sname $ 8 ;
         set __u2 ;
         vname = resolve ( '%scan(&vars,'  || substr(_name_, 2, 3) ||
')' ) ;
         sname = resolve ( '%scan(&stats,' || substr(_name_, 7)    ||
')' ) ;
      run ;

      proc transpose data = __u2 out = &out ( drop = _name_ ) ;
         by &by vname notsorted ;    /* MF: added notsorted */
         id sname ;
         var col1 ;
      run ;

      proc datasets lib = work nolist ;
        delete __u: ;
        %if %upcase(&lib) = WORK %then %do;
          modify &mem ;
          label vname="Name"
            %do i = 1 %to &nstats ;
              %let stat = %upcase ( %qscan ( &stats , &i ) ) ;
              %if %index ( &stdstats , &stat ) %then  &stat = &&&stat ;
            %end ;
          ;
        %end;
        %else %do; %* Neues PROC DATASETS;
          quit;
          proc datasets lib = &lib nolist ;
            modify &mem ;
            label vname="Name"
              %do i = 1 %to &nstats ;
                %let stat = %upcase ( %qscan ( &stats , &i ) ) ;
                %if %index ( &stdstats , &stat ) %then  &stat = &&&stat
;
              %end ;
            ;
        %end;
        quit;

      options notes;
      %if %substr(%upcase(&print),1,1) = Y %then %do;
        proc print data=&out label uniform;
	      %if %length(&by)>0 %then %do;
          by &by;
			%end;
        run ;
      %end;

    %ende:
  %mend unistats ;
