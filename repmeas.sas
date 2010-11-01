%macro repmeas(
	data=_last_,
	class=,
	model=,            /* contents of MODEL statement        */
	contrasts=,
	repeated=,         /* contents of REPEATED statement        */
	tests=univ multi, 
	mtests=Pillai,     /* multivariate tests to include         */
	outm=MultTests,    /* output dataset for multivariate tests */
	outu=UnivTests,    /* output dataset for uniivariate tests */
	print=
	);


%if %upcase(&data)=_LAST_ %then %let data=&syslast;
%local me; %let me=&sysmacroname;

ods output MultStat                   = _within_
           BetweenSubjects.ModelANOVA = _between_
		   ModelANOVA                 = _anova_;
ods output Contrasts                  = _contrasts_;

%if %length(&model) and %length(&repeated) %then %do;
ods listing close;
proc glm data=&data;
	class &class;
	model &model;
	&contrasts
	repeated &repeated;
run;quit;
ods listing;
	%end;
%else %do;
run;quit;
	%end;

%local cleanup; %let cleanup=;
%let havewithin =    %sysfunc(exist(_within_));
%let havebetween =   %sysfunc(exist(_between_));
%let havecontrasts = %sysfunc(exist(_contrasts_));
%let haveanova =     %sysfunc(exist(_anova_));

%let tests=%upcase(&tests);

%if &havecontrasts %then %do;
	proc print data=_contrasts_;
	%let cleanup = &cleanup _contraasts_ _betweenc_;
	*title 'Contrasts';
	%*-- get the between-S contrasts for multivariate report;
	data _betweenc_;
		set _contrasts_;
		where Dependent='BetweenSubjects';
		label Source='Source';
		drop ProbFGG ProbFHF;
	
	proc print data=_betweenc_;
	run;
	%*-- get the within-S contrasts for univariate report;
	%* (for now, ignoree the interaction contrasts of within x between);
	data _withinc_;
		set _contrasts_;
		where Dependent='WithinSubject';
	%end;

%if %index(&tests, MULT) %then %do;
title 'Setting up between';
*proc print data=_between_;

	%if &havebetween %then %do;
	%let cleanup = &cleanup _between_ _error_;
	data _between_ _error_;
		set _between_(in=inb)
		%if &havecontrasts %then _betweenc_(in=inc)
			;;	
		drop Dependent HypothesisType SS MS;
		if Source = 'Error' then do;
			output _error_;
			end;
		else do;
			if inb then Type='Between ';
				else Type='BetweenC';
			NumDF = df;
			output _between_;
			end;
		if _n_=1 then do;
			put HypothesisType=;
			call symput('htype', left(put(HypothesisType, 1.)));
			end;
	*proc print data=_between_;
	*proc print data=_error_;

	data _between_;
		retain Source FValue NumDF DenDF ProbF;
		if _n_=1 then set _error_(rename=(df=DenDF) );
		set _between_;
		drop df;
	%end;  /* &havebetween */
	%else %do;
		%put &me: BetweenSubjects tests have not been output.;
	%end;
	
*proc print data=_between_;

/*
proc print data=_within_;
title 'Setting up _within_';
run;
*/
	
title "Multivariate tests for dataset &data";
data &outm;
	retain Type Source Statistic Value;
   set _between_(in=inb )
       _within_ (rename=(Hypothesis = Source) in=inw)
	   end=eof
	;
	*label FValue='F Value';
	Statistic = scan(Statistic,1," -'");
	Source=translate(Source,'*', '_');
	if inb then do;
		*Type='Between';
		Statistic = 'F';
		Value=FValue;
		end;
	else Type='Within';
	drop HypothesisType PValue Error;
	if inb | index(upcase("&mtests"), upcase(trim(Statistic)));
	format Value 8.3;
run;

title "Type &htype multivariate tests for dataset &data";
proc print data=&outm label;
	id Type Source Statistic;
run;
/*
options ls=90;
proc report data=MultTests headline headskip;
   column Type Source Statistic DF SS MS FValue ProbF;
   define Type     / group   'Type' order=internal ;
   define Source   / group;
   define Statistic /  'Test/Statistic' width=9;
   define df       / analysis  format=3.0    width=3;
   define SS       / analysis  format=8.3;
   define MS       / analysis  format=8.3;
   define FValue   / analysis  format=8.2;
   define ProbF    / analysis 'Prob/> F' ;
   
   break after Type / skip;
   break after Source / skip;
   run;
*/   
%end;

%if %index(&tests, UNI) %then %do;
*-- Univariate tests;

title 'Univariate tests';
proc print data=_anova_;
	id Dependent;
	title2 _anova_ dataset;
proc print data=_withinc_;
	title2 _withinc_ dataset;
	id Dependent;

data &outu;
	length Type $8 Source $32;
	set _anova_;
	by Dependent notsorted;
	if first.Dependent then do;
		types+1;
		end;
	drop types;
	drop HypothesisType Dependent;
	select(Dependent);
		when('BetweenSubjects') do;
			order=10*types;
			Type='Between';
			if Source='Error' then order+2;
				else order+1;
			end;
		when('WithinSubject') do;
			order=10*types;
			Type='Within';
			if Source=:'Error' then order+2;
				else order+1;
			end;
		otherwise do;
			order=10*types;
			Type='WithinC';
			if Source=:'Error' then order+2;
				else order+1;
			if Source='Mean' then Source=Dependent;
				else if Source='Error' then Source = 'Error(' || trim(Dependent) ||')';
				else Source = trim(Dependent)||'*' || Source;
			Dependent = 'WithinSubject';
			end;
		end;
	format SS MS D8.3;

proc sort;
	by order ;
title "Univariate tests for dataset &data";;
proc print data=&outu;
	id Type;
	*by Type;
%end;

%done:
%if %length(&cleanup) %then %do;
*put Cleaning up...  &cleanup;
proc datasets nofs nolist nowarn;
   delete  &cleanup;
	run; quit;
	%end;

%exit:

%mend;
