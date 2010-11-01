%macro mlmsumry(
	data=_last_,
	class=,
	model=,            /* contents of MODEL statement           */
	contrasts=,        /* contents of CONTRAST statements       */
	repeated=,         /* contents of REPEATED statement        */
	tests=multi, 
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

%if %length(&model) %then %do;
ods listing close;
proc glm data=&data;
	%if %length(&model) %then %do;
		class &class;
		%end;
	model &model;
	&contrasts
	&repeated
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

proc print;
%end;
	
%mend;
