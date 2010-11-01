%macro dsq(
	data=_last_,
	var=,
	by=,
	where=,
	out=
	);

%if %length(&by) %then %do;
	proc sort data=&data out=_sorted_;
		by &by;
	%let data=_sorted_;
	%end;

proc princomp data=&data std out=prin noprint;
	var &var;
	%if %length(&where) %then %do;
		where &where;
		%end;
	%if %length(&by) %then %do;
		by &by;
		%end;

data &out;
	set prin;
	_dsq_ = uss(of prin:);
	df = n(of prin:);
	_p_ = 1-probchi(_dsq_, df);
	drop df prin: ;
%mend;

	
