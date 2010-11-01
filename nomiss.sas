%nomiss(data=_last_;
	var=_numeric_,
	cvar=_character_,
	where=,
	out=&data
	);

data &out;
	set &data;
	%if %length(&where) %then %do;
		where (&where);
		%end;

	array var {*} &var;
	do i=1 to dim(var);
		if var{i} = . then delete;
		end;

	%if %length(&cvar) %then %do;
	array cvar {*} &cvar;
	do i=1 to dim(var);
		if var{i} = . then delete;
		end;
		%end;
%mend;

