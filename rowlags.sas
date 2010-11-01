*title 'Rowlands data,  reduced to lag0, lag1 counts of duration';

*libname kung '~/sasuser/catdata/kung';

%let lib=kung;
%macro rowlags(
	lib=kung,
	data=rowlands,
	var=code,
	by=age,
	weight=,
	where=,
	nlag=1,
	select=,
	outfreq=rowfreq,
	outlag=rowlag,
	freqopt=noprint);

%lags(data=&lib..&data, var=&var,
	by=&by, 
	weight=&weight,
	where=&where,
	nlag=&nlag,
	complete=all, prefix=lag, freqopt=&freqopt,
	outfreq=&outfreq, outlag=&outlag);

data &outfreq;
	set &outfreq;
	zero = lag0 ^= lag1;
	%if %length(&select) %then %do;
		where (&select);
		%end;
%mend;

