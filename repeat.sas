%*---  Repeat a string of words, until there are at least
    a given number of words;

%macro repeat(parm, len, dlm=%str( ));
	%do k=1 %to &len;
		%if %length(%scan(&parm, &k, &dlm))=0
			%then %let parm = &parm &parm;
		%end;
	&parm
%mend;

/*
%put repeat(A B C, 8) is %repeat(A B C, 8);
*/

%*---  Repeat a string of words, until there are exactly n words;

%macro repeatw(string, n, dlm=%str( ));

	%local k index;
	%let result=;
	%let nw = %words(&string);
	%do k=1 %to &n;
		%let index =%sysevalf(1+%sysfunc(mod(%eval(&k-1), &nw)));
		%* put index = &index;
		%let word = %scan(&string, &index, &dlm);
		%let result = &result &word;
		%end;
	&result
%mend;

%put repeatw(A B C, 8) is |%repeatw(A B C, 8)|;
