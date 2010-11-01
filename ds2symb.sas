/*
Generate SYMBOL statements from a data set, to allow setting
combinations of symbols, colors, line styles, etc. programatically.
*/
%macro ds2symb(
	data=_last_,
	value=value,       /* name of the VALUE= variable */
	interpol=interpol, /* name of the INTERPOL= variable */
	color=color,       /* name of the COLOR= variable */
	font=font,         /* name of the FONT= variable */
	line=line,         /* name of the LINE= variable */
	ci=ci,             /* name of the CI= variable */
	co=co,             /* name of the CO= variable */
	cv=cv,             /* name of the CV= variable */
	height=height,     /* name of the HEIGHT= variable */
	width=width,       /* name of the WIDTH= variable */
	repeat=repeat      /* name of the REPEAT= variable */
	);

%tempfile(symbol);
*filename symbol '/tmp/symbol.out';

data _null_;
	set &data end=eof;
	file symbol;
	put 'symbol' +0 _n_ @;
	if &value ^=' '  then put 'value=' +0 &value @;
	if &interpol ^=' ' then put 'interpol=' +0 &interpol @;
	if &color ^=' '  then put 'color=' +0 &color @;
	if &font ^=' '   then put 'font=' +0 &font @;
	if &ci ^=' '     then put 'ci=' +0 &ci @;
	if &co ^=' '     then put 'co=' +0 &co @;
	if &cv ^=' '     then put 'cv=' +0 &cv @;

	if &line ^=.     then put 'line=' +0 &line @;
	if &height ^=.   then put 'height=' +0 &height @;
	if &width ^=.    then put 'width=' +0 &width @;
	if &repeat ^=.   then put 'repeat=' +0 &repeat @;
	put ';';

	if eof then do;
		file log;
		put 'NOTE:' _n_ ' SYMBOL statements have been generated';
		end;

	run;
%include symbol;

%* clear the fileref and delete the temp file;
%tempdel(symbol);		

/* An alternative method, using CALL EXECUTE

data _null_;
	set &data end=eof;
*	file symbol;
	length str $200;
	str ='symbol' || trim(left(put(_n_,3.)));
	if &value ^=' '  then str = trim(str) || ' value=' || &value;
	if &interpol ^=' ' then str = trim(str) || ' interpol=' || &interpol;
	if &color ^=' '  then str = trim(str) || ' color=' || &color;
	if &font ^=' '   then str = trim(str) || ' font=' || &font;
	if &ci ^=' '     then str = trim(str) || ' ci=' || &ci;
	if &co ^=' '     then str = trim(str) || ' co=' || &co;
	if &cv ^=' '     then str = trim(str) || ' cv=' || &cv;

	if &line ^=.     then str = trim(str) || ' line=' || trim(left(put(&line,3.0)));
	if &height ^=.   then str = trim(str) || ' height=' || trim(left(put(&height,3.1)));
	if &width ^=.    then str = trim(str) || ' width=' || trim(left(put(&width,3.0)));
	if &repeat ^=.   then str = trim(str) || ' repeat=' || trim(left(put(&repeat,3.0)));
	str = trim(str) || ';';

	call execute(str);
	put str=;
	if eof then do;
		file log;
		put 'NOTE:' _n_ ' SYMBOL statements have been generated';
		end;

	run;
*/

%mend;

data testit;
	height=1;
	interpol='none';
	line=1;
	do color='red ', 'blue';
		do value='plus  ', 'dot', 'square';
			output;
			end;
		end;
run;

options mprint;
%ds2symb(data=testit);
