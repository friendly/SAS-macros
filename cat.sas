/*
 Catenate a set of variable values, with a specified separator.

 Useful for situations were you need to combine a number of factor
 variables into a single variable, e.g., to plot means for the
 combinations of two or more factors.
 
 V9 has the function catx(sep, str1, str2, ...), that does something
 similar, and other functions, cat(str1, ...), cats(str1, ...),
 catt(str1, ...)  that don't provide an explicit separator argument.
*/
%macro cat(
	data=_last_,   /* input dataset */
	var=,          /* list of variables to be concatenated */
	catvar=,       /* output result variable */
	sep=:,         /* separator string */
	length=,       /* length of result variable */
	out=&data      /* name of output dataset */
	);

%local catstr i v;
data &out;
	set &data;
	%if %length(&length) %then %do;
		length &catvar $ &length;
		%end;
	%let catstr = trim(left(%scan(&var,1,%str( ))));
	%let i=2;
	%let v=%scan(&var, &i, %str( ));
	%do %while (%length(&v) > 0 );
		%let catstr = &catstr || "&sep" || trim(left(%scan(&var,&i,%str( ))));
		%let i = %eval(&i+1);
		%let v=%scan(&var, &i, %str( ));
		%end;
	%*put CAT: &catvar = &catstr;
	&catvar = &catstr;
	
%mend;

