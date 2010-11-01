%*-- Macro to make a valid do-list from a macro var;

%macro dolist(string,varname,type);
   %local count word;
	%if &type=1 and %upcase(%qscan(&string,2,%str( )))=TO 
		%then %do;
			%let &varname = %quote(&string);
		%end;
		
		%else %do;
			%let count=1;
			%let word=%qscan(&string,&count,%str( ));
			%if &type=1 %then %let &varname=&word;
			%else             %let &varname=%str(%')&word%str(%');
			%do %while (&word ne);
				%let count=%eval(&count+1);
				%let word=%qscan(&string,&count,%str( ));
				%if (&word ne) %then %do;
				%if &type=1 %then %let &varname=&&&varname, &word;
				%else             %let &varname=&&&varname, %str(%')&word%str(%');
					%end;
			%end;
	 	%end;
		
    %put &varname = &&&varname;
%mend;
 
