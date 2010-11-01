%macro cross(list);
%*-----------------------------------------------------;
%* Return string of v1 * v2 * v3 ... from list of words;
%*-----------------------------------------------------;
   %local i word;
   %let i=1;
   %let word = %scan(&list,&i,%str( ));
   %do %while(&word^= );
		%if &i=1
			%then %let result = &word;
			%else %let result = &result * &word;
       %let i = %eval(&i+1);
       %let word = %scan(&list,&i,%str( ));
   %end;
   &result
%mend cross;

%put cross: %cross(v1);
%put cross: %cross(v1 v2);
%put cross: %cross(v1 v2 v3);
