 /*------------------------------------------------------------*
  *    Name: reverse                                           *
  *   Title: Reverse the words in a string                     *
  *------------------------------------------------------------*/

%macro reverse(string);
   %local count word result;
   %let count=1;
   %let word = %qscan(&string,&count,%str( ));
	%let result=&word;
   %do %while(&word^= );
       %let count = %eval(&count+1);
       %let word = %qscan(&string,&count,%str( ));
		 %let result = &word &result;
   %end;
   %unquote(&result)
%mend;

