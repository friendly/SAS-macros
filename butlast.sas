 /*------------------------------------------------------------*
  *    Name: butlast                                           *
  *   Title: Return all but the last word in a string          *
  *------------------------------------------------------------*/

%macro butlast(string, dlm=%str( ));
   %local count word result;
   %let count=1;
   %let word = %qscan(&string,&count,&dlm);
   %let result=;
   %do %while(&word^= );
       %let word = %qscan(&string,&count,&dlm);
	   %if %length(%qscan(&string,%eval(&count+1),&dlm))
	    %then %let result = &result &word ;
       %let count = %eval(&count+1);
   %end;
   %unquote(&result)
%mend;

/*
%put butlast(aaa bbb c ddd eee) is |%butlast(aaa bbb c ddd eee)|;
%put butlast(aaa) is |%butlast(aaa)|;
*/
