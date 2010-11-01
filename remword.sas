%macro remword(string,remove,dlm=%str( ));
%*--------------------------------------------------;
%* Remove word contained in Remove from String
%*--------------------------------------------------;
   %local count word result;
   %let count=1;
   %let word = %scan(&string,&count,&dlm);
   %let result=;
   %do %while(%quote(&word)^= );
       %if(%index(
	    	%upcase(&dlm.&remove.&dlm),
			%upcase(&dlm.&word.&dlm) )=0)
	    	%then %let result = &result &word;
	%*put REMWORD: word=&word result=&result;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,&dlm);
   %end;
   &result
%mend;

%let words = Anno AAA b mean Cc;
%put words: &words;
%let rest = %remword(&words, mean);
%let rest = %remword(&rest, anno);
%put rest: &rest;
