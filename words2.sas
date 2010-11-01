%macro words(string,root=,dlm=%str( ));
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %qscan(%quote(&string),&count,%quote(&dlm));
   %do %while(&word^= );
	%put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(%quote(&string),&count,%quote(&dlm));
   %end;
   %eval(&count-1)
%mend words;

%* put words = %words(A B C,root=W);
%* put W1= &W1 W2= &W2 W3= &W3 ;
%put words = %words(AA BB CC);

%let args = AA,BB, CC;
%let wds = %words(%quote(&args), dlm=%str( ,));
%put In args=|&args|, wds = &wds;
%let wds = %words(&args, dlm=%str( ,));
%put In args=|&args|, wds = &wds;

