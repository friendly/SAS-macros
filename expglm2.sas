/*
I rewrite the program using the same logic as recursive call. The macro
contains two pieces. One does scaning word. The other does expanding word.
More importantly, there is no duplicates. Hope this one helps.
*/

%macro expglm(model);
	%local at ord;
	%let at = %index(&model,@);
	%let ord=0;
	%if &at > 0 %then %do;
		%let ord   = %substr(&model,%eval(&at+1));   %* find max order;
		%let model = %substr(&model,1,%eval(&at-1)); %* remove it;
		%if %datatyp(&ord) = CHAR %then %let ord=0;  %* ignore unless numeric;
	  	%end;
	%let result = %debar(&model);
	%if &ord > 0 %then 
		%let result = %maxord(&result,&ord);        %* remove higher order;
	&result
%mend;

%macro debar(term,sep=%str(|));
  %local i m1 m2;
  %let i=1;
  %let m1=%qscan(&term,&i,%str(&sep));
  %let i=%eval(&i+1);
  %let m2=%qscan(&term,&i,%str(&sep));
  %do %until(&m2 eq %str( ));
    %let m1=%exppair(&m1, &m2);
    %let i=%eval(&i+1);
    %let m2=%qscan(&term,&i,%str(&sep));
  %end;
  %cmpres(&m1)
%mend;

%macro exppair(t1, t2);
   %local i word;
   &t1
   %if &t2 ne %str( ) %then %do;
   &t2
     %let i=1;
     %let word=%qscan(&t1,&i,%str( ));

     %do %until(&word eq %str( ));
        &word*&t2
        %let i=%eval(&i+1);
        %let word=%qscan(&t1,&i,%str( ));
     %end;
   %end;
%mend;

	%*-- Remove terms higher than a given order;
%macro maxord(model, order);
	%local i j term word result;
	%let result=;
	%let i=1;	
	%let term = %qscan(&model, &i, %str( ));
	%do %while (%length(&term) > 0);
		%if %index( &term,*) = 0
			%then %let result = &result &term;
			%else %do;
				%let j=1;
				%let word = %qscan(&term,&j,*);
				%do %while(%length(&word) > 0);
					%let j=%eval(&j+1);
					%let word = %qscan(&term,&j,*);
					%end;
				%if %eval(&j-1) <= &order
					%then %let result = &result &term;
				%end;
		%let i = %eval(&i+1);
		%let term = %qscan(&model, &i, %str( ));
		%end;
	&result
%mend;

%macro testexp(model);
	%put "&model" -->; %put %str(   ) %expglm(&model);
%mend;

	%put; %put ---- Simple cases -----;
	%testexp(AA|BB);
	%testexp(A|B|C);
	%testexp(A |B |C |D );
	%testexp(Abe|B|C|D);
	%testexp(A|B|C|D@2);
