/*
I rewrite the program using the same logic as recursive call. The macro
contains two pieces. One does scaning word. The other does expanding word.
More importantly, there is no duplicates. Hope this one helps.
*/

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

/*
%let string=a|b|c|d;
%let model = %debar(&string);
%put string=&string;
%put model = &model;
*/
%macro testexp(model);
	%put "&model" -->; %put %str(   ) %debar(&model);
%mend;

	%put; %put ---- Simple cases -----;
	%testexp(AA|BB);
	%testexp(A|B|C);
	%testexp(A |B |C |D );
