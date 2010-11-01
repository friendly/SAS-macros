 /*--------------------------------------------------------------*
  *    Name: expand.sas                                          *
  *   Title: Expand a GLM model specification from bar notation  *
  *     Doc: internal                                            *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 03 Dec 1998 16:16                                   *
  * Revised: 04 Dec 1998 11:24:46                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The expand macro expands shorthand GLM/GENMOD model notation using
 the | and @ symbols to a blank-separated list of main effect and
 interaction terms.  Useful for macros which need to deal with the
 terms separately, or for procs which do not allow | notation.

=Usage:

 The macro returns an expanded string from the macro argument, for example
 
	%let model = %expand(A|B|C);

==Examples:

   %expand(A|B) --> A B A*B;
   %expand(A|B A|C) --> A B A*B C A*C;
   %expand(A|B|C@2) --> A B A*B C A*C B*C;

==Limitations:

  Doesn't check syntax, or respond gracefully to ill-formed models

  No blanks are allowed inside any term, certainly not inside ()
 
  Doesn't handle nested effects properly

		- A|B(A) should --> A B(A)
	 	- A(B)|C(D) should --> A C A*C(B*D)

 =*/
 
%macro expand(model);
	%local i j result term ord;
	%let result=;
	%let i=1;	                                  %* index blank sep terms;
	%let term = %scan(&model, &i, %str( ));       %* get first term;
	%do %while (%length(&term) > 0);
		%let j = %index(&term,@);                  %* is there a max? ;
		%let ord=20;
		%if &j > 0 %then %do;
			%let ord  = %substr(&term,%eval(&j+1));   %* find max order;
			%let term = %substr(&term,1,%eval(&j-1)); %* remove it;
			%end;
		%let term = %debar(&term);                   %* expand term;
		%if &j > 0 & %length(&ord) > 0 %then 
			%let term = %maxord(&term,&ord);        %* remove higher order;
		%let result = &result &term;               %* append terms;
		%let i = %eval(&i+1);
		%let term = %scan(&model, &i, %str( ));    %* get next term;
		%end;
	%*-- Remove duplicates, and return;
	%unique(&result)
%mend;

	%*-- Expand bar notation for one term;
%macro debar(model);
	%local i m1 m2;
	%let i=%index(&model,|);
	%if &i = 0 %then %do;
		&model
		%end;
	%else %do;
		%let m1 = %substr(&model,1,%eval(&i-1));    %*-- first term;
		%let m2 = %substr(&model,%eval(&i+1));      %*-- rest;
		&m1 %debar(&m2) %debar(&m1*&m2)
		%end;
%mend;

	%*-- Remove duplicate (blank-delimited) words from a string;
%macro unique(string);
	%local i term result;
	%let result = %scan(&string, 1, %str( ));
	%let i=2;	
	%let term = %scan(&string, &i, %str( ));
	%do %while (%length(&term) > 0);
		%if %index( %quote(%upcase(%str( )&result%str( ))),
		            %quote(%upcase(%str( )&term%str( ))) ) = 0
			%then %let result = &result &term;
		%let i = %eval(&i+1);
		%let term = %scan(&string, &i, %str( ));
		%end;
	&result
%mend;

	%*-- Remove terms higher than a given order;
%macro maxord(model, order);
	%local i j term word result;
	%let result=;
	%let i=1;	
	%let term = %scan(&model, &i, %str( ));
	%do %while (%length(&term) > 0);
		%if %index( &term,*) = 0
			%then %let result = &result &term;
			%else %do;
				%let j=1;
				%let word = %scan(&term,&j,*);
				%do %while(%length(&word) > 0);
					%let j=%eval(&j+1);
					%let word = %scan(&term,&j,*);
					%end;
				%if %eval(&j-1) <= &order
					%then %let result = &result &term;
				%end;
		%let i = %eval(&i+1);
		%let term = %scan(&model, &i, %str( ));
		%end;
	&result
%mend;

/*
Macros for testing
*/

%macro testexp(model);
	%put "&model" -->; %put %str(   ) %expand(&model);
%mend;


%macro testit;

	%put; %put ---- Simple cases -----;
	%testexp(AA|BB);
	%testexp(A|B|C);
	%testexp(A|B|C|D);
	
	%put; %put ---- Complex cases -----;
	%testexp(A|B  B|C  C|D);
	%testexp(A|B|C|D@2);
	%testexp(A|B|C@2 A|GRP|LOC@2);
	
	%put; %put ---- Buggy cases -----;
	%testexp(A|B(A));              %*  wrong;
	%testexp(A|B(A)|C(A*B));

	%put; %put ---- Error cases -----;
	%*testexp(A|B(A)|C(A B));      %*  bombs;
%mend;
	*testexp(sim(1)|dev(1) poly(1)|poly(1));	

%* uncomment to run above tests;
*testit;



