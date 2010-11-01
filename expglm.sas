 /*--------------------------------------------------------------*
  *    Name: expglm.sas                                          *
  *   Title: Expand a GLM model specification from bar notation  *
        Doc: http://www.datavis.ca/sasmac/expglm.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 03 Dec 1998 16:16                                   *
  * Revised: 12 Feb 2006 13:14:15                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The expglm macro expands shorthand GLM/GENMOD model notation using
 the | and @ symbols to a blank-separated list of main effect and
 interaction terms.  Useful for macros which need to deal with the
 terms separately, or for procs which do not allow | notation.

=Usage:

 The macro argument should be a string of legal SAS variable names
 separated by blanks for separate model terms, optionally including
 the | and @ symbols.
 The macro returns an expanded string from the macro argument, for example
 
	%let model = %expglm(A|B|C);

==Examples:

   %expglm(A|B) --> A B A*B;
   %expglm(A|B A|C) --> A B A*B C A*C;
   %expglm(A|B|C@2) --> A B A*B C A*C B*C;

==Limitations:

 Doesn't check syntax, or respond gracefully to ill-formed models.

 No blanks are allowed inside any term, certainly not inside ()

 Doesn't handle nested effects properly, but this is beyond what can be
 done using pure macro processing.

	A|B(A) should --> A B(A)
	A(B)|C(D) should --> A C A*C(B*D)

 =*/
 
%macro expglm(model);
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
		%*put term=#&term#;
		%let term = %debar(&term);                   %* expand term;
		%*put term=#&term# (debard);
		%if &j > 0 & %length(&ord) > 0 %then 
			%let term = %maxord(&term,&ord);        %* remove higher order;
		%*put term after maxord: &term;
		%let result = &result &term;               %* append terms;
		%let i = %eval(&i+1);
		%let term = %scan(&model, &i, %str( ));    %* get next term;
		%end;
	%*-- Remove duplicates, and return;
	%*put Result before unique: &result;
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
/*
		&m1 %debar(&m2) %debar(&m1*&m2)
*/
		&m1 %debar(&m2) %joinpair(&m1,%debar(&m2),sep=%str(*)) 

		%end;
%mend;

	%*-- Remove duplicate (blank-delimited) words from a string;
%macro unique(string, dlm=%str( ));
	%local i term result;
	%let result = %scan(&string, 1, %quote(&dlm));
	%let i=2;	
	%let term = %scan(&string, &i, %quote(&dlm));
	%do %while (%length(&term) > 0);
		%if %index( %quote(%upcase(%quote(&dlm)&result%quote(&dlm))),
		            %quote(%upcase(%quote(&dlm)&term%quote(&dlm))) ) = 0
			%then %let result = &result &term;
		%let i = %eval(&i+1);
		%let term = %scan(&string, &i, %quote(&dlm));
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
Join each word in A to each word in B, separated by a specified
character string
*/

%macro joinpair(A, B, sep=, dlm=%str( ));
	%local i j wA wB result;
	%let result=;

	%let i=1;
	%let wA = %scan(&A,1,%quote(&dlm));
	%do %while(%quote(&wA)^= );
		%let j=1;
		%let wB = %scan(&B,&j,%quote(&dlm));
		%do %while(%quote(&wB)^= );
			%let result = &result &wA.&sep.&wB;
			%let j = %eval(&j+1);
			%let wB = %scan(&B,&j,%quote(&dlm));
			%end;
		%let i = %eval(&i+1);
		%let wA = %scan(&A,&i,%quote(&dlm));
		%end;
	&result
%mend;
