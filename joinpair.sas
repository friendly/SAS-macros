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

/*
%put 'joinpair(A, C D, sep=*)' - > %joinpair(A, C D, sep=%str(*));
options symbolgen;
%put 'joinpair(A B, C|D, sep=*)' - > %joinpair(A B, C|D, sep=%str(*));
options nosymbolgen;
*/
