 /*--------------------------------------------------------------*
  *    Name: expgrid.sas                                         *
  *   Title: Create a data set from all combinations of factors  *
        Doc: http://www.datavis.ca/sasmac/expgrid.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 22 Feb 2002 16:57:53                                *
  * Revised: 07 Dec 2005 11:31:21                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The EXPGRID macro creates a data set from all combinations of the
 variables and their values named in the macro call.  It is useful
 in a variety of contexts, but was written for the purpose of generating
 predicted values over a grid of independent variables in any linear
 or non-linear model.  It is a SAS implementation of the expand.grid() 
 function of R/S.

=Usage:

 The EXPGRID macro is defined with an arbitrary number of arguments.
 Each of these correspond to a specification for a factor and variables in
 the output data set, separated by commas. For example,
 
	%expgrid(n=10, trials=2 4 8, time=10 to 20 by 5, sex=Male Female);

 This generates a data set containing 1 * 3 * 3 * 2 observations, with
 all combinations of the four variables, N TRIALS TIME SEX.  The first
 listed factors vary most rapidly in the output data set.

 In this version, the result always appears as a data set named _GRID_.
 
==Parameters:

 Each supplied argument must be of the form,  VAR = VALUEs, where

* VAR      is a valid SAS variable name

* VALUEs   is either a space-separated list of one or more values, which
           can be character or numeric, or a numeric DO specification,
			  of the form, val1 TO val2 <BY val3>.  The special case of a
			  single value (e.g., N=10 or SEX=Male) generates a constant 
			  variable in the output data set.   Note that this implies
			  that the string ' TO ' is treated specially; you can not have
			  a specification like WHERE=FROM TO BACK FORTH.

=Examples:

 The macro call

	%expgrid(n=10, trials=2 4 8, time=10 to 20 by 5, sex=Male Female);

 generates the data step:

	data _grid_;
		do sex='Male  ', 'Female';
			do time= 10 to 20 by 5;
				do trials=2, 4, 8;
					n = 10;
					output;
					end;
				end;
			end;

 =*/

%macro expgrid/parmbuff;

%local out;
%let out = _grid_;   %*-- Name of output data set;

%*-- Process the variable arguments, each of the form
      VAR = val1 val2 val2 ...,   (delimited by commas)
	or VAR = val1 TO val2 BY val3,
	Create macro variables &&VAR1, &VAR2, ... (Variable names)
	and &VAL1, &VAL2, ... (Variable values)
;
%local narg arg;
%let narg = 1;
%let arg = %scan(&syspbuff, &narg, %str((,)));
%do %while(%quote(&arg) ^= );
	%if %length(&arg) > 0 %then %do;
		%let arg&narg = &arg;
		%let var&narg = %scan(&arg, 1, =);
		%let val&narg = %trim(%scan(&arg, 2, =));
		%*put ARG&narg = &arg  VAR&narg=&&var&narg  VAL&narg=|&&val&narg|;
		%end;
	%let narg=%eval(&narg+1);
	%let arg=%scan(&syspbuff, &narg, %str((,)));
	%end;
%let narg=%eval(&narg-1);
%*put NARG=&narg;

data &out;
	%*-- Generate the DO statements;
	%do i = &narg %to 1 %by -1;
		%if %index(%upcase(&&val&i, %str( TO ))) %then %do;
			do &&var&i = &&val&i;
			%end;
		%else %do;
			%*-- Handle lists;
			%if %verify(&&val&i,%str(0123456789. )) ^= 0 %then %do;
				length &&var&i $8;
				%end;
			do &&var&i = %dolist(&&val&i)
			;
		%end;
	%end;

		output;

	%*-- Generate the END statements;
	%do i = 1 %to &narg;
		end;
		%end;
run;
%mend;

%*-- Translate a simplified do-list to a syntactically correct one, e.g., 
	3 4 5 -> 3,4,5
 or Male Female -> 'Male', 'Female'
;
%macro dolist(arg, split=%str( ), sep=%str(,));
	%local val i j max return;
	%let return=;
	%let max=%count(&arg, split=&split);
   %do i=1 %to &max;
		%let val=%scan(&arg, &i, &split);
		%if %verify(&val,0123456789.) ^= 0 %then %do;
			%let val = "&val";
			%end;
		%if &i=&max %then %let return=&return.&val;
		%else %let return=&return.&val.&sep;
	%end;
%unquote(&return)
%mend;

%*-- Count 'words' in a string, delimited by split;
%macro count(arg, split=%str( ));

%local i;
%let i=0;
%do %while(%length(%nrbquote(%scan(&arg, &i+1, &split))));
	%let i=%eval(&i+1);
%end;
&i
%mend;
