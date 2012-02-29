 /*-------------------------------------------------------------------*
  *    Name: dummy.sas                                                *
  *  Title:  Macro to create dummy variables                          *
        Doc: http://www.datavis.ca/sasmac/dummy.html               
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 03 Feb 98 11:32                                          *
  * Revised: 06 Sep 2011 09:19:38                                     *
  * Version: 1.3-1                                                    *
  * 1.1  Added FULLRANK parameter                                     *
  * 1.2  Now handles multiple VARiables                               *
  * 1.3  Added PREFIX=VARNAME and PREFIX=BLANK for more flexibility   *
  *      in naming dummy variables, as well as FORMAT= to provide use *
  *      of user formats to name the dummies according to formatted   *
  *      values (from Kathy Sykora, <kathy.sykora@ices.on.ca>         *
  *      Added test for missing data (V7+ only)                       *
  *      Added test for dummy names>8 chars (V7- only)                *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:

 Given a character or discrete numerical variable, the DUMMY macro creates
 dummy (0/1) variables to represent the levels of the original variable
 in a regression model.  If the original variable has c levels, (c-1)
 new variables are produced (or c variables, if FULLRANK=0).

 When the original variable is missing, all dummy variables will be 
 missing (V7+ only).

 
=Usage:

 The DUMMY macro takes the following named parameters.  The VAR= parameter
 must be specified. The arguments
 may be listed within parentheses in any order, separated by commas.
 For example:
 
   %dummy(var=sex group, prefix=);

 
==Parameters:

* DATA=    The name of the input dataset.  If not specified, the most
           recently created dataset is used.

* OUT=     The name of the output dataset.  If not specified, the new
           variables are appended to the input dataset.

* VAR=     The name(s) of the input variable(s) to be dummy coded.  Must be
           specified.  The variable(s) can be character or numeric.

* PREFIX=  Prefix(s) used to create the names of dummy variables.  The
           default is 'D_'.  You can give one or more strings, in an order
           corresponding to the VAR= variables.  You can include the
           keyword PREFIX=VARNAME, which will use the name of the
           corresponding variable followed by an underscore, or
           PREFIX=BLANK, which will make the prefix a null string,
           similar to specifying a null string in the macro argument.

* NAME=    If NAME=VAL, the dummy variables are named by appending the
           value of the VAR= variable to the prefix.  Otherwise, the
           dummy variables are named by appending numbers, 1, 2, ...
           to the prefix.  The resulting name must be 8 characters or
           less and contain only characters legal in variable names.

* BASE=    Indicates the level of the baseline category, which is given
           values of 0 on all the dummy variables.  You can give one or 
           more strings, in an order corresponding to the VAR= variables.

           BASE=_FIRST_ or BASE=LOW specifies that the lowest value 
           of the VAR= variable is the baseline group; 
           BASE=_LAST_ or BASE=HIGH specifies the highest value of
           the variable.  Otherwise, you can specify BASE=value to
           make a different value the baseline group.  For a character
           variable, you must enclose the value in quotes, e.g.,
           BASE='M'.

* FORMAT=  User formats may be used for two purposes:  (a) to name
           the dummy variables, and (b) to create dummy variables
           which are indicators for ranges of the input variable.  
           Variables using the format option must be listed first in the
           VAR= list.

* FULLRANK=  0/1, where 1 indicates that the indicator for the BASE category
           is eliminated.  

==Example:

 With the input data set,
 
   data test;
      input y group $ sex $ @@;
   cards;
   10  A M 12  A F 13  A M  18  B M 19  B M 16  C F 21  C M 19  C F 
   ;

 The macro statement:

    %dummy ( data = test, var = group) ;

 produces two new variables, D_A and D_B.  Group C is the baseline
 category (corresponding to BASE=_LAST_)

       OBS     Y    GROUP    SEX    D_A    D_B
         1     10      A       M      1      0
         2     12      A       F      1      0
         3     13      A       M      1      0
         4     18      B       M      0      1
         5     19      B       M      0      1
         6     16      C       F      0      0
         7     21      C       M      0      0
         8     19      C       F      0      0

 With the same data set,

   proc format;
      value $sex 'M'='Male' 'F'='Female';
   %dummy (data=test, var =sex group, format=$sex, prefix=blank varname) ;

 produces a dummy for SEX named FEMALE, and two dummies for GROUP:

           OBS     Y    GROUP    SEX    FEMALE    GROUP_A    GROUP_B

            1     10      A       M        0         1          0
            2     12      A       F        1         1          0
            3     13      A       M        0         1          0
            4     18      B       M        0         0          1
            5     19      B       M        0         0          1
            6     16      C       F        1         0          0
            7     21      C       M        0         0          0
            8     19      C       F        1         0          0

=*/
 
%macro dummy( 
   data=_last_ ,    /* name of input dataset                  */
   out=&data,       /* name of output dataset                 */
   var= ,           /* variable(s) to be dummied              */
   base=_last_,     /* base category                          */
   prefix = D_,     /* prefix for dummy variable names        */
   format =,        /* format used to categorize variable     */
   name  = VAL,     /* VAL: variable names are D_value        */
   fullrank=1       /* Eliminate dummy for baseline category? */
   );

	%let abort = 0;
   %if (%length(&var) = 0) %then %do;
       %put ERROR: DUMMY: VAR= must be specified;
		 %let abort=1;
       %goto done;
       %end;

%let base = %upcase(&base);
%let name = %upcase(&name);

%if %upcase(&data) = _LAST_ %then %let data = &syslast;
%if %upcase(&data) = _NULL_ %then %do;
	%put ERROR: There is no default input data set (_LAST_ is _NULL_);
	%let abort=1;
	%goto DONE;
	%end;
	
options nonotes;

%*-- Initialize output data set;
%if &out ^= &data %then %do;
	data &out;
		set &data;
	%end;
	
%let prefix = %upcase(&prefix);

%*-- j indexes variables, vari is the current variable name;
%local j vari;
%let j=1;
%*-- Find the current variable name;
%let vari= %scan(&var,    &j, %str( ));

%******************************************************************;
%*-- Loop over variables; 
%******************************************************************;
%do %while(&vari ^= );

	%*-- Find the current prefix for dummies;
	%let pre = %scan(&prefix, &j, %str( ));
	%if &pre = VARNAME | &pre = %then %let pre=&vari._;
	%*-- Keyword BLANK for prefix indicates no prefix;
	%if &pre=BLANK %then %let pre=;

	%*-- Find the current base for dummies;
	%let baseval = %scan(&base, &j, %str( ));
	%if &baseval = %then %let baseval=_LAST_;

	%*-- Find the current format for dummies;
	%let fmt = %scan(&format, &j, %str( ));

*-- determine values of variable to be dummied;
proc summary data = &out nway ;
     class &vari ;
     %if %length(&fmt) gt 0 %then %do;
	  		%*-- Make sure format name includes a '.';
        %if "%substr(&fmt, %length(&fmt))" ne "." 
		  		%then %let fmt = &fmt..;
        format &vari &fmt;
     %end;
     output out = _cats_ ( keep = &vari ) ;
	%if &syserr > 4 %then %let abort=1; 
	%if &abort %then %goto DONE;

	%if &fullrank %then %do;
	*-- Eliminate the base category;
	data _cats_;
		set _cats_ end=_eof_;
		%if &baseval = _FIRST_ | &baseval = LOW 
			%then %str( if _n_ = 1 then delete;);
		%else %if &baseval = _LAST_ | &baseval = HIGH
			%then %str( if _eof_ then delete;);
		%else %str(if &vari = &baseval then delete;);
	run;
	%end;

data _null_ ;
 set _cats_ nobs = numvals ;

 if _n_ = 1 then do;
	%*-- If there are no non-baseline values - abort macro; 
	call symput('abort',trim( left( put( (numvals=0), best. ) ) ) ) ;
	%*-- Place number of dummies into macro variable num;
	call symput( 'num', trim( left( put( numvals, best. ) ) ) ) ;
	end;

	%*-- Number the values, place in macro variables c##; 
	%if %length(&fmt) gt 0 %then %do;
		call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
								trim(left(put(&vari,&fmt)) ) );
	%end;
	%else %do;
	call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
								trim ( left ( &vari ) ) ) ;
	%end;
run ;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%******************************************************************;
%* Create list of dummy variables for the j-th input variable;
%******************************************************************;

%if "&name" = "VAL" %then %do ;
	%*-- Names by variable value;
	%let vl&j =; 
	%do k=1 %to &num;
		%if %sysevalf(&sysver  < 7 & %length(&pre&&c&k) > 8) %then %do;
			%put ERROR: Cannot generate names longer than 8 characters;
			%let abort=1;
			%goto DONE;
			%end;
		%let vl&j = &&vl&j  &pre&&c&k;
		%end; ;
%*put vl&j = &&&vl&j;

data &out;
	set &out ;
	
	array __d ( &num ) %do k=1 %to &num ;	&pre&&c&k
							%end ; ;
	%put DUMMY: Creating dummy variables &pre&&c1 .. &pre&&c&num for &vari;
	%end ;

%else %do ;
	%*-- Numeric suffix names;
	%let vl&j =; 
	%do k=1 %to &num; 
		%if %sysevalf(&sysver  < 7 & %length(&pre.&k) > 8) %then %do;
			%put ERROR: Cannot generate names longer than 8 characters;
			%let abort=1;
			%goto endloop;
			%end;
		%let vl&j = &&vl&j  &pre.&k;
		%end; ;
%*put vl&j = &&&vl&j;
run;
	
%******************************************************************;
%* Assign values to the dummy variables for the j-th input variable;
%******************************************************************;
data &out  ( rename = ( %do k=1 %to &num ;
						d&k = &pre.&k
						%end ; ) ) ;
	set &out ;
	%put DUMMY: Creating dummy variables &pre.1 .. &pre.&num;
	array __d ( &num ) d1-d&num ;
	%end ;

	%*---------------------------------------------------------;
   %*   Handle missing values (for V7+ only);
	%*-- (to do this for V6.12 requires separate processing for
	      character and numeric variables);
	%*---------------------------------------------------------;
	%if %sysevalf(&sysver  >= 7) %then %do;
     if missing(&vari) then do;
	  	 do j=1 to &num;
        __d(j)=.;
		  end;
		return;
     end;
	%end;

	%*---------------------------------------------------------;
   %*   Assign values to dummy variables;
	%*---------------------------------------------------------;
	drop j;
	do j = 1 to &num ; /* initialize to 0 */
		__d(j) = 0 ;
	end ;


     %if %length(&fmt) eq 0 %then %do;
     %*-- Case 1:  No format;
        if &vari = "&c1" then __d ( 1 ) = 1 ;  /* create dummies */
        %do i = 2 %to &num ;       
           else if &vari="&&c&i" then __d ( &i ) = 1 ;
        %end;
     %end;

     %else %do;
     %*-- Case 2:  with format;
        if put(&vari,&fmt) = "&c1" then __d ( 1 ) = 1 ;
        %do i = 2 %to &num ;       
           else if put(&vari,&fmt)="&&c&i" then __d ( &i ) = 1;
        %end;
     %end;
run ;

%*-- Find the next variable;

%let j=%eval(&j+1);
%let vari = %scan(&var, &j, %str( ));

%*put End of loop(&i): vari = &vari  pre=&pre;
%endloop:
%end;  /* %do %while */

%done:
%if &abort %then %put ERROR: The DUMMY macro ended abnormally.;
options notes;
%mend dummy ;
