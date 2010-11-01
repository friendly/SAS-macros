 /*--------------------------------------------------------------*
  *    Name: vexpand.sas                                         *
  *   Title: Expand abbreviated variable lists                   *
        Doc: http://www.datavis.ca/sasmac/vexpand.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 04 Mar 1999 16:12:00                                *
  * Revised: 27 Oct 2010 10:58:20                                *
  * Version: 1.0-1                                               *
  *  - Support 32-character names
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The VEXPAND macro expands a variables list of the forms: 
 X1-X10, VARA--VARB, _NUMERIC_, _CHARACTER_  and returns the 
 list of individual variable names.  This is useful for procedures
 and macros that can only handle a blank-separated list of
 variable names.

=Usage:

 The VEXPAND macro is called with positional parameters.
 For example 

    %let var = %vexpand(dataset, &var);

 This macro can only be used at DATA/PROC step boundaries.

==Parameters:

* DATA      The name of the input data set

* VAR       List of variables to be expanded
 
 =*/

%macro vexpand(data, var);
%*-------------------------------------------------------------------;
%*  VEXPAND expands a variables list of the forms:                   ;
%*     X1-X10, VARA--VARB, _NUMERIC_, _CHARACTER_                    ;
%*  and returns the list of individual variable names.               ;
%*                                                                   ;
%*  Usage:                                                           ;
%*    %let var = %vexpand(dataset,&var)                              ;
%*-------------------------------------------------------------------;
 %if %index(&var,-) >0 or 
 	"%upcase(&var)"="_ALL_" or 
 	"%upcase(&var)"="_NUMERIC_" or 
	"%upcase(&var)"="_CHARACTER_"  %then
 %do;
 data _null_;
    set &data (obs=1);
       %* convert shorthand variable list to long form;
     length _vname_ $ 32 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
     call symput( 'VAR', trim(_vlist_) );
  run;
  %end;
  %* return new the var list;
%put VEXPAND: Variable list expanded to &var;
  &var
%mend vexpand;

/*
data test;
	array xx{10} x1-x10;
	do i=1 to 10;
		xx[i] = i;
		end;
	a = 'a'; b='b';
	output;
	run;
	
%macro testit;
options mprint ; *symbolgen;
*put var = %vexpand(test, x1 x2 x3 x10);
%let var=_all_;
%put var = %vexpand(test, &var);
*put var = %vexpand(test, _numeric_);
%let var=x1-x10;
%put var = %vexpand(test, &var);
%mend;
%testit;
*/
