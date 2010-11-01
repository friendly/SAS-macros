%macro vexpand(data,var);
%*-------------------------------------------------------------------;
%*  VEXPAND expands a variables list of the forms:                   ;
%*     X1-X10, VARA--VARB, _NUMERIC_, _CHARACTER_                    ;
%*  and returns the list of individual variable names.               ;
%*                                                                   ;
%*  Usage:                                                           ;
%*    %let var = %vexpand(dataset,&var)                              ;
%*-------------------------------------------------------------------;
%let varlist = &var;
%put varlist = &varlist;
 %if %index(&varlist,-) >0 or 
 	"%upcase(&varlist)"="_ALL_" or 
 	"%upcase(&varlist)"="_NUMERIC_" or 
	"%upcase(&varlist)"="_CHARACTER_"  %then
 %do;
 data _null_;
    set &data (obs=1);
       %* convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &varlist;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
     call symput( 'VARLIST', trim(_vlist_) );
  run;
  %end;
  %* return new the var list;
  %let &var = &varlist;
  %put &var = &varlist;
%mend vexpand;

data test;
	array xx{10} x1-x10;
	do i=1 to 10;
		xx[i] = i;
		end;
	a = 'a'; b='b';
	output;
	run;
	
%macro testit;
options mprint ;*symbolgen;
%let var=x1 x2 x3 x4 x5;
%vexpand(test, &var);
%put VAR=== &var;
*put var = %vexpand(test, _all_);
*put var = %vexpand(test, _numeric_);
%let var = x1-x10;
%vexpand(test, &var);
%put VAR=== &var;
%mend;

%testit;