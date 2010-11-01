 /*-----------------------------------------------------------------*
  |     Name: combos.sas                                            |
  |    Title: Create some/all combinations of N things, K at a time |
        Doc: http://www.datavis.ca/sasmac/combos.html         
  | ----------------------------------------------------------------|
  |    Procs: summary sort print                                    |
  |  Macdefs: combos                                                |
  | Datasets: _temp_                                                |
  | ----------------------------------------------------------------|
  |   Author: Michael Friendly               <friendly@yorku.ca>    |
  |  Created: 17 Jun 1999 10:47:42                                  |
  |  Revised: 10 Feb 2006 09:11:26                                  |
  * Version: 1.0                                                    *
  *-----------------------------------------------------------------*/

 /*=
=Description:

 The COMBOS macro generates symbolic combinations of N things taken K
 at a time.  The combinations may be specified to include any given
 subset.  The resulting combinations are returned as an output data set,
 and in global macro variables.
  
==Method:

 The macro uses the simple trick of using PROC SUMMARY to give all
 possible combinations, using the THINGS as class variables.
 
=Usage:

 The THINGS and SIZE parameters are positional, and required.  The rest are
 optional keyword parameters, which may be specified in any order.
 

==Parameters:

* THINGS             The N things to combine: a list of blank-separated
                     words, or a variable range such as X1-X10.  These
                     things become variables in the output data set.
                     They need not exist before the macro is called,
                     but they must be valid SAS names.  The length of
                     the longest combination (including SEP characters)
                     must not exceed 200.

* SIZE               Size (K) of each combination, a numeric value.

* INCLUDE=           Items which must be included.  If a number, then that
                     number of the first items in THINGS is included in
                     each combination.   

* OUT=out            Name of the output data set containing the combinations,
                     one observation for each.   

* SEP=               Separator string used within each combination.  With
                     THINGS=A B C D, the default SEP gives combinations
                     like 'A B C', 'A B D', etc. [Default: SEP=%str( )]

* JOIN=              Separator to join all combinations in the RESULT=
                     macro variable.  With THINGS=A B C D, the default JOIN
                     gives RESULT='A B C, A B D'. [Default: JOIN=%str(, )]

* SORT=            Specifies whether the combinations are sorted in the
                     output data set. [Default: SORT=Y]

* PRINT              Whether to print the OUT= data set.[Default: PRINT=Y]

* RESULT=combos      Name of a global macro result variable containing all
                     combinations.  You may not use RESULT=RESULT (or any
                     other local macro variable). 

* NCOMB=nc           Name of macro result variable with the number of
                     combinations generated.  If INCLUDE= (empty), or
                     INCLUDE=0, this is C(N,K).                

=Examples:

    %combos(a b c d e, 2, include=2);

 produces one combination of size 2 which includes A and B:
 
                 OBS    A    B    C    D    E    COMBO    SIZE

                  1     1    1    .    .    .     A B       2

    %combos(a b c d, 3, sep=-);
    
 produces all 4 combinations of 4 things, 3 at a time:

                    OBS    A    B    C    D    COMBO    SIZE

                     1     1    1    1    .    A-B-C      3
                     2     1    1    .    1    A-B-D      3
                     3     1    .    1    1    A-C-D      3
                     4     .    1    1    1    B-C-D      3


 =*/

%macro combos(
   things,         /* the N things to combine                       */
   size,           /* size (K) of each combination                  */
   include=,       /* items which must be included                  */
   out=out,        /* output data set containing combos             */
   sep=%str( ),    /* separator within each combo                   */
   join=%str(, ),  /* separator to join all combos                  */
   sort=Y,         /* specifies whether the combinations are sorted */
   print=Y,        /* whether to print the OUT= data set            */
   result=combos,  /* name of macro result variable with all
                      combinations */
   ncomb=nc        /* name of macro result variable with C(N,K)     */
   );

	%global &result &ncomb;
	%let sort=%substr(%upcase(&sort),1,1);
	%let print=%substr(%upcase(&print),1,1);
	options nonotes;

/*
 construct a dataset which contains just a=1; b=1; c=1; etc
 for each word in things
*/
data _temp_;
	array xxx &things;
	do over xxx;
		xxx = 1;
		end;

%if %index(&things,-) > 0 %then %do;
 data _null_;
 set _temp_;
        %*  convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &things;
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
	  put "NOTE: The THINGS=&THINGS list translates to: THINGS=" _vlist_;
     call symput( 'things', trim(_vlist_) );
 run;
%end;

/*
 * use proc summary to find all combinations
 */
proc summary data=_temp_;
	class &things;
	output out=_out_;

/*
 * If &include is a number, replace it with the first few things
 */
%if %length(&include) %then %do;
	%if %verify(&include,0123456789)=0 %then %do;
		%if &include=0 %then %do;
			%let include=;
			%end;
		%else %do;	
			%let _ninc_ = &include;
			%let include=;
			%do i=1 %to &_ninc_;
				%let include = &include %scan(&things,&i);
				%end;
			%put NOTE: Translating INCLUDE=&_ninc_ to INCLUDE=&include;
			%end;
		%end;
	%end;

/*
 * find those of the right size, combine words into combo
 */
data &out;
	set _out_;
	drop _freq_ _type_ _vname_ _k_ _ok_;
	length _vname_ $8 combo $200;
	size = n(of &things);
	if size ^= &size then return;
	_ok_ = 1;
	array _xx_ &things;
	%if %length(&include) %then %do;
		array _in_ &include;
		%end;
	k = 0;
	do over _xx_;
		call vname(_xx_, _vname_);
		if _xx_ ^= . then do;
			_k_+1;
			if _k_=1
				then combo = trim(_vname_);
				else combo = trim(combo)|| "&sep" || trim(_vname_);
			end;
		end;
	%if %length(&include) %then %do;
		do over _in_;
			if _in_ = . then _ok_=0;
			end;
		%end;
	if _ok_ then output;

%if &sort=Y %then %do;
	proc sort data=&out;
		by combo;
%end;
%if &print=Y %then %do;
	proc print;
%end;

data _null_;
	set &out end=eof;
	retain result;
	length result $200;
	if _n_ = 1
		then result = trim(combo);
		else result = trim(result) || "&join" || trim(combo);
	if eof then do;
		call symput("&ncomb",  left(put(_n_,3.)));
		call symput("&result", trim(result));
		if _n_=0 then put "WARNING:  No combinations were created from &things, size &size, including &include" ;
		end;
run;

%done:
options notes;
%mend;
