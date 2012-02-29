 /*--------------------------------------------------------------*
  *    Name: missing.sas                                         *
  *   Title: Screen data for missing variables                   *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 11 Mar 2000 11:15:39                                *
  * Revised: 11 Jun 2000 11:26:00                                *
  * Version: 1.2                                                 *
  *  - Use array for list of dropped variables                   *
  *  - added CHECKLAB=;  handles (.A-.Z)                         *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The MISSING macro screens a data set for missing variables
 (for which a large percentage of the observations are missing),
 and optionally drops variables meeting some criterion of
 missingness.

=Usage:

 The MISSING macro is defined with keyword parameters.  The ID=
 parameter is required. You probably want to use the OUT=
 parameter to direct the output data set.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
   %missing();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        The name of the variable to be analyzed [Default: VAR=_NUMERIC_]

* ID=         Name of id variable, used to transpose observations.

* CHECKLAB=   Any non-blank values causes the macro to check that at
              least one variable has a variable label.  This is a
				  technical requirement (or kludge) for the output report
				  which assumes that at least one variable has a label.

* DROP=       Criterion for dropping variables, given as a SAS expression
              which evaluates to non-zero whenever a variable should be
              dropped.  May use any one or more of the variables available
              in the PRINT= list.  Use DROP=(empty string) to suppress
				  this action.
				  The default, DROP=PMISS>95, drops
              variables for which more than 95% of the scores are missing.
              [Default: DROP=PMISS>95]

* PRINT=      A list of one or more of the following to be included in the
              printed report: PMISS N NMISS MIN MAX MEAN STD (the
              default).  Use PRINT=  (empty string) to suppress this
              printout. [Default: PRINT=PMISS N NMISS MIN MAX MEAN]

* OUT=        Name of the output data set. Use a two-part name to
              create a permanent data set. [Default: OUT=Nonmiss]
                
=Limitations:

o  Only handles numeric variables

 =*/
%macro missing(
	data=_last_,
	var=_numeric_,
	id=,
	checklab=,
	drop=pmiss>95, /* drop variables with at least this % missing */
	print=pmiss n nmiss min max mean,
	out=nonmiss
	);

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;


%let abort=0;
%if %upcase(&data)=_LAST_ %then %let data = &syslast;


%if %length(&var) =0 %then %do;
   %put MISSING: No variable(s) have been specified;
   %put %str(       )Specify a VAR= variable.;
    %let abort=1;
   %goto done;
   %end;

%if %length(&checklab) > 0 %then %do;
%*-- Copy to work data set;
data _new_;
    set &data;
    keep &var;

%*-- Get dataset of variable names and properties;
%put MISSING: Getting variable list from proc contents...;
proc contents data=_new_ out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto done;

data _null_;
    set _work_(keep=name label) end=eof;
    if label^=' '
        then nlabs + 1;
        else var=name;
    if eof then do;
        call symput('nlabs', put(nlabs,4.));
        call symput('onevar', var);
        end;
    run;

%*-- Make sure at least one variable has a label (so id _label_ wont fail);
data _new_;
    set &data;
    %if &nlabs=0 %then %do;
        label &onevar = "&onevar";
        %end;
%let data=_new_;
%end;  %*-- If &checklab ;

%put MISSING: Transposing...;
proc transpose data=&data  out=_trans_;
  var &var;
  by &id notsorted;
proc sort;
   by _name_;

%put MISSING: Summarizing the variables...;
proc means data=_trans_ noprint;
   var col1;
   by _name_;
   id _label_;
   output out=_stat_ n=n nmiss=nmiss min=min max=max mean=mean std=std ;

*proc print;

data _stat_;
   set _stat_;
   drop _freq_ _type_;
	pmiss = 100*nmiss/_freq_;
	if upcase(_name_) ^= '_ID_' &
		upcase(_name_) ^= upcase("&id");

%if %length(&print)>0 %then %do;
proc print data=_stat_;
	id _name_ _label_;
	var &print;
	format pmiss 5.0;
	%end;

%if %length(&drop) %then %do;

%let nd=0;
data _null_;
   set _stat_ end=eof;
	array drop{*} $200 drop1-drop100;
	retain drop1-drop100 '' _k_;
	if (&drop) then do;
		nd+1;
		_k_ = 1+int((nd-1)/20);      *-- index to drop array
		put _k_= _name_=;
		drop{_k_} = trim(drop{_k_}) || ' ' || trim(_name_);
		end;
	if eof then do;
		put nd= _k_=;
		* put drop1= drop2= drop3= drop4=;
 		do i=1 to _k_;
		*	put 'symput: dlist' (left(put(i,3.)));
			call symput('dlist'||left(put(i,3.)), left(drop{i}));
			end;
		call symput('nd', trim(left(put(nd,3.))));
		call symput('nl', trim(left(put(_k_,3.))));
		end;
run;

%put MISSING: &nd variables meet the &drop criterion.;

%if &nd > 0 %then %do;
%put MISSING: Dropping &nd variables:;
	%do i=1 %to &nl;
		%put %str(  ) &&&dlist&i;
		%end; 

*options mprint;
data &out;
	set &data;
	drop
	%do i=1 %to &nl;
		&&&dlist&i
		%end; 
	;
	%end;
%end;  /* %length(&drop) */

proc datasets nolist nowarn;
	delete _trans_;
	run; quit;
	
%done:;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%if &abort %then %put ERROR: The MISSING macro ended abnormally.;
%mend;
