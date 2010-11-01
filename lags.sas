 /*-------------------------------------------------------------------*
  *    Name: lags.sas                                                 *
  *   Title: Macro for lag sequential analysis                        *
        Doc: http://www.datavis.ca/sasmac/lags.html                
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created: Mar 21 14:03:21 EST 1996                                 *
  * Revised:  2 Feb 2001 17:59:23                                     *
  * Version:  1.2                                                     *
  *  1.2 Added WEIGHT= variable, allowing weighted frequencies        *
  *      Added WHERE= clause, allowing subsetting observations        *
  *      Fixed PS= bug with large tables                              *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/

 /*=
=Description:

Given a variable containing event codes (char or numeric), the LAGS macro
creates:
(a) a dataset containing n+1 lagged variables, _lag0 - _lagN (_lag0
		is just a copy of the input event variable)
(b) optionally, an (n+1)-way contingency table containing frequencies of
	all combinations of events at lag0 -- lagN

Either or both of these datasets may be used for subsequent analysis
of sequential dependencies.  One or more BY= variables may be specified,
in which case separate lags and frequencies are produced for each 
value of the BY variables.  A WEIGHT= variable may be specified,
giving frequencies weighted by that variable.  For example, using the
duration of an event as a weight gives 'frequencies' which represent
the total number of time units for state sequential data.

=Usage:

One event variable must be specified with the VAR= option.  All other 
options have default values.  If one or more BY= variables are specified,
lags and frequencies are calculated separately for each combination of
values of the BY= variable(s).

The arguments may be listed within parentheses in any order, separated
by commas. For example:

   %lags(data=codes, var=event, nlag=2)

=Parameters:

* DATA=	The name of the SAS dataset to be lagged.  If DATA= is not
			specified, the most recently created data set is used.

* VAR=   The name of the event variable to be lagged.  The variable may
         be either character or numeric.

* BY=    The name of one or more BY variables.  Lags will be restarted
         for each level of the BY variable(s).  The BY variables may
			be character or numeric.

* WEIGHT=  Specifies a numeric variable whose value represents the
         frequency or weight of an observation.  The weight values
         must be non-negative, but need not be integers.

* WHERE=  Specifies the clause for a WHERE statement, used to select the
         observations for which lags are calculated.

* VARFMT=  An optional format for the event VAR= variable.  If the codes
         are numeric, and a format specifying what each number means
			is used (e.g., 1='Active' 2='Passive'), the output lag variables
			will be given the character values. 

* NLAG=   Number of lags to compute.  Default = 1.

* OUTLAG=  Name of the output dataset containing the lagged variables.  This
         dataset contains the original variables plus the lagged variables,
			named according to the PREFIX= option.

* PREFIX=   Prefix for the name of the created lag variables.  The default
         is PREFIX=_LAG, so the variables created are named _LAG1, _LAG2,
			..., up to _LAG&nlag.  For convenience, a copy of the event
			variable is created as _LAG0.  In sequential analysis, the lag0
         event is often called 'GIVEN' and the lag1 event is 'TARGET'.

* FREQOPT=  Options for the TABLES statement used in PROC FREQ for the
         frequencies of each of lag1-lagN vs lag0 (the event variable).
			The default is FREQOPT= NOROW NOCOL NOPERCENT CHISQ.
			
Arguments pertaining to the n-way frequency table:

* OUTFREQ=  Name of the output dataset containing the n-way frequency
         table.  The table is not produced if this argument is not
			specified.

* COMPLETE=    NO, or ALL specifies whether the n-way frequency table
         is to be made 'complete', by filling in 0 frequencies for
			lag combinations which do not occur in the data.

=Example:

 Assume a series of 16 events have been coded with the 3 codes, a, b, c,
 for 2 subjects as follows:
 
  Sub1:   c   a   a   b   a   c   a   c   b   b   a   b   a   a   b   c
  Sub2:   c   c   b   b   a   c   a   c   c   a   c   b   c   b   c   c
  
 and these have been entered as the 2 variables SEQ (subject) and CODE
 in the dataset CODES:

   		SEQ    CODE
		
			1      c
			1      a
			1      a
			1      b
			....
			2      c
			2      c
			2      b
			2      b
			....
 Then the macro call:

    %lags(data=codes, var=code, by=seq, outfreq=freq);

 produces the lags dataset _lags_ for NLAG=1 that looks like this:

   SEQ    CODE    _LAG0    _LAG1
   
    1      c        c         
           a        a        c
           a        a        a
           b        b        a
           a        a        b
           ....
   
    2      c        c         
           c        c        c
           b        b        c
           b        b        b
           a        a        b
            ....

 The output 2-way frequency table (outfreq=freq) looks liks this:

   SEQ    _LAG0    _LAG1    COUNT
   
    1       a        a        2
            b        a        3
            c        a        2
            a        b        3
            b        b        1
            c        b        1
            a        c        2
            b        c        1
            c        c        0
   
    2       a        a        0
            b        a        0
            c        a        3
            a        b        1
            b        b        1
            c        b        2
            a        c        2
            b        c        3
            c        c        3

 =*/

%macro lags(data=_last_, 
	outlag=_lags_,   /* output dataset containing lag variables */
	outfreq=,        /* output dataset containing nlag-way frequencies */
	var=,            /* variable containing codes for events */
	varfmt=,         /* format for event variable */
	nlag=1,          /* number of lags to compute in the outlag dataset */
	by=,             /* by variable: separate lags for each */
	weight=,         /* weight variable */
	where=,          /* where clause to subset observations */
	freqopt=norow nocol nopercent chisq,
	complete=ALL,    /* Should the contingency table be made complete?  */
	prefix=_lag);    /* prefix for names of lag variables */
	
%if &nlag = %str() %then %do;
	%put NLAG= must be specified;
	%goto done;
	%end;

%let abort=0;
%let complete = %upcase(&complete);
%if %upcase(&data) = _LAST_  %then %let data=&syslast;

%if %bquote(&by) ^=  %then %do;
   %let _byvars=;
   %let _bylast=;
   %let n=1;
   %let token=%qupcase(%qscan(&by,&n,%str( )));

   %do %while(&token^=);
      %if %index(&token,-) %then
         %put WARNING: Abbreviated BY list &token.  Specify by= individually.;
      %else %do;
         %let token=%unquote(&token);
         %let _byvars=&_byvars &token;
         %let _bylast=&token;
      %end;
      %let n=%eval(&n+1);
      %let token=%qupcase(%scan(&by,&n,%str( )));
   %end;
%let nby = %eval(&n-1);

%* put found &nby by variable(s) : &_byvars;

	proc sort data=&data;
		by &by;

%end;
	
%*-- Find type/missing value code for &var ;
proc contents data=&data out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

options nonotes;
data _null_;
	set _work_(keep=name type);
	*-- set a missing macro variable for char or numeric variables;
	if upcase(name) = upcase("&var") then do;
		if type = 2 then miss="' '";
		else miss='.';
		call symput('missing', miss);
		call symput('type', left(put(type,1.0)));
		*put type=;
	end;

data &outlag;
	set &data;
	%if %bquote(&where) ^=  %then %do;
		where (&where);
	%end;
	%if %bquote(&by) ^=  %then %do;
		by &by;
		drop cnt;
	%end;
	%do i= &nlag %to 1 %by -1;
		&&prefix.&i = lag&i( &var);
		%end;
   &prefix.0 = &var;

	%if %bquote(&by) ^=  %then %do;
		if first.&_bylast then cnt=0;
		cnt+1;
		%do i = 1 %to &nlag;
			if cnt <= &i then &&prefix.&i = &missing ;
			%end;		
	%end;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
	

*-- Frequencies for each lag vs lag0;
proc freq data=&outlag;
	%if &varfmt ^= %str() %then %do;
	format
	%do i = 0 %to &nlag;
		&&prefix.&i
		%end;  &varfmt %str(;);
	%end;
	%do i = 1 %to &nlag;
		tables &&prefix.&i * &prefix.0   / &freqopt;
		%end;		
	%if %bquote(&by) ^=  %then %do;
		by &by;
		%end;
	%if %bquote(&weight) ^=  %then %do;
		weight &weight;
		%end;
	run;
	
*-- Output nlag-way data set containg lag frequencies;
%if &outfreq ^= %str() %then %do;

	%let sparse=;
	%if &complete ^= NO %then %let sparse = sparse;
	
	proc freq data=&outlag;
		*-- generate a tables lagn * lagn-1 * ... lag 0 statement;
		tables  
		%do i= &nlag %to 1 %by -1;
			&&prefix.&i   *
			%end;
			&prefix.0 / noprint &sparse out=&outfreq;
		%if %bquote(&by) ^=  %then %do;
			by &by;
			%end;
	%if %bquote(&weight) ^=  %then %do;
		weight &weight;
		%end;
	run;
	
	%*-- delete any missing lags;
	data &outfreq;
		set &outfreq(drop=percent) ;
		%do i= &nlag %to 1 %by -1; 
			if &&prefix.&i ^= &missing ;
			 %end;
			 
	%*-- Re-sort to put BY variable(s) first;
	%if %bquote(&by) ^=  %then %do;
	proc sort data=&outfreq;
		by &by 
			%do i= &nlag %to 0 %by -1; &&prefix.&i 
			 %end;
			 %str(;);
	%end;
				
%end;

%done:
%if &abort %then %put ERROR: The LAGS macro ended abnormally.;
options notes;
%mend;

