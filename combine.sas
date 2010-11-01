 /*--------------------------------------------------------------*
  *    Name: combine.sas                                         *
  *   Title: Combine the values of two or more variables         *
        Doc: http://www.datavis.ca/sasmac/combine.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  4 Mar 1999  9:59                                   *
  * Revised: 09 Feb 2006 08:21:57                                *
  * Version: 1.1-1                                               *
  *  Added IGNMISS= to ignore missing numeric values             *
  *  Fixed buglet with length and sep                            *
  *  Added RUN; to complete data step                            *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The COMBINE macro combines two or more variables (character or
 numeric) into a single one.  This is useful for situations where you
 need two or more CLASS variables, but some procedure or macro
 stupidly only handles one.  Also handy for plots of the form

 	plot y * x = Group Sex

 where there are two or more curve variables.

=Usage:

 The COMBINE macro is called with keyword parameters.  The VAR=
 parameter is required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%combine(var=group gender, result=gp_sex);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        List of two or more variables to be combined.  *required*

* RESULT=     The name of the result variable. [Default: RESULT=_ID_]

* WHERE=      Otional WHERE clause to subset the observations written to the
              OUT= data set.

* SEP=        Separator character(s), inserted between adjacent values
              [Default: SEP=:]

* ABBREV=     If specified, each character variable in VAR= is truncated to this length
              in RESULT.  To specify different truncation lengths, use a list of numbers,
              whose order corresponds to the VAR= variables, e.g., ABBREV=2 2 4.

* LENGTH=     If specified, the RESULT= variable is truncated to this total length,
              regardless of the ABBREV= setting.

* USEFMT=     If postive, numeric variables which have formats stored in the data set
              have their formatted values combined.  [Default: USEFMT=0]

* IGNMISS=    Ignore missing values?

* OUT=        The name of the output data set.  The default (OUT=&data) means that
              the input data set is replaced. [Default: OUT=&data]

=Bugs:

 The internally calculated length for the result variable is incorrect for numeric
 variables.
 
 Should provide a way to use a formatted value of a character variable.

 =*/
%macro combine(
	data=_last_,    /* name of input dataset                       */
	var=,           /* list of variables to be combined            */
	where=,         /* WHERE clause to subset the data             */
	result=_id_,    /* result variable                             */
	sep=:,          /* separator character                         */
	abbrev=,        /* abbreviated length of each char variable    */
	length=,        /* max total length of RESULT                  */
	usefmt=0,       /* use formats for numeric variables?          */
	ignmiss=0,      /* ignore missing values?                      */
	out=&data       /* name of output dataset                      */
	);
	
%let var=%upcase(&var);
%*let var=%vexpand(&var);       *-- Uncomment to make this work for VAR=X1-X5;

%let nv=%words(&var,root=_v_);  *-- Get number of VAR= variables, create _v_1 ... ;
%if %upcase(&data)=_LAST_ %then %let data=&syslast;
/*
%if &nv < 2 %then %do;
	data &out;
		set &data;
		%if &usefmt %then %do;
		_fmt_=vformat(&var);
		put _fmt_=;
		if vtype(&var)='N' then
			&result=left(trim(put(&var, _fmt_));
		else &result = &var;
			%end;
		%else %do;
		&result = &var;
			%end;
	run;
	%put WARNING:  Only &nv VAR= variable was specified.  &data has been copied to &out;

	%goto done;
	%end;
%*put nv=&nv;
*/

options nonotes;
proc contents data=&data  noprint 
     out=_vars_(keep=name type format length);
  run;

data _null_;
	set _vars_ end=eof;
	%do i=1 %to &nv;
		if upcase(name) = upcase("&&_v_&i") then do;
			call symput('_t_'||"&i", put(type,1.0));
			if format ^= ' ' and index(format,'.')=0 then format=trim(format)||'.';
			call symput('_f_'||"&i", format);
			* put name= format=;
			len + length+%length(&sep);
			end;
		%end;
	if eof then do;
		call symput('_len_', left(put(len, 8.0)));
		end;
run;

%if %length(&length)>0 %then %do;
	%if %verify(&length,  %str(0123456789))=0
		%then %let _len_ = &length;
	%end;

%if %length(&abbrev)>0 %then %do;
	%if %verify(&abbrev,  %str(0123456789 ))>0
		%then %do;
			%put WARNING:  Non-numeric ABBREV= &abbrev has been ignored.;
			%let abbrev=;
			%end;
	%end;

%put COMBINE: Length of &result = &_len_;
options notes;
data &out;
	set &data;
	%if %length(&where) %then %do;
		where &where;
		%end;
	length &result $&_len_;;
	&result = '';
	%let s=;
	%do i=1 %to &nv;
		
		%if &i>1 & %length(&sep)>0 %then %do;
			&result = trim(&result) || trim("&sep");
			%end;

		%if &&_t_&i = 1 %then %do;  /* numeric */
			length  _tmp_ $&_len_;;
	 		drop _tmp_ ;
			%if &ignmiss>0 %then %do;
				if not missing(&&_v_&i) then do;
			%end;
			%if &usefmt>0 and %length(&&_f_&i)>0 %then %do;
				_tmp_ = left(put(&&_v_&i, &&_f_&i));
				%end;
			%else %do;
				_tmp_ = left(put(&&_v_&i, best8.));
				%end;
			&result = trim(&result) || _tmp_;

			%if &ignmiss>0 %then %do;
				end;
				%end;
			%end;  /* numeric */
			
		%else %do;  /* character */
				%if %length(&abbrev)>0 %then %do;
				   %let ab = %scan(&abbrev,&i);
				   %if &ab= %then %let ab= %scan(&abbrev,1);
				   &result = trim(&result) || 
					   substr(&&_v_&i,1,min(&ab,length(&&_v_&i)));
				   %end;
			   %else %do;
				   &result = trim(&result) || &&_v_&i;
				   %end;
			%end;  /* character */
%next:
		%end;
run;
%done:	
%mend;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;

