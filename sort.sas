 /*-------------------------------------------------------------------*
  *    Name: sort.sas                                                 *
  *   Title: Generalized dataset sorting by format or statistic       *
        Doc: http://www.datavis.ca/sasmac/sort.html                
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 04 Nov 98 17:17                                          *
  * Revised: 19 Nov 1998 12:26:55                                     *
  * Version: 1.1                                                      *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The SORT macro generalizes the idea of sorting the observations in
 a dataset to include:

 - sorting according to the values of a user-specified format.  
 With appropriate user-defined formats, this may be used to arrange the
 observations in a dataset in any desired order.

 - reordering according to the values of a summary statistic computed 
	on the values in each of serveral groups, for example, the mean or 
	median of an analysis variable.  Any statistic computed by 
	PROC UNIVARIATE may be used.

=Usage:

 You must specify one or more BY= variables.  To sort by the value
 of a statistic, specify name the statistic with the BYSTAT= parameter,
 and specify the analysis variable with VAR=.  To sort by formatted
 values, specify the variable names and associated formats with
 BYFMT=.  
 
 If neither the BYSTAT= or BYFMT= parameters are specified,
 an ordinary sort is performed.

 The sort macro is called with keyword parameters.  The arguments
 may be listed within parentheses in any order, separated by commas.
 For example:
 
	%sort(by=age sex, bystat=mean, var=income);

(sorting observations by mean INCOME, for AGE, SEX groups) or

	proc format;
		value age   0='Child' 1='Adult';
	%sort(by=age decending sex,  byfmt=age:age.);

(sorting by the formatted values of AGE).
 
==Parameters:

* DATA=		Name of the input dataset to be sorted.  The default is the
            most recently created data set.

* VAR=		Specifies the name of the analysis variable used for BYSTAT
            sorting.

* OUT=		Name of the output dataset.  If not specified, the 
            output dataset replaces the input dataset.

* BY=       Names of one or more classification (factor, grouping)
            variables to be used in sorting.  The BY= argument may
				contain the keyword DESCENDING before a variable name
				for ordinary or formatted-value sorting.  For BYSTAT
				sorting, use ORDER=DESCENDING.  The BY= variables may
				be character or numeric.

* BYFMT=    A list of one or more terms, of the form, VAR:FMT or
            VAR=FMT, where VAR is one of the BY= variables, and FMT is
            a SAS format.  Do not specify BYSTAT= when sorting by
				formatted values.

* VAR=      Name of the analysis variable to be used in determining
            the sorted order.

* BYSTAT=   Name of the statistic, calculated for the VAR= variable
            for each level of the BY= variables.  BYSTAT may be the
				name of any statistic computed by PROC UNIVARIATE.

* FREQ=     For BYSTAT sorting, specify the name of a frequency variable
            if the input data consists of grouped frequency counts.

* ORDER=    Specify ORDER=DESCENDING to sort in descending order
            when sorting by a BYSTAT.  The ORDER= parameter applies
				to all BY= variables in this case.

=Example:
 Given a frequency table of Faculty by Income, sort the faculties
 so they are arranged by mean income:
	
	%sort(data=salary, by=Faculty, bystat=mean, var=income, freq=count);

=*/
 
%macro sort(
	data=_last_,  /* data set to be sorted           */
	out=&data,    /* output dataset                  */
	by=,          /* name of BY/CLASS variable(s)    */
	byfmt=,       /* variable:format list            */
	var=,         /* name of analysis variable       */
	freq=,        /* frequency variable, for bystat  */        
	bystat=,      /* statistic to sort by            */
	order=        /* DESCENDING for decreasing order */
	);

%if %upcase(&data) = _LAST_ %then %let data = &syslast;

%local abort;
%let abort=0;
%if %length(&by)=0 %then %do;
	%put ERROR: At least one BY= variable must be specified for sorting;
	$let abort=1;
	%goto done;
%end;


%*-- If there is no bystat, check byfmt;
%if %length(&bystat)=0 %then %do;
	%if %length(&byfmt)>0 %then %do;
		%sortfmt(data=&data, by=&by, byfmt=&byfmt, out=&out);
		%end;
	%else %do;	
	%*-- There is no bystat, and no byfmt: just an ordinary sort step;
	proc sort data=&data out=&out;
		by &order &by;
		run;
		%end;
%end;

%else %do;    /* BYSTAT sorting */
	%if %length(&var)=0 %then %do;
		%put ERROR: Exaxtly one VAR= variable must be specified for sorting by &bystat;
		$let abort=1;
		%goto done;	
	%end;
	%if %length(%scan(&var,2)) > 0 %then %do;
		%let ovar=&var;
		%let var = %scan(&var,1);
		%put WARNING: VAR=&ovar was specified.  Using VAR=&var;
	%end;

	%*-- Reorder according to &by variables in reverse order;
	%let rby = %reverse(&by);
	%*put Reversed by list: &rby;

   %let count=1;
   %let word = %unquote(%qscan(&rby,&count,%str( )));
	%*-- Do the first one;
	%reorder(data=&data, out=&out, var=&var, class=&word, bystat=&bystat,
			order=&order, freq=&freq);
   %*-- Do the rest;
   %do %while(&word^= );
       %let count = %eval(&count+1);
       %let word = %unquote(%qscan(&rby,&count,%str( )));
		 %if &word ^= %then %do;
			%reorder(data=&out, out=&out, var=&var, class=&word, bystat=&bystat,
			order=&order, freq=&freq);
			%end;
   %end;
 
%end;

%done:
%if &abort %then %put ERROR: The SORT macro ended abnormally.;
%mend;


 /*------------------------------------------------------------*
  *    Name: reorder                                           *
  *   Title: Sort a dataset by the value of a statistic        *
  *------------------------------------------------------------*/

%macro reorder(
	data=_last_,   /* name of input dataset                */
	out=&data,     /* name of output dataset               */
	var=,          /* name of analysis variable            */
	freq=,         /* frequency variable, for bystat  */        
	class=,        /* name of class variable for this sort */
	bystat=,       /* statistic to sort by                 */
	order=,        /* DESCENDING for decreasing order      */
	outvar=,       /* Name of output statistic variable    */
	prefix=_
	);
	
%if %upcase(&data) = _LAST_ %then %let data = &syslast;
*put REORDER: class=&class;

proc sort data=&data;
   by &class ;
	run;
	
%if %length(&outvar)=0
   %then %let outvar =  &&prefix.&class;

proc univariate noprint data=&data;
   by &class ;
   var &var;
	%if %length(&freq) %then %str(freq &freq;) ;
   output out=_stat_ &bystat = &outvar;
   run;
*proc print;


%*-- Merge the statistics with the input dataset;
data &out;
   merge &data _stat_(keep=&class  &outvar);
	by &class;

%*-- Sort them by the statistic;
proc sort data=&out;
	by &order  &outvar;
	run;
%mend;


 /*------------------------------------------------------------*
  *    Name: reverse                                           *
  *   Title: Reverse the words in a string                     *
  *------------------------------------------------------------*/

%macro reverse(string);
   %local count word result;
   %let count=1;
   %let word = %qscan(&string,&count,%str( ));
	%let result=&word;
   %do %while(&word^= );
       %let count = %eval(&count+1);
       %let word = %qscan(&string,&count,%str( ));
		 %let result = &word &result;
   %end;
   %unquote(&result)
%mend;

 /*------------------------------------------------------------*
  *    Name: sorftmt                                           *
  *   Title: Sort variables by formatted values                *
  *------------------------------------------------------------*/

%macro sortfmt(data=, by=, byfmt=, out=&data);

%let tempvar=;
%if %length(&byfmt)>0 %then %do;
	data _temp_;
		set &data;
		%let i=1;
			%*-- terms are separated by spaces.  Each is var:fmt or var=fmt;
		%let term = %scan(&byfmt,1,%str( ));
		%do %while(&term^= );
      	%let i = %eval(&i+1);
			%let var = %scan(&term,1, %str(=:));
			%let fmt = %scan(&term,2, %str(=:));
			%if %index(&fmt,%str(.))=0 %then %let fmt=&fmt..;

				%*-- Create surrogate variable;
			_&var = put( &var, &fmt );

			%let tempvar = &tempvar _&var;
				%*-- Replace the by variable with its surrogate;
			%let by = %replace(&by, &var, _&var);
			%put var=&var fmt=&fmt by=&by;
      	%let term = %scan(&byfmt,&i,%str( ));
		%end;
%*	proc print data=_temp_;
	
	proc sort data=_temp_ out=&out
		%if %length(&tempvar)>0 %then (drop=&tempvar);;
		by &by;
	%end;
%mend;

%*-- Replace a substring with a new string;
%macro replace(string, old, new);
%local i;
%let i=%index( %upcase(&string), %upcase(&old) );
%if &i=0
	%then %let result = &string;
	%else %do;
		%let len = %length(&old);
		%let pre=;
		%if &i>1 %then %let pre=%substr( &string, 1, %eval(&i-1));
		%let result = &pre.&new.%substr( &string,%eval(&i+&len));
		%end;
	&result
%mend;

