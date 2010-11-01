 /*--------------------------------------------------------------*
  *    Name: sortfmt.sas                                         *
  *   Title: Sort variables by formatted values                  *
  *     Doc: internal                                            *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 04 Nov 98 17:17                                     *
  * Revised: 04 Nov 98 17:17                                     *
  * Version: 1.0                                                 *
  *--------------------------------------------------------------*/
/*=
=Description:
 
 The SORTFMT macro generalizes the idea of sorting the observations in
 a dataset to include:

   - sorting according to the values of a user-specified format.  With
	  appropriate user-defined formats, this may be used to arrange the
	  observations in a dataset in any desired order.

==Method:

 The macro generates a character-valued surrogate variable for each
 variable specified in the BYFMT= list, and replaces the name of the
 actual variable in the BY= list for sorting.  A PROC SORT step is
 then used to do the sorting (more efficient than SQL for large data
 sets).

=Usage:

 You must specify one or more BY= variables.  To sort by formatted
 values, specify the variable names and associated formats with
 BYFMT=.  If the BYFMT= parameter is not specified,
 an ordinary sort is performed.

 The sortfmt macro is called with keyword parameters.  The arguments
 may be listed within parentheses in any order, separated by commas.
 For example:
 
proc format;
  value age   0='Child' 1='Adult';
%sortfmt(by=age decending sex,  byfmt=age:age.);
 
==Parameters:

* DATA=		Name of the input dataset to be sorted.  The default is the
            most recently created data set.

* OUT=		Name of the output dataset.  If not specified, the 
            output dataset replaces the input dataset.

* BY=       Names of one or more classification (factor, grouping)
            variables to be used in sorting.  The BY= argument may
				contain the keyword DESCENDING before a variable name
				for ordinary or formatted-value sorting. The BY= variables may
				be character or numeric.

* BYFMT=    A list of one or more terms, of the form, VAR:FMT or
            VAR=FMT, where VAR is one of the BY= variables, and FMT is
            a SAS format.

=Bugs:
	Chokes on 8-char variable names
	No sanity checking

=*/

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

