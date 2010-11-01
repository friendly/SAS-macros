 /*--------------------------------------------------------------*
  *    Name: corder.sas                                          *
  *   Title: Reorder variables in a data set by correlations     *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 15 Dec 1999 15:33:48                                *
  * Revised: 15 Dec 1999 15:33:48                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The CORDER macro

=Usage:

 The CORDER macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%corder();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        List of variables to be reordered

* COPY=       Other variables to be copied

* ORDERBY=    Order by PRINn or ANGLE [Default: ORDERBY=PRIN1]

* COV=        What to analyze?

* OUT=        Name of output data set [Default: OUT=CORDER]
                

 =*/
%macro corder(
	data=_last_,
	var=,           /* list of variables to be reordered */
	copy=,          /* other variables to be copied */
	orderby=PRIN1,  /* order by PRINn or ANGLE */
	cov=COV,        /* reset to null to analyze correlations */
	out=corder,     /* name of output data set */
	);

 /*---------------------------------------*
  |  Re-order vars by PRIN                |
  *---------------------------------------*/
	%let orderby=%upcase(&orderby);
	proc princomp data=&data &cov outstat=_comp_ noprint;
		var &var;
	
	data _comp_;
		set _comp_;
		where _type_='SCORE';
		drop _name_ _type_;
	proc transpose data=_comp_ out=_comp_ prefix=prin;
	%if &orderby=ANGLE %then %do;
	data _comp_;
		set _comp_;
		angle = atan(prin2/prin1);
		%end;
	proc sort;
		by &orderby;
	
	data _null_;
		set _comp_ end=eof;
		length vars $200;
		retain vars;
		if _n_=1
			then vars = _name_;
			else vars = trim(vars) || ' ' || _name_;
		if eof then call symput('var', trim(vars));
	run;
	data &out;
		retain &var &copy;
		set &data;
	%global vorder;
	%let vorder = &var;
%mend;
