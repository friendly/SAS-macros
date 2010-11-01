 /*-------------------------------------------------------------------*
  *    Name: interact.sas                                             *
  *   Title: Create interaction variables                             *
        Doc: http://www.datavis.ca/sasmac/interact.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 18 Aug 98  8:32                                          *
  * Revised: 25 May 2003 09:50:45                                     *
  * Version: 1.1                                                      *
  *      Removed , from last macro parameter.  Fixed internal doc     *
  *  1.1  Allow CENTER= to specify list of variables to be centered   *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The INTERACT macro creates interaction variables, formed as the product
 of each of the variables given in one set (V1=) with each of the
 variables given in a second set (V2=).  It is useful for procedures
 such as PROC REG and PROC LOGISTIC (V7-) which do not allow symbolic
 interaction effects to be specified on the MODEL statement.

=Usage:

 The INTERACT macro is called with keyword parameters.  The V1= and V2=
 parameters must be specified.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
   %interact(v1=age sex, v2=I1 I2 I3);

 The interaction variables may be named according to a prefix and numeric
 suffix, or by providing an explicit set of names in the NAMES= parameter.
 
==Parameters:

* DATA=		The name of the input dataset.  If not specified, the most
            recently created dataset is used.

* V1= 		Specifies the name(s) of the first set of variable(s).

* V2= 		Specifies the name(s) of the second set of variable(s).

* OUT=		The name of the output dataset.  If not specified, the new
            variables are appended to the input dataset.

* PREFIX=   Prefix(s) used to create the names of interaction variables.
            The default is 'I_'. The names are of the form I_11 I_12 ...
			I_1m I_21 I_22 ... I_nm, where there are n variables in V1
			and m variables in V2.

* NAMES=    A list of n*m names, to be used as the names of the interaction
            variables.  If specified, the PREFIX= parameter is ignored.

* CENTER=   If non-blank, CENTER= lists the names of variables from 
            the V1 and V2 variables are mean-centered prior to
            forming their interaction products.
 =*/
 
%macro interact( 
	data=_last_ ,    /* name of input dataset */
	out=&data,       /* name of output dataset */
	v1= ,            /* first variable(s) */
	v2= ,            /* second variable(s) */
	prefix = I_,     /* prefix for interaction variable names */
	names=,          /* or, a list of n*m names */
	center=          /* mean center first? */
	);

%let abort = 0;
%if (%length(&v1) = 0 or %length(&v2) = 0) %then %do;
		%put ERROR: INTERACT: V1=  and V2= must be specified;
		%goto done;
		%end;

%if %bquote(&data) = _last_ %then %let data = &syslast;
%if %bquote(&data) = _NULL_ %then %do;
	%put ERROR: There is no default input data set (_LAST_ is _NULL_);
	%goto DONE;
	%end;
	
%if %length(&center) %then %do;
proc standard data=&data out=&data m=0;
	var &center;
	%end;

data &out;
	set &data;

	%local i j k w1 w2;
	
	%let k=0;
	%let i=1;
	%let w1 = %scan(&v1, &i, %str( ));
	%do %while(&w1 ^= );
	
		%let j=1;
		%let w2 = %scan(&v2, &j, %str( ));
	
		%do %while(&w2 ^= );
			%* put i=&i j=&j;
			%let k=%eval(&k+1);
			%let name = %scan(&names, &k, %str( ));
			%if %length(&name) %then %do;
				&name = &w1 * &w2;
				%end;
			%else %do;
				&prefix.&i.&j = &w1 * &w2;
				%end;
			%let j=%eval(&j+1);
			%let w2 = %scan(&v2, &j, %str( ));
			%end;
	
		%let i=%eval(&i+1);
		%let w1 = %scan(&v1, &i, %str( ));
		%end;
run;

%done:
%if &abort %then %put ERROR: The INTERACT macro ended abnormally.;

%mend;
