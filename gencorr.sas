 /*--------------------------------------------------------------*
  *    Name: gencorr.sas                                         *
  *   Title: Generalized correlations for ordinal and continuous *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 27 Mar 2002 12:11:51                                *
  * Revised: 27 Mar 2002 12:11:51                                *
  * Version: 1.0                                                 *
  *   Created using code liberally stolen from %polychor         *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The GENCORR macro calculates a generalized correlation
 matrix for a data set containing a mixture of continuous and
 ordinal variables.  For pairs of ordinal variables, the polychoric
 (or, for 2x2 tables, the tetrachoric) correlation is calculated; 
 otherwise (in is version), the Pearson correlation is calculated. 
 Alternatively, a distance matrix, calculated as 1-r**2 may be reqested. 

==Method:

 The program simply runs PROC FREQ (for pairs of ordinal variables)
 or PROC CORR for each pair of variables,  and assembles the results 
 in an output data set.  It is therefore horribly inefficient for large
 problems, since it makes p*(p-1)/2 passes over the input data set.

=Usage:

 The GENCORR macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%gencorr();
 
==Parameters:

* DATA=       The name of the input data set. If the DATA= option is not
              supplied, the most recently created SAS data set is
              used. [Default: DATA=_LAST_]

* VAR=        The names of the variables to be analyzed. Individual variable
              names, separated by blanks, must be specified.  
              By default, all numeric variables found in the data set will 
              be used. [Default: VAR=_NUMERIC_]

* WEIGHT=     Specifies the name of an input variable to be used
              as an observation weight. If specified, you must list
              the VAR= variables explicitly, rather than using the
              default (_NUMERIC_).

* VTYPE=      A list of C, for continous, and O for ordinal, corresponding
              to the variables in the VAR= list.  If the VTYPE= list is
              shorter than the VAR= list, the former is repeated as
              necessary.  The default treats all variables as ordinal.
              [Default: VTYPE=O]

* OUT=        The name of the output data set [Default: OUT=_CORR]

* TYPE=       Specifies the type of matrix to be created.  If 
              TYPE=CORR (the default), then a correlation matrix is 
              computed and the output data set is assigned a data set 
              type of CORR.  If TYPE=DISTANCE, then a distance matrix 
              is computed and the output dat set is assigned a data 
              set type of DISTANCE.

=Example:

	*-- Create a set of 5 ordinal variables with an AR(1) structure
	    plus two continuous variables;
	data mixed;
		array x{5} x1-x7; 
		do n=1 to 50;
			do i=1 to 5;
				if i=1 
					then x{i}=rantbl(238423, .1,.2,.4,.2,.1);
					else x{i}=rantbl(238423, .1,.2,.4,.2,.1) + 4 * x{i-1};
			end;
			x6 = normal(238423);
			x7 = sum(x1, x2, x6) + 2* normal(238423);
			keep x1-x7; 
			output;
		end; 
		run;

	%gencorr(data=mixed, 
		var=   x1 x2 x3 x4 x5 x6 x7,
		vtype=  O  O  O  O  O  C  C);

=*/

%macro gencorr(
   data=_last_,
   var=_numeric_,
   weight=,
   vtype=o,
   out=_corr,
   type=corr
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

%if &data=_last_ %then %let data=&syslast;

/* Verify that TYPE=CORR or DISTANCE */
%if %upcase(&type) ne CORR and %upcase(&type) ne DISTANCE %then %do;
  %put ERROR: TYPE= must be CORR or DISTANCE.;
  %goto exit;
%end;

data _null_;
 set &data;
 array x{*} &var;
 length name $8.;
 if _n_=1 then
 do i=1 to dim(x);
   call vname(x{i} , name);
   call symput('_v'||trim(left(put(i,4.))) , name);
 end; 
 p=dim(x);
 call symput('_p',trim(left(put(p,4.))));
 run;

%let vtype = %repeat(&vtype, &_p);
%put vtype = &vtype;

%local _i _j ti tj;

%do _i=1 %to &_p;
	%let ti = %upcase(%scan(&vtype, &_i, %str( )));
	%do _j=%eval(&_i+1) %to &_p;
		%let tj = %upcase(%scan(&vtype, &_j, %str( )));

		%*-- Polychoric correlation for two ordinal variables;
		%if &ti = O and &tj = O %then %do;
			proc freq data=&data noprint; 
				%if %length(&weight) %then %do;
				weight &weight;
				%end;
			   tables &&_v&_i * &&_v&_j / plcorr;
				output out=_tmp plcorr;
				run;
			data _null_;
				set _tmp;
				value= %if %upcase(&type)=CORR %then _plcorr_;
						 %if %upcase(&type)=DISTANCE %then 1-_plcorr_**2;
				;
    			call symput("p&_i._&_j" , value);
				run;
			%end;

		%*-- For the case of one ordinal and one continuous, we should
				do something different, but for now, we just fall through;
		%else %do;
			proc corr data=&data noprint outp=_tmp; 
				%if %length(&weight) %then %do;
				weight &weight;
				%end;
				var &&_v&_i  &&_v&_j ;
				run;
			data _null_;
				set _tmp;
				where(upcase(_type_)='CORR' & upcase(_name_)="%upcase(&&_v&_i)");
				value= %if %upcase(&type)=CORR %then &&_v&_j;
						 %if %upcase(&type)=DISTANCE %then 1-(&&_v&_j)**2;
				;
    			call symput("p&_i._&_j" , value);
				run;
			%end;

	%end;
%end;

data &out
  %if %upcase(&type)=CORR %then %do;
    ;
    _type_='CORR';
    length _name_ $8.;
  %end;
  %if %upcase(&type)=DISTANCE %then %str( (type=distance); );

  /* Create matrix */
  array x{*}     %do i=1 %to &_p;
                     &&_v&i
                 %end;
    ;
  do i=1 to dim(x);
    do j=1 to i;

      /* Set diagonal values */
      if i=j then x{j}=   %if %upcase(&type)=CORR %then 1;
                          %if %upcase(&type)=DISTANCE %then 0;
      ;

      /* Set lower triangular values */
      else
      x{j}=symget("p"||trim(left(put(j,4.)))||"_"||trim(left(put(i,4.))));
    end;

    /* Create _NAME_ variable for CORR data sets */
    %if %upcase(&type)=CORR %then 
      %str( _name_=symget("_v"||trim(left(put(i,4.)))); );
    drop i j;
    output;
  end;
  run;

/* Add _TYPE_=MEAN, STD and N observations to CORR data sets */
%if %upcase(&type)=CORR %then %do;
  proc summary data=&data;
    var &var;
    output out=_simple (drop=_type_ _freq_ rename=(_stat_=_type_));
    run;
  data &out (type=corr);
    set _simple (where=(_type_ in ('MEAN','STD','N'))) &out;
    run;
%end;

options notes;
%if &syserr=0 %then 
%if %upcase(&type)=CORR %then
  %put NOTE: Polychoric correlation matrix was output to data set %upcase(&out).;
%else %do;
  %put NOTE: Distance matrix based on polychoric correlations was output;
  %put %str(      to data set %upcase(&out).);
%end;

%exit:

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;

%*---  Repeat a string of words, until there are at least
    agiven number of words;

%macro repeat(parm, len);
	%do k=1 %to &len;
		%if %length(%scan(&parm, &k, %str( )))=0
			%then %let parm = &parm &parm;
		%end;
	&parm
%mend;

