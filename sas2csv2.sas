/*----------------------------------------------------------------*
  Name: sas2csv.sas
 Title: Create a comma-delimited (CSV) file from a SAS dataset

 Based on csvfile.sas from SAS Institute
 Based on makeraw.sas, from UCLA
 (http://www.oac.ucla.edu/training/sas/faq/)
 
 Example: convert work.auto to auto.csv
	%sas2csv(data=auto);

 Options:                                          

 NAMES=YES - put variable names on line 1         
 QUOTE=NO  - omit quotes around each value        
 DLM= <delimiter of your choosing> or TAB for tab delimited

 Example: make a file with the variable names on line 1
          omit the quotes around every variable
          delimit the variables with spaces " "
 %sas2csv(dsname,rawfile,NAMES=yes,QUOTE=no,DLM=" ");

 same as above, except use a tab delimiter
 %sas2csv(dsname,rawfile,NAMES=yes,QUOTE=no,DLM=TAB);

*----------------------------------------------------------------*/


%macro sas2csv(
	libname=work,    /* name of the SAS library                   */
	data=_last_,     /* name of the data set to be converted      */
	keep=,           /* list of variables to be kept              */
	drop=,           /* list of variables to be dropped           */
	out=,            /* pathname of the output file               */
	names=YES,       /* include first line with variable names?   */
	dlm=",",         /* delimiters between fields: chars or TAB   */
	quote=yes,       /* quote values? Yes|No|Char                 */
	where=           /* where clause to select observations       */
	);

* mnm added dlm ;
%if %upcase(&DLM)=TAB %then %LET DLM="	";%*-- a tab character;

%let quotech = '"' ;
%let quote=%upcase(&quote);
%let names=%upcase(&names);
%if &quote=NO  %then %let quotech =  ;

%if %quote(%upcase(&out)) ^= PRINT %then %do;
	%if %length(&out)=0 
		%then %let out = &data..csv;
	filename csvfile "&out" lrecl=1000;
	%end;
	
/*
 Create a view with all the labels, variables names, and formats
 for all the variables on the dataset, these dictionary views are     
 automatically maintained by SAS. 
 PROC PRINT DATA=DICTIONARY.COLUMNS;   will serve to show the 
 information in these dictionary entries.  Documentation is in   
 SAS Technical Report P-222 
*/

	%if %length(&keep.&drop) %then %do;
		data _keep_;
			%if %length(&keep) %then %do; keep &keep; %end;
			%if %length(&drop) %then %do; drop &drop; %end;
			set &libname..&data;
		%let data=_keep_;
		%let libname=work;
		%end;

	%if %sysevalf(&sysver  >= 6.12) %then %do;
  proc sql noprint;
    create view work.VARLIST as select format, label, name, type
      from dictionary.columns
      where libname="%upcase(&LIBNAME)" and memname="%upcase(&data)";
    select count(name) into: NUMVARS from dictionary.columns
      where libname="%upcase(&LIBNAME)" and memname="%upcase(&data)";
		%end;

	%else %do;
		proc contents data=&libname..&data out=varlist noprint;
		data varlist;
			set varlist(rename=(type=typ)) end=eof;
			keep name type label format;
			if typ=1 then type='N';
				else type='C';
			if eof then
				call symput('numvars', left(put(_n_,4.)));
		%end;

  /* now use the data step symput call routine to move all the
     variable formats, names and labels from data step view variables to
     macro variables */


  data _null_;
    set work.varlist;

    call symput('frmt'||left(put(_n_,4.)),trim(format));
    call symput('labl'||left(put(_n_,4.)),trim(label));
    call symput('name'||left(put(_n_,4.)),trim(name));
    call symput('type'||left(put(_n_,4.)),trim(type));
  run;

  data _null_;

   set &LIBNAME..&data end=eof;
	%if %length(&where) %then %do;
		where (&where);
		%end;
		

	%if %quote(%upcase(&out)) ^= PRINT %then %do;
    file csvfile col=col;
		%end;
	%else %do;
	 file print;
	 	%end;
	

    /*  now create a put statement that will write out the variable
        names or labels, depending on the parameter value.  Note that
        we check the _n_ variable to make sure we write out the header
        line only the first time through the datastep.
    */

    if _n_ = 1 then do;
      %if &names^=NO %then
        %do;
          put
          %do i = 1 %to &NUMVARS;

			%if &quote = YES or (&quote = CHAR and &&type&i = C) %then
              """&&name&i.""" ;
			%else
              "&&name&i." ;
            %if &i ^= %left(%trim(&NUMVARS)) %then &DLM;

          %end;
        %end;
        ;

    end;

/*  now create a put statement with the variable names and the formats
    for each variable */

/*
    put
      %do i = 1 %to &NUMVARS;

			%if &quote = YES or (&quote = CHAR and &&type&i = C) %then %do;
       &quotech &&name&i.. &&frmt&i..
       %if &&frmt&i.. = %then +(-1);
       &quotech
			 	%end;
			%else %do;
       &&name&i.. &&frmt&i..
       %if &&frmt&i.. = %then +(-1);
				%end;
       %if &i ^= %left(%trim(&NUMVARS)) %then &DLM;
      %end;
    ;
*/
	col=1;
	array c1{&numvars} _temporary_;
	array c2{&numvars} _temporary_;
      %do i = 1 %to &NUMVARS;
	c1{&i} = col;
    put

			%if &quote = YES or (&quote = CHAR and &&type&i = C) %then %do;
       &quotech &&name&i.. &&frmt&i..
       %if &&frmt&i.. = %then +(-1);
       &quotech
			 	%end;
			%else %do;
       &&name&i.. &&frmt&i..
       %if &&frmt&i.. = %then +(-1);
				%end;
       %if &i ^= %left(%trim(&NUMVARS)) %then &DLM;
    @; c2{&i} = col-1;
      %end;
	put;

	if eof then do;
		file print;
		%do i=1 %to &numvars;
			put c1{&i} 3. '-' c2{&i} 3. '   ' "&&name&i" '   ' "&&labl&i" ;
			%end;
		end;
  run;

%mend;

