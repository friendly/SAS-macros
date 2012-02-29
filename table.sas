 /*-----------------------------------------------------------------*
  |     Name: table.sas                                             |
  |    Title: Construct a grouped frequency table, with recoding    |
        Doc: http://www.datavis.ca/sasmac/table.html             
  | ----------------------------------------------------------------|
  |    Procs: freq printto                                          |
  |  Macdefs: table join tempfile tempdel                           |
  | ----------------------------------------------------------------|
  |   Author: Michael Friendly               <friendly@yorku.ca>    |
  |  Created: 09 Jul 1999 16:52:05                                  |
  |  Revised: 29 Feb 2012 09:37:51                                  |
  |  Version: 1.4-1                                                 |
  *  1.1  Inlined %tempfile %tempdel                                *
  *  1.2  Added WHERE= to subset categories of table variables      *
  *  1.3  Added ORDERBY= to order variables by formatted values     *
  *  1.4  Fixed problem in SAS 9.3 regarding use of PROC PRINTO
  *                                                                 *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)  *         
  *-----------------------------------------------------------------*/
/*=
=Description:

 The TABLE macro constructs a grouped frequency table suitable for 
 input to %mosaics or for other purposes. 

 The input data may be individual observations,
 or a contingency table,  which may be collapsed to fewer variables.
 Factor variables may be recoded and/or converted to character using 
 user-supplied formats.

=Usage:

 The TABLE macro takes keyword arguments.  The VAR= parameter
 is required.
 
==Parameters:

* DATA=              The name of the input dataset.  [Default: DATA=_LAST_]

* VAR=               Names of all factor (classification) variables to be
                     included in the
                     output dataset.  The observations are summed over
                     any other factors, weighted by the WEIGHT= variable,
                     if any.  

* WHERE=             A WHERE-clause, to be used to subset the categories of 
                     the table variables in the PROC FREQ step.  E.g., 
                     WHERE=Sex not in (' ', 'Other')

* CHAR=              If non-blank, forces the VAR= variables to be
                     converted to character variables (using formatted
                     values) in the output dataset.  If CHAR= a
                     numeric value (e.g., CHAR=8), it specifies the
                     length of each character variable; otherwise, the
                     character variables default to length 16.

* WEIGHT=            Name of a frequency variable, if the input dataset
                     is already in frequency form.

* ORDER=             Specifies the order of the variable levels used
                     in the PROC FREQ step.  ORDER=INTERNAL|FREQ|DATA|
                     FORMATTED are valid option values.

* ORDERBY=           An SQL 'order by' clause to reorder the table, for
                     example to reorder a variable according to the levels
                     of its formatted value.

* FORMAT=            List of variable(s), format pairs (suitable for a
                     format statement).  The FORMAT= option may be used
                     to recode the values of any of the VAR= variables.

* OUT=               Name of output dataset.  The variables in the output
                     dataset are the VAR= variables, plus COUNT, the
                     frequency variable for each cell. [Default: OUT=TABLE]

=Limitations:

None of the factor variables may be named COUNT.  Formatted values
forced to character variables (CHAR=n) may not include embedded blanks.

=Example:

 This example reads a three-way frequency table (gender x admit x dept),
 where admit and dept are numeric variables, and collapses it to a
 two-way table, with Gender and Admit as character variables.
 
   %include data(berkeley);
   %table(data=berkeley, var=gender admit, weight=freq, char=Y,
          format=admit admit. gender $sex., order=data, out=berk2);
   %mosaic(data=berk2, var=Gender Admit);

 The formats admit. and $sex. are created with proc format:

   proc format;
      value admit 1="Admitted" 0="Rejected";
      value $sex  'M'='Male'   'F'='Female';

=*/ 

%macro table (
   data=_last_,     /* Name of input dataset                        */
   var=,            /* Names of all factor variables                */
   where=,          /* WHERE clause to subset observations          */
   char=,           /* Force factor variables to character?         */
   weight=,         /* Name of a frequency variable                 */
   order=,          /* Specifies the order of the variable levels   */
   format=,         /* List of var, format pairs                    */
   orderby=,        /* An SQL 'order by' clause to reorder the table*/
   out=table        /* Name of output dataset                       */
   );

%local abort ls;
%let abort=0;
%let ls=120;
%*-- Save original linesize;
%if %sysevalf(&sysver >=7) %then %do;
   %local lso pso dto cto o2;
   %let lso=%sysfunc(getoption(ls,keyword));
   %let pso=%sysfunc(getoption(ps,keyword));
   %let dto=%sysfunc(getoption(date));
   %let cto=%sysfunc(getoption(center));
	%let o2 = %sysfunc(getoption(validvarname,keyword));
   %end;
%else %do; %let lso=; %let pso=; %let dto=; %let cto=; %let o2=; %end;


%if %length(&var)=0 %then %do;
   %put ERROR:  The VAR= variables must be specified.;
   %end;

%let table = %join(&var, *);

proc freq data=&data
   %if %length(&order) %then order=&order;
   ;
   %if %length(&where) %then %do;
      where (&where);
      %end;
   %if %length(&weight) %then %do;
      weight &weight;
      %end;
   tables &table / noprint sparse out=&out(drop=percent);
   %if %length(&format) %then %do;
      format &format;
      %end;
	run;

%if %length(&char)>0 %then %do;
   /*
    * Force the VAR= variables to character.  To do this cleanly, we
    * resort to printing the &out dataset, then reading it back as
    * character. 
    * In SAS 9.3, this only works if ODS LISTING is turned on. 
    */
   %tempfile(table,&ls);
   %if %sysevalf(&sysver >9.2) %then %do;
   	ods listing;
   %end;
   proc printto new print=table;
   options nodate nocenter nonumber ls=&ls ps=10000;
   proc print data=&out;
      id &var;
      var count;
      run;
   %if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
   proc printto print=print;


   %let tvar = %join(&var, $) $;
   %*put tvar=&tvar;

   %if %verify(&char,  %str(0123456789))=0
      %then %let clen=&char;
      %else %let clen=16;
   data &out;
      infile table length=len;
      length string $&ls &var $&clen;
      retain skipping 1;
      drop string skipping;
      input @1 string $varying. len @;
      if skipping=0 & string ^= ' ' then do;
         input @1 &tvar count;
         output;
         end;
      else input;
      if index(string,'COUNT')>0 then skipping=0;
   run;
   *proc contents data=&out;

%tempdel(table);
 %if %sysevalf(&sysver >9.2) %then %do;
 	ods listing close;
 %end;
%end;

%if %length(&orderby) %then %do;
proc sql;
    create table &out as
    	select * from &out
	order by &orderby;
    quit;
%end;

%done:;
%if &abort %then %put ERROR: The TABLE macro ended abnormally.;
options notes &lso &pso &dto &cto;
%if %sysevalf(&sysver <7) %then %do;
	options center date ls=80 ps=60;
	%end;

%mend;

 /*=
=Description:

Join the &delim-separated words in &string with &sep.

=Usage:

 %let result = %join(A B C, *);   *-- returns: A * B * C;

==Parameters:

* STRING        A &delim-separated string of 'words'    

* SEP           The separator character(s) used between each pair

* DELIM         The delimiters [Default: %str( )]

 =*/

%macro join(string, sep, delim);

   %local count word;
   %if %length(&delim) %then %let delim=%str( );
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %let result = &word;
   %do %while(&word^= );
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
       %if %length(&word)
          %then %let result = &result &sep &word;
   %end;
   &result
   
%mend;

%macro tempfile(fileref,ls);
   %global tempfn;
   %if %length(&ls)=0 %then %let ls=80;
   %if &sysscp = CMS
      %then %let tempfn=&fileref output a;
   %else %if &sysscp = WIN 
      %then %let tempfn=c:\temp\&fileref..out;
   %else /* %if &sysscp = NEXT | &sysscp = RS6000 
      %then  */ %let tempfn=/tmp/&fileref..out;
   filename &fileref "&tempfn" lrecl=&ls;
%mend;

%macro tempdel(fileref);
   %global tempfn;
    *-- Avoid annoying flash with X commands;
    %if %sysevalf(&sysver  > 6.10) %then %do;
        %let rc=%sysfunc(fdelete(&fileref));
        %let rc=%sysfunc(filename(&fileref,''));
    %end;

    %else %do;
   %if &sysscp = CMS
      %then cms erase &tempfn;
   %else %if &sysscp = WIN
      %then %do;
         options noxsync noxwait; run;
         %sysexec(erase &tempfn); run;
         options   xsync   xwait; run;
      %end;
   %else /* assume flavor of UNIX */
       %sysexec(rm -f &tempfn);
    %end;
%mend;
