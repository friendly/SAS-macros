%macro SAS2html (sdsetnam, outfname, title=, keep=, where=);
* writes all vars & observations from a SAS dataset to an
  HTML table
;
* Sample call:

     *sas2html(sasuser.fitness,temp.htm, where=);

* Parameters:

     sdsetnam  = Name of SAS dataset to be converted to a table.

     outfname  = data set name for the output file.

     keep      = optional keep statement controls the variables that will be
printed.

     where     = optional where statement to subset the observations to be
printed.
;

* requires the *VAR macro;
;

* cut the operating system loose;
%if &sysver > 6.07 %then %do;
options noxwait noxsync;
%end;

%let sdsetnam = %upcase(&sdsetnam);

%let keep     = %upcase(&keep);

%if &sdsetnam eq %str() %then %do;
  %put NOTE: The DATASET was not specified, using the _last_ dataset;
  %let ddname = %scan(&sysdsn,1);
  %let dsetnam= %scan(&sysdsn,2);
  %end;
%else %do;
  %let ddname = %scan(&sdsetnam,1);
  %let dsetnam= %scan(&sdsetnam,2);
  %end;

%if &outfname eq %str() %then %do;
  %let outfname = tablfile.htm;
  %put NOTE: The Output File Name was not specified, so writing to &outfname.;
  %end;
%global nobs nvar;

filename htmlout "&outfname";

* get information from the dictionary about the dataset to be copied by
calling the *var macro.
;
%var(&ddname, &dsetnam, prntvars=N);

* proc sql is still running.
;
select nobs into: nobs from dictionary.tables
  where libname eq "&DDNAME" and memname eq "&DSETNAM";
select nobs into: nVAR from dictionary.tables
  where libname eq "WORK" and memname eq "VAR";
quit;

%if %eval(&nobs gt 0) %then %do;

  * write a put statement in a temporary file based on the information in
    the work.var dataset. cumulate information about the total width of the
    data.
  ;
  data _null_;
  set var
    %if &keep ne %str() %then (where=(indexw("&keep",name)));
    end=eof;

  * put the dataset labels as the column headers;
  file htmlout;
  if _n_ eq 1 then do;
     %if &title ne %str() %then
        put '<h1>' /
        "&title"  /
         '</h1>' %str(;);
     %else
        put '<h1>' /
        "SAS dataset &sdsetnam"  /
         '</h1>'%str(;);
     put '<TABLE border><Tr align=left valign=bottom>';
     end;
  if label eq ' ' then put '<td>' name $ '</td>';
  else put '<td>' label $ '</td>';
  if eof then put '</tr>';

  file 'temp00.sas';
  if _n_ eq 1 then put 'put';
  if type eq: 'c' then do;
     if format eq ' ' then format = '$';
     put ' "<td alilgn=left>" ' name $8. +3 format $ ' "</td>" ' /;
     end;
  else do;
    if format eq ' ' then format = '10.2';
    put ' "<td align=right>" ' name $8. +3 format $ ' "</td>" ' /;
    end;
  if eof then put ';';
  run;

  * write the actual data.
  ;
  data _null_;
  set &ddname..&dsetnam
    %if &where ne %str() %then (where=(&where));
    end=eof;
  file htmlout mod;
  put '<tr>';
  %include 'temp00.sas'; ;
  put '</tr>';
  if eof then put '</table>';
  run;

  * clean up by removing temporary files, datasets and filename pointers.
  ;
  x 'del temp00.sas';
  proc datasets ddname=work nolist;
  delete var;
  quit;
  %end;

%else %do;
  %put Note: Dataset &sdsetnam was not found or had no observations;
  %end;
%mend;

%macro VAR (libname, memname,outdset=var,prntvars=Y);
%let libname = %upcase(&libname);
%let memname = %upcase(&memname);
%if %index(&libname,.) gt 0 %then %do;
  %let memname = %scan(&libname,2,.);
  %let libname = %scan(&libname,1,.);
  %end;
proc sql noprint;
create table &outdset as select
NAME, LENGTH FORMAT=6., TYPE FORMAT=$1., LABEL, FORMAT, INFORMAT,
  IDXUSAGE, LIBNAME, MEMNAME, NPOS, VARNUM, MEMTYPE
  FROM dictionary.columns
  where libname eq "&libname" and memname eq "&memname";
%if &prntvars eq Y %then %do;
  reset print;
  select name from &outdset;
  %end;
%mend;
