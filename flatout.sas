/*
This is an update of the Utility macro previously
placed on the SAS-L.  This one has an extra parameter
for LRECL.  Without it, the parameter defaults to whatever
is the LINESIZE option.  I have also added more info from
the metadata to the output listing. My apologies to 
anyone who may have been inconvinienced.

Adam Hendricks
ICOS Corporation
Bothell, WA
*/
%* SAS Macro Program
%*
%* SAS Version 6.11 on SunOS 4.1.3 (UNIX)
%*
%* Name: flatout.sas
%*
%* Usage: 
%*   %flatout(<SAS libname>, <SAS dataset or view>, <Record Length>)
%*
%* Global Macrovariables Generated: none
%*
%* Parameters: 1 - Valid SAS libname
%*                 (Not required for WORK dataset or view)
%*             2 - Valid SAS dataset or view name (Required)
%*             3 - Record Length - LRECL parameter (Required)
%*
%* Function: Generates a tab delimited flat file from a valid SAS dataset
%*           or view.  Flat file is created in default directory under
%*           the same name as the dataset or view (lowercase in UNIX)
%*           with the extension '.txt'.  Generates a SAS listing
%*           containing the following information for the output
%*           flat file:
%*
%*             1. Column Number
%*             2. SAS Column Name
%*             3. SAS Column Type
%*             4. SAS Column Format
%*             5. Column Label
%*
%* Limitations: - This *should* work on versions 6.07 or later on UNIX,
%*                Windows, VMS, or OS/2 platforms.  Should work on MVS
%*                if libraries are allocated with LIBNAME statements
%*                and not JCL or TSO statements.  No check was done
%*                to see what the proper EBCDIC code is for TAB. If it
%*                is not '9' then it must be changed in the output
%*                data _null_ section of the macro.
%*
%*              - Tested on SAS v6.11 on SunOS 4.1.3 (UNIX) only.
%*
%*              - Will overwrite any file of the same name as is described
%*                in 'Function:' section above.
%*
%*              - Will generate an ERROR if unknown format is used in
%*                dataset or view AND the option FMTERR is in effect.
%*
%*              - Will output dates and times in numeric format if proper
%*                format not applied to variable in source dataset.
%*
%*              - Writes out one line per observation.  Subject to OS
%*                limits on record length.
%*
%* Programmer: Adam Hendricks, ahendric@icos.com,
%*            (206)485-1900 ext. 2295
%*
%* Date: 2/22/96
%*
%* Update: 3/29/96 - Add LRECL parameter.
%*                   Added SAS column type and format to listing.
%*                   Updated header info and comments.
%*;
%macro flatout(lib=work, data=_last_, lrecl=132, out=&data, dlm=,);

%let data=%upcase(&data);
%if data=_LAST_ %then %do;
	%let data=&syslast;
	%let out=%scan(&data,2);
	%end;

%if %upcase(&dlm)=TAB then %let dlm=byte(9);
	
%* Declare local macrovariables *;
%local loclindx fn lrecl reclen;

* Error checking *;
data _null_;
  * Grab input parameter to variable *;
  lib   = compress("&lib");
  lrecl = compress("&lrecl");

  * Default to WORK libname if none specified *;
  if lib = ' '
  then call symput('lib','WORK');

  * Check if LRECL is numeric *;
  if lrecl*1 = .
  then error "ERROR: LRECL parameter '&lrecl' is non-numeric.";
  else do;
         lrecl = floor(lrecl);
         call symput('lrecl', left(lrecl));
       end;
run;

/*
* Check to see if dataset or view actually exists. *;
proc sql noprint;
  validate
  select *
  from &lib..&data;
*/

* Generate Local Macrovariables *;
data _null_;
  length lib data $8;
  lib = upcase(compress("&lib"));
  data = upcase(compress("&data"));
  call symput('lib', compress(lib)); * Libname *;
  call symput('data', compress(data)); * Dataset or View *;
  call symput('fn', compress(lowcase(data))); * Filename w/o extension *;
run;

* Check number of observations in dataset or view. *;
* Call error if no observations found.             *;
proc sql noprint;
  select count(*) into :nobs
  from &lib..&data;
run;

%if %eval(&nobs) = 0
%then %do;
data _null_;
  error "ERROR: SAS dataset or view '&lib..&data' has no observations.";
run;
%end;

%if &sysver < 6.09 %then %do;
	proc contents data=&lib..&data noprint out=metadata;
	data metadata;
		set metadata end=eof;
		keep varnum name type format label length;
		reclen+length;
		if eof then do;
			call symput('reclen', left(put(reclen,4.0)));
			end;
	%end;
%else %do;
* Get metadata on SAS object *;
proc sql noprint;
  create table metadata as
  select varnum, name, type, format, label
  from dictionary.columns
  where libname = "&lib" and
        memname = "&data"
  order by 1;

%let vars = &sqlobs;

  * Correct LRECL parameter if too short *;
  select sum(length) into :reclen
  from dictionary.columns
  where libname = "&lib" and
        memname = "&data";
%end;
%if %eval(&lrecl) < %eval(&reclen)
%then %do;
	%put LRECL parameter increased from &lrecl to &reclen;
	%let lrecl = &reclen;
%end;


proc print data=metadata label noobs;
  title1 "Layout for Tab Delimited Text SAS Extract '&fn..txt'";
  title2 " ";
  title3 "Date: &sysdate  Time: &systime";
  var varnum name type format label;
  label name   = 'SAS Column Name'
        type   = 'SAS Column Type'
        format = 'SAS Column Format'
        label  = 'Column Label';
run;

* Generate macrovariable array for data _null_ put statement *;
data _null_;
  set metadata end=eof;
    by varnum;
  call symput('name'||left(varnum), compress(name));
  call symput('format'||left(varnum), compress(format));
run;

* Generate tab delimited flat file. *;
filename flat "&fn..txt";
data _null_;
  set &lib..&data;
  dlm = &dlm;
  file flat noprint lrecl=&lrecl;
  put
%do loclindx = 1 %to &vars;
    &&name&loclindx &&format&loclindx
  %if %eval(&loclindx) < %eval(&vars)
  %then dlm;
%end;
;
run;
%mend;

/*
* Sample usage program *;
options errorabend macrogen ls=128 ps=42;
proc sql;
  create table tables as
  select libname, memname
  from dictionary.tables
  where libname eq 'A951I'
  order by 1,2;

%let ntables = &sqlobs;

data _null_;
  set tables;
    by libname memname;
  call symput('lib'||left(_n_), compress(libname));
  call symput('data'||left(_n_), compress(memname));
run;

%macro runit;
%do i = 1 %to &ntables;
  %flatout(&&lib&i, &&data&i, 500)
%end;
%mend;

*runit
*/

