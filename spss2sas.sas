 /*--------------------------------------------------------------*
  *    Name: spss2sas.sas                                        *
  *   Title: Convert an SPSS file to SAS with value labels       *
        Doc: http://www.yorku.ca/dept/psych/lab/sas/spss2sas.htm  
  *--------------------------------------------------------------*
  *  Author:  original from USC, modified by M. Friendly         *
  * Created: 18 Apr 1999 10:14:47                                *
  * Revised:  4 Jan 2001 08:55:25                                *
  * Version: 1.1                                                 *
  *  1.1 Minor changes for SAS v8.01 & SPSS for Windows v10.0.5  *
  *      (from Mark Clements <markc@nswcc.org.au>)               *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SPSS2SAS macro converts an SPSS Portable (.por) file, with
 value labels (saved from SPSS in a text file) into a SAS data
 set and PROC FORMAT statements required to re-create the information
 in the SPSS file.

 The WWW documentation explains how to create and save the SPSS
 .POR file and the data dictionary.
  
=Usage:

 The SPSS2SAS macro may be run interactively or with keyword parameters.
 The macro is run interactively when invoked with no parameters (but
 in that case options to select variables or observations copied to
 the SAS file are not available):
 
	%spss2sas();
 
 It is run non-interactively when called with keyword parameters.
 In this case, the PORFILE= parameter must be given.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%spss2sas(porfile=spss.por, dictfile=spss.dct);
 
==Parameters:

* SPSSPATH=   Path to SPSS files

* PORFILE=    Name of SPSS portable file

* FILEPATH=   Path used for created output format files

* DICTFILE=   Name of SPSS data dictionary file.  If empty, the
              SPSS file is converted, but no value labels are processed.

* SASDS=      Name for SAS data set

* FMTPREF=    Prefix for format names [Default: FMTPREF=F]

* PERMTEMP=   SAS formats: PERManent or TEMPorary ? [Default: PERMTEMP=TEMP]

* KEEP=       List of variables to keep. [Default: _all_]

* DROP=       List of variables to drop. [Default: none]

* WHERE=      Expression to subset observations

* CONTENTS=   Display data set contents? [Default: CONTENTS=Y]

* SORTBY=     Determines whether the input variables from the SPSS file
              are sorted by variable name (VARNAME) or variable position
				  (VARPOS) in creating the names of FORMATS.
                

 =*/

%macro spss2sas(
	spsspath=,       /* path to SPSS files */
	porfile=,        /* name of SPSS portable file */
	filepath=,       /* path used for created output format files */
	dictfile=,       /* name of SPSS data dictionary file */
	sasds=,          /* name for SAS data set */
	fmtpref=f,       /* prefix for format names */
	permtemp=TEMP,   /* SAS formats: PERManent or TEMPorary ? */
	keep=,           /* list of variables to keep */
	drop=,           /* list of variables to drop */
	where=,          /* expression to subset observations */
	contents=Y,      /* display data set contents? */
	sortby=varname   /* sort variables by VARNAME or VARPOS (position) */
	);
	
%if %length(&porfile)=0 
	%then %do;

  dm 'clear log;clear output';
  %let porfile=spss.por;
  %let dictfile=spss.dct;
  %let sasds=new1;
  %let permtemp=temp;


***define windows;
%window convert1 irow=2 rows=22 icolumn=5 columns=73
    #1 @4 "Date: &sysday., &sysdate.."
    #2 @4 "SPSS to SAS Conversion Program"
    #4 @7  "This program is for SPSS data files that have value labels"
    #5 @7  "If you have no value labels, use the much simpler programs"
    #6 @7  "found at http://www.usc.edu/ucs/userserv/statistics/sas/faq/"
    #9 @7  "You will need two files in order to run this program:"
   #10 @10 "--SPSS Transport (Portable) file"
   #11 @10 "--Text file containing the SPSS dictionary information"
   #13 @7  "Instructions on how to extract these files from an SPSS"
   #14 @7  "SAVE (*.sav) file are at the WWW site listed above"
   #16 @4  "To Continue, Press" @24 "ENTER" attr=underline
   #17 @4  "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;

%window convert2 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #4 @7  "Running this program produces several SAS files:"
    #6 @7  "  --SAS data set with imbedded format references"
    #7 @7  "  --PROC FORMAT program, stored in a *.prc file for future use "
    #8 @7  "  --FORMAT statement, stored in a *.fmt file for future use"
   #10 @7  "Optionally, you may also ask SAS to create or update a"
   #11 @7  "permanent format library by choosing PERMANENT in one of the"
   #12 @7  "windows that follow"
   #16 @4  "To Continue, Press" @24 "ENTER" attr=underline
   #17 @4  "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;

%window convert3 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #4 @6 "If your SPSS Portable file and Dictionary output file are in"
    #5 @6 "a directory other than the one where you started this program"
    #6 @7 "enter that directory path here (otherwise leave blank):"
    #7 @9 "NOTE: end with a slash, as in c:\tmp\  or  ~/dir1/"
    #8 @10 spsspath 40 attr=underline
   #10 @5 "Enter the name of your SPSS transport (.por) file here:"
   #11 @20 porfile 45 attr=underline
   #13 @5 "Enter the name of your SPSS dictionary output here:"
   #14 @20 dictfile 45 attr=underline
   #16 @4 "To Continue, Press" @24 "ENTER" attr=underline
   #17 @4 "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;

%window convert4 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #4 @6 "SAS will store your SAS data set wherever you choose"
    #5 @6 "By default, SAS will use the current directory (the directory"
    #6 @6 "where you launched this program), or you can specify another"
    #8 @6 "Enter the library location (subdirectory path) for the"
    #9 @6 "subdirectory you want SAS to use to store the SAS Data Set:"
   #10 @9 "NOTE: end with a slash, as in c:\tmp\  or  ~/dir1/"
   #11 @10 libpath 40 attr=underline
   #13 @6 "What would you like the new SAS data set to be called?"
   #14 @6 "This same name will be used for the FORMAT programs made here."
   #15 @8 "(eight characters or less, beginning with a character):"
   #16 @20 sasds 8 attr=underline
   #17 @4 "To Continue, Press" @24 "ENTER" attr=underline
   #18 @4 "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;


%window convert5 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #4 @6 "This program creates two output files for itself and for your"
    #5 @6 "potential future use:"
    #6 @6 "  --the PROC FORMAT program that creates and stores the formats"
    #7 @6 "  --the FORMAT statement that is used in the DATA STEP"
    #9 @6 "By default, SAS will use the current directory (the directory"
   #10 @6 "where you launched this program), or you can specify another"
   #11 @6 "Enter the library location (subdirectory path) for the"
   #12 @6 "subdirectory you want SAS to use to store the two files"
   #13 @6 "(Can be the same directory you specified above if you wish):"
   #14 @9 "NOTE: end with a slash, as in c:\tmp\  or  ~/dir1/"
   #15 @10 filepath 40 attr=underline
   #17 @4 "To Continue, Press" @24 "ENTER" attr=underline
   #18 @4 "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;

%window convert6 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #5 @6 "Finally, you will need to choose a format prefix and "
    #6 @6 "decide whether you want TEMPORARY or PERMANENT formats"
    #8 @6 "The format prefix can be any one or two-character combination,"
    #9 @6 "such as 'a' or 'xy'.  This prefix will become the first part"
   #10 @6 "of the format names created here.  Choose a prefix different"
   #11 @6 "from any you might have used before, to keep format names unique."
   #13  @10 "Format Prefix:" @25 fmtpref 2 attr=underline
   #17 @4 "To Continue, Press" @24 "ENTER" attr=underline
   #18 @4 "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;

%window convert7 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #6 @6 "Formats can be stored TEMPORARILY (requiring PROC FORMAT to be"
    #7 @6 "run each time the SAS Data Set is accessed) or PERMANENTLY "
    #8 @6 "(requiring the format library to be present whenever the SAS"
    #9 @6 "Data Set is accessed)."
   #10 @30 "Enter PERM or TEMP:" @50 permtemp 4 attr=underline
   #12 @6 "Permanent formats will be stored in the same directory you"
   #13 @6 "chose above for the SAS Data Set, in a file called 'formats.sc2'"
   #14 @6 "or 'formats.sct01' -- you can move them later, if you wish."
   #17 @4 "To Continue, Press" @24 "ENTER" attr=underline
   #18 @4 "    to Exit, go to Command Line (CTRL-U) and type"
       @55 "QUIT" attr=underline;

  %display convert1;  %if &syscmd=QUIT %then %goto done;
  %display convert2;  %if &syscmd=QUIT %then %goto done;
  %display convert3;  %if &syscmd=QUIT %then %goto done;

  %let filepath=&spsspath.;

  %display convert4;  %if &syscmd=QUIT %then %goto done;
  %display convert5;  %if &syscmd=QUIT %then %goto done;
  %display convert6;  %if &syscmd=QUIT %then %goto done;
  %display convert7;  %if &syscmd=QUIT %then %goto done;

%end;

  %let libpath=&spsspath.;
  %if %nrstr(&libpath.) ne  %then %do;
		libname convert "&libpath.";
		libname library "&libpath.";
		%end;
  %else %if &libpath.=  %then %do;
		libname convert '.';
		libname library '.';
		%end;

  %let pt=%upcase(%substr(&permtemp,1,1));

%if %length(&dictfile)>0 %then %do;

	/*
	%if %sysfunc(fileexist(&spsspath.&dictfile) = 0 %then %do;
		%put Dictionary file &spsspath.&dictfile does not exist;
		%goto copy_;
		%end;
	*/		
	%put Reading SPSS dictionary file &spsspath.&dictfile ...;
data one;
  infile "&spsspath.&dictfile." truncover;
  order+1;
*  drop test2;
  length varname $ 8;
  retain varname; retain varpos 1;
  input @1 test1 $9. @13 test2 $11. @9 value $7.
        @20 label $40. @72 test3 $8.;
  if index(test1,':')=0 and index(test1,'Name')=0 and
     test1 ne ' ' then varname=test1;
  if substr(varname,1,1)='0c'x then varname=translate(varname,' ','0c'x);
  if index(varname,'@')>0 then varname=translate(varname,'_','@');
  if substr(test1,1,1)='0c'x then test1=translate(test1,' ','0c'x);
  if varname ne lag(varname) then do;
  		value=' '; label=' ';  varpos+1;
		end;
  if substr(test1,1,1)='-' or
     test1='Preceding' or
	  test1='File:' or
     (test1=' ' and test2='List of var') or
     (test1='List of' and test3='Position') or
     test2 = 'SPSS for MS' or
     varname=' ' or
     substr(varname,1,1) in
       ('-','1','2', '3','4','5','6','7','8','9','0') or
     index(varname,'|')>0 or
     (varname='Notes' and lag2(varname)='File In') or
     varname='File In' or
     index(value,'Print')>0 or
     index(value,'Write')>0 or
     index(value,'Value')>0 or
     index(value,'Missi')>0 or
     index(label,'Unix, Release')>0 or
     index(label,'Southern Cal')>0 or
     index(value,'Measu')>0 or index(value,'Colum')>0 or
     value='List of' or
     value=' '
     then delete;
  if scan(test2,2)='M' then miss=1;
  if substr(value,1,1) in ('1','2', '3','4','5','6','7','8','9','0')
     then chartag=' ';
	  else chartag='$';

*proc print data=one;

proc sort data=one;
	by &sortby order;

data three;
  set one end=out;
  z=symget('fmtpref');
  if z=' ' then z='f';
  if varname ne lag(varname) then counter+1;
  if chartag ne ' ' then
     fmtname='$'||trim(left(z))||trim(left(counter))||'fmt';
  else if chartag eq ' ' then
     fmtname=trim(left(z))||trim(left(counter))||'fmt';

%put Writing the PROC FORMAT statement file &filepath.&sasds..prc ...;
data _null_;
	file "&filepath.&sasds..prc";
  	put " /*  &sasds..prc   created &sysday, &sysdate &systime from &spsspath.&porfile */";
  %if &pt.=P %then %do;
    put "proc format library=library;"; %end;
  %else %do;
    put "proc format;"; %end;

/*** create the PROC FORMAT statement file ***/
data _null_;
  set three;
  by &sortby; 
  file "&filepath.&sasds..prc" mod;
  if index(label,'"')>0
  		then labelout="'"||trim(left(label))||"'";
  		else labelout='"'||trim(left(label))||'"';
	vout = '   /* ' || trim(left(varname)) || ' */';
  
  if first.&sortby then put @1 'value' @7 fmtname  vout /
     @9 value '= ' labelout ;
     else put @9 value '= ' labelout;
  if last.&sortby then put @1 ';';

%put Writing the FORMAT statement file &filepath.&sasds..fmt ...;
/*** creation of FORMAT statement file ***/
data _null_;
  set three end=out;
  by &sortby;
  file "&filepath.&sasds..fmt";
  fmtthis=trim(left(fmtname))||'.';
  if _n_=1 then do;
  		put " /*  &sasds..fmt   created &sysday, &sysdate &systime from &spsspath.&porfile */";
  		put @1 'format' @8 varname fmtthis;
		end;
  else if first.&sortby then put @8 varname fmtthis;
  if out then put ';';

%include "&filepath.&sasds..prc";
%end;  /* %if &dictfile */


%copy_:
libname spssin spss "&spsspath.&porfile.";
%put Copying SPSS file &spsspath.&porfile ...;
proc copy in=spssin out=convert;

%put Writing SAS dataset convert.&sasds ...;
data convert.&sasds.;
  set convert._first_;
  %if %length(&keep) %then %do;
  		keep &keep;
		%put %str(   )keep &keep;
		%end;
  %if %length(&drop) %then %do;
  		drop &drop;
		%put %str(   )drop &drop;
		%end;
  %if %length(&where) %then %do;
  		where (&where);
		%put %str(   )where &where;
		%end;
%if %length(&dictfile)>0 %then %do;
  %include "&filepath.&sasds..fmt";
%end;

options nonotes;
proc datasets nolist library=convert; delete _first_;
proc datasets nolist library=work; delete one three;
options notes;

%if %upcase(&contents)=Y %then %do;
	proc contents data=convert.&sasds; run;
	%end;

%put SPSS to SAS Conversion Program: Done;
%put Check that the following files were created correctly:;
%put;
%put %str(    )SAS data set %str(                )convert.&sasds;
%put %str(    )PROC FORMAT statements %str(      )&sasds..prc;
%put %str(    )DATA step FORMAT statement %str(  )&sasds..fmt;
%if &permtemp=PERM %then %do;
	%if &sysscp=WIN %then %do;
		%put %str(    )FORMATS.SC2 formats library;
		%end;
	%else %do;
		%put %str(    )FORMATS.SCT01 formats library;
		%end;
	%end;

%window convert8 irow=2 rows=22 icolumn=5 columns=73
    #2 @4 "SPSS to SAS Conversion Program"
    #4 @6 "The conversion process is now complete."
    #6 @6 "Check to see that the following files were created correctly:"
    #7 @6 "  --SAS Data Set with imbedded format references"
    #8 @6 "    (this can be checked by running PROC CONTENTS in SAS;"
    #9 @6 "    specific formats can be checked by running PROC FREQ on"
   #10 @6 "    variables that should have formats assigned)"
   #11 @6 "  --formats.sct01 or formats.sc2 (if PERM was selected for"
   #12 @6 "    the format library)"
   #13 @6 "  --&sasds..prc (the PROC FORMAT program statements)"
   #14 @6 "  --&sasds..fmt (the FORMAT statement used in the DATA STEP)"
   #17 @4 "To End this Program, Press" @31 "ENTER" attr=underline;

  *display convert8;

%done:
%mend spss2sas;

*spss2sas

*run;
