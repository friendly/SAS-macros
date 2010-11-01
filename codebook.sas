%macro codebook(
	data=, 
	library=work, 
	maxfmts=0, 
	def_othr=yes,
    w_label=20, 
	w_format=20, 
	w_raw=9, 
	addvalue=yes,
	sortby=name,      /* sorby: name or varnum */
	out=codebook ) ;

/*------------------------------------------------------------------
 This macro creates a codebook for a SAS data set, describing
 each of the variables in the data set, its length, type, label,
 user-defined format, and the possible values it can have based on
 the format assigned to it. It does this by combining the output
 of a PROC CONTENTS and a PROC FORMATS and printing out the
 results using PROC REPORT.

 The macro has the following arguments:

 data       Identifies the SAS data set to document.
 library    Identifies the libname that is assigned to the format
            library holding the formats for the data.
 maxfmts    Restricts the number of entries of output for each
            variable.  If more are to be printed, a note is
            displayed indicating as such.  Zero means all entries
            print.
 def_othr   Designates which formats do not have a formatted value
            for 'other' values.  Is not in effect if addvalue=no.
 w_label    Defines the width of the field holding the variable
            label.  Labels in excess of this width will wrap.
 w_format   Defines the width of the formated values for each
            variable.
 addvalue   Determines whether unformatted values of each variable
            will display in addition to the formatted values.
 w_raw      Defines the width of the unformatted values in the
            output.
 out        Names the output data set

 Note: By manipulating the PAGESIZE and LINESIZE options of your
       SAS session, as well as the W_LABEL, W_FORMAT and W_RAW you
       can orient the output to either a landscape or portrait
       mode.

 Example :

 %codebook(data=sas.data,library=library, def_othr=yes,
         maxfmts=5, w_label=40,w_format=20, addvalue=yes, w_raw=9)

*-------------------------------------------------------------------
 CODEBOOK (was DOCSPGM)
 Written By : Steve James  spj1@cdc.gov
 Last Updated 1/22/1997
 Modified By : Michael Friendly  friendly@yorku.ca
 Last Updated 16 Feb 1999 10:11:33
*-------------------------------------------------------------------*/

%let dsn = %scan(&data,2,'.') ;
%if &dsn =
%then %do ;
      %let libname = work ;
      %let dsn = &data ;
      %end ;
%else %let libname = %scan(&data,1,'.') ;

options pageno=1 nonotes;

proc format ;
  value vartype 1='Num' 2='Char' ;
  run;

*-------------------------------------------------------------------;
* Now create the data set that contains the formats and their       ;
* possible values by taking the output from a PROC FORMAT.          ;
*-------------------------------------------------------------------;

proc format library=&library
  cntlout=formats(rename=(fmtname=format label=fmtlabel type=ftype)
                  keep=fmtname type label start end sexcl eexcl hlo
                  where=(ftype in ('N','C'))) ;
  run ;

*-------------------------------------------------------------------;
* Create a data set with a list of all the variables and any        ;
* user-defined format names.                                        ;
*-------------------------------------------------------------------;

proc contents data=&data noprint
             out=vars(keep=name type format label length engine nobs varnum) ;
  run ;

*---------------------------------------------------------------------;
* Combine the two data sets by format name.  PROC SQL is because MERGE;
* will not handle the case where the same format is used for more than;
* one variable, and there is no need to strip the leading '$' from the;
* name of character formats.                                          ;
*---------------------------------------------------------------------;

proc sql ;
  create table &out as
  select vars.name, vars.type, vars.format, vars.label, vars.length,
         vars.engine, formats.start, vars.varnum,
		 formats.end, formats.fmtlabel,
         vars.nobs, formats.sexcl, formats.eexcl, formats.hlo
  from vars left join formats
  on scan(vars.format,1,'$') = formats.format
  order by &sortby, start
  ;

data &out ;
  length format $10 rawvalue $&w_raw fmtlabel $&w_format
         connect $3 typelen $9 ;
  set &out end=eof;
  by &sortby ;
  drop engine nobs start end sexcl eexcl hlo;
  
  *-- determine maximum label width;
  retain maxlab maxfmt 0;
  maxlab = max(maxlab, length(label));
  maxfmt = max(maxfmt, length(fmtlabel));
  if eof then do;
  	call symput('maxlab',left(trim(put(maxlab,8.))));
  	call symput('maxfmt',left(trim(put(maxfmt,8.))));
	end;
  
  if type = 1 then typelen = 'Num:' || left(put(length,3.));
              else typelen = 'Char:' || left(put(length,3.));

  if format = ' ' then format ='* None *' ;

  if start = end then rawvalue = trim(left(start)) ;
  else do ;
       connect = '-' ;
       if sexcl = 'Y' and eexcl='Y' then connect = '<-<' ;
       else if sexcl = 'Y' then connect = '<-' ;
       else if eexcl = 'Y' then connect = '-<' ;
       rawvalue = trim(left(start)) || trim(left(connect))
                  || trim(left(end)) ;
       end ;

  *----------------------------------------------------;
  * If MAXFMTS is specified, count the number of      ;
  * entries for each variable and delete those past the;
  * limit.                                             ;
  *----------------------------------------------------;
  if &maxfmts > 0
  then do ;
       if first.name then count= 0 ;
       count+1 ;
       if count = &maxfmts and not last.name
          then fmtlabel = '... and more ...' ;
          else if count > &maxfmts then delete ;
       end ;

  output  ;
 %if %upcase(&def_othr) = YES and %upcase(&addvalue)=YES
 %then %do ;
       if last.name and start ne '**OTHER**' and format ne '* None *'
       then do ;
             rawvalue = '**OTHER**' ;
             fmtlabel = '*No Format*' ;
             output ;
             end ;
        %end ;
  run;

%if %length(w_label)=0 %then %let w_label = &maxlab;
%if &maxlab < &w_label %then %let w_label = &maxlab;

*-------------------------------------------------------------------;
* The number of observations in the data set VARS equals the number ;
* of variables in the data set being documented.  Also in that      ;
* data set is the number of observations.  Get that information     ;
* about the data set and assign it and other known data to macro    ;
* variables to be used later as a header.                           ;
*-------------------------------------------------------------------;

%if &sysver > 6.09 %then %do;
data _null_ ;
  set sashelp.vslib ;
  if libname = upcase("&libname")
  then do ;
       call symput('path',trim(left(path))) ;
       stop ;
       end ;
  run ;
%end;
%else %let path=&libname;

data header ;
  length heading $25 data $80 ;
  set vars point=_n_ nobs=numvars ;

  %headout(Host Path or Library Name, upcase("&path")) ;
  %headout(SAS Data Set Name, upcase("&dsn"));
  %headout(SAS Data Engine, engine) ;
  %headout(Number of Variables, left(put(numvars,comma5.))) ;
  %headout(Number of Observations, left(put(nobs,comma9.)));

  stop ;
  run ;

proc report data=header nowd headskip ;
  column heading data ;
  define heading / display ' ' ;
  define data / display ' ' width=40 flow ;
  run ;

*-------------------------------------------------------------------;
* Now use PROC REPORT to print out the data.                        ;
*-------------------------------------------------------------------;

%let print = ;
proc report data = &out nowindows headskip headline missing split='/' ;
  column name typelen label format rawvalue fmtlabel ;
  define name / Group width=8 'Variable/Name' ;
  define typelen / Group width=9 'Type:/Length' left ;
  define label / Group width=&w_label 'Label'
      %if &maxfmts < 40 %then flow ;;
  define format  / Group width=10 'Format/Name' ;
  define rawvalue / group 'Raw/Value' width=&w_raw
     %if %upcase(&addvalue) eq NO %then noprint ;
     ;
  define fmtlabel / group width=&w_format 'Possible/Values' ;
  break after name / suppress skip ;
  run ;

options notes;
%mend ;

%macro headout(heading, data);
	heading="&heading";  data=&data;
	output;
%mend;
