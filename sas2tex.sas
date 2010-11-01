 /*-------------------------------------------------------------------*
  *    Name: sas2tex.sas                                              *
  *   Title: Generate a LaTeX table from a SAS dataset                *                          
  *-------------------------------------------------------------------*
  * Author:  Michael Friendly              <friendly@yorku.ca>;
  * Created: 25 Jan 1998 12:00:27
  * Revised: 30 Apr 1998 11:12:21
  * Version 1.2
  *  -- Handles only simple tables (no \multicolumn, \cline, etc)
  *  -- Determines reasonable formats, column labels, justification
  *     from the data set info.  You probabily want to fix them up
  *     anyway.
  * 1.1 Added where= option to select observations
  *  -- Added by= option for multiple tables
  * 1.2 Added ability to handle abbreviated var lists (X1-X12)
  *-------------------------------------------------------------------*/

%macro sas2tex(
   data=_last_,        /* name of input dataset                     */
	id=,                /* ID variable(s), or _NULL_                 */ 
   var=,               /* list of variables to be printed           */
	just=,              /* var justification: rrrlllcc               */
	miss=,              /* what to print for missing data            */ 
	where=,             /* where clause, use where=%str(var=val)     */
	by=,                /* separate tables for each by value         */
	out=FILE,           /* output fileref: FILE, PRINT, STDOUT       */
	outdir=./,          /* directory for output file                 */
	outfile=,           /* name of output TeX file                   */
   caption=,           /* table caption text                        */
	tabtype=table,      /* table type: TABLE|SIDEWAYSTABLE|LONGTABLE */ 
	tabid=tab:&data,    /* table \label{} value                      */
	tabpos=htb,         /* table float position                      */
	tabenv=,            /* table environment, eg, \small             */
	headenv=,           /* environment for headers, eg, \bfseries    */
	vrule=,             /* | for vertical rules, else null           */
	texalone=,          /* produce stand-alone document?             */
	ls=120              /* output linesize                           */ 
	);


%*-- Output is either to a file (fileref TeXOUT) or to the listing
     (fileref PRINT).  If FILE, try to assign default name.;
	  
%if %nrbquote(%upcase(&out))=FILE %then %do;
	%if &outfile eq %str() %then %do;
		%let outfile = &data..tex;
		%put NOTE: Output File Name was not specified, so writing to &outfile.;
		%end;
	%else %if %index(&outfile,%str(.)) = 0 %then %let outfile = &outfile..tex;	
	filename texout "&outdir.&outfile";
	%let out=texout;
%end;


%*-- Get variable name, type, label & format info;
data _tmp_;
	set &data;
	stop;

%*** use summary to reorder the variables in order of var list;
proc summary data=_tmp_(firstobs=1 obs=1);
	id &var &id;
	output out=_tmp1_(drop=_TYPE_ _FREQ_);
	
proc contents data=_tmp1_(keep=&var &id) noprint 
	out=_vars_(keep=name type length label format formatl formatd npos);
	run;
	%*** sort by position of variables in the list;
proc sort data=_vars_;
	by npos;
proc print;

%if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 data _null_;
 	set _vars_ end=eof;
	length vars $200;
	retain vars;
	ok=1;
	%if %length(&id) %then %do;
*		if name=upcase("&id") then ok=0;
		if index(upcase("&id"),trim(name)) then ok=0;
		%end;
	if ok then vars = trim(vars) || ' ' || name;
	if eof then do;
		put vars=;
		call symput('var', trim(left(vars)));
		end;
run;
%end;
data _null_;
	set _vars_ end=eof;
	length aligni alignv align $100;
	retain aligni alignv;

	%*-- Store variable info in macro variables: name1, type1, etc;
	call symput("name"||left(put(_n_,5.)),trim(name));
	call symput("type"||left(put(_n_,5.)),put(type,1.));
	call symput("len"||left(put(_n_,5.)),put(length,3.));
	call symput("lab"||left(put(_n_,5.)),trim(label));

	vars = upcase("&var");
	if type=1      /* numeric */
		then if index(vars,trim(name)) 
			then alignv=trim(alignv) || 'r';
			else aligni=trim(aligni) || 'r';
		else       /* character */
		    if index(vars,trim(name)) 
			then alignv=trim(alignv) || 'l';
			else aligni=trim(aligni) || 'l';
	put name= type= label= aligni= alignv= vars=;	

	if format=' ' then do;
		if type=1 then format=" ";
		else format="CHAR.";
		end;
	else do;
		if formatl>0 then format = trim(format) ||trim(left(put(formatl,3.)));
		format = trim(format)||'.';
		if formatd>0 then format = trim(format) ||trim(left(put(formatd,3.)));
		end;
	call symput("fmt"||left(put(_n_,5.)),trim(format));
	if eof then do;
		call symput('nv', trim(left(put(_n_,5.))));
		align = "&vrule" || trim(aligni) || alignv || "&vrule";
		call symput('align', trim(left(align)));
		put align=;
		end;
run;

data _null_;
	set &data end=last;
	length tabid $30 var $50;
	%if &where ^= %str() %then %do;
		where (&where);
	%end;
	%if &by ^= %str() %then %do;
		by &by;
	%end;

	file &out notitle noprint lrecl=&ls;
			
		%* -- Table header --;
		%if &by ^= %str() %then %do;
			if first.&by then do;
			_by_+1;
			tabid = trim("&tabid" || '-' || left(put(_by_,3.)));
		%end;
		%else %do;
			if _n_=1 then do;
			tabid = "&tabid";
			%if %length(&texalone) %then %do;
				put "\documentclass{article}";
				put "\begin{document}";
				%end;
		%end;
			put "\begin{&tabtype}[&tabpos]";
			put "  \caption{&caption}";
			put '  \label{' tabid +(-1) '}';
			%if (%length(&tabenv)) %then %do;
					put "  {&tabenv";
			%end;
			put '  \begin{center}';
			put "    \begin{tabular}{&align}";

			%if &by ^= %str() %then %do;
				put _by_;
			%end;
			%header(&id,&var);
			end;	/* end first.&by or _n_=1 */
	
		%*-- Print the table rows;
		%row(&id,&var);
	
		%*-- Finish up;
		if last 
		%if &by ^= %str() %then %do;
			| last.&by
		%end;		
			then do;
			put '    \hline';
			put '    \end{tabular}';
			put '  \end{center}';
			%if (%length(&tabenv)) %then %do;
					put "  }";
			%end;
			put "\end{&tabtype}";
			%if %length(&texalone) %then %do;
				put "\end{document}";
				%end;
			end;
	run;	
%done:;

%mend;

%*-- Macro to output one table row;
%macro row(id,varlist);
	%global cols ids;
		%if &id ^= _NULL_ %then %do;
			%if &id = %str() %then %do;
				put @6  _n_ '& ' @;
			%end;

			%else %do i=1 %to &ids;
				%let j = %eval(&i+&cols);
				%let var = %scan(&id, &i);
				put  &var &&fmt&j  '& ' @;
				%end;
		%end;
		
		%do i = 1 %to &cols;
			%let var = %scan(&varlist, &i);
			type = &&type&i;
			if type=1 then do;
				if &var = . 
					then put &miss @;
					else put  &var &&fmt&i @;
				end;
			else do;
				put  &var &&fmt&i @;
				end;
			%if &i < &cols %then %do;
				put '& ' @;
				%end;
			%end;
		put  ' \\';
%mend;

%*-- Macro to output table headings;
%macro header(id, varlist);
	%global cols ids;
	%let cols = %numwords(&varlist);
	%let ids = %numwords(&id);
*	length var $40;
		%*-- Output header cells for variable names or labels;
		put @6 '\hline';
		%if &id ^= _NULL_ %then %do;
			%if &id = %str() %then %do;
				put @6 'Obs &' @;
			%end;

			%else %do i=1 %to &ids;
				%let j = %eval(&i+&cols);
				var = "&&lab&j";
				if var = ' ' then
					var = "%scan(&id, &i)";
				%if (%length(&headenv)) %then %do;
						var = "{&headenv " || trim(var)  || '}' ;
				%end;
				put ' ' var  '& ' @;
				%end;
		%end;
		
		%do i = 1 %to &cols;
			%* -- Use variable label if it exists;
			var = "&&lab&i";
			if var = ' ' then
				var = "%scan(&varlist, &i)";
			%if (%length(&headenv)) %then %do;
					var = "{&headenv " || trim(var)  || '}' ;
			%end;
			put ' ' var @;
			%if &i < &cols %then %do;
				put '& ' @;
				%end;
			%end;
		put ' \\';
		put @6 '\hline';
%mend;

%macro numwords(lst,wordchar);
   %let i = 1;
   %if (%length(&wordchar)) %then %do;
      %let v = %scan(&lst,&i,&wordchar);
      %do %while (%length(&v) > 0);
         %let i = %eval(&i + 1);
         %let v = %scan(&lst,&i,&wordchar);
         %end;
      %end;
   %else %do;
      %let v = %scan(&lst,&i);
      %do %while (%length(&v) > 0);
         %let i = %eval(&i + 1);
         %let v = %scan(&lst,&i);
         %end;
      %end;
   %eval(&i - 1)
%mend;


