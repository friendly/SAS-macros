 /*-------------------------------------------------------------------*
  *    Name: htmltab.sas                                              *
  *   Title: Generate an HTML table from a SAS dataset                *                          
  *-------------------------------------------------------------------*
  * Author:  Michael Friendly              <friendly@yorku.ca>;
  * Revised: Mon May 20 12:18:17 EDT 1996
  * Version 1.0
  *  -- made macro name/file name the same for autocall use
  *  -- fixed bug where column labels were not ordered properly
  *  -- Added where= option to select observations
  *  -- Added by= option for multiple tables
  *-------------------------------------------------------------------*/

%macro htmltab(
   data=_last_,        /* name of input dataset                   */
	where=,             /* where clause, use where=%str(var=val)   */
	by=,                /* separate tables for each by value       */
	out=FILE,           /* output fileref: FILE, PRINT, STDOUT     */
	outfile=,           /* name of output HTM[L] file              */
	tmpfile=table,      /* name of temp file                       */
	tabid=&data,        /* table NAME= attribute                   */
	id=,                /* ID variable, or _NULL_                  */ 
   vars=,              /* list of variables to be printed         */ 
   caption=,           /* table caption text                      */ 
   tattrib=BORDER,     /* <table > attributes                     */
   colspec=,           /* list of column alignments and widths [not used]*/
	ls=80,              /* output linesize                         */ 
   htmlver=3           /* HTML table style                        */
	);


%*-- Output is either to a file (fileref HTMLOUT) or to the listing
     (fileref PRINT).  If FILE, try to assign default name based on
	  operating system.  Not very general here (due to wide variety of
	  values that &sysscp can take on);
	  
%if %nrbquote(%upcase(&out))=FILE %then %do;
	%if &outfile eq %str() %then %do;
		%if &sysscp = WIN 
			%then %let outfile = table.htm;
			%else %let outfile = table.html;
		%put NOTE: Output File Name was not specified, so writing to &outfile.;
		%end;	
	filename htmlout "&outfile";
	%let out=htmlout;
%end;

%if &htmlver < 3 %then %do;
%*-------------------------------------------------------;
%* -- HTML 2: Use <pre>, </pre> around proc print results;
%*-------------------------------------------------------;
	%tempfile(&tmpfile,&ls);

	proc printto new print=table;
	options nocenter nodate ls=&ls;
	proc print data=&data label;
		%if &where ^= %str() %then %do;
			where (&where);
		%end;
		%if &id ^= %str() %then %do;
			id &id;
		%end;
		%if &by ^= %str() %then %do;
			by &by;
		%end;
		var &vars ;
	proc printto print=print;

	data _null_;
		file &out notitle noprint lrecl=&ls;
		length string $&ls;
		infile table length=len end=last;
		if _n_=1 then do;
			*-- Eat the first (title) line;
			input @1 string $varying. len;
			put /;
			%if %length(&caption) > 0 %then %do;
				put '<a name="#' @;
				put "&tabid"  '">' "&caption</a>"; 
				%end;
			put / '<pre>';
			end;
		input @1 string $varying. len;
		%*-- Handle carriage control chars in col 1;
		%if &sysscp = CMS %then %do;
			cc=substr(string,1,1);
			string=substr(string,2);
			if cc='1' then put _page_;
		%end;
			put string $char.;
		%*-- Finish up;
		if last then do;
			put '</pre>';
			end;	

	%end;


%*-------------------------------------------------------;
%* -- HTML 3: Use <table>, </table> and other table tags ;
%*-------------------------------------------------------;
%else %do;
	%*-- Get variable name, type, label & format info;
	data _tmp_;
		set &data;
		stop;
	%*** use summary to reorder the variables in order of var list;
	proc summary data=_tmp_(firstobs=1 obs=1);
		id &vars &id;
		output out=_tmp1_(drop=_TYPE_ _FREQ_);
		
	proc contents data=_tmp1_(keep=&vars &id) 
		noprint out=_vars_(keep=name type length label format npos);run;
	%*** sort by position of variables in the list;
	proc sort data=_vars_;
	   by npos;
*proc print;

	data _null_;
		set _vars_;
		%*-- Store variable info in macro variables: name1, type1, etc;
		call symput("name"||left(put(_n_,5.)),trim(name));
      call symput("type"||left(put(_n_,5.)),put(type,1.));
      call symput("len"||left(put(_n_,5.)),put(length,3.));
		call symput("lab"||left(put(_n_,5.)),trim(label));

		if format=' ' then do;
			if type=1 then format="BEST.";
			else format="CHAR.";
		end;
      call symput("fmt"||left(put(_n_,5.)),trim(format));
	run;

	data _null_;
		set &data end=last;
		length tabid $30;
		%if &where ^= %str() %then %do;
			where (&where);
		%end;
		%if &by ^= %str() %then %do;
			by &by;
		%end;

*		file &out lrecl=&ls;
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
		%end;
			put '<table ' @;
			put 'id="#' tabid  '"' @;
			%if &tattrib ^= %str() %then %do;
				put +1 "&tattrib" @;
				%end;
			put '>';
			put "  <caption align=top>&caption" @;
			%if &by ^= %str() %then %do;
				put _by_;
			%end;
			put '  </caption>';
			%header(&id,&vars);
			end;
	
		%*-- Print the table rows;
		%row(&id,&vars);
	
		%*-- Finish up;
		if last 
		%if &by ^= %str() %then %do;
			| last.&by
		%end;		
			then do;
			put '</table>';
			end;
	run;	
%end;
%done:;
*tempdel;

%mend;

%macro row(id,varlist);
	%global cols;
	%if &htmlver >= 3 %then %do;
		put '  <tr>';
		%*-- Use <th> for ID variable --> font=bold;
		%if &id ^= _NULL_ %then %do;
			%if &id = %str() %then %do;
				put @6 '<th>' _n_ '</th>' @;
			%end;
			%else %do;
				put @6 '<th>' &id  +(-1) '</th>' @;
				%end;
		%end;
		
		%do i = 1 %to &cols;
			%let var = %scan(&varlist, &i);
			type = &&type&i;
			if type=1 
				then put '  <td align=right>' &var  +(-1) '</td>' @;
				else put '  <td align=left >' &var  +(-1) '</td>' @;
			%end;
		put / '  </tr>';
	%end;
%mend;

%macro header(id, varlist);
	%global cols;
	%let cols = %numwords(&varlist);
	length var $40;
		%*-- Output header cells for variable names or labels;
		%*-- default alignment is <th align=center>, so no need to specify;	
		put  '  <tr>';
		%if &id ^= _NULL_ %then %do;
			%if &id = %str() %then %do;
				put @6 '<th>Obs</th>' @;
			%end;
			%else %do;
				%let i=%eval(&cols+1);
				var = "&&lab&i";
				if var = ' ' then
					var = "%upcase(&id)";
				put  @6 '<th>' var  +(-1) '</th>' @;
				%end;
		%end;
		
		%do i = 1 %to &cols;
			%* -- Use variable label if it exists;
			var = "&&lab&i";
			if var = ' ' then
				var = "%upcase(%scan(&varlist, &i))";
			put ' <th>' var  +(-1) '</th>' @;
			%end;
		put / '  </tr>';
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

%macro tempfile(file, ls);
	%global tempfn;
	%if &sysscp = CMS
		%then %let tempfn=&file output a;
	%else %if &sysscp = WIN
		%then %let tempfn=c:\&file..out;
	%else 	/* or flavors of ... UNIX */
    	    %let tempfn=/tmp/&file..out;
	filename table "&tempfn" lrecl=&ls ;
%mend;

%macro tempdel;
	%global tempfn;
	%if &sysscp = CMS
		%then cms erase &tempfn;
	%else %if &sysscp = WIN
		%then %do;
			options noxsync noxwait; run;
			x erase &tempfn;   run;
			options   xsync   xwait; run;
		%end;
	%else   x rm &tempfn;	/* or UNIX */
		
%mend;
		
