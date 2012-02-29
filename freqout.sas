 /*-------------------------------------------------------------------*
  * Name:    freqout.sas                                              *
  * Title:   Extract measures from PROC FREQ output                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  3 Nov 1996 17:21:19                                     *
  *-------------------------------------------------------------------*/

 /*=
=Description:
 The FREQOUT macro extracts numerical measures of association from the
 printed output of PROC FREQ, returning these measures in a SAS data set
 for further analysis or display.

=Usage:

 The macro arguments are as follows:
 
* DATA=_LAST_  Specifies the SAS data set to be analyzed.  If the DATA= 
					option is not supplied, the most recently created SAS data
					set is used.

* ROW=         REQUIRED.  Specifies the variable defining the rows of the
					table.

* COL=        REQUIRED.  Specifies the variable defining the columns of
					the table.

* LAYER=     Specifies the variable(s) defining the layers of the table.
				  If sepeicifed, a separate set of measures is output for each
				  value of the LAYER= variable.
					

* COUNT=     Specifies a variable containing the cell counts of the table.
				  Omit this option if each observation in the DATA= data set 
				  represents only a single entry in the table.

* ORDER=INTERNAL

* COMPUTE=CHISQ MEASURES
					Specifies the collection of statistics to be computed by
					PROC FREQ.  Valid options include one or more of the
					keywords ALL, CHISQ, CMH, EXACT, MEASURES.  

=Example:

data arth;
   input sex $ treat $ @;
   do improve = 'None  ', 'Some' , 'Marked';
      input count @;
      output;
      end;
cards;
Female  Active    6  5  16
Female  Placebo  19  7   6
Male    Active    7  2   5
Male    Placebo  10  0   1
;
%freqout(data=arth, layer=sex, row=treat, col=improve, count=count);

 =*/
  
%macro freqout(
	data=_last_,
	row=,
	col=,
	layer=,
	count=,
	order=internal,
	out=freqout,
	compute=chisq measures,
	stats=chisq cramer gamma n,
	scores=table,
	ls=80,
	options=noprint
	);
	
%if %upcase(&data)=_LAST_ %then %let data=&syslast;

%if %bquote(&row)= %then %do;
   %put ERROR: The ROW= argument must be specified.;
   %goto done;
%end;

%if %bquote(&col)= %then %do;
   %put ERROR: The COL= argument must be specified.;
   %goto done;
%end;

%if %bquote(&layer)^= %then %do;
   proc sort data=&data;
    by &layer;
%end;

%tempfile(freqout);
options nocenter nodate; run;
proc printto new print=freqout;

proc freq data=&data;
	%if %length(&layer)>0 %then %do;
		by &layer;
	%end;
	%if %length(&count)>0 %then %do;
		weight &count;
	%end;
	table &row * &col / 
		&compute scores=&scores missprint &options;
		
proc printto print=print;

%*-- Number of statistics we can extract;
%let nm=29;
data stats;
   length _name_ $8 string $&ls _label_ _rest_ $35;
	array name{&nm} $8 _temporary_ 
	(
	"CHISQ",
	"LRCHI",
	"ADJCHI"
	"MH_CHISQ",
	"EXACT_L",
	"EXACT_R",
	"EXACT_2",
	"PHI",
	"CONTGY",
	"CRAMV",
	"GAMMA",
	"TAU_B",
	"TAU_C",
	"SOMER_CR",
	"SOMER_RC",
	"PEAR_R",
	"RANK_R",
	"LAM_CR",
	"LAM_RC",
	"LAM_SYM",
	"UNCER_CR",
	"UNCER_RC",
	"UNCER_SYM",
	"NONZERO_R",
	"ROW_MEAN",
	"GEN_ASSOC"
	"ODDS_RATIO",                  
	"COL1RISK",
	"COL2RISK" 
	);
	%*-- The label array defines the strings that are searched for in the 
	PROC FREQ output, and (in most cases) the _LABEL_ for the statistic
	in the output dataset.
	;
	array label{&nm} $35 _temporary_ 
	(
	"Chi-Square",
	"Likelihood Ratio Chi-Square",
	"Continuity Adj. Chi-Square",                         
	"Mantel-Haenszel Chi-Square",
	"Fisher's Exact Test (Left)",                          
	"(Right)",                       
	"(2-Tail)",                          
	"Phi Coefficient",
	"Contingency Coefficient",
	"Cramer's V",
	"Gamma",
	"Kendall's Tau-b",
	"Stuart's Tau-c",
	"Somers' D C|R",
	"Somers' D R|C",
	"Pearson Correlation",
	"Spearman Correlation",
	"Lambda Asymmetric C|R",
	"Lambda Asymmetric R|C",
	"Lambda Symmetric",
	"Uncertainty Coefficient C|R",
	"Uncertainty Coefficient R|C",
	"Uncertainty Coefficient Symmetric",
	"Nonzero Correlation",
	"Row Mean Scores Differ",
	"General Association",
	"Case-Control",                  
	"Cohort (Col1 Risk)",
	"Cohort (Col2 Risk)" 
	);

	%*-- Array stats defines the statistics to be extracted for each
		summary measure;
	array stats{&nm}  _temporary_
	(
	3	/* Chi-Square*/
	3	/* Likelihood Ratio Chi-Square*/
	3	/* Continuity Adj. Chi-Square*/                         
	3	/* Mantel-Haenszel Chi-Square*/
	0	/* Fisher's Exact Test (Left)*/                          
	0	/* (Right)*/                       
	0	/* (2-Tail)*/                          
	1	/* Phi Coefficient*/
	1	/* Contingency Coefficient*/
	1	/* Cramer's V*/
	2	/* Gamma*/
	2	/* Kendall's Tau-b*/
	2	/* Stuart's Tau-c*/
	2	/* Somers' D C|R*/
	2	/* Somers' D R|C*/
	2	/* Pearson Correlation*/
	2	/* Spearman Correlation*/
	2	/* Lambda Asymmetric C|R*/
	2	/* Lambda Asymmetric R|C*/
	2	/* Lambda Symmetric*/
	2	/* Uncertainty Coefficient C|R*/
	2	/* Uncertainty Coefficient R|C*/
	2	/* Uncertainty Coefficient Symmetric*/
	3	/* Nonzero Correlation*/
	3	/* Row Mean Scores Differ*/
	3	/* General Association*/
	4	/* (Odds Ratio)*/                
	4	/* (Col1 Risk)*/
	4	/* (Col2 Risk)*/
	);

   infile freqout length=len;
	drop string lloc word i st _rest_;

   input @1 string $varying. len;
	string=left(string);
	word = scan(string,1,' -=');
	if word = ' ' then delete;
	if upcase(word) in ('ESTIMATES', 'STATISTIC', 'STATISTIC', 'SUMMARY')
		then do; delete; return; end;

	%if %length(&layer)>0 %then %do;
		retain &layer;
		lloc = index(string, trim(upcase("&layer"))||'=');
		if lloc > 0 then do;
			&layer = trim(substr(string,lloc+1+length("&layer")));
			delete; return;
		end;
	%end;

	if index(string,'Sample Size =') > 0 then do;
		value = input(scan(string,4,' '),best.);
		_name_='N';
		_label_='Sample Size';
		return;
	end;
	
	%*-- Fixup line for CMH statistics;
	if word in ('1', '2', '3') then
		string = left(substr(string, 2));

	%*-- Split the line into _label_ (to 2 blanks) and _rest_;
	lloc = index(string,'  ');
	_label_= substr(string,1,lloc-1);
	_rest_= trim(left(substr(string,lloc+2)));
	if _label_=' ' | _rest_=' ' then delete;
	do i=1 to &nm;
		if _label_ = label[i] then goto found;
		end;
	%*-- Not found in the list of labels: bye-bye;
	delete; return;

found:
	st = stats[i];
	_name_ = name[i];
	select(st);
		when(0) do;
			value=  input(scan(_rest_,1,' '),best.);
			prob =  input(scan(_rest_,1,' '),best.);
			if substr(_label_,1,1)='(' then
				_label_="Fisher's Exact Test " || _label_; 
		end;
		when(1) value = input(scan(_rest_,1,' '),best.);
		when(2) do;
			value = input(scan(_rest_,1,' '),best.);
			ase =   input(scan(_rest_,2,' '),best.);
		end;
		when(3) do;
			df =    input(scan(_rest_,1,' '),best.);
			value = input(scan(_rest_,2,' '),best.);
			prob =  input(scan(_rest_,3,' '),best.);
		end;
		when(4) do;
			value =  input(scan(_rest_,1,' '),best.);
			lower =  input(scan(_rest_,2,' '),best.);
			upper =  input(scan(_rest_,3,' '),best.);
			if value='.' then delete;
		end;
		otherwise;
	end;
output:
run;			

%*-- Select the statistics that we really want;
%if %length(&stats) %then %do;
data &out;
	set stats;
	if index(upcase("&stats"),trim(_name_))>0;
%end;

%*-- See if we need to retain the DF, PROB, LOWER, and UPPER variables;
%let drop_df =;
%let drop_cl =;
data _null_;
	set &out end=eof;
	retain drop_df drop_cl 1;
	drop drop_df drop_cl;
	%*-- Are any of the values non-missing?;
	if drop_df>0 then drop_df = (df = .);
	if drop_cl>0 then drop_cl = (lower = .);
	if eof then do;
		call symput('drop_df', put(drop_df,1.));		
		call symput('drop_cl', put(drop_cl,1.));		
	end;
run;

data &out;
	_row_ = upcase("&row");
	_col_ = upcase("&col");
	set &out;
	%if &drop_cl %then %do;
		drop lower upper;
		%end;
	%if &drop_df %then %do;
		drop df prob;
		%end;
	
*options ls=100;
*proc print;
	
/*
data &out;
   length string $&ls;
	file print;
   infile freqout length=len;
	retain &layer;
	drop  string lloc word;
   input @1 string $varying. len;
   put string $char.;
*	string = substr(string,2);
	word = left(scan(string,1));
	if word = ' ' then delete;
	if word = 'STATISTICS' then delete;
	lloc = index(string, trim(upcase("&layer"))||'=');
	if lloc > 0 then do;
		&layer = trim(substr(string,lloc+1+length("&layer")));
	end;
	%if %index(%upcase(&stats),PHI) %then %do;
		retain phi;
		if word='Phi'
			then phi = input(left(scan(string,3,' ')),best.);
	%end;
	%if %index(%upcase(&stats),CRAMER) %then %do;
		retain cramer;
		if scan(string,1," ")="Cramer's"
			then cramer = input(scan(string,3,' '),best.);
	%end;
	%if %index(%upcase(&stats),CONTINGENCY) %then %do;
		retain conting;
		if word='Contingency'
			then conting = input(scan(string,3,' '),best.);
	%end;
	%if %index(%upcase(&stats),GAMMA) %then %do;
		retain gamma;
		if word='Gamma'
			then gamma = input(scan(string,3,' '),best.);
	%end;
	if &layer = ' ' then delete;
		
data &out;
	set &out;
	by &layer;
	if last.&layer then output;
proc print;	
*/

*tempdel;
%done:;
options center date; run;
%mend;
%macro tempfile(fileref,ls,options);
   %global tempfn;
   %if %length(&ls)=0 %then %let ls=80;

   %if %sysevalf(&sysver  > 6.10) %then %do;
		filename &fileref temp &options
			%if %length(&ls)>0 %then lrecl=&ls;
			;		
		%end;
	%else %do;
		%if &sysscp = CMS
			%then %let tempfn=&fileref output a;
		%else %if &sysscp = WIN 
			%then %let tempfn=&fileref..tmp;
		%else /* assume unix */ 
			%let tempfn=/tmp/&fileref..tmp;
		filename &fileref "&tempfn" lrecl=&ls &options;
		%end;
%mend;

%macro tempdel(fileref);
   %global tempfn;
	%if length(&tempfn)=0 %then %goto done;
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
	 filename &fileref clear;
	 %let tempfn=;
%done:
%mend;

