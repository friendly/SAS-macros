/*
Original by: Carl.Pierchala@nhtsa.dot.gov
From: NESUG 16, 2003

Title:  Display patterns of missing data
Version:  2.0
Revised:  05 Jan 2005 10:27:12
By:  M. Friendly

-- Added VAR= parameter to select variables for missing pattern analysis
-- Simplified code:  variables in &out= dataset are named V1, V2, V3, ...
   and stored as 0/1 values (not-missing/missing), but printed using
   a format as X/.
*/


*MISPATMA.SAS REVISED 09-JUL-03;
*GIVES UTILITY MACROS, THEN MACRO MISS_PAT, SUITABLE FOR A ZIP FILE;

/*      Disclaimer:  These macros are not waranteed nor guaranteed.  */
/*      It is the responsibility of the user of these macros to      */
/*      verify that they are fit for the user's applications.         */


* MACRO MISSPAT FOLLOWS;

* MISSPAT.SAS REVISED 08-JUL-2003;

*  MACRO CODE TO SUBSTITUTE ALIASES FOR VARIABLE NAMES IN MISSING
   PATTERN ANALYSIS MODELED AFTER PROC MI;

%macro misspat(
	data=_last_,   /* name of input dataset */
	var=_ALL_,     /* allow selecting VAR=_NUMERIC_ or _CHARACTER_ */
	sortby=descending percent,
	BY=,           /* list of blank-separated BY variables         */
	COLLAPSE=NO,
	out=misspat    /* name of output dataset */
	);

* MISS_PAT.SAS VERSION 1.0
* THIS MACRO WILL RUN A MISSING PATTERN ANALYSIS ON THE DATA SET &data;
* 'BY' VARIABLES CAN BE USED BY SPECIFYING &BY;
* If 'by' variables are specified and collapse=YES, then statistics
  for missing patterns collapsed accross all 'by' variables are 
  also printed;
* All VAR= variables except the 'by' variable are used in the analysis;

* TITLES CREATED BY THIS MACRO ARE PUT IN TITLE3, and TITLE4 LINES;

%LOCAL SORTIN; * TO CONTROL DATA SET USE IN SORT BELOW, DEPENDING ON CIRCUMSTANCE;

%* Guarantee at least one trailing blanks at end of &by;
%* so as to be able to search the &by list for unique individual
   substrings that do not contain blanks (i.e., search for the
   individual 'by' variables;
%* Guarantee no more than one blank seperating variable names in &by;
%* so as to replace the blanks with asterisks for use in proc freq;
%* also guarantee that characters are upper case for comparisons later;
%LET BY=%UPCASE(%CMPRES(&BY))%STR( );
%PUT BY=&BY***;

* Determine the variables in the data set &data using proc contents ;
proc contents data=&data noprint
  out=cont_ds(
    keep=name varnum label type
    rename=(name=variable)
    label="Contents of &data: Selected Variables"
     );
run;

proc print;

* Set up formats for the variable 'type' and for missingness;
proc format;
  value typef
    1='NUMERIC'
    2='CHARACTER'
        ;
  value miss
    0='X'
    1='.'
        ;
run;

%let var=%upcase(&var);

TITLE3 "Variable Name Aliases for &data for Use in Mising Value Pattern Analysis";
* Note: the proc contents above sorted the data set by 'VARIABLE';
* If &VAR^=_ALL_, the variable aliases will be inconveniently labeled;
data aliases;
  attrib alias length=$6;
  format type typef.;
  set cont_ds;
  alias='V'||left(put(varnum,5.0));
  	*-- Subset by TYPE or variable name;
        %if &var=_ALL_       %then %str(;);
  %else %if &var=_NUMERIC_   %then %do;  if type=1; %end;
  %else %if &var=_CHARACTER_ %then %do;  if type=2; %end;
  %else %do;  if index("&var", upcase(trim(variable)));     %end;

run;
TITLE4 "Sorted by VARIABLE";
proc print data=aliases;
	id variable;
	var alias label type;
run;

TITLE4 "Sorted by ALIAS";
proc sort data=aliases;
* note: sorting by alias leads to trouble when greater than 9 variables;
  by varnum;
run;
proc print data=aliases;
	id alias;
	var variable label type;
run;

* Create macro variables for 'rename' statement, etc. later;
* exclude variables in the &by list;
* create the macro variables in order sorted by varnum;
* also create a list of macro variables from the &by list;
%local aliases variables;
data _null_;
  retain  bylist "&by"; * previously added a blank at the end to be
                          able to delimit a string by a trailing blank;
  set aliases end=eof;
  retain aliases variables;
  length aliases variables $200;
  * Seperate processing if variable is in the &by list;
   if not index(bylist,trim(upcase(variable))||' ')
  then do;
    nanalyze+1;
	aliases = trim(aliases) || ' ' || trim(alias);
	variables = trim(variables) || ' ' || trim(variable);
    put 'Outputting macro variables for variable ' NANALYZE '(' variable ') -> ' alias;
	%* &A1, &A2, etc. will contain the alias names;
    call symput('A'||left(put(nanalyze,5.0)),trim(alias));
	%* &VAR1, &VAR2, etc. will contain the variable names;
    call symput ('VAR'||left(put(nanalyze,5.0)),variable);
    %* &T1, &T2, etc. will contain the variable type;
    call symput ('T'||left(put(nanalyze,5.0)),type);
  end;
  else do;
    nbyvars+1;
	put 'Outputting macro variable for BY variable ' NBYVARS '(' variable ')';
    CALL SYMPUT ('BYVAR'||LEFT(PUT(NBYVARS,5.0)),VARIABLE);
  end;
  if eof then do;
    call symput('nanalyze',nanalyze);
	call symput('nbyvars',nbyvars);
	call symput('aliases',aliases);
	call symput('variables',variables);
  end;
run;

%put aliases = &aliases;
%put variables = &variables;


* Create a data set 'shortnam' with aliases substituted for variable
  names, etc.;
DATA SHORTNAM;
* SET UP THE V1, ETC. AS ONE BYTE CHARACTER VARIABLES;
  length &aliases $1;

  * Set up mispat as character of length &nanalyze;
  length mispat $ &nanalyze;
  set &data;
  ;
  * CREATE THE V1_MISS, ETC. VARIABLES, DEPENDING ON VARIABLE TYPE;
  %DO I=1 %TO &NANALYZE;
     ;
     %IF &&T&I = 1 %THEN
         %STR(* NUMERIC VARIABLE;)
     %STR(IF &&VAR&I LE .Z THEN &&A&I='.'; ELSE &&A&I='X';);
         %ELSE
     %STR(* CHARACTER VARIABLE;)
         %STR(IF &&VAR&I EQ ' ' THEN &&A&I='.'; ELSE &&A&I='X';);

         * ITERATIVELY CONSTRUCT MISPAT;
     %IF &I EQ 1 %THEN %STR(MISPAT=&&A&I;);
         %ELSE %STR(MISPAT=TRIM(MISPAT)||&&A&I;);
  %END;
  ;
  * TRANSLATE MISPAT TO A BINARY STRING;
  MISPAT=TRANSLATE(MISPAT,'01','X.');
  ;
  * DROP UNNEEDED DATA;
  DROP &variables;
  /*
  %DO I=1 %TO &NANALYZE;
       &&VAR&I
  %END;
  */
  ;
RUN;
*proc print data=shortnam(obs=20);


PROC FREQ DATA=SHORTNAM;
  TABLE MISPAT%qblankta(%qtrim(%str( )%qcmpres(&by)))/OUT=BYMISPAT NOPRINT MISSING;
RUN;

* Add the group number to the observations;
data bymispa2;
  set bymispat;
  by mispat ;
  if first.mispat then Group+1;
  rename count=Freq;
run;


* Print the missing patterns if by groups not specified via &by, otherwise
  print the ungrouped missing patterns if requested via &collapse;
%IF (%QTRIM(&BY) EQ ) OR ((%QTRIM(&BY) NE ) AND %UPCASE(&COLLAPSE) EQ YES)
 %THEN %DO;

   %IF %QTRIM(&BY) NE %THEN %DO;

    * Summarize collapsing on &by variables ;
    proc freq data=bymispa2;
          table mispat/out=bymispa3 noprint missing;
          weight freq;
    run;

    * Now read the group number to the observations;
    data bymispa4;
      set bymispa3;
      by mispat ;
      if first.mispat then Group+1;
          rename count=Freq;
    run;

        %LET SORTIN=BYMISPA4;
   %END;
   %ELSE %LET SORTIN=BYMISPA2;


    * Results to be printed without by variables;
    * Translate mispat back into v1, v2, etc.;
	*-- Use binary (0/1) variables rather than character and formats for printing as X or .;
	*-- Provide variable name as label for Vi in output dataset;
	*-- Delete OBS variable;
    DATA &out;
      set &sortin;
*      obs+1;
      drop mispat;
*	  misbin = mispat;  *-- save binary pattern (for testing);

		%do i=1 %to &nanalyze;
			&&A&i = (substr(mispat,&i,1) = '1');
			format &&A&i miss.;
			label  &&A&i = "&&var&i";
			%end;
	nmiss = sum(of &aliases);	*-- number of missing variables;

      LABEL Group='Group'
            Freq='Freq'
            Percent='Percent'
			nmiss = '# missing';
      FORMAT PERCENT 5.1;
*      LABEL OBS='Obs';
    RUN;

    * Next sort the results by percent descending;
    proc sort data=&out;
          by &sortby;
        run;
    PROC PRINT DATA=&out /* LABEL */; 
    TITLE4 "Missing data patterns: sorted by &sortby";
      VAR &aliases
         group freq percent;
    RUN;
%END;

* If appropriate do computations then print grouped (by &by) missing
  patterns ;
%IF &BY NE %STR( ) %THEN %DO;
	proc sql;
	create table work.groupct as
	select mispat %qblanktc(%qtrim(%str( )%qcmpres(&by))),freq,group,
    	   sum(freq) as bytot,
    	   100*freq/sum(freq) as percent
        	 from bymispa2
        	 group by %qblanktc(%qcmpres(&by))
        	 order by %qblanktc(%qcmpres(&by)),percent desc,group
    	;
	quit;

* TRANSLATE MISPAT BACK INTO V1, V2, ETC.;
DATA &out;
/*
* SET UP THE V1, ETC. AS ONE BYTE CHARACTER VARIABLES;
  LENGTH
  %DO I=1 %TO &NANALYZE;
      &&A&I
  %END;
  $ 1;
*/

  SET GROUPCT(DROP=BYTOT);
 %IF &BY NE %STR( ) %THEN %STR(
  BY &BY ;
  IF FIRST.%QLASTVAR(&BY)THEN OBS=0 ;
);
  OBS+1;
		%do i=1 %to &nanalyze;
			&&A&i = (substr(mispat,&i,1) = '1');
			format &&A&i miss.;
			label  &&A&i = "&&var&i";
			%end;
*  DROP MISPAT;
   * TRANSLATE BACK FROM BINARY STRING;
 /*
  MISPAT=TRANSLATE(MISPAT,'X.','01');
  %DO I=1 %TO &NANALYZE;
  &&A&I=SUBSTR(MISPAT,&I,1);
  %END;
 */
  LABEL GROUP='Group';
  LABEL FREQ='Freq';
  LABEL PERCENT='Percent';
  FORMAT PERCENT 5.1;
  LABEL OBS='ByObs';
RUN;

PROC PRINT DATA=&out LABEL ;
  %IF &BY NE %STR( ) %THEN %DO;
    %STR(    BY &BY ;);
    %DO I=1 %TO &NBYVARS;
	  *Suppress the label so by variable names, rather than labels, are printed;
	  LABEL &&BYVAR&I..=' ';
    %END;
  %END;
  TITLE4 'Missing Data Patterns: Sorted By Descending Percent';
  VAR &aliases
     GROUP FREQ PERCENT;
RUN;
%END;
*-- Clear title statements;
title3; run;
%MEND;

/* Finding maximal monotone sequences:
>Here is a problem that perhaps someone out here has an idea about.  It
>> vaguely reminds me of something
>> I've seen before, but can't place.  Can anyone help?
>> 
>> For multiple imputation, there are simpler methods available if  the
>> patterns of missing data are 'monotone' ---
>> if Vj is missing then all variables Vk, k>j are also missing, vs. more
>> complex methods required when the patterns are not monotone.  The
>> problem is to determine if, for a collection of variables, there is an
>> ordering of them with a monotone
>> missing data pattern, or, if not, what the longest monotone sequence
>> is.


Here's my take - no idea about implementation though.

You need to draw a directed graph.  Nodes are 1...n and the rule is
that if Vk is missing in a pattern, you draw an arrow from each j for
which Vj is nonmissing, to k. If this graph has no cycles, the
collection of patterns is monotone and there is a straightforward
method of putting them in order (pick a node with no ancestors,
remove it from the graph, repeat). A longest monotone sequence is
obtained by finding a maximal cycle-free subgraph. So it all reduces
to graph theory. 


*/

* END OF MISS_PAT.SAS;

*UTILITY MACROS USED BY MISS_PAT;

%macro qcmprltb(text);                                                   
%*********************************************************************; 
%*                                                                   *; 
%*  MACRO: QCMPRLTB                                                  *; 
%*                                                                   *; 
%*  USAGE: 1) %qcmprltb(argument)                                    *; 
%*                                                                   *; 
%*  DESCRIPTION:                                                     *; 
%*    form with multiple blanks compressed to single blanks but with *; 
%*    with leading and trailing blanks retained, unlike %qcmpres.    *; 
%*                                                                   *; 
%*    Eg. %let macvar=%qcmprltb(&argtext)                            *; 
%*                                                                   *; 
%*  NOTES:                                                           *; 
%*    The %QLEFT macro in the autocall library is used in this macro.*; 
%*                                                                   *; 
%*********************************************************************; 
%local i;                                                               
%let i=%index(&text,%str(  ));                                          
%do %while(&i^=0);                                                      
  %let text=%qsubstr(&text,1,&i)%qleft(%qsubstr(&text,&i+1));           
  %let i=%index(&text,%str(  ));                                        
%end;   
&text
%mend;                                                                  

%macro qblankta(text);                                                   
%*********************************************************************; 
%*                                                                   *; 
%*  MACRO: QBLANKTA                                                  *;
%*                                                                   *; 
%*  USAGE: 1) %qblankta(argument)                                    *; 
%*                                                                   *; 
%*  DESCRIPTION:                                                     *; 
%*    REPLACE BLANKS BY ASTERISKS:  MODELED AFTER QCMPRES FOUND IN   *; 
%*    !SASROOT\core\sasmacro.                                        *; 
%*                                                                   *; 
%*    Eg. %let macvar=%qblankta(&argtext)                            *; 
%*                                                                   *; 
%*  NOTES:                                                           *; 
%*                                                                   *; 
%*********************************************************************; 
%local i;                                                               
%let i=%index(&text,%str( ));                                          
%do %while(&i^=0);   
  %IF &I GT 1 %THEN 
    %if &i lt %length(&text) %then
    %let text=%qsubstr(&text,1,&i-1)%str(*)%qsubstr(&text,&i+1);
    %else %let text=%qsubstr(&text,1,&i-1)%str(*); 
  %ELSE %let text=%str(*)%qsubstr(&text,&i+1);
  %let i=%index(&text,%str( ));  
%end;                                                                   
&text                                                
%mend;                                                                  

%macro qblanktc(text) ;
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QBLANKTC                                                  *;
%*                                                                   *;
%*  USAGE: 1) %qblanktc(argument)                                    *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    REPLACE BLANKS IN ARGUMENT BY COMMAS.                          *;
%*                                                                   *;
%*    Eg. %let macvar=%qblanktc(&argtext)                            *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    USES %QSYSFUNC AND TRANSLATE FUNCTIONS TO ACCOMPLISH THE       *;
%*    OBJECTIVE.                                                     *;
%*                                                                   *;
%*********************************************************************;

  %if &text ne %then %qsysfunc(translate(&text,%str(,),%str( ))) ;
%mend  qblanktc ;

%macro qlastvar(text);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QLASTVAR                                                  *;
%*                                                                   *;
%*  USAGE: 1) %qlastvar(argument)                                    *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    Finds the last variable name in the argument, which consists   *;
%*    of variable names delimited by blanks.  The name is returned   *;
%*    without leading or trailing blanks.                            *;
%*                                                                   *;
%*    Eg. %let macvar=%qlastvar(&argtext)                            *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    The %QCMPRES macro in the autocall library is used in this     *;
%*    macro.                                                         *;
%*                                                                   *;
%*********************************************************************;
%local i;
%let text=%qcmpres(&text);
%let i=%index(&text,%str( ));
%do %while(&i^=0);
  %let text=%qsubstr(&text,&i+1);
  %let i=%index(&text,%str( ));
%end;
&text
%mend;
*END OF UTILITY MACROS;

/*
* TWOVARS.SAS REVISED 3-JUL-03;
* DATA FOR EXAMPLE IN NESUG 16 MISS_PAT PAPER;
DATA TWOVARS;
INPUT Variab01 1 Second_Var $ 3-5;
LABEL Variab01='VARIABLE NUMBER 1';
LABEL Second_Var='VARIABLE NUMBER 2';
cards;
2
1 ABC

1
1
2 DEF

2
1
1 GHI
;
PROC PRINT DATA=TWOVARS;
TITLE 'TWOVARS';
RUN;

* Examples of calls to MISS_PAT;

OPTIONS MPRINT;
 %MISS_PAT(DS=WORK.TWOVARS) 
OPTIONS NOMPRINT;

OPTIONS MPRINT;
 %MISS_PAT(DS=fars01.person,by=state month,collapse=yes) 
OPTIONS NOMPRINT;
*/
