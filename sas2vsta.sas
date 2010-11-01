 /*--------------------------------------------------------------*
  *    Name: sas2vsta.sas                                        *
  *   Title: Generate a ViSta input file from a SAS dataset      *
        Doc: http://www.datavis.ca/sasmac/sas2vsta.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:   4 Mar 1999 12:10                                  *
  * Revised:  11 Jul 2001 09:40:38                               *
  * Version: 1.3                                                 *
  *  1.2 Inlined %combine macro                                  *
  *  1.3 Now handle missing numeric values (. -> nil)            *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SAS2VSTA macro reads a SAS data set and produces an output file
 suitable as input to ViSta.  Handles multivariate data and
 frequency classification data.  Doesn't handle frequency table
 data yet.  See: http://forrest.psych.unc.edu/research/ for information
 about ViSta.

=Usage:

 The SAS2VSTA macro is called with keyword parameters.  The VAR=
 parameter is required.  The arguments may be listed within
 parentheses in any order, separated by commas. For example:
 
   %sas2vsta(data=new, var=Name Group X1 X2 X3, id=name,
      about=%str(A sample data set, converted to ViSta));

 Observations may be selected by the WHERE= parameter.  They appear in
 the output in their order in the input data set. Sort them first
 if you want some alternative order.
 
 By default, output is written to a file of the same name as the
 SAS data set, with the extension '.lsp', in the current SAS
 directory.
  
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* DNAME=      Data name, the name of the data set for ViSta.
              [Default: DNAME=&DATA]

* VAR=        List of variable names to be output, a blank separated
              list.  Variables appear in the output in the order listed,
              and in the :VARIABLES list in the case given.

* FREQ=       Name of a frequency variable, if any.  Added as the last
              variable, and the :FREQ t flag is set.  All other variables
              are treated as Category variables in this case.

* ID=         The name(s) of observation ID variable(s), which are used
              as row :LABELS.  If two or more variables are given, their
              values are joined using the SEP= character for each observation.

* SEP=        Separator character just to join adjacent ID variables.
              If you want to use a character which the SAS macro processor
              treats as special, use, e.g., SEP=%str(,).

* ABBREV=     Maximum length of any ID variable when there are 2 or more
              listed in ID=.  If ABBREV= is specified, each ID= variable
              is abbreviated to this length to construct a single observation
              label.

* WHERE=      WHERE clause to subset the observations in the output file.
              Use WHERE=%str(var=value) if value contains any funny characters.

* TITLE=      Title string for the data for ViSta [Default: TITLE=&DATA]

* ABOUT=      :ABOUT description; use %str() if it contains ','.  If
              not specified, we use the data set label, if there is
              one.  NB: you must use quotes, as in

                   data foobar (label="My foobar data");

              for the label to be accessible to this macro.

* OUT=        Output destination, one of OUT=FILE, PRINT or STDOUT 
              [Default: OUT=FILE]

* OUTFILE=    Name of output LSP file. [Default: &DATA.lsp]

* OUTDIR=     Name of output directory. [Default: Current SAS directory]

* LS=         Output linesize [Default: 80]
                
=Limitations:

 SAS stores date and time values as numeric variables (e.g., seconds
 past midnight) and uses formats to print them in readable form.  These
 will cause problems in ViSta if, for example, time values like '12:33'
 are produced as numeric variables in the output file.  In these cases,
 you must associate a purely numeric format with the variable, rather
 than a time or date format.  For example,

	data lifeboat;
		set lifeboat;
		*-- Transform launch to minutes past midnight;
		launch = 60*hour(launch) + minute(launch);
		format launch 6.;


 =*/
%macro sas2vsta(
   data=_last_,        /* Name of input data set                  */
   dname=&data,        /* Data name                               */
   var=,               /* List of variable names to be output     */
   freq=,              /* Name of a frequency variable            */
   id=,                /* name(s) of ID variables (row labels)    */
   sep=,               /* separator, for multiple IDs             */
   abbrev=,            /* Max length for multiple IDs in row labels */
   where=,             /* where clause, use where=%str(var=val)   */
   title=&data,        /* data set :title                         */
   about=,             /* :about description; use $str() if contains ',' */
   out=FILE,           /* output fileref: FILE, PRINT, STDOUT     */
   outfile=,           /* name of output LSP file                 */
   outdir=,            /* name of output directory                */
   ls=80               /* output linesize                         */ 
   );
   
%if %upcase(&data)=_LAST_ %then %let data=&syslast;

%*-- Output is either to a file (fileref LSPOUT) or to the listing
     (fileref PRINT).  If FILE, try to assign default outdir based on
     operating system.;
     
    %if %length(&outdir) > 0 %then %do;
        %let outdir = %trim(&outdir);
        %if &sysscp=WIN
            %then %let dirsep=\;
            %else %let dirsep=/;
        %if "%substr(&outdir, %length(&outdir), 1)" ^= "&dirsep"
            %then %let outdir = &outdir.&&dirsep;
        %end;

   %if %length(&outfile)=0 %then %let outfile=&data..lsp;
   
   %if %nrbquote(%upcase(&out))=FILE %then %do;
      filename lspout "&outdir.&outfile";
      %let out=lspout;
   %end;

   data _tmp_;
      set &data;
      stop;

   %*** use summary to reorder the variables in order of var list;
   proc summary data=_tmp_(firstobs=1 obs=1);
      id &var &id;
      output out=_tmp1_(drop=_TYPE_ _FREQ_);
      
   proc contents data=_tmp1_(keep=&var &id) 
      noprint out=_vars_(keep=name type length label format npos);
      run;
   %*** sort by position of variables in the list;
   proc sort data=_vars_;
      by npos;
   proc print;

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
		else if format in ('HHMM', 'MMSS', 'TIME')
			then format='BEST.';
      call symput("fmt"||left(put(_n_,5.)),trim(format));
   run;

   %if %length(&id)>0 %then %do;
      %*-- More than one ID variable?  ;   
   %let nid = %numwords(&id,%str( ));
   %if &nid > 1 %then %do;
      %combine(data=&data, var=&id, result=_id_, abbrev=&abbrev,
       sep=&sep,usefmt=1);
      %let id=_id_;
      %end;
   
   %*-- Concatenate all the row labels into a set of quoted strings;
   data _lab_;
      set &data end=last;
      length lab1-lab10 $200;
      retain lab1-lab10 ;
      keep   lab1-lab10 ;
      array idlab{*} lab1-lab10;
      if _n_=1 then do;
         i=1;
         lab1='';
         end;
      idlab[i] = trim(idlab[i]) || ' "' || trim(left(&id)) || '"';
      if length(idlab[i])>(&ls-10) then i+1;

      if last then output;
*   proc print;
      %end;
   
   %if %length(&about)=0 %then %do;
      %*-- Try to get data set label from proc contents;
      proc contents data=&data(keep=&var) 
         noprint out=_vars_(keep=memlabel);run;
      data _null_;
         set _vars_ end=eof;
         if _n_=1 then do;
            call symput('about', memlabel);
            end;
         run;
      %end;

   %*-- Pernament data set? -- Fix dname;
   %if %length(%scan(&data,2,%str(.)))>0 %then %do;
      %let dname= %scan(&data,2,%str(.));
      %end;

   data _null_;
      set &data end=last;
      %if &where ^= %str() %then %do;
         where (&where);
      %end;

      file &out notitle noprint 
         %if &out=FILE %then lrecl=&ls;
         ;
         
      if _n_=1 then do;
         put '(DATA "'  +(0) "&dname"  '"' /
             '  :TITLE "'  +(0) "&title" '"' ;
         %if %length(&about) %then %do;
            put '  :ABOUT "'  +(0) "&about"  +(0) '"';
            %end;
         %if %length(&freq)>0 %then %do;
            put '  :FREQ t';
            %end;
         %header(&var, &freq);
         put  '  :DATA (QUOTE (';
         end;

      %row(&var, &freq);
      
      %*-- Finish up;
      if last 
         then do;
         put '   ))';          %*-- end :DATA;
         put ')';
         end;
   run;   
%mend;

%macro header(varlist, freq);
   %global cols;
   %let cols = %numwords(&varlist);
   length var $40;

   %*-- Output variable names and types;
   put  '  :VARIABLES (QUOTE (' @;
   %do i = 1 %to &cols;
      var = "%scan(&varlist, &i)";
      put ' "' var  +(-1) '"' @;
      %end;
   retain isfreq;
   isfreq=0;
   %if %length(&freq)>0 %then %do;
      isfreq=1;
      put ' "' "&freq" '"' @;
      %end;
   put '))';

   put  '  :TYPES (QUOTE (' @;
   %do i = 1 %to &cols;
      type = &&type&i;
      if type=1 & isfreq=0 /* numeric */
         then put ' "Numeric"'  @;
         else put ' "Category"'  @;
      %end;
   %if %length(&freq)>0 %then %do;
      put ' "Numeric"' @;
      %end;
   put '))';

   %if %length(&id)>0 %then %do;
      set _lab_;
      array idlab{*} lab1-lab10;
      put  '  :LABELS (QUOTE (';
      do i=1 to 10;
         if length(idlab[i])>1 then
            put '   ' idlab[i];
         end;
      put '    ))';
      %end;
%mend;

%macro row(varlist, freq);
   %global cols;
      put '  ' @;
      %do i = 1 %to &cols;
         %let var = %scan(&varlist, &i);
         type = &&type&i;
         if type=1  & isfreq=0  then do;   /* numeric, but not freq */
				if &var = .
					then put ' nil ' @;
            	else put ' ' &var  +(-1) ' ' @;
				end;
            else put ' "' +(0) &var +(-1) '" ' @;
         %end;

      %if %length(&freq)>0 %then %do;
         put ' ' &freq  @;
         %end;
      put ;
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

 /*--------------------------------------------------------------*
  *    Name: combine.sas                                         *
  *   Title: Combine the values of two or more variables         *
        Doc: http://www.datavis.ca/sasmac/nodoc.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  4 Mar 1999  9:59                                   *
  * Revised:  5 Mar 1999 12:58:49                                *
  * Version: 1.1                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The COMBINE macro combines two or more variables (character or
 numeric) into a single one.  Useful for situations where you
 need two or more CLASS variables, but some procedure or macro
 stupidly only handles one.  Also handy for plots of the form
    plot y * x = Group Sex
 where there are two or more curve variables.

=Usage:

 The COMBINE macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
   %combine(var=group gender, result=gp_sex);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        list of variables to be combined.  *required*

* RESULT=     The name of the result variable. [Default: RESULT=_ID_]

* SEP=        Separator character(s), inserted between adjacent values

* ABBREV=     If specified, each character variable in VAR= is truncated to this length
              in RESULT.  To specify different truncation lengths, use a list of numbers,
              whose order corresponds to the VAR= variables, e.g., ABBREV=2 2 4.

* LENGTH=     If specified, the RESULT= variable is truncated to this total length,
              regardless of the ABBREV= setting.

* USEFMT=     If postive, numeric variables which have formats stored in the data set
              have their formatted values combined.

* OUT=        The name of the output data set [Default: OUT=&data]

=Bugs:

 The internally calculated length for the result variable is incorrect for numeric
 variables.
 
 Should provide a way to use a formatted value of a character variable.

 =*/
%macro combine(
   data=_last_,
   var=,           /* list of variables to be combined            */
   result=_id_,    /* result variable                             */
   sep=,           /* separator character                         */
   abbrev=,        /* abbreviated length of each char variable    */
   length=,        /* max total length of RESULT                  */
   usefmt=0,
   out=&data);
   
%let var=%upcase(&var);
%*let var=%vexpand(&var);       *-- Uncomment to make this work for VAR=X1-X5;

%let nv=%words(&var,root=_v_);  *-- Get number of VAR= variables, create _v_1 ... ;

%if &nv < 2 %then %do;
   %put WARNING:  Only &nv VAR= variable was specified.  Nothing has been done.;
   %goto done;
   %end;
%*put nv=&nv;

%if %upcase(&data)=_LAST_ %then %let data=&syslast;

options nonotes;
proc contents data=&data  noprint 
     out=_vars_(keep=name type format length);
  run;

data _null_;
   set _vars_ end=eof;
   %do i=1 %to &nv;
      if name = "&&_v_&i" then do;
         call symput('_t_'||"&i", put(type,1.0));
         if format ^= ' ' and index(format,'.')=0 then format=trim(format)||'.';
         call symput('_f_'||"&i", format);
         * put name= format=;
         len + length;
         end;
      %end;
   if eof then do;
      call symput('_len_', left(put(len, 8.0)));
      end;
run;

%if %length(&length)>0 %then %do;
   %if %verify(&length,  %str(0123456789))=0
      %then %let _len_ = &length;
   %end;

%if %length(&abbrev)>0 %then %do;
   %if %verify(&abbrev,  %str(0123456789 ))>0
      %then %do;
         %put WARNING:  Non-numeric ABBREV= &abbrev has been ignored.;
         %let abbrev=;
         %end;
   %end;

options notes;
data &out;
   set &data;
   length &result _tmp_ $&_len_;;
   drop _tmp_ ;
   &result = '';
   %let s=;
   %do i=1 %to &nv;
      %if &i>1 & %length(&sep)>0 %then %do;
         &result = trim(&result) || trim("&sep");
         %end;
      %if &&_t_&i = 1  /* numeric */
         %then %do; 
            %if &usefmt>0 and %length(&&_f_&i)>0 %then %do;
               _tmp_ = left(put(&&_v_&i, &&_f_&i));
               %end;
            %else %do;
               _tmp_ = left(put(&&_v_&i, best8.));
               %end;
            &result = trim(&result) || _tmp_;
            %end;
         %else %do;
            %if %length(&abbrev)>0
               %then %do;
                  %let ab = %scan(&abbrev,&i);
                  %if &ab= %then %let ab= %scan(&abbrev,1);
                  &result = trim(&result) || 
                     substr(&&_v_&i,1,min(&ab,length(&&_v_&i)));
                  %end;
               %else %do;
                  &result = trim(&result) || &&_v_&i;
                  %end;
            %end;
      %end;
%done:   
%mend;

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
   %*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;

