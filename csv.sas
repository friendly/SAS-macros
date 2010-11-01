/*
From: Andrew Smith (suqajsmi@reading.ac.uk)
 Subject: Re: Writing tab-delimited data 
 Newsgroups: comp.soft-sys.sas
 Date: 1996/05/23 

I've included the macro code below. It's written and named with csv files 
in mind, but you can easily change it to write tab-separated data instead 
by setting tab as the delimiter where indicated by comments in the code.

Andrew Smith 
University of Reading
*/

------------------------------------------------------------------------

%macro csv(
	data=_last_, 
	csvfile=,
	quoteall=0,     /* put quote marks around all text values?   */
	quote='"',
	showname=1,     /* write variable names as column headers?   */
	delim=','
	);

 
/*********************************************************************
                                                               
 Export data from SAS data set &dataset to csv file &csvfile.  
                                                               
   data      is simply the name of a SAS data set              
             (optionally followed by data set options).        
   csvfile   is a predefined fileref or a quoted file name     
             (optionally followed by data step 'file' statement
             options).                                         
                                                               
 Use quoteall = 1 to put quote marks around all text values    
 Use quoteall = 0 to quote only values that contain delimiters 
                                                               
 (Note: In Excel, for quoted text values that contain double   
 quotes to be read correctly, the contained quotes need to be  
 duplicated; this program does not do that. Also, text values  
 that look numeric may be read as numeric even when quoted.)   
 
 Use quote = "'" to use single quotes on quoted text values.   
 Use quote = '"' for double quotes to be used (suits Excel).   

 Use showname = 1 to write variable names as column headers.   
 Use showname = 0 to suppress variable names as column headers.

 Specifies delimiter between fields in the output data file.   
 Use delim = ',' for comma-separated values.                   
 Use delim = '09'x  for tab-delimited data on ASCII systems.   
 
 Adapted from a macro published in 'SAS Observations' in 1995. 
                                                               
 Note: does not handle special numeric formats (such as date   
 or time formats) intelligently. To use special formats, copy  
 the program and simply insert the desired format statement    
 after the "format _numeric_ best12.;" statement.              
 
*********************************************************************/
  
  proc contents data=&data out=_temp_ (keep=name type npos) noprint;
  run;
  
  proc sort data=_temp_; 
  	by npos; run;
  
  data _null_;
    set _temp_ end=eof;
    call symput('var'||(left(put(_n_,5.))), name);
    call symput('typ'||(left(put(_n_,5.))), left(put(type,8.)));
    if eof then call symput('total', left(put(_n_,8.)));
  run;

  %if &showname %then %do;
    data _null_;
      file &csvfile noprint;
      set _temp_ end=eof;
      %if &quoteall %then %do;
        put &quote name +(-1) &quote &delim @;
      %end;
      %else %do;
        put        name +(-1)        &delim @;
      %end;
      if (eof) then put +(-1) ' '; /* remove the extra delimiter at the end */
    run;
  %end;

  data _null_;
    file &csvfile noprint %if &showname %then mod; ;
    set &data;
    format _numeric_ best12.;
    %do i = 1 %to &total;
      %if &&typ&i=1 %then %do; /* numeric variable */
        if (n(&&var&i))
        then put &&var&i +(-1) &delim @;
        else put               &delim @;
      %end;
      %else %do;  /* character variable */
        %if &quoteall %then %do;
          if (&&var&i = '')
          then put &quote &quote               &delim @;
          else put &quote &&var&i +(-1) &quote &delim @;
        %end;
        %else %do;
          if (&&var&i = '') then put &delim @;
          else do;
            if (index(&&var&i,&delim))
            then put &quote &&var&i +(-1) &quote &delim @;
            else put        &&var&i +(-1)        &delim @;
          end;
        %end;
      %end;
    %end;
    put +(-1) ' ';  /* remove the extra delimiter at the end */
  run;
  
%mend csv;
