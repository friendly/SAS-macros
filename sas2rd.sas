 /*--------------------------------------------------------------*
  *    Name: sas2rd.sas                                          *
  *   Title: Write .Rd documentation for a SAS dataset           *
  *     Doc: http://datavis.ca/sasmac/sas2rd.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 29 Nov 2012 08:58:10                                *
  * Revised: 30 Nov 2012 17:09:57                                *
  * Version: 1.0                                                 *


  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SAS2RD macro produces an R .Rd file to document a SAS dataset
 for an R package. It produces results similar to that of

   promptData()

 in R but uses variable labels from the SAS dataset to make the .Rd
 file more complete.

=Usage:

 The SAS2RD macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
        %sas2rd();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* LIBNAME=    Library name of input dataset [Default: LIBNAME=WORK]

* FILE=       Name of output .Rd file [Default: FILE=&DATA..RD]

* VERBOSE=    Print variable names and labels?
                
==Examples:

  %sas2rd(data=class, libname=sasuser);
                

 =*/


%macro sas2rd(
   data= _last_,     /* Name of input dataset                       */
   libname= work,    /* Library name of input dataset               */
   file= &data..Rd,  /* Name of output .Rd file                     */
   verbose=0          /* Print variable names and labels?           */
	);

    %*-- Reset required global options;
    %if &sysver >= 7 %then %do;
        %local o1 o2;
        %let o1 = %sysfunc(getoption(notes));
        %*let o2 = %sysfunc(getoption(validvarname,keyword));
        options nonotes;
        %end;
    %else %do;
       options nonotes;
        %end;

%if %upcase(&data)=_LAST_ %then %let data=&syslast;

proc sql noprint ;
	create view varlist as 
		select name, label, type from dictionary.columns
			where libname="%upcase(&libname)" and memname="%upcase(&data)";
		select count(name) into: NVAR from dictionary.columns
			where libname="%upcase(&libname)" and memname="%upcase(&data)";

data _null_;
	if 0 then set &libname..&data nobs=_nobs;
	call symput('nobs', trim(left(put(_nobs,12.))));
run;
%put NOBS=&NOBS NVAR=&NVAR;

proc print data=varlist;
run;

filename outrd "&file";


%*--- write the Rd header ;


data _null_;
	file outrd;
put "\name{&data}";
put "\alias{&data}";
put "\docType{data}";
put "\title{";
put "%%   ~~ data name/kind ... ~~";
put "}";
put "\description{";
put "%%  ~~ A concise (1-5 lines) description of the dataset. ~~";
put "}";
put "\usage{data(&data)}";
;

%*--- write the Rd format ;

data _null_;
	set varlist end=eof;
	file outrd mod;
	if _n_=1 then do;
		put '\format{';
  		put "A data frame with &NOBS observations on the following &NVAR variables.";
		put '  \describe{';
		end;
	if type='num' 
		then typ = ', a numeric variable';
		else typ = ', a character variable';
	lab = trim(label) || typ;
	put '    \item{\code{' +0 name +(-1) '}}{' lab +(-1) '}';
	if eof then do;
		put '  }';
		put '}';
		end;
run;

%*--- write the Rd footer ;

data _null_;
	file outrd mod;;

put "\details{";
put "%%  ~~ If necessary, more details than the __description__ above ~~";
put "}";
put "\source{";
put "%%  ~~ reference to a publication or URL from which the data were obtained ~~";
put "}";
put "\references{";
put "%%  ~~ possibly secondary sources and usages ~~";
put "}";
put "\examples{";
put "data(&data)";
put "## maybe str(&data) ; plot(&data) ...";
put "}";
put "\keyword{datasets}";
run;

%put SAS2RD: Wrote .Rd output to &file;

    %*-- Restore global options;
    %if &sysver >= 7 %then %do;
        options &o1;
        %end;
    %else %do;
       options notes;
        %end;

%mend;
                                                                                             
