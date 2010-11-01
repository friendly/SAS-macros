/*
	Name: getopt.sas
	Title: Retrieve a SAS system option to a macro variable

Example:
	%getopt(linesize,var=ls);

*/

%macro getopt(optname,var=);

   %let optname = %upcase(&optname);
   %if &var =  %then %let var = &optname;

   %global &var;

	%if &sysver > 6.07 %then %do;
   data _null_;
        set sashelp.voption(where=(optname = "&optname"));
        call symput("&var",trim(left(setting)));
        stop;
   run;
	%end;
	%else %let &var =;
%mend getopt;
