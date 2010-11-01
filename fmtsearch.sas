/*
Macro to get or append to current fmtsearch path
cf. SAS Comm 12(2), 1996, 55-56.

Usage:

%let currpath = %fmtsearch();
%let newpath = %fmtsearch(append=c:\sasuser\myformats);

*/

%macro fmtsearch(
	append=
	);

%*-- Get current format search path into &currpath;
proc sql;
	select (compress(setting,'()')) into :currpath
		from sashelp.voption
		where optname='fmtsearch';
	quit;

%if %length(&append) %then %do;
	%let currpath = %str(%trim(&currpath) &append);
	options fmtsearch=(&currpath);
	run;
	%end;

&currpath
%mend;

