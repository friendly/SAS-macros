/*
	Name: getparm.sas
	Title: Retrieve a SAS command paramter to a macro variable

Example:
	%let sasfile=%upcase(%getparm(SYSIN));

*/

%macro getparm/parmbuff;
/* Copyright (c) 1996, SAS Institute Inc.   All Rights Reserved      */
%* 96-11-27, SB: Fetched from ftp.sas.com, /pub/sugi21/opttest.sas;

  %let arglist = &syspbuff;
  %sysfunc(getoption(%varargs(&arglist)))
%mend;
