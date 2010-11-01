 /*--------------------------------------------------------------*
  *    Name: require.sas                                         *
  *   Title: Check requirements for a macro or program           *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 23 Apr 2000 11:39:16                                *
  * Revised: 07 Jan 2008 12:37:30                                *
  * Version: 2.1-0                                               *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The REQUIRE macro is designed to be used in SAS programs or macros
 that depend on a given SAS version, particular SAS products installed,
 or other macros or files that need to be available in order to run. 
 As presently implemented, it's just a sketch, with a long TO DO list.


=Usage:

 The REQUIRE macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%require(version=9.1,proc=iml);
 
==Parameters:
* SASVER=      Minimal required SAS version, e.g. 8.2
* VERSION=     synonym for SASVER=
* SASPROD=     Name of a required SAS product, e.g., IML
* PROC=        Name of a required SAS procedure
* SASAUTOS=    Name of a required SAS macro, found on the SASAUTOS path(s)
* INCLUDE=     Name or fileref of a required file

* ACTION=      Final macro action after checking all specified requisites.
               RC:  return a list of return codes
			   OK:  return 0 if any requirement fails, otherwise 1
			   action: execute the SAS code in &action (and return a result)
 =*/

%require(
	sasver=,      /* minimal required SAS version, e.g. 8.2 */
	version=,     /* synonym, for SASVER= */
	sasprod=,     /* required SAS product(s), e.g., IML */
	proc=,        /* required procedure */
	sasautos=,    /* required macro, found on SASAUTOS path(s) */
	include=,     /* required file, named as in a %include statement */
	action=rc     /* ending action: RC (return codes) or OK (0|1) */
	);
	

	%local ok rc;
	%let ok=1;
	%let rc=;      %*--return code(s);
	
	%*-- Check required SAS version;
	%if %length(&version)
		%then %let sasver=&version;
	%if %length(&sasver) %then %do;
		%if &sasver > &sysver %then %do;
			%put ERROR: This program requires SAS Version &sasver.. It cannot run in &sysver;
			%let ok=0;
			%end;
		%end;

	%local products;
	%*-- (incomplete) list of SAS products;
	%let products = CONNECT ETS GRAPH INSIGHT IML STAT OR EIS GIS ;
	%*-- Check for SAS product installed;
	%*-- TODO: allow a blank separated list;
	%if %index(&products, %upcase(&sasprod))> 0 %then %do;
		%if %sysprod(&sasprod) = 0 %then %do;
			%let rc=&rc 2;
			%let ok=0;
			%put ERROR:  This program requires the SAS &sasprod product;
		%end;
	
	%if %length(&proc) %then do;
	%*-- TODO: allow a blank separated list;
		proc &proc; quit;
		%if &syserr>0 %then %do;
			%let rc=&rc &syserr;
			%let ok=0;
			%put ERROR:  This program requires the SAS &proc procedure;
			%end;
		%end;

	%if %length(&sasautos) %then do;
	%*-- How to check if named macro can be found on the SASAUTOS path?;
		%let cautos = %sysfunc(getoption(sasautos));
		%end;

	%if %length(&include) %then do;
	%*-- How to check if named file can be %included?;
		%include &include;
		%if &syserr>0 %then %do;
			%let rc=&rc &syserr;
			%let ok=0;
			%put ERROR:  This program requires that &include can be %str(%)included;
			%end;
		%end;

 
%done:
	%if %upcase(&action) = RC %then %do;  /* return all return codes */
		&rc
		%end;
	%else %if %upcase(&action) = OK %then %do;  /* return 0|1 */
		&ok
		%end;
	%else %do;  /* execute the SAS code in &action */
		&action;
		%end;
%mend;

	
