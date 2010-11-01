 /*--------------------------------------------------------------*
  *    Name: require.sas                                         *
  *   Title: Check requirements for a macro or program           *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 23 Apr 2000 11:39:16                                *
  * Revised: 11 Jan 2005 17:28:05                                *
  * Version: 2.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The REQUIRE macro is designed to be used in SAS programs or macros
 that depend on a given SAS version, particular SAS products installed,
 or other macros or files that need to be available in order to run. 
 As presently implemented, it's just a sketch, with a long TO DO list.

=Usage:

 The REQUIRE macro is defined with one positional parameter, a
 blank-separated list of requirements, and optional keyword
 parameter(s), e.g., URL=, specifying a web location for a downloadable
 file.
 It returns a null value
 on success (all requirements met), otherwise a numeric return
 code.

 For example: 
 
	%let rc = %require(6.12 IML);
	%let rc = %require(data(baseball) macros(lowess));
	%let rc = %require(sasautos(lowess), url=http://euclid.psych.yorku.ca/ftp/sas/sssg/macros/lowess.sas);

 A numeric value is tested as a SAS version (&sysver); a character
 value included in the list SASPROD is tested as a SAS product;
 otherwise, the string is tested as a %include- able file.  Includ-able
 files may be specified as full-path names (not very general),
 or as members of a SAS library pre-declared in a libname statement.

=Limitations:

 - This should be a built-in macro function, perhaps with an extended
   syntax (version=, product=, etc.)
 - Fails if can't %include
 - Should be able to say 
    %require(sasautos::lowess sasautos::words)
	or (better):
    %require(sasautos(lowess) sasautos(words))
 to check if these macros are availabile via SASAUTOS
 - Should be able to say
    %require(sasautos(lowess), url=http://www.math.yorku.ca/SCS/sasmac/)
 to download (and install) these.
 
 =*/

%macro require(stuff,url=, action=rc);

%local i v;
%let rc=;      %*--return code;
%let i=1;
%let v = %upcase(%scan(&stuff,1,%str( )));
%*-- (incomplete) list of SAS products;
%let sasprod = CONNECT ETS GRAPH INSIGHT IML STAT OR EIS GIS ;

%do %while(&v^= );
%put Checking v=&v;
	%*-- Check for proper SAS version;
	%if %datatyp(&v)=NUMERIC %then %do;
		%if &v > &sysver %then %do;
			%let rc=&rc 1;
			%put ERROR:  This program requires version &v of The SAS System;
			%end;
		%end;
	%*-- Check for SAS product installed;
	%else %if %index(&sasprod, %upcase(&v))> 0 %then %do;
		%if %sysprod(&v) = 0 %then %do;
			%let rc=&rc 2;
			%put ERROR:  This program requires the SAS &v product;
		%end;
	%*-- Check for include-able file;
	%* (This will fail if the file cannot be found);
	%else %do;
		%include &v;
		%end;

	%let i = %eval(&i+1);
	%let v = %upcase(%scan(&stuff,&i,%str( )));;
   %end;

%done:
    &rc

%mend;
