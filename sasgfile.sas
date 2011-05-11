 /*--------------------------------------------------------------*
  *    Name: sasgfile.sas                                        *
  *   Title: Set the filename of the output graphics file        *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 12 Feb 2001 10:24:14                                *
  * Revised: 16 Feb 2003 10:46:17                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SASGFILE macro is used by other macros to set the basename
 and extension of output graphics files in a flexible way.
 The basename may come from either a global macro variable,
 SASFILE (corresponding to the name of the current SAS file),
 or from an environment variables SASFILE or GSASFILE, or from an 
 explicit character string passed as the second argument.

=Usage:

 The SASGFILE macro is defined with positional parameters.
 For example: 
 
	%sasgfile(eps);
 
==Parameters:
 =*/

%global gsasfile;
%macro sasgfile(ft,fn);
  %* -----------------------------------------------------------;
  %* Set the filename of the output graphics file               ;
  %* -----------------------------------------------------------;

%local gsasfn sasfn;
%if %length(&fn) %then %let gsasfile = &fn..&ft;
%else %if %defined(sasfile) %then %let gsasfile = &sasfile..&ft;
%else %do;
	%let sasfn =%scan(%SYSGET(SASFILE),1,%str(.)); 
	
	*-- Set the name of the output file;
	%let gsasfn =%SYSGET(GSASFILE); 
	%if %length(&gsasfn)=0 %then %if %length(&sasfn) > 0 
		%then %let gsasfn =&sasfn..&ft;
		%else %let gsasfn=grfout.&ft;
	%let gsasfile=&gsasfn;
%end;
%put SASGFILE: gsasfile is &gsasfile;
%mend sasgfile;
