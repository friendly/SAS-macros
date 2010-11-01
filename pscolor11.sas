 /*--------------------------------------------------------------*
  *    Name: pscolor.sas                                         *
  *   Title: Set graphics parameters for PostScript file output  *
  *                                                              *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  5 Dec 1996 14:30:47                                *
  * Revised:  5 Jan 1997 10:46:34                                *
  * Version:  1.1                                                *
  * Dependencies:                                                *
  *   defined                                                    *
  *--------------------------------------------------------------*/
 /*=
=Description:

 The PSCOLOR macro initializes SAS/GRAPH to produce PostScript output.
 It sets the name of the output postscript file based on the SASFILE
 or GSASFILE environment parameters, or 'grfout.ps' if neither of these
 variables are defined.
 
 The output file is written to the current SAS directory.  You can
 override this by defining a global macro variable GSASDIR.

 =*/
 
%global gsasfile gsasdir devtyp;

%macro pscolor;

	%let devtyp=PS;
	%let dev=pscolor;
	%local gprolog gaccess;

	%if %defined(sasfile) %then %let gsasfile = &sasfile..ps;
	%else %do;
		%let sasfn =%scan(%SYSGET(SASFILE),1,.); 
		
		*-- Set the name of the output file;
		%let gsasfn =%SYSGET(GSASFILE); 
		%if %length(&gsasfn)=0 %then %if &sasfn ^=%str() 
			%then %let gsasfn =&sasfn..ps;
			%else %let gsasfn=grfout.ps;
		%let gsasfile=&gsasfn;
	%end;
   %if %defined(gsasdir)=0 %then %let gsasdir=;

   %put PSCOLOR: gsasfile is: "&gsasdir.&gsasfile";
   filename gsasfile  "&gsasdir.&gsasfile";

	%if &sysver < 6.08 %then %do;
		%let gprolog='2521'x;
		%let gaccess=sasgaedt;
		%end;
	%else %do;
		%let gprolog=;
		%let gaccess=gsasfile;
		%end;

goptions device=&dev gaccess=&gaccess gsfname=gsasfile gsflen=80 
/*   hpos=80   vpos=75                 NeXT pscolor device */
   hpos=70   vpos=65                /* match pscolor device */
   gsfmode=append  gprolog=&gprolog;	
goptions lfactor=3;
goptions ftext=hwpsl009; 
%mend;
