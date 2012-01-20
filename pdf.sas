 /*--------------------------------------------------------------*
  *    Name: pdf.sas                                             *
  *   Title: Set graphics parameters for PDF file output         *
  *                                                              *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  5 Dec 1996 14:30:47                                *
  * Revised:  5 Jan 1997 10:46:34                                *
  * Version:  1.1                                                *
  * Dependencies:                                                *
  *   sasgfile                                                   *
  *--------------------------------------------------------------*/
 /*=
=Description:

 The PDF macro initializes SAS/GRAPH to produce PDF output.
 It sets the name of the output file based on the SASFILE
 or GSASFILE environment parameters, or 'grfout.pdf' if neither of these
 variables are defined.
 
 The output file is written to the current SAS directory.  You can
 override this by defining a global macro variable GSASDIR.

 =*/
 
%global gsasfile gsasdir devtyp;

%macro pdf(
	fn,
	hsize=6in, 
	vsize=6in
	);

	%let devtyp=PDF;
	%let dev=pdf;
	%local gprolog gaccess;

	%*-- Get the basename of the graphic file(s);
   %sasgfile(pdf,&fn);

   %put PDF: gsasfile is: "&gsasdir.&gsasfile";
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
   hpos=70   vpos=65                /* match pscolor device */
   gsfmode=append  gprolog=&gprolog;	
goptions lfactor=3;
goptions ftext='helvetica'; 
goptions hsize=&hsize vsize=&vsize;
%mend;
