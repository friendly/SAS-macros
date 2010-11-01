 /*--------------------------------------------------------------*
  *    Name: eps.sas                                             *
  *   Title: Set graphics parameters for EPS file output         *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  5 Dec 1996 14:30:47                                *
  * Revised: 09 May 2005 08:24:20                                *
  * Version:  1.2                                                *
  *  1.2  Added some documentation, and made some defaults       *
  *       optional keyword parameters                            *
  *       Fixed GEND problem for 9.1.3                           *
  * Dependencies:                                                *
  *   sasgfile (get basename of current SAS file)                *
  *   defined (is a macro variable defined?)                     *
  * Related:                                                     *
  *   gskip (advance &FIG device independently)                  *
  *   gdispla (turn graphics output on/off device independently) *
  *--------------------------------------------------------------*/
/*=
=Description:

 The EPS macro is designed to make it easier to produce SAS/Graph
 output in .eps format, particularly when SAS is run in batch or
 from a script, and when the SAS program may produce multiple graphs,
 which should be output to separate .eps files.  In this case, a
 file, myprog.sas should produce files myprog1.eps, myprog2.eps, ...
 
 The EPS macro initializes a particular EPS driver (&DRIVER), a
 device type (&DEVTYP), an output directory (&GSASDIR), a
 basename (&GSASFILE) for the generated .eps files, and a figure
 number (&FIG), all of which are global macro parameters.

 The current implementation was designed to allow the basename of
 graphic output files (&GSASFILE) and the EPS driver (&DRIVER)
 to be set as environment variables in a shell script or .bat file,
 so these things could be changed without modifying the .sas source.
 It is, unfortunately, not possible to test for the existence of
 a variable with %SYSGET() without generating a (harmless) warning
 message.  
 
 Between separate graphic procedures, call the %gskip() macro to
 advance the figure (&FIG) counter (ignored for devices which
 accommodate multiple graphic outputs).  You can also use the
 %gdispla() macro to suspend/restore graphic output in a device-
 independent way, e.g., to generate a series of graphs to a
 graphic catalog, and then replay them together with GREPLAY
 without producing separate outputs of the individual graphs.
 
=*/

%global driver fig gsasfile gsasdir devtyp;

%macro eps(fn,
	defdriv=PSLEPSFC,  /* Default EPS driver (thin lines/color) */
	ftext=hwpsl009     /* Default text font: Helvetica */
	);

   %sasgfile(eps,&fn);
	%let driver =%SYSGET(DRIVER);
   %if &driver=%str() %then %let driver = &defdriv;  *-- PSLEPSF for b/w;
	%let dev=&driver;

	%*-- DEVTYP is used by %gskip and %gdispla;
	%let devtyp = EPS;
	%let fig=1;

   %if %defined(gsasdir)=0 %then %let gsasdir=;
	%if %length(&gsasdir) > 0 %then %do;
		%let gsasdir = %trim(&gsasdir);
		%if &sysscp=WIN
			%then %let dirsep=\;
			%else %let dirsep=/;
		%if "%substr(&gsasdir, %length(&gsasdir), 1)" ^= "&dirsep"
			%then %let gsasdir = &gsasdir.&&dirsep;
		%end;

	%*-- Fix some goptions for version differences;
	%local gstuff;
	%let gstuff=;
	%if &sysver < 6.08 %then %do;  %*-- SAS 6.07 puts out bad EPS;
		%let gstuff=%str(gend='0A'x  gepilog='showpage' '0A'x);
		%end;
	%else %if &sysver < 9 %then %do;
		%let gstuff = nofileonly;
		%*-- need to set gsasfile=null here, but that requires
		   using name="&gsasfile" throughout, so just revert to; 
		%let gstuff =;
		%end;
	/*  Kludge added to fix GEND problem with SAS 9.1.3 */
	%else %do;
		%let gstuff =%str(gend='0A'x);
		%let gstuff =%str(gsfname=gsasfile gend='0A'x);
		%end;

   %put EPS: gsasfile is: "&gsasdir.&gsasfile";
   filename gsasfile  "&gsasdir.&gsasfile";

	goptions horigin=.5in vorigin=.5in; *-- override, for BBfix;
	goptions device=&dev gaccess=gsasfile 
		 &gstuff
		 characters ftext=&ftext /*target= */
	   hpos=70   vpos=65  lfactor=3  /* match pscolor device */
		 gsflen=80 gsfmode=replace;
%mend eps;
