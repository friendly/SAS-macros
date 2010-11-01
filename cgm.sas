 /*--------------------------------------------------------------*
  *    Name: cgm.sas                                             *
  *   Title: Set graphics parameters for CGM file output         *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  5 Dec 1996 14:30:47                                *
  * Revised: 17 Apr 2003 13:59:37                                *
  * Version:  1.2                                                *
  * Dependencies:                                                *
  *   sasgfile                                                   *
  *   defined                                                    *
  *--------------------------------------------------------------*/

/*=
=Description:

=Parameters:

* FN       Basename of the graphic file(s).  If not specified,
           the basename is looked for in the environment variables
		   SASFILE or GSASFILE.

* DEVICE=  Set the default CGM device driver.  If not specified,
           the macro looks for a global macro variable, DRIVER,
           or an ENVIRONMENT variable by the same name.  Otrherwise,
           the default CGMOF97L driver is used.

* HSIZE=   Horizontal size of the CGM output file(s).  If not specified,
           the maximum hsize for the selected device is used.

* VSIZE=   Vertical size of the CGM output file(s).  If not specified,
           the maximum vsize for the selected device is used.

=Notes:

 The default driver=cgmof97L defines a landscape format with
   XMAX=11 in, YMAX=8.5 in
   Scalable fonts --
       HWCGM001 - Helvetica
       HWCGM002 - HelveticaBold
       HWCGM003 - HelveticaItalic
       HWCGM004 - HelveticaBoldItalic
	   HWGCM005-008 - Times Roman
	   HWGCM009-012 - Courier
	   HWCGM013 - Symbol
	   HWCGM014 - ZapfDingbats

=*/
%global driver fig gsasfile gsasdir devtyp;
%macro cgm(
	fn,
	device=, 
	hsize=, 
	vsize=,
	ftext=hwcgm001
	);

	%*-- Get the basename of the graphic file(s);
   %sasgfile(cgm,&fn);

	%if %length(&device) %then %do;
		%let dev=&device;
		%end;
	%else %do;
		%if not %defined(DRIVER) %then
    		%let driver =%SYSGET(DRIVER);
		%if &driver=%str() %then %let driver = CGMOF97L;
		%let dev=&driver;
    	%end;
		
   %let devtyp = CGM;
   %let fig=1;
   %if %defined(gsasdir)=0 %then %let gsasdir=;
   %put CGM: gsasfile is: "&gsasdir.&gsasfile" (&dev driver);
   filename gsasfile  "&gsasdir.&gsasfile";

	goptions device=&dev gaccess=gsasfile 
/*   hpos=80   vpos=75                 match pscolor device */
		 gsflen=80 gsfmode=replace;
	goptions vsize=&vsize hsize=&hsize ftext=&ftext;
%mend cgm;
