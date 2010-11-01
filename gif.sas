 /*--------------------------------------------------------------*
  *    Name: gif.sas                                             *
  *   Title: Set graphics parameters for GIF file output         *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  5 Dec 1996 14:30:47                                *
  * Revised: 17 May 2005 10:07:30                                *
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

* DEVICE=  Set the default GIF device driver.  If not specified,
           the macro looks for a global macro variable, DRIVER,
           or an ENVIRONMENT variable by the same name.  Otrherwise,
           the default GIF driver is used.

* HSIZE=   Horizontal size of the GIF output file(s).  If not specified,
           the maximum hsize for the selected device is used.  You can
		   also use GOPTIONS HSIZE= (and VSIZE=) options to control the
		   size of the image.

* VSIZE=   Vertical size of the GIF output file(s).  If not specified,
           the maximum vsize for the selected device is used.

=Notes:

 The default driver=gif defines a landscape format with
   XMAX=8.42 in, YMAX=6.31 in
 No scalable or hardware fonts -- use software fonts (e.g., ftext=swiss)


=*/
%global driver fig gsasfile gsasdir devtyp;
%macro gif(
	fn,
	device=, 
	hsize=, 
	vsize=
	);

	%*-- Get the basename of the graphic file(s);
   %sasgfile(gif,&fn);

	%if %length(&device) %then %do;
		%let dev=&device;
		%end;
	%else %do;
		%if not %defined(DRIVER) %then
    		%let driver =%SYSGET(DRIVER);
		%if &driver=%str() %then %let driver = gif;
		%let dev=&driver;
    	%end;
		
   %let devtyp = GIF;
   %let fig=1;
   %if %defined(gsasdir)=0 %then %let gsasdir=;
   %put GIF: gsasfile is: "&gsasdir.&gsasfile" (&dev driver);
   filename gsasfile  "&gsasdir.&gsasfile";

	goptions device=&dev gaccess=gsasfile 
/*   hpos=80   vpos=75                 match pscolor device */
		 gsflen=80 gsfmode=replace;
	goptions vsize=&vsize hsize=&hsize;
%mend gif;
