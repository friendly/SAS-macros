 /*--------------------------------------------------------------*
  *    Name: png.sas                                             *
  *   Title: Set graphics parameters for PNG file output         *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:   5 Dec 1996 14:30:47                               *
  * Revised:  30 May 2011 09:01:26                               *
  * Version:  1.1-1                                              *
  * Dependencies:                                                *
  *   sasgfile                                                   *
  *   defined                                                    *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The PNG macro sets graphic device options for .png graphics output,
 allowing multiple graphics files to be produced with a given base-
 name.  The PNG macro is typically used together with the GSKIP
 macro (to increment the &fig global variable and re-define the
 gsasfile filename). The DISPLA macro can be used to control whether 
 graphic output is produced or suppressed, typically for use with
 PROC GREPLAY or the PANELS macro.

=Usage:

	%png(); 	    *-- tries to get basename from ENV{'SASFILE'};
	%png(mygraph);  *-- sets basename to mygraph.png;

	proc gplot ... ;
		run;
	%gskip;
	proc gplot ...
		run;

	%gskip;
	%gdispla(OFF);
	proc gplot ...
		by sex employed;
	%gdispla(ON);
	%panels(rows=2, cols=2);
	
=Notes:

 This macro does not set the size or resolution of the generated .png
 images.  You may want to set the XMAX, YMAX, XPIXELS and YPIXELS
 graphics options in a GOPTIONS statement after calling this macro.
 E.g. for 600 dpi 6x4 images, use:
 
   goptions xmax=6in ymax=4in xpixels=3600 ypixels=2400;

 Under SAS 9.2, the PNG driver may produce lower resolution graphical output
 with large XPIXELS and YPIXELS.  Use the ZPNG driver, or PNG300 for 300 dpi
 output.
  
=Bugs:

 Doesn't work with procedures that produce multiple output graphics, e.g.,
 with a BY statement.  Workaround: Use a macro to produce the set of graphs
 by subsettting the data with a WHERE statement. See:
	http://ftp.sas.com/techsup/download/technote/ts411.html 
 
 =*/

%global driver fig gsasfile gsasdir devtyp;

%macro png(fn,
	defdriv=PNG  /* Default to PNG driver  */
);
   %sasgfile(png,&fn);
   %if not %defined(DRIVER) %then
       %let driver =%SYSGET(DRIVER);
   %if &driver=%str() %then %let driver = &defdriv;  
   %*if &driver=%str() %then %let driver = png;
   %let dev=&driver;
   %let devtyp = PNG;
   %let fig=1;
   %if %defined(gsasdir)=0 %then %let gsasdir=;
   %put PNG: gsasfile is: "&gsasdir.&gsasfile" (&dev driver);
   filename gsasfile  "&gsasdir.&gsasfile";

	goptions device=&dev gaccess=gsasfile 
/*   hpos=80   vpos=75                 match pscolor device */
		 gsflen=80 gsfmode=replace;
%mend png;
