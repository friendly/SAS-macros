 /*--------------------------------------------------------------*
  *    Name: jpg.sas                                             *
  *   Title: Set graphics parameters for JPG file output         *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:   5 Dec 1996 14:30:47                               *
  * Revised:  29 Dec 2002 13:16:43                               *
  * Version:  1.1                                                *
  * Dependencies:                                                *
  *   sasgfile                                                   *
  *   defined                                                    *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The JPG macro sets graphic device options for .jpg graphics output,
 allowing multiple graphics files to be produced with a given base-
 name.  The JPG macro is typically used together with the GSKIP
 macro (to increment the &fig global variable and re-define the
 gsasfile filename). The DISPLA macro can be used to control whether 
 graphic output is produced or suppressed, typically for use with
 PROC GREPLAY or the PANELS macro.

=Usage:

	%jpg(); 	    *-- tries to get basename from ENV{'SASFILE'};
	%jpg(mygraph);  *-- sets basename to mygraph.jpg;

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

 This macro does not set the size or resolution of the generated .jpg
 images.  You may want to set the XMAX, YMAX, XPIXELS and YPIXELS
 graphics options in a GOPTIONS statement after calling this macro.
 E.g. for 600 dpi 6x4 images, use:
 
   goptions xmax=6in ymax=4in xpixels=3600 ypixels=2400;
 
=Bugs:

 Doesn't work with procedures that produce multiple output graphics, e.g.,
 with a BY statement.  Workaround: Use a macro to produce the set of graphs
 by subsettting the data with a WHERE statement. See:
	http://ftp.sas.com/techsup/download/technote/ts411.html 
 
 =*/

%global driver fig gsasfile gsasdir devtyp;

%macro jpg(fn);
   %sasgfile(jpg,&fn);
   %if not %defined(DRIVER) %then
       %let driver =%SYSGET(DRIVER);
   %if &driver=%str() %then %let driver = jpeg;
   %let dev=&driver;
   %let devtyp = JPG;
   %let fig=1;
   %if %defined(gsasdir)=0 %then %let gsasdir=;
   %put JPG: gsasfile is: "&gsasdir.&gsasfile" (&dev driver);
   filename gsasfile  "&gsasdir.&gsasfile";

	goptions device=&dev gaccess=gsasfile 
/*   hpos=80   vpos=75                 match pscolor device */
		 gsflen=80 gsfmode=replace;
%mend jpg;
