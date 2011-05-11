 /*--------------------------------------------------------------*
  *    Name: gsize.sas                                           *
  *   Title: Set SAS/Graph size in a device-independent way      *
        Doc: http://datavis.ca/sasmac/nodoc.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 11 May 2009 15:17:52                                *
  * Revised: 24 Jan 2010 13:08:41                                *
  * Version: 1.0-1                                               *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The GSIZE macro is designed to deal with the following infelicity
 in SAS/Graph:  For most graphic devices, the size of graphs is set
 by the HSIZE and GSIZE graphic options; however, for pixel-based
 devices, you must use XPIXELS and YPIXELS.  Thus, you have to change
 the GOPTIONS you set to switch from, e.g., a PS or PDF device to
 a pixel-based device (e.g., JPG, GIF).
 
 The GSIZE macro allows all graphic output sizes to be set via
 HSIZE and VSIZE, with units in IN or CM.

=Usage:

 The GSIZE macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%global devtyp; %let devtyp=JPG;
	%gsize(hsize=6, vsize=6, options=htext=1.8);
 
==Parameters:

* HSIZE=      Horizontal size

* VSIZE=      Vertical size

* UNIT=       Unit for HSIZE and VSIZE: either IN or CM [Default: UNIT=IN]

* XPIXELS=    Horizontal size (pixels)

* YPIXELS=    Vertical size (pixels)

* OPTIONS=    Other graphic options

* DPI=        Dots per inch, for pixel devices [Default: DPI=96]

==Global parameters:

 The macro uses one global macro parameter, DEVTYP, to determine whether
 the current graphic device driver is pixel-based. This parameter can be initialized in 
 the AUTOEXEC.SAS file, or in device-specific macros

* DEVTYP    String value, the type of graphic device driver.  The
            values GIF, JPG and PNG cause the macro to translate
			HSIZE and VSIZE to XPIXELS and YPIXELS.
            All other values are ignored.


=Notes:

 Device drivers in SAS/Graph have a maximum horizontal and vertical size
 set as XMAX and YMAX. This macro doesn't check whether the computed
 sizes exceeed those limits.
                

 =*/
%global devtyp;

%macro gsize(
	hsize=,     /* Horizontal size (in.)     */
	vsize=,     /* Vertical size (in.)       */
	unit=IN,
	xpixels=,   /* Horizontal size (pixels)  */
	ypixels=,   /* Vertical size (pixels)    */
	options=,   /* Other graphic options     */
	dpi=96      /* dots per inch, for pixel devices */ 
	);

	%*-- List of device types requiring size in pizels;
	%local pixdevs thisdev;
	%let pixdevs = %str( ) GIF JPG PNG %str( );
	%if %symexist(devtyp) %then %do;
		%let thisdev = %str( )%upcase(&devtyp)%str( );
		%end;
	%else %do;
		%let devtyp=;
		%let thisdev = &SYSDEVIC;
		%end;
	%*put GSIZE: pixdevs = |&pixdevs| thisdev=|&thisdev| devtyp=|&devtyp|;

	%*-- Determine current hsize and vsize settings.  Assume inches;
	%*put HSIZE = %sysfunc(getoption(HSIZE));
	%*put VSIZE = %sysfunc(getoption(VSIZE));
	
	%let HS = %scan(%sysfunc(getoption(HSIZE)), 1, %str( ));
	%let VS = %scan(%sysfunc(getoption(VSIZE)), 1, %str( ));
	%let VU = %scan(%sysfunc(getoption(VSIZE)), 2, %str( ));
	%let GU = %upcase(%scan(&VU, 1, %str( .)));
	%let XM = %scan(%sysfunc(getoption(XMAX)), 1, %str( ));
	%let YM = %scan(%sysfunc(getoption(YMAX)), 1, %str( ));
	
	%put GSIZE: current HSIZE= &HS  VSIZE= &VS  GU=&GU  XMAX=&XM  YMAX=&YM;
	%if %length(&hsize)=0 %then %let hsize=&HS;
	%if %length(&vsize)=0 %then %let vsize=&VS;

	%*-- If the current device is a pixel device,translate (HSIZE, VSIZE) to (XPIXELS, YPIXELS);
	%if %index(&pixdevs, &thisdev) > 0 %then %do;
		%let mult=1;
		%if %upcase(&unit) = CM %then %let mult=2.54;
		%if %length(&xpixels)=0 %then %let xpixels=%sysevalf(&hsize*&dpi*&mult);
		%if %length(&ypixels)=0 %then %let ypixels=%sysevalf(&vsize*&dpi*&mult);
		goptions xpixels=&xpixels ypixels=&ypixels &options;
		%put GSIZE: Setting xpixels=&xpixels ypixels=&ypixels &options;
	%end;

	%else %do;
		goptions hsize=&hsize &unit. vsize=&vsize &unit. &options;
		%put GSIZE: Setting hsize=&hsize &unit. vsize=&vsize &unit. &options;
	%end;
	

%mend;
