/*
http://support.sas.com/techsup/unotes/SN/013/013133.html
   Beginning in SAS Version 8, you can increase the resolution of PNG,
   JPEG, and TIFFP device driver output directly from the GOPTIONS
   statement using the XMAX, YMAX, XPIXELS, and YPIXELS options.  For
   example, the following statement would create a 300 dpi PNG image:
 
     goptions dev=png xmax=5 ymax=4 xpixels=1500 ypixels=1200;
 
   The ratio of pixels-to-inches determines the resolution.
 
   You can also create a modified device driver at this resolution using
   the following macro:
*/
 
/*
The PNGSZ macro creates a new device driver at the specified size and
resolution. The LFACTOR and default text height can also be set. The
parameters are as follows:

	 Parameter	Description	Default Value
	 DPI		resolution	600
	 NEWDEV		name of new device driver	PNG
	 PTSIZE		default height of text, in points	12
	 W			default width of output	8.5
	 H			default height of output	11
	 UNITS		unit of measurement for H and W	IN (inches)
	 LIBREF		library; the new device driver is stored in LIBREF.devices. The libname must be defined before the macro is run.	GDEVICE0
*/
     %macro PNGSZ(dpi=600, newdev=PNG, ptsize=12, w=8.5, h=11, units=IN,
            libref=GDEVICE0);
       %let XPIXELS  = %sysevalf(&dpi * &w);
       %let YPIXELS  = %sysevalf(&dpi * &h);
       %let ROWS     = %sysevalf(&h / (&PTSIZE/72), floor);
       %let LFACTOR  = %sysevalf(&dpi / 100, floor);
       proc gdevice c=&libref..devices nofs noprompt;
         copy PNG from=sashelp.devices newname=&newdev;
         mod &newdev
             xmax    = &w &units  ymax    = &h &units
             xpixels = &XPIXELS   ypixels = &YPIXELS
             lrows   = 0          lcols   = 0
             prows   = &ROWS      pcols   = 70
             lfactor=&lfactor
             devopts = '3502304009280008'x
         %if &sysver GE 9.1 %then
             charrec = (0, 1, 1, 'SAS Monospace', 'Y');
         ;
       quit;
     %mend PNGSZ;
/* 
   This device driver also adjusts the font sizing and line widths so they
   do not appear too small.
 
   For example, you can create a 300 dpi device driver as follows:
 
     libname gdevice0 '&lt;DIRECTORY&gt;';
     %pngsz(dpi=300,newdev=PNG300);
 
   Reference this device driver on the GOPTIONS statement, and use HSIZE
   and VSIZE options to control the image size, as follows:
 
     goptions dev=png300 hsize=5 vsize=4;
 
   In subsequent SAS sessions, the GDEVICE0 library must be assigned before
   the device will be available.  However, you will not need to recreate
   the device using the macro.  If you are running SAS 9.1 or higher, it is
   recommended that the FONTREG procedure be run before using this device
   driver to provide additional fonts.
 
   If you will be using the new device driver with the ODS RTF destination,
   see SAS Note <a href="http://support.sas.com/techsup/unotes/SN/013/013130.html">SN-013130</a>.
*/
