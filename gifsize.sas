/*
 | The GIFSIZE macro lets you modify the size of the image area
 |    in pixel units.
 |    
 |    Notes:  
 |       1. The dots per inch value (dpi) should be the same as
 |          that used by the driver (dpi=xpixels/xmax).
 | 
 |       2. The maximum width and height values specified as 
 |          parameters should not exceed the xmax and ymax values
 |          of the device used. The default values used below
 |          are arbitrary.
 */
%macro gifsize(w=1280, h=1024, dpi=95, vpos=43, hpos=83);

   %if &dpi<=0 %then
      %put DPI must be greater than zero.;
   %else %do;
      goptions hsize=%sysevalf(&w/&dpi)in vsize=%sysevalf(&h/&dpi)in
               hpos=&hpos                 vpos=&vpos;
   %end;
%mend gifsize; 

/*
 filename out 'sample.gif';                    
goptions dev=gif gsfname=out gsfmode=replace; 
%gifsize(w=300, h=200, dpi=95, vpos=30, hpos=50);
proc gtestit pic=1;run;                       
*/
