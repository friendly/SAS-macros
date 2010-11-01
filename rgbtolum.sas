/*  -----------------------------------------------------------------------
    Program  :  RGBtoLum.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Path     :  Pgm\Mac

    Purpose  :  Use Foley and Van Dam Procedure RGB_TO_HLS on p. 618
                to calculate Tektronix HLS given RGB code from
                MS PowerPoint.               
    ------------------------------------------------------------------- */

   %macro rgbtolum(r,g,b);
       %local red green blue mmax mmin ll lite;
       %let red = %sysevalf(&r/255);
       %let green =%sysevalf(&g/255);
       %let blue=%sysevalf(&b/255);
       %let mmax=%sysfunc(max(&red,&green,&blue));
       %let mmin=%sysfunc(min(&red,&green,&blue));
       %let ll=%sysevalf((&mmax+&mmin)/2);
       %let lite=%sysfunc(round(%sysevalf(&ll*255)));
       %sysfunc(putn(&lite,3.))
    %mend rgbtolum;