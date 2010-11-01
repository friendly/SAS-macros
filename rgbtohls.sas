/*  -----------------------------------------------------------------------
    Program  :  RGBtoHLS.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Project  :  NESUG-16 AdvTut Color
    Path     :  Pgm\Mac

    Purpose  :  Use Foley and Van Dam Procedure RGB_TO_HLS on p. 618
                to calculate Tektronix HLS given an RGB code.

    Notes    :  This macro simply calls three subordinate macros for
                the three components of an HLS code. 
    ------------------------------------------------------------------- */

   %macro rgbtohls(r,g,b);
     %rgbtohue(&r,&g,&b),%rgbtolum(&r,&g,&b),%rgbtosat(&r,&g,&b)
   %mend rgbtohls;