/*  -----------------------------------------------------------------------
    Program  :  RGBDec.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Project  :  NESUG-16 AdvTut Color
    Path     :  Pgm\Mac
 
    Purpose  :  Convert an RGB Hex color color to three decimal numbers
                PowerPoint understands (inverse of RGBHEX.sas)

    Input    :  A SAS RGB color Code CXrrggbb where rrggbb are
                three hexadecimal numbers. 

    Output   :  A String representing three decimal numbers
                Drrrgggbbb.
    ------------------------------------------------------------------- */

 %macro rgbdec(CXrrggbb);
  %local rr gg bb;
  %let rr=%substr(&CXrrggbb,3,2);
  %let gg=%substr(&CXrrggbb,5,2);
  %let bb=%substr(&CXrrggbb,7);
  %sysfunc(compress(D%sysfunc(putn(%sysfunc(inputn(&rr,hex2.)),z3.))
                     %sysfunc(putn(%sysfunc(inputn(&gg,hex2.)),z3.))
                     %sysfunc(putn(%sysfunc(inputn(&bb,hex2.)),z3.))))
 %mend rgbdec;
