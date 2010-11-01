/*  -----------------------------------------------------------------------
    Program  :  RGBHex.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Project  :  NESUG-16 AdvTut Color
    Path     :  Pgm\Mac
 
    Purpose  :  Convert Decimal RGB color to HEX which SAS understands.
                Round first, because HEX truncates.

    Input    :  Three decimal numbers representing a color in terms
                of its red, greeen, and blue components.

    Output   :  a SAS RGB color Code CXrrggbb where rrggbb are
                three hexadecimal numbers.
    ------------------------------------------------------------------- */

 %macro rgbhex(rr,gg,bb);
  %let rr=%sysfunc(putn(&rr,3.));
  %let gg=%sysfunc(putn(&gg,3.));
  %let bb=%sysfunc(putn(&bb,3.));
  %sysfunc(compress(CX%sysfunc(putn(&rr,hex2.))
                     %sysfunc(putn(&gg,hex2.))
                     %sysfunc(putn(&bb,hex2.))))
 %mend rgbhex;
