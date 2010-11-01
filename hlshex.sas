/*  -----------------------------------------------------------------------
    Program  :  HLSHex.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Project  :  NESUG-16 AdvTut Color
    Path     :  Pgm\Mac                         

    Purpose  :  Convert SAS Decimal HLS color to HEX which SAS understands.
                Round first, because HEX fcns truncate.

    Input    :  Three decimal numbers for HLS color components.
  
    Output   :  a SAS HLS color Code Hhhhllss where hhh,ll,ss are
                three hexadecimal numbers.
    ------------------------------------------------------------------- */

 %macro hlshex(hhh,ll,ss);
  %let hhh=%sysfunc(putn(&hhh,3.));
  %let ll=%sysfunc(putn(&ll,3.));
  %let ss=%sysfunc(putn(&ss,3.));

  %sysfunc(compress(H%sysfunc(putn(&hhh,hex3.))
                     %sysfunc(putn(&ll,hex2.))
                     %sysfunc(putn(&ss,hex2.))))
 %mend hlshex;
