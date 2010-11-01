/*  -----------------------------------------------------------------------
    Program  :  HLSDec.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Project  :  NESUG-16 AdvTut Color
    Path     :  Pgm\Mac

    Purpose  :  Convert a SAS HEX HLS color to its decimal equivalent.  
                (For more readable codes).
 
    Input    :  A SAS HLS color Code Hhhhllss where hhhllss are
                three hexadecimal numbers hhh, ll, and ss. 

    Output   :  A String representing three decimal numbers
                Dhhhlllsss.
    ------------------------------------------------------------------- */

 %macro hlsdec(Hhhhllss);
  %local hue lite sat;
  %let hue=%substr(&Hhhhllss,2,3);
  %let lite=%substr(&Hhhhllss,5,2);
  %let sat=%substr(&Hhhhllss,7);
  %sysfunc(compress(D%sysfunc(putn(%sysfunc(inputn(&hue,hex3.)),z3.))
                     %sysfunc(putn(%sysfunc(inputn(&lite,hex2.)),z3.))
                     %sysfunc(putn(%sysfunc(inputn(&sat,hex2.)),z3.))))
 %mend hlsdec;


