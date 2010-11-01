/*  ---------------------------------------------------------------------------
    Program  :  RGBComp.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003

    Project  :  NESUG-16 AdvTut Color
    Path     :  Pgm\Mac

    Purpose  :  Convert a DECIMAL RGB code to its DECIMAL complement

    Notes    :  The output is comma delimited for the nested invocation
                of the RGBHEX conversion macro.
    ------------------------------------------------------------------------  */

   %macro rgbcomp(rr,gg,bb);
     %local rrc ggc bbc;
     %let rrc=%eval(255-&rr);
     %let ggc=%eval(255-&gg);
     %let bbc=%eval(255-&bb);
     &rrc.,&ggc.,&bbc.
   %mend rgbcomp;
