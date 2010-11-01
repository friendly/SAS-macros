/*  -----------------------------------------------------------------------
    Program  :  RGBtoSat.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Path     :  Pgm\Mac

    Purpose  :  Use Foley and Van Dam Procedure RGB_TO_HLS on p. 618
                to calculate Tektronix HLS given RGB code from
                MS PowerPoint.               
    ------------------------------------------------------------------- */

   %macro rgbtosat(r,g,b);
       %local red green blue mmax mmin ll sat;
       %let red = %sysevalf(&r/255);
       %let green =%sysevalf(&g/255);
       %let blue=%sysevalf(&b/255);
       %let mmax=%sysfunc(max(&red,&green,&blue));
       %let mmin=%sysfunc(min(&red,&green,&blue));
       %let ll=%sysevalf((&mmax+&mmin)/2);
       %if &mmax eq &mmin %then %let sat=0;
       %else %do;
         %if %sysfunc(putn(&ll,3.1)) le 0.5 %then 
           %let sat=%sysfunc(round((%sysevalf((&mmax-&mmin))/%sysevalf((&mmax+&mmin)))*255));
         %else
           %let sat=%sysfunc(round((%sysevalf((&mmax-&mmin))/%sysevalf((2-&mmax-&mmin)))*255));
        %end;
       %sysfunc(putn(&sat,3.))
    %mend rgbtosat;