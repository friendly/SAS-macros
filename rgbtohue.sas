/*  -----------------------------------------------------------------------
    Program  :  RGBtoHUE.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Path     :  Pgm\Mac

    Purpose  :  Use Foley and Van Dam Procedure RGB_TO_HLS on p. 618
                to calculate Tektronix HUE given RGB code from
                MS PowerPoint. Return a decimal value.            
    ------------------------------------------------------------------- */

   %macro rgbtohue(r,g,b);
    %local red green blue mmax mmin hue;
    %let red = %sysevalf(&r/255);
    %let green =%sysevalf(&g/255);
    %let blue=%sysevalf(&b/255);
    %let mmax=%sysfunc(max(&red,&green,&blue));
    %let mmin=%sysfunc(min(&red,&green,&blue));
    %if &mmax eq &mmin %then %let hue=0;
    %else %do;
      %let rc=%sysevalf((&mmax-&red)/(&mmax-&mmin));
      %let gc=%sysevalf((&mmax-&green)/(&mmax-&mmin));
      %let bc=%sysevalf((&mmax-&blue)/(&mmax-&mmin));
      %if &red eq &mmax %then %let hue=%sysevalf(&bc-&gc);
      %else %if &green eq &mmax %then %let hue=%sysevalf(2+&rc-&bc);
      %else %if &blue eq &mmax %then %let hue=%sysevalf(4+&gc-&rc);
      %let hue=%sysevalf(&hue*60);
      /*if hue lt 0*/
      %if %index(&hue,'-') gt 0 %then %let hue=%sysevalf(&hue+360);
      %let hue=%sysfunc(round(&hue+120));
      %if &hue ge 360 %then %let hue=%eval(&hue-360);
     %end;
    %sysfunc(putn(&hue,3.))
   %mend rgbtohue;