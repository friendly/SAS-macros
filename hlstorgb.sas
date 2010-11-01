/*  -----------------------------------------------------------------------
    Program  :  HLStoRGB.sas

    Author   :  Perry Watts
    Date     :  Friday, April 11, 2003
    
    Path     :  Pgm\Mac

    Purpose  :  Use Foley and Van Dam Procedure HLS_TO_RGB on p. 619
                to calculate the corresponding RGB value.  
    ------------------------------------------------------------------- */

   %macro hlstorgb(h,l,s);
       %local hue light sat m1 m2 rhue ghue bhue r g b rr gg bb;
       %let hue=%sysevalf(&h - 120);
       %let light =%sysevalf(&l/255);
       %let sat=%sysevalf(&s/255);
       %if &light le 0.5 %then %let m2=%sysevalf(&light*(1+&sat.));
       %else %let m2=%sysevalf(&light+&sat.-&light*&sat);
       %let m1 = %sysevalf(2 * &light. - &m2.);
       %if &sat eq 0 %then %do;
         %let r=&l; %let g=&l; %let b=&l;
       %end;
       %else %do;
         %let rhue=%eval(&hue+120);
         %if &rhue gt 360 %then %let rhue= %eval(&rhue.-360);
         %if &rhue lt 0 %then %let rhue= %eval(&rhue.+360);
         %if &rhue lt 60 %then %let r = %sysevalf((&m1+(&m2-&m1)*&rhue./60)*255);
         %else %if &rhue lt 180 %then %let r=%sysevalf(255*&m2.);
         %else %if &rhue lt 240 %then %let r=%sysevalf((&m1+(&m2-&m1)*(240-&rhue)/60)*255);
         %else %let r=%sysevalf(255*&m1);
 
         %let ghue=&hue; 
         %if &ghue gt 360 %then %let ghue= %eval(&ghue.-360);
         %if &ghue lt 0 %then %let ghue= %eval(&ghue.+360);
         %if &ghue lt 60 %then %let g = %sysevalf((&m1+(&m2-&m1)*&ghue./60)*255);
         %else %if &ghue lt 180 %then %let g=%sysevalf(255*&m2.);
         %else %if &ghue lt 240 %then %let g=%sysevalf((&m1+(&m2-&m1)*(240-&ghue)/60)*255);
         %else %let g=%sysevalf(255*&m1);

         %let bhue=%eval(&hue-120); 
         %if &bhue gt 360 %then %let bhue= %eval(&bhue.-360);
         %if &bhue lt 0 %then %let bhue= %eval(&bhue.+360);
         %if &bhue lt 60 %then %let b = %sysevalf((&m1+(&m2-&m1)*&bhue./60)*255);
         %else %if &bhue lt 180 %then %let b=%sysevalf(255*&m2.);
         %else %if &bhue lt 240 %then %let b=%sysevalf((&m1+(&m2-&m1)*(240-&bhue)/60)*255);
         %else %let b=%sysevalf(255*&m1);
       %end;
       %let rr=%sysfunc(putn(&r,3.));
       %let gg=%sysfunc(putn(&g,3.));
       %let bb=%sysfunc(putn(&b,3.));
       &rr.,&gg.,&bb.

    %mend hlstorgb;