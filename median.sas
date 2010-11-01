/*
Find the median of an array of variables

data med_on1;
   array a(&nvars);
   set x;
   med = %median(a);
 run;

*/
%macro median(vprefix);
   mean(ordinal(floor((dim(&vprefix)+1)*.5),of &vprefix(*)),
        ordinal(ceil ((dim(&vprefix)+1)*.5),of &vprefix(*)))
%mend median;
