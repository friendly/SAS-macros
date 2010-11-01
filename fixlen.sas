/*****************************************************
 *      Pad or trim a tring to a fixed length        *
 * It is used to ajust the title width in display so *
 * that in the animated gif file the length of the   *
 * title will remain the same.                       *
 *****************************************************/
%macro fixlen(in_num, len);

	data _null_;
	  in_n = input(&in_num, 16.);
      call symput('sgn', sign(in_n));
	run;
 %if %sysevalf(%sysevalf(&in_num) = 0) %then 
     %let bstring = %substr(%str(+0.0000), 1, %sysevalf(&len));
 %else %do;
    %let tstr=%str(.);
    %if %sysevalf(%index(%str(&in_num), &tstr) ne 0) %then %do;
        %if %sysevalf(%eval(&sgn = 1)) %then 
         %let bstring = %substr(%str(+)%str(&in_num)%str(00000), 1, %sysevalf(&len));
        %else %let bstring = %substr(%str(&in_num)%str(00000), 1, %sysevalf(&len));
    %end;
	%else %do;
      %if %sysevalf(%eval(&sgn = 1)) %then 
          %let bstring = %substr(%str(+)%str(&in_num)%str(.0000), 1, %sysevalf(&len));
      %else   %let bstring = %substr(%str(&in_num)%str(.0000), 1, %sysevalf(&len));
	%end;
 %end;
%mend;

