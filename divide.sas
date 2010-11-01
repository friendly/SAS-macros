%*--- division macro ---;
%macro divide(a,b,decimals);
   %let div = %eval(&a/&b);
   %let rem = %eval(&a - &b * &div);
   %if &rem = 0
       %then &div;
   %else %do;
       %let div = &div.. ;
       %do i=1 %to &decimals;
          %let r = %eval(10*&rem/&b);
          %let div = &div&r;
          %let rem = %eval(10*&rem - &b*&r);
   %put i: &i div: &div (rem &rem) R &r ;
          %end;
       &div;
       %end;
%mend;
 
%put divide(100,3,2) = %divide(100,3,2);
%put divide(300,3,2) = %divide(300,3,2);
%put divide(100,7,9) = %divide(100,7,9);
