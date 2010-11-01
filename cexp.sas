/*  macro for conditional expressions, of the form,
    var = exprA ? exprB : exprC
 
translate to:
 
    if (exprA) then var = exprB ;
               else var = exprC ;
*/
 
%macro cexp(var, exprABC);
 
   %let exprA = %scan(&exprABC,1,':?');
   %let exprB = %scan(&exprABC,2,':?');
   %let exprC = %scan(&exprABC,3,':?');
 
   %put exprA=|&exprA| ;
   %put exprB=|&exprB| ;
   %put exprC=|&exprC| ;
 
   %if &exprB = %str() %then %do;
      &var = &exprA ;
   %end;
   %else %do;
      if (&exprA) then &var = &exprB %str(;)
                  else &var = &exprC %str(;)
   %end;
 
%mend cexp;
 
*cexp(y, A<5 ? 1 : 0);
Data test;
  %cexp(x,27);
  do i=1 to 10;
     %cexp(position, mod(i,2)=0 ? "4" : "6" )
     output;
     end;
proc print;
