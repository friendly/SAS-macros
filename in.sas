/*
Here is another version of David's macro.  It eliminates the need to place
surrounding symbols around the list.  Some may see this as advantage, while
others may think it a disadvantage.  It also allows the separator to be any
of the standard ones accepted by SCAN and used in a legal manner ( i.e. no
use of 1(2(3 ).  One should also point out that either macro allows
characters and possibly characters in quotes.

In any case, it illustrates the use of the PARMBUFF option.
*/

%macro in ( var , list ) / parmbuff ;
   %local i w return ;
   %let list = %quote(&syspbuff) ;
   %let list = %qsubstr( &list,2, %length(&list)-2 ) ;
   %let return = 0 ;
   %let i = 1 ;
   %let var = %scan(&list,&i) ;
   %let i = %eval (&i + 1) ;
   %let w = %scan(&list,&i) ;
   %do %while ( %quote(&w) ^= %str() ) ;
       %let return = %eval ( &&&var = &w ) ;
       %if &return = 1 %then %goto mexit ;
       %let i = %eval (&i + 1) ;
       %let w = %scan(&list,&i) ;
   %end ;
%mexit:
   &return
%mend in ;

/*
%let x = 7 ;
%put %eval(%in(x,1,2,3)) ;
%put %eval(%in(x,(1,2, 3, 7))) ;

IanWhitlock@westat.com
*/

