/*
From: shiling@math.wayne.edu
Newsgroups: comp.soft-sys.sas
Subject: Re: Can I use "notin" or "in" in macro?
Date: Sat, 10 Jul 1999 22:42:10 GMT

This macro one has the function similar to data step function 'indexw'
*/

%macro indexw(varlist, var);
     %if %index(%str( )&varlist%str( ), %str( )&var%str( )) %then 1;
     %else 0;
   %mend;

/*
   %let vlist= aaaaa aaaa aaa aa;
   %let v=a;
   %put >>>>%indexw(&vlist,&v);
   %let v=aaa;
   %put >>>>%indexw(&vlist,&v);
*/