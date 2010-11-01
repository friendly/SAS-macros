%macro unique(s);                               
   %let i = 1;                                 
   %let w1 = %scan(&s,1);                      
   %do %while (&&w&i ne );                     
       %let i = %eval(&i + 1);                 
       %let w&i = %scan(&s,&i);                
   %end;                                       
   %do j=1 %to %eval(&i-1);                    
      %do k=%eval(&j+1) %to %eval(&i-2);       
          %if &&w&j = &&w&k %then %let w&k = ; 
      %end;                                    
   %end;                                       
   %let z =;                                   
   %do j=1 %to %eval(&i-1);                    
      %let z = &z &&w&j;                       
   %end;                                       
   &z                                          
%mend unique;           
/*
From: "Paul M. Dorfman" <pdorfma@ucs.att.com>
Newsgroups: comp.soft-sys.sas
Subject: Re: Removing duplicate strings in a macro variable?
Date: Sun, 18 Oct 1998 17:08:33 -0400

Now we can simply use this macro as a macro function, for instance:

%LET DDSTR = %unique (AAA BBB CCC AAA BBB 111);    

Of course, the argument of %unique can also be a macro reference, and the
result can be assigned either to the macrovariable being deduped or
something else. For example:

%LET &VAR1 = %unique (&VAR1);    

%LET &VAR2 = %unique (&VAR1);    
*/