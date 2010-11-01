%macro parse
            (
             var,                 /* input character var or string       */
             with,                /* characters to parse on              */
             action,              /* action for each parsed word         */
             nextword=nextword,   /* variable to assign next word to     */
             nwords=nwords        /* the index of the word in the string */
            );
 
%* Parse "var" with "with". For each parsed word, do "action".;
%*
%*
%* "VAR" is a SAS character variable or character string.
%*
%* "WITH" is a character or set of characters that break "var"
%* into words or phrases (passed to the "scan" function).
%*
%* "ACTION" is an action to take for each word or phrase.
%*
%*   I usually use "LINK label" as the action, and have the subroutine
%*     at "label" issue a "return" to get back here.
%*   If the action is invoking a macro, use the "%nrquote" function.
%*
%*
%*                                          barry grau
%*                                          u42054@uicvm.bitnet
%*                                          u42054@uicvm.uic.edu
%*                                          fall 1987
%*
;
 
length &nextword $ 80;
 
string=%quote(&var);
&nwords=0;
 
* for each parsed "word", set &nextword="word" and do the "action".;
do until (&nextword=' ');
     &nwords=&nwords+1;
     &nextword=scan(string,&nwords,&with);
     if ^(&nextword=' ')
          then do;
              %quote(&action);
              end;
     end;
 
%mend;
