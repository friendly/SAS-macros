%macro greset;
%*--------------------------------------------------------*;
%*   Handle resetting graphics options to allow programs  *;
%*   from SSSG to be run sequentially in the same session *;
%*--------------------------------------------------------*;
   %if &sysver >= 6
       %then
          %do;
            GOPTIONS RESET=GLOBAL V5COMP;
            OPTIONS  NOGWINDOW;
          %end;
       %else
          %do;
             TITLE; FOOTNOTE;
             %do i = 1 %to 10;
                 SYMBOL&i ;
                 PATTERN&i;
             %end;
          %end;
%mend;
