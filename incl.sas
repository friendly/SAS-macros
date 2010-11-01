%macro incl(program);
%*---------------------------------------------------------*;
%* System-independent version of INCLUDE statement         *;
%* Assumes DDNAME on MVS is 'IN'                           *;
%* (has not been tested on other systems, however)         *;
%*---------------------------------------------------------*;
   %if &SYSSCP = MVS %then %str(%include IN(&program););
                     %else %str(%include &program;);
%mend;
