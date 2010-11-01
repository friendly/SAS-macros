%macro version(v);
%*--------------------------------------------------------;
%* title: Check for proper SAS version for a macro        ;
%* Macro to reflect version requirements of SSSG programs ;
%*   v=5.18 indicates programs which must run on 5.18     ;
%*   v=6 or v=6.06 indicate programs which must run on    ;
%*          the specified version or later.               ;
%*--------------------------------------------------------;
%if &v = 5.18 and &v = &sysver %then %goto nop;
%if &v ^=5.18 and &v <= &sysver %then %goto nop;
    %put This program requires SAS Version &v.. It cannot run in &sysver;
    data _null_;
       abort;
    run;
%nop:
%mend;
