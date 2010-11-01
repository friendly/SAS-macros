%macro gdelete(catalog, what);
%if %length(&catalog)=0
    %then %let catalog=work.gseg;
%if %length(&what)=0
    %then %let what=_all_;

%if &sysver>6.12 %then %do;
    %if %sysfunc(cexist(&catalog)) %then %do;
        proc greplay igout=&catalog nofs;
            delete &what;
        %end;
    %end;
%else %do;
        proc greplay igout=&catalog nofs;
            delete &what;
    %end;

%mend;
