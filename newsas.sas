  %global sasfile;
%macro newsas(program);
%* title:  Restart page numbers, clear windows under SAS Display Mgr;
%* Set the name of the current program & restore page numbers;
  options pageno=1;

  %if &sysenv = FORE and &sysver < 7 %then %do;
   dm 'clear log';
   dm 'clear output';
  %end;

  %let sasfile = %scan(&program,1,.);
%mend;
