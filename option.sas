%macro option(optname, setting);
%* Set a macro variable for a system option, or set a new value, storing
%* the old value in the macro variable.
%* Bugs: For setting a new value, it does not distinguish between
%*       ordinary options and goptions.
%* ;
%let optname=%upcase(&optname);
data _null_;
   set sashelp.voption end=eof;
   retain found 0;
   if optname="&optname"
      then do;
      opt = substr(optname,1,8);
      call symput(opt, setting);
*     put optname= setting=;
      found = 1;
      end;
   %if &setting ^= %str() %then
      %str(options &optname = &setting;);
   if eof then do;
      if ^found then put "Option &optname not found in sashelp.voptions";
      end;
run;  /* force the datastep to complete */
%mend;
