/* ---------------------------------------------------------------
TITLE:  Handle temporary files in a system- and version-independent way.
 --------------------------------------------------------------- */

%macro tempfile(fileref,ls,options);
   %global tempfn;
   %if %length(&ls)=0 %then %let ls=80;

   %if %sysevalf(&sysver  > 6.10) %then %do;
		filename &fileref temp &options
			%if %length(&ls)>0 %then lrecl=&ls;
			;		
		%end;
	%else %do;
		%if &sysscp = CMS
			%then %let tempfn=&fileref output a;
		%else %if &sysscp = WIN 
			%then %let tempfn=&fileref..tmp;
		%else /* assume unix */ 
			%let tempfn=/tmp/&fileref..tmp;
		filename &fileref "&tempfn" lrecl=&ls &options;
		%end;
%mend;

%macro tempdel(fileref);
   %global tempfn;
	%if length(&tempfn)=0 %then %goto done;
    *-- Avoid annoying flash with X commands;
    %if %sysevalf(&sysver  > 6.10) %then %do;
        %let rc=%sysfunc(fdelete(&fileref));
        %let rc=%sysfunc(filename(&fileref,''));
    %end;

    %else %do;
		%if &sysscp = CMS
			%then cms erase &tempfn;
		%else %if &sysscp = WIN
			%then %do;
				options noxsync noxwait; run;
				%sysexec(erase &tempfn); run;
				options   xsync   xwait; run;
			%end;
		%else /* assume flavor of UNIX */
				%sysexec(rm -f &tempfn);
    %end;
	 filename &fileref clear;
	 %let tempfn=;
%done:
%mend;
