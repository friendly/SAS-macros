 /*-------------------------------------------------------------------*
  *    Name: splot.sas                                                *
  *   Title: Schematic plot (boxplot) for one or more variables       *
        Doc: http://www.datavis.ca/sasmac/splot.html            
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  12 Aug 1991 15:46:41                                    *
  * Revised:  29 Feb 2012 09:50:24                                    *
  * Version:  1.7-1                                                   *
  *  - Added ps= option, %sysfunc functions to delete temp file       *
  * 1.7  Updated to use ODS for V7+                                   *
  *                                                                   *
  *-------------------------------------------------------------------*/
%macro splot(data=_last_,    /* name of input data set           */
               var=,         /* dependent variable(s) for splot  */
               class=,       /* name of grouping variable(s)     */
               id=,          /* ID variable                      */
               ls=80,        /* linesize for output              */
               ps=);

%*-- Save original linesize;
%if %sysevalf(&sysver >6.10) %then %do;
   %let lso=%sysfunc(getoption(ls,keyword));
   %let pso=%sysfunc(getoption(ls,keyword));
   %end;
%else %do; %let lso=; %let pso=; %end;

%let abort=0;
%if &var = %str() %then %do;
   %put SPLOT: No variable(s) have been specified;
   %put %str(       )Specify a VAR= variable.;
   %goto done;
   %end;
%if &class = %str() %then %do;
   data _sorted_;
      set &data;
      __all__=1;
      run;
   %let data=_sorted_;
   %let class=__all__;
   %put SPLOT: No class variable(s) have been specified. ;
   %put %str(       )__ALL__ has been used.;
   *goto done;
   %end;
 
options ls=&ls nonotes
%if %length(&ps) %then ps=&ps;
;

proc sort data=&data out=_sorted_;
   by &class;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
 
%if %sysevalf(&sysver >7) %then %do;
	ods select SSPlots;
	proc univariate plot data=_sorted_;
		var &var;
		by &class;
		%if %length(&id) %then ID &id;;
	run;
	%end;

%else %do;  /* Pre V7 */
%tempfile(univar);
proc printto new print=univar;
proc univariate plot data=_sorted_;
   var &var;
   by &class;
   %if %length(&id) %then ID &id;;
	
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
proc printto print=print;
 
data _null_;
   file print notitle noprint;
   length string $&ls;
   infile univar length=len;
   retain skipping 1;
   input @1 string $varying. len;
   if index(string,'Schematic Plot')>0 then skipping=0;
   if skipping=0 then do;
	%if &sysscp = CMS %then %do;
      cc=substr(string,1,1);
      string=substr(string,2);
      if cc='1' then put _page_;
	%end;
      put string $char.;
      end;
run;
%tempdel(univar);
%end;  /* < Version 7 */

%done:;
%if &abort %then %put ERROR: The SPLOT macro ended abnormally.;
options notes &lso &pso;
%mend;

%macro tempfile(fileref);
	%global tempfn;
	%if &sysscp = CMS
		%then %let tempfn=univar output a;
	%else %if &sysscp = WIN 
		%then %let tempfn=c:\univar.out;
	%else /* %if &sysscp = NEXT | &sysscp = RS6000 
		%then  */ %let tempfn=/tmp/univar.out;
	filename &fileref "&tempfn" lrecl=&ls;
%mend;

%macro tempdel(fileref);
	%global tempfn;
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
%mend;

