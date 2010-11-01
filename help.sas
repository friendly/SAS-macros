/*  Macro for context-sensitive help in SAS.  Assign %help to F1
    and change the Help button on the toolbar so they execute
	 %help.  If there is marked text, the macro will search the
	 help files for that block.
*/
%macro help(clip);
	%*let clip=;
	%if %length(&clip)=0 %then %do;
		%*-- store text nearest to cursor;
		find ' ' prev;
		mark; forward; store; backward;
		gsubmit "%nrstr(%%let) clip=%nrstr(%%nrstr%()";
		gsubmit buf=default;
		gsubmit ");";
		%end;
	%if %length(&clip)>0 %then %do;
		%let clip=%sysfunc(left(%scan(&clip,1)));
		%let clip=%sysfunc(right(&clip));
		%end;
	%let rc = %sysfunc( winhelp( help_key,
		%sysget(sasroot)\core\winhelp\base.hlp, %nrbquote(&clip) ) );
%mend;



	