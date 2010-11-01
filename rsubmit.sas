%macro rsubmit;
	%global remote;
	%if %length(&REMOTE)>0 %then %do;
		goptions nodisplay;
		signon &REMOTE;
		rsubmit;
		%let REMOTE=&remote;
		%put REMOTE=&remote;
	%end;
%mend;
 
%macro endrsub;
	%global remote;
	%if %length(&REMOTE)>0 %then %do;
		endrsubmit;
		signoff;
	%end;
%mend;
