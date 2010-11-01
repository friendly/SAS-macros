%macro endrsub;
	%global remote;
	%if %length(&REMOTE)>0 %then %do;
		endrsubmit;
		signoff;
	%end;
%mend;
