%macro ifelse(
	condition,
	iftrue,
	iffalse
	);

	%if (&condition) %then &iftrue;
	                 %else &iffalse;

%mend;

%put Running %ifelse(&sysver<9, Version 8, Version 9);

%let version = %ifelse(&sysver<9, Version 8, Version 9);
%put version = &version;
