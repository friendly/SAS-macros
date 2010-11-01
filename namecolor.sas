/*
Look up closest matchs for colors in a color data base
*/

%macro namecolor(
	data=,        /* data set containing a SAS color variable */
	colorvar=,
	id=,
	colordata=colors.sascolors,
	colorname=colorname,
	maxdist=100,
	matches=2,
	out=names
	);

data &out;
	set &data;
	%if %length(&id)=0 %then %do;
		%let id=_item_;
		_item_+1;
		%end;
	drop r g b red blue green colorvar;
	colorvar = &colorvar;
	if upcase(substr(colorvar,1,2))='CX' then do;
		r = input(substr(colorvar,3,2), hex2.);
		g = input(substr(colorvar,5,2), hex2.);
		b = input(substr(colorvar,7,2), hex2.);
		end;
	else if upcase(substr(colorvar,1,1))='H' then do;
		end;
	do ii=1 to nobs;
		set &colordata(keep=red blue green &colorname) point=ii nobs=nobs;
		dist = sqrt( (r - red)**2 + (g - green)**2 + (b - blue)**2 );
		if dist < &maxdist then output;
		end;
proc sort data=&out;
	by &id dist;
data &out;
	set &out;
	by &id ;
	retain match;
	if first.&id then match=0;
	match+1;
	if match <= &matches then output;

proc print data=&out;
	id &id; 
%mend;
