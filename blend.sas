%macro blend(
	data=&syslast, 
    out=&data,
    colorvar=color,
	var=,
    r=,
	g=,
	b=,
	std=range,
	);

%let std=%upcase(&std);
%if &STD=RANGE %then %do;
	%stdize(data=&data,
		var=&r &g &b,
		method=range,
		out=scaled);
	%let data=scaled;
	%end;

data &out;
	set &data;
	&colorvar = 'cx' 
		|| %xfer(&r)
		|| %xfer(&g)
		|| %xfer(&b)
	;
%mend;


*-- Transfer function, mapping an RGB color component to a hex number;
%macro xfer(component);
/*	put(255*(1-&component), hex2.) */
	put(max(0,min(255,255*&component)), hex2.) 
%mend;

%macro rgbblend(r,g,b);
%let rgb = 'cx' || %xfer(&r) || %xfer(&g) || %xfer(&b);
&rgb
%mend;

