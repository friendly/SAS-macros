%macro ds2tdef(
	data=_LAST_,       /* data set containing coordinates */
	template=&data,    /* name of template to be created */
	fileref=d2t,       /* fileref for TDEF statement */
	llx=llx,           /* name of variable for LL X coordinate */
	lly=lly,           /* name of variable for LL Y coordinate */
	urx=,              /* name of variable for UR X coordinate */
	ury=,              /* name of variable for UR Y coordinate */
	wx=,               /* X dimension of panel (var or const) */
	wy=,               /* Y dimension of panel (var or const) */
	panel=             /* name of panel # variable */
	clip=,
	preview=Y
	);

%tempfile(&fileref);
data _null_;
	set &data end=eof;
	file &fileref;
	if _n_=1 then do;
		put "TDEF &template";
		end;
	llx = &llx;
	lly = &lly;
	
	%if %length(&urx) %then %do;
		ulx = &llx;
		uly = &ury;
		lrx = &urx;
		lry = &lly;
		%end;


	%else %do;
		ulx = &llx;
		uly = &lly + &wy;
		lrx = &llx + &wx;
		lry = &lly;
		%end;
	
	%if %length(&panel) %then %do;
		panel = _n_;
		%end;
	%else %do;
		panel = &panel;
		%end;
		
	put '   ' panel '/' 
		' LLX=' llx ' LLY=' lly
		' ULX=' ulx ' ULY=' uly
		' LRX=' lrx ' LRY=' lry
		' URX=' urx ' URY=' ury ;
	%if %length(&clip) %then %do;
		%if &clip = Y or "&clip" = "1" %then %str(put '   CLIP';);
		%else %do;
			if &clip then put '    CLIP';
			%end;
		%end;
	if eof then put '   ;';
run;

%if &preview = Y %then %do;
proc greplay nofs template=&template tc=test;
	%include &fileref;
	list template;
	preview &template;
%end;

%mend;
