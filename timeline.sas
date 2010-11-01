%macro timeline(
	data=,        /* name of input data set                   */
	time=year,    /* name of the time variable                */ 
	info=item,    /* name of the info variable                */
	from=,        /* lowest value of time variable displayed  */ 
	to=,          /* highest value of time variable displayed */
	by=10,        /* step size for time axis                  */
	symbol=dot,   /* symbol for each info item                */
	htext=0.8,    /* height of info text                      */
	ctext=black,  /* color of info text                       */
	fold=100,
	title=        /* title for timeline                       */
	);

proc sort data=&data;
	by &time;

data _times_;
	set &data;
	by &time;
	length text $200 function color $8 position $1;
	xsys = '1';

	%if %length(&title) > 0 %then %do;
		if _n_=1 then do;
			ysys = '3';
			x=50; y=99;
			function = 'label';
			text = "&title";
			output; 
			end;
		%end;

	if &from <= &time <= &to;
	if first.&time;
	
	ysys='2';
	%if %upcase(&symbol) ^= NONE %then %do; 
		y = &time;
		x = 3; 
		function='symbol'; 
		color='red'; 
		text="&symbol"; output;
		%end;
	
	x = 4; 
	function='label';
	size = &htext;
	text = &info; 
	color='black';
	if length(text) < &fold then do;
		position = '6'; output;
		end;
	else do;
		text = substr(text, 1, &fold);
		loc = index(reverse(text),' ');    *-- last blank;
		text = substr(text, 1, &fold-loc);
		position = '6'; output;
		text = substr(&info, &fold-loc+1);
		position = '9'; output;
		end;


%axis(on=Y, at=1.5, values=&from to &to by &by, out=_axis_);


data _times_;
	set _times_ _axis_;

proc print;
	var &time text x y xsys ysys;
	format text $30.;
proc ganno anno=_times_ datasys;
%mend;

