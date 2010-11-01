/*
Cohen - Sutherland line clipping algorithm;
*/
%macro clipdraw(
	data=_last_,
	x0=x0, y0=y0,
	x1=x1, y1=y1,
	xmin=xmin, xmax=xmax,
	ymin=ymin, ymax=ymax,
	line=,
	color=,
	out=clipped
	);

	data &out;
		set &data;
		accept=0; done=0;
		length _code_ _code0_ _code1_ _out_ $4;
		drop _out_ _code_ _code0_ _code1_;
		file print;

		x0 = &x0; y0 = &y0;
		x1 = &x1; y1 = &y1;
		put 'Starting' / '    ' x0= y0= x1= y1=;
		xmin = &xmin;  xmax = &xmax;
		ymin = &ymin;  ymax = &ymax;
		do until (done^= 0);
			iter+1;

			x = x0; y = y0; link outcode;  _code0_ = _code_;
			x = x1; y = y1; link outcode;  _code1_ = _code_;
/*
			%outcode(x0, y0);  _code0_ = _code_;			
			%outcode(x1, y1);  _code1_ = _code_;
*/
	
			if _code0_ = ' ' and _code1_ = ' '	then do;
				*-- All inside, so accept and exit;
				accept=1; done=1;
				end;
			else if _code0_^=' ' & (indexc(trim(_code0_), trim(_code1_))>0) 
			then do;
				*-- All outside, so reject and exit;
				done=1;
				end;
			else 
				do;
				*-- at least one endpoint is outside- pick it;
					if _code0_ ^=' ' 
						then _out_ = _code0_;
						else _out_ = _code1_;
						*put _out_=;
					if index(_out_, 'T') then do;
						*-- divide line at top of clip rectangle;
						x = x0 + (x1-x0) * (ymax - y0) / (y1 - y0);
						y = ymax;
						end;
					if index(_out_, 'B') then do;
						*-- divide line at bottom of clip rectangle;
						x = x0 + (x1-x0) * (ymin - y0) / (y1 - y0);
						y = ymin;
						end;
					if index(_out_, 'R') then do;
						*-- divide line at right edge of clip rectangle;
						y = y0 + (y1-y0) * (xmax - x0) / (x1 - x0); 
						x = xmax;
						end;
					if index(_out_, 'L') then do;
						*-- divide line at left edge of clip rectangle;
						y = y0 + (y1-y0) * (xmin - x0) / (x1 - x0); 
						x = xmin;
						end;

					*-- Move outside point to intersection with plot window;
					if _out_ = _code0_ then do;
						link outcode;
						x0 = x; y0 = y;
						%*outcode(x0, y0);  
						_code0_ = _code_;			
						end;
					else do;
						link outcode;
						x1 = x; y1 = y;
						%*outcode(x1, y1);  
						_code1_ = _code_;			
						end;
				end;

		end; /* until done */

		if accept then do;
			end;
		return;
		
outcode:
	_code_ = ' ';
	if y > ymax then _code_ = 'T';
	else if y < ymin then _code_ = 'B';
	if x > xmax then _code_ = trim(_code_) || 'R';
	else if x < xmin then _code_ = trim(_code_) || 'L';
	put iter= x= y= _code_=;
	return;
			
proc print noobs;
%mend;

/*
%* calculate outcode for a given point wrt the clip rectangle;
%macro outcode(
	x, y
	);

	_code_ = ' ';
	if &y > ymax then _code_ = 'T';
	else if &y < ymin then _code_ = 'B';
	if &x > xmax then _code_ = trim(_code_) || 'R';
	else if &x < xmin then _code_ = trim(_code_) || 'L';
	put iter= &x= &y= _code_=;
%mend;
*/
	
data lines;
	xmin=0;  xmax=10;
	ymin=0;  ymax=10;

	x0=5;  y0=0;  x1=10;  y1=10; output;
	x0=0;  y0=0;  x1=20;  y1=10; output;
	x0=-5;  y0=-5;  x1=20;  y1=20; output;
	x0=15;  y0=15;  x1=12;  y1=10; output;

%clipdraw(data=lines);
