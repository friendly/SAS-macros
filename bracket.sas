%macro bracket(
	x0, y0,
	x1, y1,
	thickness,
	orient=,        /* L, R, U, D */
	color=black,
	line=1,
	width=1
	);

	length function $8;
	orient="%upcase(&orient)";
	w = &thickness;
	drop w cx cy;
	select(orient);
		when('L') do;
			%arc(&x0, &y0-w, 90, 90, w, &color, &line, &width);
			cx = &x0 - 2*w;
			cy = (&y0 + &y1)/2;
			%line(&x0-w, &y0-w, &x0-w, cy+w, &color, &line); 
			%arc(cx, cy+w, 0, -90, w, &color, &line, &width);
			%arc(cx, cy-w, 0, 90,  w, &color);
			%line(cx+w, cy-w, &x1-w, &y1+w, &color, &line);
			%arc(&x1, &y1+w, 180, 270, w, &color, &line, &width); 
			end;
		when('R') do;
			%arc(&x0, &y0-w, 0, 90, w, &color, &line, &width);
			cx = &x0 + 2*w;
			cy = (&y0 + &y1)/2;
			%line(&x0+w, &y0-w, &x0+w, cy+w, &color, &line); 
			%arc(cx, cy+w, 180, 90, w, &color, &line, &width);
			%arc(cx, cy-w, 90, 90,  w, &color, &line, &width);
			%line(cx-w, cy-w, &x1+w, &y1+w, &color, &line);
			%arc(&x1, &y1+w, 0, -90, w, &color, &line, &width); 
			end;
		otherwise;
		end;
%mend;	

%macro arcpie(cx, cy, ang, rot, rad, color);

	x=&cx;
	y=&cy;
	angle=&ang;
	rotate=&rot;
	size=&rad;
	line=0;
	function = 'PIE        ';
	%if %length(&color) %then color="&color";;
	output;
%mend;

%macro line(x1, y1, x2, y2, color, line);
	x=&x1;
	y=&y1;
	%if %length(&color) %then color="&color";;
	line = &line;
	function = 'MOVE       ';
	output;
	x=&x2;
	y=&y2;
	function = 'DRAW       ';
	output;
%mend;
	
