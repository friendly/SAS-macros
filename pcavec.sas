/*
Draw a vector corresponding to first principal component through a
set of points
*/

%macro pcavec(
	data=,          /* Input data set                      */
	out=vectors,    /* Output annotate data set            */
	x=,             /* Horizontal variable                 */
	y=,             /* Vertical variable                   */
	by=,            /* BY variable                         */
	where=,         /* WHERE clause to select observations */
	vec=prin1,      /* PC vectors to plot                  */
	color=red,      /* Line color(s)                       */
	line=,          /* Line style                          */
	len=1
	);

%let vec = %upcase(&vec);
%if %upcase("&by") = "_TYPE_" %then %do;
	data &data;
		set &data;
		_temp_ = &by;
	%let by = _temp_;
	%end;

proc princomp cov data=&data
     out=prin outstat=_stats_ noprint;
   var &x &y;
	%if %length(&where) %then %do;
		where (&where);
		%end;
	%if %length(&by) %then %do;
		by &by;
		%end;

data _wts_;
   set _stats_;
   retain mx my l1 l2;
   drop _TYPE_;
   if upcase(_TYPE_)='MEAN' then do;
      mx = &x; my = &y;
      end;
   if upcase(_TYPE_)='EIGENVAL' then do;       * get lengths of PC vectors;
      l1= sqrt(&x); l2= sqrt(&y);
      end;
   if upcase(_TYPE_)='SCORE' & upcase(_NAME_) in ("%upcase(&vec)") then output;

proc print;

proc summary data=&data nway;
	var &x &y;
	%if %length(&where) %then %do;
		where (&where);
		%end;
	%if %length(&by) %then %do;
		by &by;
		%end;
	output out=minmax(drop=_type_ _freq_) min=xmin ymin max=xmax ymax;

data _wts_;
	merge _wts_ minmax;
	%if %length(&by) %then %do;
		by &by;
		%end;

/*
proc print data=_wts_;
   id _name_;
*/

data &out;
   set _wts_;
	%if %length(&by) %then %do;
		by &by;
		%end;
   length function $8;
   xsys='2'; ysys='2';
	%if %upcase(&x) = X %then %do;
		_x_ = &x;
		%let x = _x_;
		drop _x_;
		%end;
	%if %upcase(&y) = Y %then %do;
		_y_ = &y;
		%let y = _y_;
		drop _y_;
		%end;

   drop /*&x &y*/ mx my l1 l2 xmax ymax xmin ymin;
 
   color = "&color";
	%if %length(&line) %then %do;
		line = &line;
		%end; 
		if _NAME_ = 'PRIN1'
			then do; l = &len * l1; end;
			else do; l = &len * l2; end;
	%if &len > 0 %then %do;
		function = 'MOVE    ';
		x = mx - l * &x;               /* draw them*/
		y = my - l * &y;  output;
		x = mx; y = my;
		function = 'DRAW  ';   output;
		x = mx + l * &x;
		y = my + l * &y;  output;
		%end;
	%else %do;
		b = (&y)/(&x);
		x = xmin;
		y = my + b * (xmin-mx);
		function = 'MOVE';   output;
		x = xmax;
		y = my + b * (xmax-mx);
		function = 'DRAW';   output;
		%end;
proc print;
%mend;
