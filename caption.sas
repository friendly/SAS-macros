/*
Title:  Annotate a plot title or caption inside the plot frame
*/

%macro caption(
	label,            /* Caption text                     */
	x=2, y=96,        /* Plot x, y location               */
	htext=,           /* Text height                      */ 
	sys=1,            /* Annotate coordinate system       */
	pos=,             /* Annotate position for label      */
	color=,
	out=caption       /* Name of output Annotate data set */
	);

	data &out;
	retain xsys ysys "&sys";
	length text $%sysfunc(max(40, %length(&label)));
	length position $1;
	x=&x;
	y=&y;
	%if %length(&pos)=0 %then %do;
		if &x < 20
			then position = '6';
		else if &x > 80
			then position = '4';
		else position = '5';
		%end;
	%else %do;
		position = "&pos";
		%end;
	%if %length(&htext) %then %do;
		size = &htext;
		%end;
	%if %length(&color) %then %do;
		color = "&color";
		%end;
	function ='label   ';
	text = "&label";
%mend;

