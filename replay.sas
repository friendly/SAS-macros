/*
title:  Macro to generate replay statement for rectangular, lower triangular
 *  and upper triangular displays
 */
 
%macro replay(
	data=,         /* allow a dataset containing variables PLOT and PANEL */
	rows=,
	cols=,
	first=1,
	last=,
	plots=&rows*&cols,
	order=,
	shape=rect    /* RECT, LOWTRI, LOWTRID, UPTRI */
	);

%let shape=%upcase(&shape);
%let order=%upcase(&order);

%if %length(&data)=0 %then %do;
   %if &shape=RECT or &SHAPE=SQUARE %then %do;
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do plot = 1 to &plots;
			   panel = &first + plot-1;
			   output;
			   end; 
		   %end;
	   %else %do;
	   data _replay_;
		   array plots(&rows, &cols)  _temporary_;
		   drop i j;		
		   plot=&first;
		   do j=1 to &cols;
			   do i=1 to &rows;
			   plots[i,j] = plot;
			   plot+1;
			   end; end;
		   do i= 1 to &rows;
			   do j=1 to &cols;
			   panel+1;
			   plot = plots[i,j];
			   output;
			   end; end;
		   %end;

   %end;
   %else %if &shape=LOWTRI %then %do;
   /*
	  Assumes plots were generated in the lower triangle of a square n x n
	  matrix, so rows=n-1 and cols=n-1 in the macro call.
	  Assumes ORDER=DOWN is also specified.
   */
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do i=2 to &rows+1;
			   do j=1 to i-1;
			   plot+1;
			   panel = (j) + (&cols) *(i-2);
			   output;
			   end;
		   end;
		   %end;
	   %else %do;
	   data _replay_;
		   do j=1 to &cols-1;
			   do i=2 to &rows+1;
			   plot+1;
			   panel = (j) + (&cols) *(i-2);
			   output;
			   end;
		   end;
		   %end;
   %end;

   %else %if &shape=LOWTRID %then %do;
   /*
	  Assumes plots were generated in the lower triangle of a square n x n
	  matrix, including the diagnonal, so rows=n and cols=n in the macro call.
	  Assumes ORDER=DOWN is also specified.
   */
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do i=1 to &rows;
			   do j=1 to i;
			   plot+1;
			   panel = (j) + (&cols) *(i-1);
			   output;
			   end;
		   end;
		   %end;
   %end;

   %else %if &shape=UPTRI %then %do;
   /*
	  Assumes plots were generated in the lower triangle of a square n x n
	  matrix, so rows=n-1 and cols=n-1 in the macro call.
	  Assumes ORDER=DOWN is also specified.
   */
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do i=1 to %eval(&rows);
			   do j=i to &cols;
			   plot+1;
			   panel = (j) + (&cols) *(i-1);
			   put i= j= plot= panel=;
			   output;
			   end;
		   end;
		   %end;
	   %else %do;   /* not tested */
	   data _replay_;
		   do j=1 to &cols-1;
			   do i=2 to &rows+1;
			   plot+1;
			   panel = (j) + (&cols) *(i-2);
			   output;
			   end;
		   end;
		   %end;
   %end;
%let data=_replay_;
%end;

data _replay_;
	set &data;
	replay = trim(left(put(panel, 2.))) || ':' || trim(left(put(plot, 2.)));
	run;
proc sort data=_replay_;
	by plot;
proc print data=_replay_;
	id panel plot;
run;

proc sql noprint;
	select replay into :replay separated by ' '
	from _replay_;
	quit;

%put replay = &replay;
%mend;

*options mprint;
/*
title 'Rect 2x3';
%replay(rows=2, cols=3);

title 'Rect 2x3 byrows';
%replay(rows=2, cols=3, order=byrows);
*/
title 'Low Triangle 2x2';
%replay(rows=2, cols=2, shape=lowtri);
title 'Low Triangle 3x3';
%replay(rows=3, cols=3, shape=lowtri);

title 'Low Triangle, 3x3 byrows';
*replay(rows=3, cols=3, shape=lowtri, order=byrows);
title 'Low Triangle with diagonal 3x3';
*replay(rows=3, cols=3, shape=lowtrid);

title 'Up Triangle 2x2';
%replay(rows=2, cols=2, shape=uptri);
title 'Up Triangle 3x3';
%replay(rows=3, cols=3, shape=uptri);
