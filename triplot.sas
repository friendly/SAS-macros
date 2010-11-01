 /*-------------------------------------------------------------------*
  *    Name: triplot.sas                                              *
  *   Title: Macro for trilinear plots                                *
        Doc: http://www.datavis.ca/sasmac/triplot.html             
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  6 Aug 1996 09:11:33                                     *
  * Revised: 16 Jan 2009 15:28:55                                     *
  * Version:  1.2-1                                                   *
  *  - Fixed legend bug; added label location (LABLOC=) parameter     *
  *    Fixed xsys/ysys error                                          *
  *    Fixed bug with upcase(name)                                    *
  *    Fixed buglet when VAR=X Y Z (thx: Andre Wielki)                *
  *    Added OUT= to obtain X,Y coordinates in triangular space       *
  *    Added PLOT= to suppress the plot                               *
  *    Fixed obscure if(eof) buglet (thx: Rodney Sparapani)           *
  *    Added LABPOS= to control position of variable labels           *
  *    Fixed error with length of POSITION                            *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The TRIPLOT macro plots three variables (rows of an n x 3 table)
 in an equilateral triangle, so that each point represents the
 proportions of each variable to the total for that observation.

=Usage:

 The TRIPLOT macro is called with keyword parameters.
 The names of three variables must be given in the VAR= parameter.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	data tridemo;
		input A B C point $12.;
		label point='Point';
	cards;
	40 30 30  (40,30,30)
	20 60 20  (20,60,20)
	10 10 80  (10,10,80)
	;
	%triplot(var=A B C, class=Point, id=point, gridby=25,
		symbols=dot dot dot, idht=1.6, axes=bot,
		symht=4, gridclr=gray);
 
==Parameters:

* DATA=       The name of data set to be plotted. [Default: DATA=_LAST_]

* VAR=        The names of three variables used as the axes in the
              plot.  The values of each observation are normally all
				  non-negative.  Missing values are treated as 0.

* CLASS=      The name of a class variable determining plotting symbol.
              Different values of the CLASS= variable are represented
				  by the values in the COLORS= and SYMBOLS= lists, used
				  sequentially.

* ID=         The name of an observation identifier (label) variable

* BY=         The name of a BY variable, for separate plots

* WHERE=      WHERE-clause to subset observations to be plotted.

* IDHT=       Height of ID label [Default: IDHT=2]

* IDCLR=      Color of ID label [Default: IDCLR='BLACK']

* IDPOS=      Position of ID label [Default: IDPOS=8]

* IDSUBSET=   A SAS expression (which may use any data set variables
              used to subset ID labels.  If an ID= variable is given,
				  and the IDSUBSET= expression evaluates to non-zero,
				  the observation is labelled in the plot.
				  [Default: IDSUBSET=1]

* INTERP=     Interpolation between points, a SYMBOL statement option.
              If INTERP=JOIN, points within the same CLASS= value are
				  connected by lines.  Most other SYMBOL statement
				  interpolation options would give bizare results.
				  [Default: INTERP=NONE]

* SYMHT=      Height of point symbols [Default: SYMHT=2]

* SYMBOLS=    A list of one or more symbols for points, corresponding
              to the levels of the CLASS= variable.  The symbols are
				  reused cyclically if there are more class levels than
				  symbols.
				  [Default: SYMBOLS=%STR(DOT CIRCLE SQUARE $ : TRIANGLE = X _ Y)]

* COLORS=     A list of one or more colors for points, corresponding
              to the levels of the CLASS= variable.  The colors are
				  also reused cyclically as required.
              [Default: COLORS=BLACK RED BLUE GREEN BROWN ORANGE PURPLE YELLOW]

* BACKCLR=    Background color inside the trilinear plot.
              [Default: BACKCLR=WHITE]

* BACKPAT=    Background fill pattern. For a plot with a light gray
              background, for example, specify BACKPAT=SOLID and
				  BACKCLR=GRAYD0. [Default: BACKPAT=EMPTY]

* GRIDBY=     Grid line interval. For grid lines at 25, 50, and 75%,
              for example, specify GRIDBY=25. [Default: GRIDBY=20]

* GRIDCLR=    Grid line color [Default: GRIDCLR=GRAY]

* GRIDLINE=   Style of grid lines [Default: GRIDLINE=34]

* AXES=       Type of axes, one of NONE, FULL, TOP, or BOT.
              AXES=NONE draws no coordinate axes;  AXES=FULL draws
				  a line from 0 to 100% for each of the three coordinates;
				  AXES=TOP draws a line from the apex to the centroid only;
				  AXES=BOT draws a line from the centroid to the base only. 
              [Default: AXES=NONE]

* AXISCLR=    Color of axis lines [Default: AXISCLR=BLUE]

* AXISLINE=   Style of axis lines [Default: AXISLINE=1]

* XOFF=       X offset, in %, for adjusting the plot [Default: XOFF=2]

* XSCALE=     X scale factor for adjusting the plot.  Before plotting
              the X coordinates are adjusted by X = XOFF + XSCALE * X.
				  [Default: XSCALE=.96]

* YOFF=       X offset, in %, for adjusting the plot [Default: YOFF=2]

* YSCALE=     Y scale factor for adjusting the plot.  Before plotting
              the Y coordinates are adjusted by Y = YOFF + YSCALE * Y.
				  [Default: YSCALE=.96]

* LEGEND=     The name of legend statement or 'NONE'.  If LEGEND= is
              not specified, and there is more than one group defined
				  by a CLASS= variable, a legend statement is constructed
				  internally. If LEGEND=NONE, no legend is drawn; otherwise
				  the LEGEND= value is used as the name of a legend statement.

* LABHT=      Height of variable labels, in GUNITs [Default: LABHT=2]

* LABLOC=     Location of variable label: 0 or 100 [Default: LABLOC=100]

* LABPOS=     Label positions for x,y,z: 3 characters [Default: LABPOS=FDE
              when LABLOC=100, LABPOS=22E otherwise]

* PLOT=       Produce the plot? [Y/N]

* NAME=       Name of the graphics catalog entry [Default: NAME=TRIPLT]
                

 =*/
 /*

Ideas from: Fedencuk & Bercov, "TERNPLOT - SAS creation of ternary plots",
        SUGI 16, 1991, 771-778.
*/

%macro triplot(
	data=_last_,    /* name of data set to be plotted           */
	var=,           /* 3 variable names for plot axes           */
	class=,         /* class variable defining plotting symbol  */
	id=,            /* point identifier (label) variable        */
	by=,            /* BY variable for separate plots           */
	where=,         /* where-clause to subset observations      */

	idht=2,         /* height of ID label                       */
	idclr='BLACK',  /* color of ID label                        */
	idpos=8,        /* position of ID label                     */
	idsubset=1,     /* expression to subset ID labels           */
	interp=none,    /* interpolation between points             */
	symht=2,        /* height of point symbols                  */
	symbols=%str(dot circle square $ : triangle = X _ Y),
	symfont=,
	colors=BLACK RED BLUE GREEN BROWN ORANGE PURPLE YELLOW,

	backclr=WHITE,  /* background color                         */
	backpat=EMPTY,  /* background fill pattern                  */
	gridby=20,      /* grid line interval                       */
	gridclr=gray,   /* grid line color                          */
	gridline=34,    /* style of grid lines                      */
	axes=none,      /* type of axes: NONE, FULL, TOP, BOT       */
	axisclr=blue,   /* color of axis lines                      */
	axisline=1,     /* style of axis lines                      */

	xoff=2,         /* X offset, in %, for adjusting the plot   */
	xscale=.96,     /* X scale factor, for adjusting the plot   */
	yoff=2,         /* Y offset, in %, for adjusting the plot   */
	yscale=.96,     /* Y scale factor, for adjusting the plot   */
	legend=,        /* name of legend statement or 'NONE'       */
	labht=2,        /* height of variable labels, in GUNITs     */
	labloc=100,     /* location of variable label: 0 or 100     */
	labpos=,        /* Label positions for x,y,z, e.g., FD2     */
	out=tridata,    /* output data set */
	outanno=trianno,
	plot=yes,       /* produce the plot? */
	name=triplot    /* name of graphic output in the catalog    */
	);

%let scale = 1;
%let lab = 100;
%let lbc = 100;
%let lac = 100;
%let abort=0;
%let legend=%upcase(&legend);
%let axes  =%upcase(&axes);
%let plot  =%upcase(&plot);

%local xvar yavr zvar xlab ylab zlab;

%*-- parse variable names and labels;
%let pre=x y z;
%do i=1 %to 3;
	%let l = %scan(&pre, &i);
	%let &l.var = %scan(&var, &i);
	%end;
%if %length(&zvar)=0 %then %do;
	%put ERROR:  VAR= must give three variable names;
	%let abort=1;
	%goto done;
%end;
%*put xvar=&xvar yvar=&yvar zvar=&zvar;

   %*-- make &data reusable if _LAST_ was specified;
%if %bquote(&data) = %bquote(_last_) %then %let data = &syslast;

options nonotes;
proc contents data=&data out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

   %*-- get variable labels, assign names if no labels;
data _null_;
   set _work_(keep=name type label);
    	name=upcase(name);
		select (name);
			when (upcase("&xvar")) do;
				if label = ' ' then call symput('xlab', "&xvar");
							else call symput('xlab', trim(label));
				end;
			when (upcase("&yvar")) do;
				if label = ' ' then call symput('ylab', "&yvar");
							else call symput('ylab', trim(label));
				end;
			when (upcase("&zvar")) do;
				if label = ' ' then call symput('zlab', "&zvar");
							else call symput('zlab', trim(label));
				end;
			otherwise ;
			end;
run;
%*put xlab=&xlab ylab=&ylab zlab=&zlab;

data &outanno;
	retain xsys ysys  '1';
	drop triht;
	length text $20 position $1;
	format x y 6.1;

	line  = 1;
	size  = 4;

	triht = &scale * sqrt(3) * &lab /2      ;
	%if %upcase(&backpat) ^= EMPTY %then %do;
		style = "&backpat";
		color = "&backclr";
		%end;

	x = 0;      y = 0;     function='poly    '; output;

	color = 'BLACK';       /* color of triangular frame */
	x = &lab;   y = 0;     function='polycont'; output;
	x = &lab/2; y = triht; function='polycont'; output;
	x = 0;      y = 0;     function='polycont'; output;

	size = &labht;
	style = ' ';
	function='label';
	%if &labloc=100 %then %do;    /* labels at corners */
		%if %length(&labpos)=0 %then %let labpos=FD2;
		*position='F';
		position=substr("&labpos",1,1);
		text = "&xlab";  x = 0;      y = 0;     output;
		*position='D';
		position=substr("&labpos",2,1);
		text = "&ylab";  x =100;     y = 0;     output;
		*position='2';
		position=substr("&labpos",3,1);
		text = "&zlab";  x = &lab/2; y = triht; output;
		%end;
	%else %do;                    /* labels at sides  */
		%if %length(&labpos)=0 %then %let labpos=22E;
		angle=-60;
		position=substr("&labpos",1,1);
		text = "&xlab";  x = .75*&lab;  y = triht/2; output;
		angle=60;
		position=substr("&labpos",2,1);
		text = "&ylab";  x = .25*&lab;  y = triht/2; output;
		angle=0;
		position=substr("&labpos",3,1);
		text = "&zlab";  x = &lab/2;    y = 0;       output;
		%end;
	
run;

%if &gridby > 0 %then %do;
%let mg=9;     /* max number of grid positions */
data trigrid;
	retain xsys ysys  '1';
	length function $8 position $1;;
	drop triht i gridby ngrid ni;
	array gabx [&mg] _temporary_ ;
	array gaby [&mg] _temporary_ ;
	array gacx [&mg] _temporary_ ;
	array gacy [&mg] _temporary_ ;
	array gbcx [&mg] _temporary_ ;
	array gbcy [&mg] _temporary_ ;

	array tick [&mg] $2 _temporary_ ;
	array tax [&mg]     _temporary_ ;
	array tbx [&mg]     _temporary_ ;
	array tcx [&mg]     _temporary_ ;
	array tay [&mg]     _temporary_ ;
	array tby [&mg]     _temporary_ ;
	array tcy [&mg]     _temporary_ ;

	triht = &scale * sqrt(3) * &lab /2      ;

	gridby = &gridby;
	ngrid  = min(&mg, (100/gridby) - 1);
	do i = 1 to ngrid;
		gabx[i] = gridby * i;
		gaby[i] = 0;
		gacx[i] = gridby * i / 2;
		gacy[i] = triht * i * gridby / 100;
		gbcx[i] = 50 + (gridby * i) / 2;
		gbcy[i] = triht * (ngrid+1 - i) * gridby / 100;

		tick[i] = put( (gridby * i), 2.);

		tax[i]  = 75 - (.75 * gridby * i);
		tay[i]  = (triht - (triht * (i*gridby/100))) / 2;
		tbx[i]  = 25 + (.75 * gridby * i);
		tby[i]  = tay[i];
		tcx[i]  = 50;
		tcy[i]  = triht * i * gridby / 100;

		end;

	line = &gridline;
	color = "&gridclr";
	size = 1;
	position = 'B';
	rotate = 0;

	*-- grid lines parallel to AB;
	angle  = 0;
	do i = 1 to ngrid;
		ni = ngrid+1 - i;
		x = gbcx[i];    y = gbcy[i];     function='move';  output;
		x = gacx[ni];   y = gacy[ni];    function='draw';  output;
		text = tick[i];
		x = tcx[i];     y = tcy[i];      function='label'; output;
		end;

	*-- grid lines parallel to AC;
	angle  = -120;
	do i = 1 to ngrid;
		x = gabx[i];    y = gaby[i];     function='move';  output;
		x = gbcx[i];    y = gbcy[i];     function='draw';  output;
		text = tick[i];
		x = tbx[i];     y = tby[i];      function='label'; output;
		end;

	*-- grid lines parallel to BC;
	angle  = 120;
	do i = 1 to ngrid;
		x = gabx[i];    y = gaby[i];     function='move';  output;
		x = gacx[i];    y = gacy[i];     function='draw';  output;
		text = tick[i];
		x = tax[i];     y = tay[i];      function='label'; output;
		end;
%end; /*-- &gridby > 0 */

*let axes=BOT;
%if &axes ^= NONE %then %do;
			%if &axes=TOP %then %do; %let f1=draw; %let f2=move; %end;
	%else %if &axes=BOT %then %do; %let f1=move; %let f2=draw; %end;
	%else  /* &axes=FULL */   %do; %let f1=draw; %let f2=draw; %end;

data triaxes;
	retain xsys ysys  '1' line &axisline color "&axisclr";
	length function $8;

	root3 = sqrt(3);
	triht = &scale * root3 * &lab /2;
	cx = &lab/2; cy = triht/3;
	drop root3 triht cx cy;

	x = &lab/2;  y = triht;         function='move'; output;
	y = cy;                         function="&f1";  output;
	y = 0;                          function="&f2";  output;

	x = 0;  y = 0;                  function='move'; output;
	x = cx; y = cy;                 function="&f1";  output;
	x = &lab * .75; y=triht/2;      function="&f2";  output;

	x = &lab; y=0;                  function='move'; output;
	x = cx; y = cy;                 function="&f1";  output;
	x = &lab * .25; y=triht/2;      function="&f2";  output;
%end;  /* &axes ^= NONE */

options notes;
data &outanno;
	set &outanno
		%if &axes ^=NONE %then
		triaxes ;
		%if &gridby > 0 %then
		trigrid ;
				;
	x = &xoff + &xscale * x;
	y = &yoff + &yscale * y;

%if %length(&by) or %length(&class) %then %do;
	proc sort data=&data;
	by &by &class;
	%end;

data &out;
	retain xsys ysys  '1'
		xa  ya
		xb  yb
		xc  yc
		root3 xa1 ya1 coef_bc _class_ 0;
	drop xa ya xb yb xc yc root3 coef_bc sum xa1 ya1 xaa yaa;
	length position $1;
	*drop &xvar &yvar &zvar;

	if _n_ = 1 then do;
		root3 = sqrt(3);
		xa = 0;   ya=0;
		xb = 100; yb=0;
		xc = (xb - xa) / 2;
		yc = root3 * xc;
		ya1= yc / 2;
		xa1= root3 * ya1;
		coef_bc = (yc - yb) / (xc - xb);
		end;

	set &data end=eof;
	%if %length(&class)
		%then %do; by &class; if first.&class then _class_+1; %end;
		%else %do;   _class_=1; %end;

	%if %length(&where) %then
		where (&where)%str(;);

	if (&xvar = .) then &xvar=0;
	if (&yvar = .) then &yvar=0;
	if (&zvar = .) then &zvar=0;
	if &xvar < 0 | &yvar < 0 | &zvar < 0
		then put 'WARNING: One or more values are negative in obs' _n_
		&xvar= &yvar= &zvar=;

	sum = &xvar + &yvar + &zvar;
	*-- do this before the delete to make sure ngroups gets written;
	if eof then do;
		call symput('ngroups', put(_class_, 3.));
	end;
	if (sum=0) then delete;

	&xvar = &xvar / sum;
	&yvar = &yvar / sum;
	&zvar = &zvar / sum;

	xaa = xa1 * (1 - &xvar);
	yaa = xaa / root3;

	y = yc * &zvar;
	x = ( (y-yaa)/coef_bc ) + xaa;

	%if %length(&id) %then %do;
		if (&idsubset) then do;
			text = &id;
			size = &idht;
			color= &idclr;
			position = "&idpos";;
			function = 'label';
			end;
	%end;

	x = &xoff + &xscale * x;
	y = &yoff + &yscale * y;

run;
%*put ngroups=&ngroups;
%if %length(&class)=0 %then %let class=_class_;

data &outanno;
	set &outanno
	%if %length(&id) %then %do;
	&out(keep=xsys ysys x y text size color position function
				where=(function='label'))
	%end;
	;

*proc print;

%if &plot=YES %then %do;
%gensym(n=&ngroups,
        h=&symht, symbols=&symbols, colors=&colors, interp=&interp,
		  font=&symfont);

axis1 offset=(0) order=(0 to 100)
        label=none value=none major=none minor=none style=0;

%if %length(&legend)=0 & &ngroups>1 %then %do;
	legend1  position=(top right inside) across=1 offset=(0,-2) 
		mode=share frame;
	%let legend=legend=legend1;
	%end;
%else %if &legend=NONE or &ngroups=1 %then %do;
	%let legend=nolegend;
	%end;
%else %do;
	%let legend=legend=&legend;
	%end;

proc gplot data=&out anno=&outanno;
	plot y * x = &class
				/ vaxis=axis1 haxis=axis1 noaxes &legend 
				name="&name"
				des="Triplot of &var";
	%if %length(&by) %then %do;
				by &by ;
				%end;
run; quit;
%end;

%DONE:
%if &abort %then %put ERROR: The TRIPLOT macro ended abnormally.;
options notes;

%mend;
