 /*-------------------------------------------------------------------*
  *    Name: sunplot.sas                                              *
  *   Title: Sunflower plot for X-Y dataset                           *
        Doc: http://www.datavis.ca/sasmac/sunplot.html          
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  10 Jul 1996 10:26:49                                    *
  * Revised:  03 Jun 2007 13:18:51                                    *
  * Version:  1.3-1                                                     *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by Michael Friendly                        *
  * - transfer variable labels to plotting variables                  *
  * 1.2 Added COUNT= for grouped data                                 *
  * 1.3 Added OUT= for annotate data set and PLOT= to control plotting*
  *     Fixed use of COLOR=                                           *
  *-------------------------------------------------------------------*/

 /*=
The sunflower plot displays a bivariate dataset using "sunflower
symbols" to show the number of observations in the neighborhood
of each XY point.

 =*/
 
%*-- Requires that the program sunfont.sas has been previously run to
	create the sunflower 'sun' font, stored in the libname designated
	by gfont0;
	
*libname gfont0 '~/sasuser/gfont';

%macro sunplot(
	data=_LAST_,   /* name of the input dataset              */
	x=,            /* name of the horizontal variable        */
	y=,            /* name of the vertical variable          */
	count=,        /* name of a count/freq variable          */
	weight=,       /* name of a count/freq variable          */
	nx=20,         /* number of slices of the horizontal var */
	ny=20,         /* number of slices of the vertical var   */
	color=black,   /* color for plotting symbols             */
	sunfont=sun,   /* name of sunflower font                 */
	hsym=1.5,      /* symbol height                          */
	vaxis=,        /* optional name of axis statement        */
	haxis=,        /* optional name of axis statement        */
	plot=yes,
	plotopt=nolegend frame,  /* additional plot options      */
	anno=,         /* input annotate data sets */
	out=,          /* output annotate data set */
	name=sun,      /* name of graphics catalog entry         */
	gout=
	);
	
	%let plot=%substr(%upcase(&plot),1,1);
	%if &x=%str() or &y=%str() %then %do;
		%put SUNPLOT: X= and Y= variables must be specified;
		%goto DONE;
		%end;
 
	%if %length(&gout)>0 %then
		%let gout=GOUT=&gout;

	%if %length(&weight) %then %do;
		%let count=&weight;
		%end;

	%if %length(&count) %then %do;
		%let plotdata=&data;
		%let xd=&x;
		%let yd=&y;
*		%sunsym(data=&plotdata, ht=&hsym, sunfont=&sunfont, count=&count);
		%end;
	%else %do;
		%cell(data=&data, x=&x, y=&y, nx=&nx, ny=&ny, out=_binned_);
		%let plotdata=_binned_;
		%let xd=xd;
		%let yd=yd;
		%let count=count;
*		%sunsym(data=_binned_, ht=&hsym, sunfont=&sunfont, count=&count);
		%end;

*-proc print data=&plotdata;
	
	%if &plot=Y %then %do;
	%*-- Generate symbol statements for &count variable;
	%sunsym(data=&plotdata, ht=&hsym, sunfont=&sunfont, count=&count,
	color=&color);

	proc gplot data=&plotdata &GOUT ;
		plot &yd * &xd = &count /
         &plotopt
			%if %length(&vaxis)>0 %then vaxis=&vaxis ;
			%if %length(&vaxis)>0 %then haxis=&haxis ;
			%if %length(&anno)>0  %then anno=&anno ;
         name="&name"
			des="sunflower plot of &data (&y * &x)" ;
		run; quit;
	goptions reset=symbol;
	%end;
	
	%*-- Produce an output annotate data set?;
	%if %length(&out) %then %do;
		%sunanno(data=&plotdata, ht=&hsym, sunfont=&sunfont, count=&count,
			x=&xd, y=&yd, out=&out);
		%if %length(&anno)>0  %then %do;
			data &out;
				set &out(keep=xsys ysys x y size function style text) 
				&anno;
			%end;
		%end;

%done:
%mend;


 /*------------------------------------*
  | Macro to cellulate an X-Y data set |
  *------------------------------------*/
%macro cell( data=_LAST,     /* input data set */
             out=_DATA_,
             x=X, y=Y,       /* input X, Y variables */
             xd=xd, yd=yd,   /* output X, Y variables */
             nx=20, ny=20);  /* number of bins in X & Y directions */
 
 /*-------------------------------------------------*
  | Quantize the X and Y values into discrete cells |
  *-------------------------------------------------*/
proc means data=&data noprint;
   var &x &y;
   output out=_temp_ range=rx ry;
 
proc contents data=&data out=_work_(keep=name type label) noprint;

*proc print data=_work_;

data _null_;
   set _work_;
	if upcase(name)=upcase("&x") then do;
		if label ^= ' ' then call symput('lx', trim(label));
		else call symput('lx', "&x");
		end;
	if upcase(name)=upcase("&y") then do;
		if label ^= ' ' then call symput('ly', trim(label));
		else call symput('ly', "&y");
		end;
	
data _cell_;
   if _n_=1 then set _temp_;
   set &data;
   drop rx ry;
   deltax = rx / &nx;
   deltay = ry / &nx;
   &xd = deltax * (round( &x/deltax )+.5);
   &yd = deltay * (round( &y/deltay )+.5);
run;
%*put lx=&lx ly=&ly;
 /*----------------------------------------*
  | Find number of points in each X-Y cell |
  *----------------------------------------*/
proc freq data=_cell_ ;
   tables &xd * &yd / noprint out=&out ;
	run;
  *-- assign variable labels to plotting variables;
data &out;
	set &out;
	drop percent;
	label xd = "&lx"
	      yd = "&ly";
run;	
%mend;

 /*:
 :*/
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each COUNT |
  *----------------------------------------------------*/
%macro sunsym( data=_LAST_, ht=1.5, color=black, sunfont=sun, count= );
%local alpha ch repeat;
%let alpha=ABCDEFGHIJKLMNOPQRSTUVWXYZ;
 
proc means data=&data noprint;
   var &count;
   output out=_temp_ max=maxcount;
data _null_;
   set _temp_;
   maxcount = min( maxcount, 26 );
   call symput('HIGH',put(maxcount,2.));
run;
 
  %let repeat=;
  %do i=1 %to &HIGH %by 1;
     %let ch = &i;
     %if &i = %length(&alpha) %then %let repeat = r=100;
     symbol&i h=&ht f=&sunfont c=&color v=%substr(&alpha,&ch,1) &repeat ;
  %end;
%mend;

 /*----------------------------------------------------*
  |  Macro to generate an ANNOTATE data set for COUNTs |
  *----------------------------------------------------*/
%macro sunanno(data=_LAST_,
	x=,
	y=, 
	ht=1.5, color=black, sunfont=sun, count=, out=sunanno );

%let alpha=ABCDEFGHIJKLMNOPQRSTUVWXYZ;
data &out;
	set &data;
	retain xsys ysys '2';
	length function $8 color $8;
	x = &x;
	y = &y;
	size = &ht;
	function = 'symbol';
	style = "&sunfont";
	color = "&color";
	count = min(&count, %length(&alpha));
	text=substr("&alpha", count, 1);
	drop count;
	
%mend;
