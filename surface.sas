 /*-----------------------------------------------------------------*
  *    Name: surface.sas                                            *
  *   Title: Draw color 3D surface plot with contours in X-Y plane  *
        Doc: http://www.datavis.ca/sasmac/surface.html        
  *-----------------------------------------------------------------*
  * Original author: Russ Wolfinger                                 *
  *  Author:  Michael Friendly            <friendly@yorku.ca>       *
  * Created: 26 Oct 2006 09:59:00                                   *
  * Revised: 26 Oct 2006 09:59:00                                   *
  * Version: 1.0                                                    *
  *                                                                 *
  *-----------------------------------------------------------------*/
 /*=
=Description:
 
 The SURFACE macro draws a 3D color-coded surface plot, showing a contour
 plot in the X-Y plane.  The result is most useful for smooth surfaces, e.g.,
 for functional relations, Z=f(X,Y), bivariate density estimates computed
 with PROC KDE, or with the result of PROC G3GRID.

==Method:

 The surface and the contour plot are constructed using filled polygons
 in Annotate datasets.  The contour is identical to the surface, with the
 Z coordinate set to the constant 'floor'. 
 
=Usage:

 The SURFACE macro is defined with keyword parameters.  The X=, Y= and
 Z= parameters are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%surface(x=temperature, y=pressure, z=yield);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* X=          X variable for plot [Default: X=X]

* Y=          Y variable for plot [Default: Y=Y]

* Z=          Z variable for plot [Default: Z=Z]

* WHERE=      Where clause to subset the data

* COLORS=     Color scheme (HLS|HLSllss) or list of colors. COLORS=HLS generates 
              NLEVELS= colors varying in hue in the HLS color scheme Hhhh80FF.
              COLORS=HLSllss, where llss are hex digits, generates HLS colors
			  with lightness ll and saturations ss.  Otherwise, COLORS= is taken
			  as a blank separated list of SAS colors of length NLEVELS=.
			  [Default: COLORS=HLS]

* NLEVELS=    Number of levels for color [Default: NLEVELS=12]

* TICKNUM=    Number of tick marks on axes.  Specify 1-3 values, for X, Y and Z. [Default: TICKNUM=5]

* DROP=       Amount to drop contour plot beneath lowest z-value, in data units. [Default: DROP=1]

* TILT=       G3D TILT= value(s): One or more numbers giving the number of degrees
              to tilt the plot. [Default: TILT=75]

* ROTATE=     G3D ROTATE= value(s): One or more numbers giving the number of degrees
              to rotate the plot. [Default: ROTATE=40]

* FUZZ=       Fuzz value for binning X, Y [Default: FUZZ=1E-6]

* OPTIONS=    REVERSE and/or NOGRID.  When COLORS=HLS, OPTIONS=REVERSE reverses the
              assignment of colors to heights (Z= values) in the plot.

* ANNO=       Additional input annotate data set

* OUTANNO=    Output annotate data set used with G3D [Default: OUTANNO=SURFANNO]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=SURFACE]
                
=Example:

   *-- Generate bivariate normal data;
   data bivar;
	  do i = 1 to 10000;
    	 x = rannor(2140);
    	 y = -x + 3* rannor(2140);
    	 output;
	  end;
   run;

   *-- Do bivariate density estimation;
   proc kde data=bivar out=kdeout;
	  var x y;
   run;

   %include macros(surface);
   %surface(data=kdeout, x=x, y=y, z=density,
       where=-3<x<3)


 =*/

 /*---
 Title: Draw color 3D surface plot with contours in X-Y plane
 
 macro to draw color surface plot with contours underneath,
      code modified from Example 6 in SAS/GRAPH Examples book,
      version 6, supported by Russ Wolfinger (sasrdw@unx.sas.com)---
 ---*/

%macro surface(
  data=,             /* name of input data set */
  x=x,               /* X variable for plot                                */
  y=y,               /* Y variable for plot                                */
  z=z,               /* Z variable for plot                                */
  where=,            /* where clause to subset the data                    */
  colors=HLS,        /* color scheme (HLS) or list of colors               */
  nlevels=12,        /* number of levels for color                         */
  ticknum=5,         /* number of tick marks on axes                       */
  drop=1,            /* amount to drop contour plot beneath lowest z-value */
  tilt=75,           /* G3D TILT= value                                    */
  rotate=40,         /* G3D ROTATE= value                                  */
  scatopt=,          /* Any other options for the G3D SCATTER stmt         */
  fuzz=1e-6,         /* fuzz value for binning X, Y                        */
  options=,          /* REVERSE and/or NOGRID                              */
  anno=,             /* additional input annotate data set                 */
  outanno=surfanno,  /* output annotate data set used with G3D             */
  gout=gseg,         /* name of output graphic catalog                     */
  name=surface       /* name of graphic catalog entry                      */
  );

 /*---default data set---*/
 %if %bquote(&data)= %then %let data=&syslast;

 %local me; %let me=&sysmacroname;
 %if (%length(&x)=0 or %length(&y)=0 or %length(&z)=0 )
 	%then %do;
		%put ERROR: (&me) You must supply X=, Y= and Z= variable names;
		%goto DONE;
	%end;

 %let options = %qupcase(&options);

 %*-- Parse COLORS=HLS or COLORS=HLSllss;
 %let colors  = %qupcase(&colors);
 %if %substr(&colors,1,3) = HLS %then %do;
 	%if %length(&colors) = 3
		%then %let llss=80FF;
		%else %let llss=%substr(&colors,4);
	%let colors=HLS;
 	%end;
	

 %let xtick = %scan(&ticknum &ticknum &ticknum, 1);
 %let ytick = %scan(&ticknum &ticknum &ticknum, 2);
 %let ztick = %scan(&ticknum &ticknum &ticknum, 3);
 
 options nonotes;
 /*---compute ranges for the three coordinates---*/
 proc means data=&data noprint min max;
    %if %length(&where) %then %do;
       where &where;
    %end;
    var &x &y &z;
    output out=_range
       min=xmin ymin zmin
       max=xmax ymax zmax;
 run;

 /*---set up macro variables for the ranges---*/
 %local xmin xmax ymin ymax zmin zmax floor ceil zstep;
 data _null_;
    set _range;
    call symput('xmin', put(xmin,best.));
    call symput('xmax', put(xmax,best.));
    call symput('ymin', put(ymin,best.));
    call symput('ymax', put(ymax,best.));
    call symput('zmin', put(zmin,best.));
    call symput('zmax', put(zmax,best.));
    zrange = zmax - zmin;
    call symput('floor',put((zmin-(&drop)*zrange),best.));
    call symput('ceil',put(zmax,best.));
    call symput('zstep',put(zrange/&nlevels,best.));
 run;

 /*---determine step sizes for x and y---*/
 proc sort data=&data out=_xgrid nodupkey;
    by &x;
 run;

 data _xgrid;
    set _xgrid;
    keep &x xstep;
    xstep = &x - lag(&x);
    if (_n_ = 2) then do;
       call symput('xstep',put(xstep,best.));
    end;
 run;

 proc sort data=&data out=_ygrid nodupkey;
    by &y;
 run;

 data _ygrid;
    set _ygrid;
    keep &y ystep;
    ystep = &y - lag(&y);
    if (_n_ = 2) then do;
       call symput('ystep',put(ystep,best.));
    end;
 run;

 data _null_;
    set _ygrid nobs=count;
    call symput('ny',put(count,best.));
 run;

 %*-- Nested macro for color computation;
 %macro surcolor(arg);
	   %if &colors = HLS %then %do;
	    	length hue $3;
    	   %if %index(&options,REVERSE) %then %do;
        	  deg = round((&arg)*320,2) + 120;
    	   %end;
    	   %else %do;
        	  deg = 440 - round((&arg)*320,2);
    	   %end;
    	   hue = put(deg,$hex3.);
    	   color = 'H'||hue||"&llss";
		   drop hue;
           %end;
		%else %do;
			color = scan("&colors", ncol);
			%end;
 %mend;
 
 /*---create annotate data set for legend---*/
 data _legend;
    length function color $ 8;
    retain xsys ysys zsys '2';
    drop legend ncol;
    do legend = &zmin to (&zmax-&zstep) by &zstep;
       x = &xmin; 
       y = &ymax; 
       z = legend;
       function = 'poly'; 
       style = 'solid';
       ncol = min(&nlevels,int(1+(legend+(&zstep/2)-&zmin)/&zstep));
	   %surcolor(ncol/&nlevels);
	   /*
	   %if &colors=HLS %then %do;
    	   %if %index(&options,REVERSE) %then %do;
        	  deg = round((ncol/&nlevels)*320,2) + 120;
    	   %end;
    	   %else %do;
        	  deg = 440 - round((ncol/&nlevels)*320,2);
    	   %end;
    	   hue = put(deg,$hex3.);
    	   color = 'H'||hue||"&llss";
           %end;
		%else %do;
			color = scan(&colors, ncol);
			%end;
		*/
       output;
       z = legend + &zstep;
       function = 'polycont'; 
       %if not %index(&options,NOGRID) %then %do;
          color = 'black';
       %end;
       output;
       x = &xmin + (&xmax-&xmin)*.05; 
       output;
       z = legend; 
       output;
    end;
 run;

 /*---create annotate data sets for plane and surface; sort by
      the sum of coordinates so graphs will be drawn from
      back to front---*/
 data _zval;
    set &data;
    %if %length(&where) %then %do;
       where &where;
    %end;
    x = &x;
    y = &y;
    z = &z;
    keep x y z;
 run;

 proc sort data=_zval;
    by x y;
 run;

 data _surface;
    set &data;
    %if %length(&where) %then %do;
       where &where;
    %end;
    sum = (&x - &xmin)/&xstep + (&y - &ymin)/&ystep;
    keep &x &y &z sum;
 run;

 proc sort data=_surface;
    by descending sum;
 run;

 /*---draw 4-sided polygons counter-clockwise---*/
 data _surface;
    set _surface;
    length function color $ 8;
    retain xsys ysys zsys '2';
    /* drop &x &y &z; */
    x = &x; 
    y = &y; 
    if (abs(x-&xmin) < &fuzz*&xstep) or 
       (abs(y-&ymin) < &fuzz*&ystep) then delete;
    function = 'poly';
    style = 'solid';
	%*surcolor;
    ncol=min(&nlevels,int(1+(&z-&zmin+&fuzz*&zstep)/&zstep));
	%surcolor((&z-&zmin+&fuzz*&zstep)/&zstep/&nlevels);

/*	
	%if &colors=HLS %then %do;
    	%if %index(&options,REVERSE) %then %do;
    	   deg = round(((&z-&zmin+&fuzz*&zstep)/&zstep/&nlevels)*320,2) + 120;
    	%end;
    	%else %do;
    	   deg = 440 - round(((&z-&zmin+&fuzz*&zstep)/&zstep/&nlevels)* 320,2);
    	%end;
    	hue = put(deg,$hex3.);
    	color = 'H'||hue||'80FF';
		%end;
		%else %do;
			color = scan("&colors", ncol);
			%end;
*/	
    when = 'A';
    idx = &ny*(x-&xmin+&fuzz*&xstep)/(&xstep) + 
              (y-&ymin+&fuzz*&ystep)/(&ystep) + 1;
    index = idx;
    set _zval point=idx;
    output;
    function = 'polycont'; 
    %if not %index(&options,NOGRID) %then %do;
       color = 'black';
    %end;
    x = x - &xstep;
    idx = &ny*(x-&xmin+&fuzz*&xstep)/(&xstep) + 
              (y-&ymin+&fuzz*&ystep)/(&ystep) + 1;
    index = idx;
    set _zval point=idx;
    output;
    y = y - &ystep;
    idx = &ny*(x-&xmin+&fuzz*&xstep)/(&xstep) + 
              (y-&ymin+&fuzz*&ystep)/(&ystep) + 1;
    index = idx;
    set _zval point=idx;
    output;
    x = x + &xstep;
    idx = &ny*(x-&xmin+&fuzz*&xstep)/(&xstep) + 
              (y-&ymin+&fuzz*&ystep)/(&ystep) + 1;
    index = idx;
    set _zval point=idx;
    output;
 run;

 options notes;
 data _plane;
    set _surface;
    z = &floor;
 run;

 /*---merge annotate data sets---*/
 data &outanno;
    set _legend _plane _surface &anno;
 run;

 /*---create input data set for PROC G3D---*/
 data _plot;
    &x = &xmin; 
    &y = &ymin; 
    &z = &floor; 
    output;
    &x = &xmax; 
    &y = &ymax; 
    output;
 run;

 /*---draw the surface and contours---*/
 proc g3d data=_plot gout=&gout ;
    scatter &y*&x=&z  / 
	 %if %length(&rotate) %then %do;
       rotate=&rotate %end; 
	 %if %length(&tilt) %then %do;
       tilt=&tilt %end; 
	 xticknum=&xtick yticknum=&ytick zticknum=&ztick 
	 shape='point' 
     zmin=&floor zmax=&ceil 
	 annotate=&outanno  &scatopt 
	 name="&name" des="Surface plot of &y*&x=&z"
	   ;
 run;

  *-- Clean up datasets no longer needed;
proc datasets nofs nolist nowarn library=work memtype=(data);
    delete _xgrid _ygrid  _zval _range _legend _plane _surface _plot;
	run; quit;

%done:

%mend surface;

