 /*-------------------------------------------------------------------*
  *    Name: stars.sas                                                *
  *   Title: Star plot of multivariate data                           *
  *      Plots each observation as a star figure with one ray for     *
  *      each variable, ray length proportional to the size of that   *
  *      variable.                                                    *
        Doc: http://www.datavis.ca/sasmac/stars.html              
  *                                                                   *
  * Reference: Chambers, Cleveland, Kleiner & Tukey, Graphical methods*
  *            for data analysis, Wadsworth, 1983. pp158-162.         *
  *-------------------------------------------------------------------*
  * Author:   Michael Friendly            <friendly@yorku.ca>         *
  * Created:  31 Mar 1988 00:15:25                                    *
  * Revised:  10 Sep 2010 13:58:42                                    *
  * Version:  1.7-1                                                   *
  *  - Replaced IML scaling with datastep                             *
  *  - Added color=option to control color of individual stars        *
  *  - Added MISSING= option                                          *
  *  - Added KEYPAGE= option: variable key on same/new page           *
  *  - Fixed bug with scaling of text in variable key                 *
  *  - Added std= to standardize, and sortby= to sort obs             *
  * 1.7 Added VORDER=PRINn to order varibles by PRINn                 *
  *     Added CFILL= to fill stars with given color                   *
  *     Added RAYLINE= to draw rays with different line styles        *
  *     Added OUT= to name output data set                            *
  *     Allow KEYPAGE=-1 to suppress variable key                     *
  *     Default for MISSING= changed to 0                             *
  *     Fixed %if -- %then buglet                                     *
  *                                                                   *
  * Dependencies:  %gskip (needed for eps/gif only)                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The STARS macro draws a star plot of the multivariate observations
 in a data set.  Each observation is depicted by a star-shaped figure
 with one ray for each variable, whose length is proportional to the
 size of that variable.  

 Each variable is scaled from 0 to 1.  The STD= option permits the
 variables to be standardized to a given mean and standard deviation
 before this scaling.  The SORTBY= option allows the observations
 to be sorted by a variable or a statistic.  The VORDER= option
 allows the order of variables around the star to be rearranged
 according to their values on a given principal component.

==Method:

 The macro constructs an ANNOTATE data set to draw the entire display,
 possibly with several pages.  The graphic ouotput is produced using
 PROC GSLIDE.


=Usage:

 The STARS macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%stars(data=auto);
 
==Parameters:

* DATA=       Data set to be displayed [Default: DATA=_LAST_]

* VAR=        Names of variables, as ordered around the star,
              from angle=0 (horizontal), unless the VORDER= option is used. 
			  [Default: VAR=_NUMERIC_]

* ID=         Observation identifier (a character variable) 

* STD=        Standardizes the variables first to given mean
              and (optionally) a given standard deviation.
			  STD=0 standardizes to mean=0; STD=0 1 standardizes
			  to mean=0, std=1.  By default the variables are not
			  standardized, but are scaled to 0,1.


* SORTBY=     Sorts the observations by a variable in the
              data set, or a statistic calculated across all
			  variables in the VAR= list.  If the SORTBY=
			  value end in a '.', it is assumed to be the name
			  of a statistic.  For example SORTBY=SUM. sorts
			  by the total of scaled values for all variables.


* VORDER=     You can specify VORDER=PRINn to request that the variables are reordered
              according to their weights on the nth principal components of the correlation
			  matrix, where, typically n=1 or 2. VORDER=ANGLE requests that the variables
			  are ordered according to the angles between variable vectors in a plot of
			  the first two principal compontents,  

* MINRAY=     Minimum ray length, a number between 0 and 1 [Default: MINRAY=.1]

* ACROSS=     Number of stars across a page [Default: ACROSS=6]

* DOWN=       Number of stars down a page. If the
              product of ACROSS and DOWN is less than the
              number of observations, multiple graphs are
              produced. [Default: DOWN=6]

* COLOR=      Specifies the color used to draw rays for individual observations, either as a 
              quoted string constant, or the name ofa variable in the input data set 
			  (but don't use COLOR=COLOR !) [Default: COLOR='BLACK']

* CFILL=      Background color for the stars [Default: CFILL='']

* RAYLINE=    Line style(s) of rays [Default: RAYLINE=1]

* RAYTHICK=   Line thickness of rays [Default: RAYTHICK=1]

* HTVLABEL=   Height of variable labels in the variable key [Default: HTVLABEL=2.5]

* A0=         Angle offset, the angle for first ray [Default: A0=0]

* CIRCLE=     Draw a circle around each star at 100%?  A non-zero value specifies the
              line style used for the circle. [Default: CIRCLE=0]

* MISSING=    Specifies the ray length to be used when an
	          observation has a missing value.  Specify MISSING = . (a period)
	          to have observations with missing values deleted. [Default: MISSING=0]

* NAME=       Name for graphic catalog entry [Default: NAME=STARS]

* KEYPAGE=    Draw the variable key on separate page?  If KEYPAGE is a positive number,
              a separate page for the variable key is produced.  If KEYPAGE < 0, the
			  variable key is suppressed.  If KEYPAGE = 0, the
			  variable key is drawn in the top left corner of the first page.
              [Default: KEYPAGE=1]

* OUT=        The name of the output ANNOTATE data set used to draw the stars. [Default: OUT=STARS]
                
==GOPTIONS:

 The font used for text labels is controled by the FTEXT= setting.
 The HSIZE= and VSIZE= options should be used to scale the graph
 so that the individual stars are approxmately the same height
 and width.

 =*/
 
%macro stars(
   data=_LAST_,      /* Data set to be displayed                       */
   var=_NUMERIC_,    /* Variables, as ordered around the star          */
                     /* from angle=0 (horizontal)                      */
   id=,              /* Observation identifier  (char variable)        */
   std=,             /* Standardize first: M [SD]                      */
   sortby=,          /* Sort obs by a variable or stat.                */
   vorder=,          /* Order variables by PRINx ?                     */
   minray=.1,        /* Minimum ray length, 0<=MINRAY<1                */
   across=6,         /* Number of stars across a page                  */
   down=6,           /* Number of stars down a page                    */
   color='BLACK',    /* star color, quoted string or variable name     */
   cfill='',         /* Background color                               */
   rayline=1,        /* Line style(s) of rays                          */
   raythick=1,       /* Line thickness of rays                         */
   htvlabel=2.5,     /* Height of variable labels in key               */
   a0=0,             /* Angle offset - angle for first ray             */
   circle=0,         /* Draw circle around each star at 100%?          */
   missing=0,        /* value assigned to missing data, or . to delete */
   name=stars,       /* name for graphic catalog entry                 */
   keypage=1,        /* draw variable key on separate page?            */
   out=stars         /* name of output data set                        */         
   );

%if %bquote(&data)= %bquote(_LAST_) %then %let data = &syslast;
%if %length(&color) = 0 %then %let COLOR = 'BLACK';
 
 /*---------------------------------------*
  |  Find out how many variables and obs. |
  *---------------------------------------*/
data _null_;
   file log;
   array p{*} &var ;
   point=1;
   set &data point=point nobs=nobs;
   k = dim(p);
   put "NOTE: STARS plots for data set &DATA" ;
   put @7 'Number of variables    = ' k ;
   put @7 'Number of observations = ' nobs /;
   call symput('NV'  , left(put(k, 2.)));
   call symput('NOBS', put(nobs, 5.));

	lines = "&rayline";
	this = scan(lines,1);
	do i=2 by 1 until(this=' ');
		this = scan(lines,i);
		end;
	call symput('NL'  , left(put(i-1, 2.)));

   stop;     /* Don't forget this ! */
run;

 /*---------------------------------------*
  |  Re-order vars by PRIN                |
  *---------------------------------------*/
%if %length(&vorder) %then %do;
	%let vorder=%upcase(&vorder);
	%if %index(|PRIN| |ANGL|, |%substr(&vorder,1,4)|) = 0 %then %do;
		%put STARS:  VORDER=&vorder not recognized.  Using VORDER=ANGLE;
		%let vorder=ANGLE;
		%end;
	%if %substr(&vorder,1,4) = 'PRIN' or &vorder=ANGLE %then %do;
	proc princomp data=&data cov outstat=_comp_ noprint;
		var &var;
	
	data _comp_;
		set _comp_;
		where _type_='SCORE';
		drop _name_ _type_;
	proc transpose data=_comp_ out=_comp_ prefix=prin;
	%if &vorder=ANGLE %then %do;
	data _comp_;
		set _comp_;
		angle = atan(prin2/prin1);
		%end;
	proc sort;
		by &vorder;
	
	data _null_;
		set _comp_ end=eof;
		length vars $200;
		retain vars;
		if _n_=1
			then vars = _name_;
			else vars = trim(vars) || ' ' || _name_;
		if eof then call symput('var', trim(vars));
	run;
	%end;
%end;

 /*----------------------------------------------------*
  |  Standardize each variable to given mean, std      |
  *----------------------------------------------------*/
%if %length(&std) %then %do;
	%let mean  = %scan(&std,1);
	%let stdev = %scan(&std,2);
	%if %length(&stdev) and &stdev ^= .
		%then %let stdev=s=&stdev;
		%else %let stdev=;
	proc standard data=&data out=&data m=&mean &stdev;
		var &var;
%end;

 /*----------------------------------------------------*
  |  Sort observations by variable or statistic        |
  *----------------------------------------------------*/
%if %length(&sortby) %then %do;
	%if %index(&sortby,.) %then %do;
	%let sortby = %substr(&sortby,1, %eval(%index(&sortby,.)-1));
	data &data;
		set &data;
		&sortby = &sortby(of &var);
	%end;
	proc sort;
		by &sortby;
%end;

 /*----------------------------------------------------*
  |  Scale each variable to range from MINRAY to 1.0   |
  *----------------------------------------------------*/
proc univariate noprint data=&data;
   var &var;
   output out=_range_ min=mn1-mn&nv max=mx1-mx&nv;
run;

data _scaled_;
   set &data;
   if _n_=1 then set _range_;
	drop i keep mn1-mn&nv mx1-mx&nv;
   array vars{*} &var;
   array mn{*}  mn1-mn&nv ;
   array mx{*}  mx1-mx&nv ;
	keep = 1;
   do i = 1 to &nv;
		if vars(i) = .
			then do; vars(i) = &missing; keep=&missing; end;
			else vars(i) = &minray + (1-&minray)*(vars(i)-mn(i))/
								(mx(i)-mn(i));
      end;
	if keep = . then delete;
	
  %put &DATA dataset variables scaled to range &MINRAY to 1;
 
 /*---------------------------------------------------*
  |  Text POSITIONs corresponding to rays of varying  |
  |  angle around the star                            |
  *---------------------------------------------------*/
proc format;
   value posn     0-22.5 = '6'  /* left, centered  */
               22.6-67.5 = 'C'  /* left, above     */
               67.6-112.5= '2'  /* centered, above */
              112.6-157.5= 'A'  /* right, above    */
              157.6-202.5= '4'  /* right, centered */
              202.6-247.5= 'D'  /* right, below    */
              247.6-292.5= 'E'  /* centered, below */
              other='F';        /* left, below     */
run;
 
 /*------------------------------------------*
  |  Construct ANNOTATE data set to draw and |
  |  label the star for each observation.    |
  *------------------------------------------*/
data &out;
     length function varname color $8;
     array p(k) &var ;
 
     retain s1-s&nv c1-c&nv;
     retain cols   &across      /* number of observations per row  */
            rows   &down        /* number of rows per page         */
            xsys   '1'          /* use data percentage coordinates */
            ysys   '1'          /* for both X and Y                */
            lx ly page 1        /* cell X,Y and page  counters     */
            rx ry r             /* cell radii                      */
				a0;
     array s(k)  s1-s&nv;              /* sines of angle      */
     array c(k)  c1-c&nv;              /* cosines of angle    */
	  array l{&nl} _temporary_ (&rayline);
 
     drop cols rows rx ry cx cy s1-s&nv c1-c&nv &var x0 y0  &id k save;
     drop varname showvar r ang a0 i;
     if _n_=1 then do;
        *--- precompute ray angles;
		  a0 = &a0 * (3.1415926/180);
        do k= 1 to &nv;
           ang = a0 + (2 * 3.1415926 * (k-1)/&nv);
           s = sin( ang );
           c = cos( ang );
           p = 1.0;
        end;

     *--- set size of one cell;
        rx= (100/cols)/2;
        ry= (100/rows)/2;
 
        text = 'Variable Assignment Key';
        showvar=1;                     /* For variable key */
		  
		  %if &keypage < 0 %then %do;
				lx = 0;
				ly = 0;
		  %end;
		  %else %if &keypage > 0 %then %do;
				x0 = 50; y0 = 50;
				r  = 30;
				size=&htvlabel;
				x  = x0; y = 8;
				function = 'LABEL';   output;
		
				link DrawStar;                 /* Do variable key  */
				page+1;
				lx = 0;
				ly = 0;
		  %end;
		  %else %do;
				cy  = 100 * (rows-1) / rows;
				x0 = rx;  y0 = ry + cy;
				r = .5 * min(rx,ry);
				x = x0; y = 	y0 + .9*ry;
				size=.9; 
				function = 'LABEL';   output;
				x = 0; y=cy;  function='MOVE'; output;
				x = 100/cols; function='DRAW'; output;
				y = 100;      function='DRAW'; output;
				link DrawStar;                 /* Do variable key  */
				lx = 1;
				ly = 0;
		  %end;
     end;
 
     set _scaled_ end=lastobs;
     showvar=0;
     r = .90 * min(rx,ry);
 
     /* (CX,CY) specify location of lower left corner */
     /*  as percent of data area                      */
     cx  = 100 * (lx) / cols;
     cy  = 100 * ((rows-1)-ly) / rows;
 
     color = 'BLACK';
     function = 'LABEL';          /* Label the observation centered */
     size = round(r/12,.1);       /* at bottom of the cell          */
     size = min(max(1,size),2);  /* .8 <= SIZE <= 2                */
     text = &id;
     position='5';
     x =rx+cx; y=1+cy;
     output;
 
     x0 = cx + rx;                     /* Origin for this star */
     y0 = cy + ry;

     link drawstar;
     %if &circle>0 %then %do;
	  	size=.; color='yellow'; line=&circle;
	  	link circle;
	  	%end; 
     if ( lastobs ) then do;
        call symput('PAGES',trim(left(page)));
        put 'STARS plot will produce ' page 'page(s).';
     end;
     lx + 1;                 /* next column */
     if lx = cols then do;
        lx = 0;
        ly + 1; end;         /* next row    */
     if ly = rows then do;
        lx = 0;
        ly = 0;
        page + 1; end;       /* next page   */
     return;
 
DrawStar:
     *-- Draw star outline;
     do k = 1 to &nv;
        x = x0 + p * r * c;
        y = y0 + p * r * s;
        if k=1 then do;
          %if %length(&cfill)>0 %then %do;
				color = &cfill;
				if color ^=' ' then style = 'solid';
			 	%end;
		  	function = 'POLY';
			end;
        else do;
			color = &color;
			function = 'polycont';
			end;
        output;
     end;
 
     *-- draw rays from center to each point;
     *-- label with the variable name if showvar=1;
     do k = 1 to &nv;
        x=x0; y=y0;
        function='MOVE';   output;
        x = x0 + p * r * c;
        y = y0 + p * r * s;
		  line = l{1+mod(k-1, &nl)};
		  save = size; size = &raythick;
        function = 'DRAW'; output;
		  size=save;
        if showvar = 1 then do;
           ang = a0 + (2 * 3.1415926 * (k-1)/&nv);
           varname= ' ';
           call vname(p,varname) ;
           text = trim(left(varname));
			  text = substr(text,1,1) || lowcase(substr(text,2));
           position = left(put(180*ang/3.14159,posn.));
           function = 'LABEL';  output;
        end;
     end;
  return;

circle:
	do i = 0 to 2*3.1415926 by (2*3.1415926/36);
		if i=0 
			then function='move' ;
			else function='draw' ;
		x=x0+r*cos(i);  y=y0+r*sin(i);  output;
		end;
	return;

run;                         /* Force SAS to do it (DONT REMOVE)  */

 /*----------------------------------------*
  |  Plot each page with GSLIDE:           |
  |   - Copy observations for current page |
  |   - Draw plot                          |
  |   - Delete page data set               |
  *----------------------------------------*/
%do pg = 1 %to &pages;
   data slide&pg;                    /* Select current page to plot */
      set &out;
      if page = &pg;

   proc gslide annotate=slide&pg /*&GOUT*/   /* Plot current page        */
		name="&name&pg" des="stars plot of &data";
   run;
	%if &pages>1 %then %gskip;
   proc datasets nolist lib=work; delete slide&pg;
%end; /* end of page */
 
%mend;
