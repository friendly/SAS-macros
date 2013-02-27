 /*-------------------------------------------------------------------*
  *    Name: ellipses.sas                                             *
  *   Title: Plot bivariate data ellipses                             *
        Doc: http://www.datavis.ca/sasmac/ellipses.html         
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created: 24 Oct 2006 16:10:05                                     *
  * Revised: 07 Oct 2011 15:22:31                                     *
  * Version:  2.5-1                                                   *
  * Requires: IML                                                     *
  *                                                                   *
  *    Renamed from contour.sas, originally from SSSG.  contour.sas   *
  *    will no longer be enhanced.                                    *
  *                                                                   *
  *  Added VAR= shorthand for x,y,z                                   *
  *  Added PLOTOPT= to supply options to plot statement               *
  *  Avoid diag(wt) to calculate cov when unweighted                  *
  *  Fixed buglet with INANNO
  *  Added comments to OUT= annotate data set
  *  WIDTH=0 will now suppress the ellipse
  * V 2.5
  *  Added IWIDTH= to control width of interpolated line.
  *  IWIDTH=1 is default
  *                                                                   *
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The ELLIPSES macro plots a bivariate scatterplot with a bivariate
data ellipse for one or more groups.

=Usage:

 The ELLIPSES macro is defined with keyword parameters.  The X= and Y= 
 variables are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%ellipses(data=auto, x=price, y=weight);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* X=          Name of the X variable

* Y=          Name of the Y variable

* Z=          Name of a Z variable (for G3D)

* VAR=        Two (or three) variable names x y (z), separated by spaces.  
              Instead                                                            
			                                 of specifying X= and Y= separately, you can specify
			  the names of the variables with the VAR= parameter.

* WHERE=      WHERE clause to select observations

* WEIGHT=     Numeric weight for observations.  Together with the OUTLIER 
              or ROBCOV macro, the WEIGHT= variable can be used to produce
			  robust data ellipses.

* GROUP=      Name of a Group variable (optional). If a GROUP= variable is
              specified, one ellipse is produced for each value of this
              variable in the data set.  If no GROUP= variable is specified, 
              a single ellipse is drawn for the entire sample.  
              The GROUP= variable may be character or numeric.

* CLASS=      Synonym for GROUP=

* GPFMT=      Name of a Format for group labels.

* PVALUE=     Confidence coefficient (1-alpha). This is the proportion of 
              data from a bivariate normal distribution contained within
              the ellipse.  Several values may be specified in a list 
              (e.g., PVALUE=.5 .9), in which case one ellipse is generated
              for each value. [Default: PVALUE=0.68]

* LINE=       Line style(s) for ellipse [Default: LINE=5]

* WIDTH=      Line width(s) for ellipse [Default: WIDTH=1]

* ANNOADD=    Additional annotations added to the plot. [Default: ANNOADD=MEAN GPLABEL]

* INANNO=     Additional (input) annotations data set

* STD=        Error bar metric: STD or STDERR. STD=STDERR gives error
              bars equal to each mean +- one standard error for both
              variables.  STD=STD gives error bars whose
              length is one standard deviation for both variables.
              [Default: STD=STDERR]

* POINTS=     Number of points on each ellipse [Default: POINTS=40]

* ALL=        Include an ellipse for total sample? Specifies whether the 
              ellipse for the total sample should be drawn in addition to 
			  those for each group.  If there is no GROUP= variable,
              ALL=YES just draws the ellipse twice. [Default: ALL=NO]

* OUT=        The name of the output Annotate data set used to draw the
              ellipses, error bars and group labels. [Default: OUT=ELLIPSES]

* PLOT=       Plot the results? If PLOT=YES, the macro plots the data together
              with the generated ellipses.  Otherwise, only the output 
			  Annotate data set is generated.[Default: PLOT=YES]

* HAXIS=      Name of an AXIS statement for the horizontal axis.  By default,
              the plot range of the X= variable is defined by the data, so
              the ellipses may be clipped and generate warnings in the PROC
              GPLOT step.  To avoid this, define an AXIS statement that
              defines a suitable range and specify that with HAXIS=.

* VAXIS=      Name of an AXIS statement for the vertical axis.  See description
              for HAXIS=.

* I=          SYMBOL statement interpolate option. Use I=RL to include the
              regression line, I=RQ to draw a quadratic, etc. [Default: I=NONE]

* INTERP=     (synonym for I=)

* IWIDTH=     Width for SYMBOL statement interpolate option

* COLORS=     List of colors for each of the groups.  If there are  g  groups,
              specify  g  colors if ALL=NO, and g +  1  colors if ALL=YES.
              The colors specified are recycled as needed.
              [Default: COLORS=RED BLUE GREEN BLACK PURPLE BROWN ORANGE YELLOW ]

* SYMBOLS=    List of symbols, separated by spaces, used for plotting points in
              each of the groups. Recycled as needed.
			  [Default: SYMBOLS=DOT SQUARE CIRCLE + STAR -     PLUS   :      $     =]

* HSYM=       Height of plot symbols [Default: HSYM=1.2]

* HTEXT=      Height of text in the plot [Default: HTEXT=1.5]

* PLOTOPT=    Additional options supplied to the PLOT statement, e.g., HREF=0

* NAME=       The name of the graph in the graphic catalog [Default: NAME=ELLIPSES]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]
                
==Notes:

 Robust data ellipses may be obtained by first using the ROBUST macro,
 then feeding the output dataset to ELLIPSES, specifying WEIGHT=WEIGHT.

=Examples:

  %include data(auto) ;
  %ellipses(data=auto,
           x=price, y=weight, group=origin,
           pvalue=.5);
	
 =*/
 
%macro ellipses(
	data=_LAST_,           /* input data set                      */
	x=,                    /* X variable                          */
	y=,                    /* Y variable                          */
	z=,                    /* Z variable (for G3D)                */
	var=,                  /* shorthand for x,y,z                 */
	where=,                /* WHERE clause to select observations */
	weight=,               /* numeric weight for observations     */
	group=,                /* Group variable (optional)           */
	class=,                /* syn for GROUP=                      */
	gpfmt=,                /* format for group labels             */
	pvalue=0.68,           /* Confidence coefficient (1-alpha)    */
	line=5,                /* line style(s) for ellipse           */
	width=1,               /* line width(s) for ellipse           */
	annoadd=MEAN GPLABEL,  /* additional annotations              */
	inanno=,               /* additional (input) annotations      */
	std=STDERR,            /* error bar metric: STD or STDERR     */
	points=40,             /* points on each contour              */
	all=NO,                /* include ellipse for total sample?   */
	out=ellipses,          /* output annotate data set            */
	plot=YES,              /* plot the results?                   */
	haxis=,                /* AXIS statement for horizontal axis  */
	vaxis=,                /* AXIS statement for vertical axis    */
	i=none,                /* SYMBOL statement interpolate option */
	interp=,               /* (synonym for I=)                    */
	iwidth=1,
	colors=RED BLUE GREEN BLACK PURPLE BROWN ORANGE YELLOW ,
	symbols=dot square circle + star -     plus   :      $     =,
	hsym=1.2,               /* Height of plot symbols              */
	htext=1.5,            /* Height of text                        */ 
	plotopt=,              /* additional options for plot stmt    */       
	name=ellipses,         /* Name for graphic catalog entry      */
	gout=gseg
	 );
 
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes /* validvarname=upcase */;
		%end;
	%else %do;
	   options nonotes;
		%end;

%local me; %let me=&sysmacroname;
%let all = %upcase(&all);
%let annoadd = %upcase(&annoadd);

%*-- Check for required parameters;
%local abort;
%let abort=0;

%if %length(&x)=0 %then %let x = %scan(&var,1);
%if %length(&y)=0 %then %let y = %scan(&var,2);
%if %length(&z)=0 %then %let z = %scan(&var,3);
%if &x=%str() or &y=%str() %then %do;
	%put ERROR: (&me) The X= and Y= variables (or VAR=) must be specified;
	%let abort=1;
   %goto DONE;
   %end;

%if %length(&class)>0 %then %let group=&class;
%if %length(&interp)>0 %then %let i=&interp;

%*-- Set up where clause to select observations;
%if %length(&where)>0 %then %let where=where (&where);
 
*-- Check for IML procedure;
proc iml; quit;
%if &syserr>4 %then %do;
	%put ERROR: (&me) The IML procedure is required for this macro.;
	%let abort=1;
	%goto done;
	%end;

proc iml;
start ellipse(c, x, y, wt, npoints, pvalues, formean);
  /*----------------------------------------------------------------*
   |  Computes elliptical contours for a scatterplot                |
   |   C       returns the contours as consecutive pairs of columns |
   |   X,Y     coordinates of the points                            |
   |   NPOINTS scalar giving number of points around a contour      |
   |   PVALUES column vector of confidence coefficients             |
   |   FORMEAN 0=contours for observations, 1=contours for means    |
   *----------------------------------------------------------------*/
 
   xx = x||y;
   n  = nrow(x);
	if type(wt) ^='N' then wt = j(n,1);
   *-- Correct for the mean --;
   if all(wt=1) then do;
	  mean = xx[+,]/n;
	  xx = xx - mean @ j(n,1,1);
	  xx = xx` * xx / (n-1);
   end;
   else do;
	  mean = (wt#xx)[+,]/sum(wt);
	  xx = xx - mean @ j(n,1,1);
	  xx = xx` * diag(wt) * xx / (sum(wt)-1);
   end;
   *-- Find principal axes of ellipses --;
   * print 'Variance-Covariance Matrix',xx;
   call eigen(v, e, xx);
 
   *-- Set contour levels --;
   c =  2*finv(pvalues,2,int(sum(wt))-1,0);
   if formean=1 then c = c / (sum(wt)-1) ;
   * print 'Contour values',pvalues c;
   *-- protect against negative eigenvalues;
   a = sqrt(c* max(v[ 1 ],0) );
   b = sqrt(c* max(v[ 2 ],0) );
 
   *-- Parameterize the ellipse by angles around unit circle --;
   t = ( (1:npoints) - 1) # atan(1)#8/(npoints-1);
   s = sin(t);
   t = cos(t);
   s = s` * a;
   t = t` * b;
 
   *-- Form contour points --;
   s = ( ( e*(shape(s,1)//shape(t,1) )) +
         mean` @ j(1,npoints*ncol(c),1) )` ;
   c = shape( s, npoints);
   *-- C returned as NCOL pairs of columns for contours--;
finish;
 
start dogroups(x, y, wt, gp, pvalue);
   d  = design(gp);
   %if &all=YES %then %do;
      d = d || j(nrow(x),1,1);
   %end;
   seg=0;
   do group = 1 to ncol(d);
      * print group;
      *-- select observations in each group;
      col = d[, group ];
      xg  = x[ loc(col), ];
      yg  = y[ loc(col), ];
      wg  = wt[loc(col), ];

      *-- Find ellipse boundary ;
	  if nrow(xg) >1 then do;
    	 run ellipse(xyg,xg,yg,wg, &points, pvalue, 0 );
    	 nr = nrow(xyg);

    	 *-- Output contour data for this group;
    	 cnames = { X Y PVALUE _GP_ };
    	 do c=1 to ncol(pvalue);
        	col=(2*c)-1 : 2*c ;
			seg = seg+1;
        	xygp = xyg[,col] || j(nr,1,pvalue[c]) || j(nr,1,group);
        	if seg=1
               then create contour from xygp [colname=cnames];
        	append from xygp;
    	 end;
	  end;
	  else print 'Warning:  group' group 'has only' (nrow(xg)) 'observations.';
   end;
finish;
 
start nomiss(matrix, obsnames, keep);
   *-- Remove rows with any missing data from matrix and obsnames;
   *   (pass ' ' for obsnames if no row labels are present);
   miss = loc(matrix <=.Z);
   if type(miss)='U'
      then do;
		   keep=1:nrow(matrix); 
			return;           /* no missing data */
			end;
      else do;
         nr = nrow(matrix);
         nc = ncol(matrix);
         rows= 1+floor((miss-1)/nc);
         rows= unique(rows);
         keep=remove(1:nrow(matrix),rows);
         deleted = nr - ncol(keep);
         matrix = matrix[keep,];
         reset noname;
         print 'Warning:' deleted 'row(s) with missing data have been removed.';
         reset name;
         if obsnames ^={' '}
            then do;
               obs = obsnames[rows];
               obs = shape(obs,1,nrow(obs)#ncol(obs));
               if type(obs)='N' then obs=char(obs,3,0);
               obsnames=obsnames[keep];
            *  print rows[c=obs], obs;
            end;
         end;
finish;

*-- Get input data: X, Y, GP;
   use &data;
   read all var {&x} into x [colname=lx] &where;
   read all var {&y} into y [colname=ly] &where;
	xy = x || y;
	run nomiss(xy, ' ', kept);
%if %length(&weight) > 0 %then %do;
   read all var {&weight} into wt [colname=lg] &where;
	if ncol(keep) ^= nrow(xy) then wt = wt[kept];
   %end;
%else %do;
	wt = j(ncol(kept),1);
	%end;

%if %length(&group) > 0 %then %do;
   read all var {&group} into gp [colname=lg] &where;
   %end;
%else %do;
   gp = j(nrow(x),1,1);
   %end;
	x = xy[,1];
	y = xy[,2];
	if ncol(keep) ^= nrow(xy) then gp = gp[kept];
   close &data;
 
*-- Find ellipses for each group;
   run dogroups(x, y, wt, gp, { &pvalue} );
quit;
%if &syserr > 4 %then %let abort=1; %if &abort>0 %then %goto DONE;
 
 /*-----------------------------------*
  |  Plot the ellipses using ANNOTATE |
  *-----------------------------------*/
goptions reset=symbol;
data &out;
   set contour end=eof;
   by _gp_ pvalue notsorted;
	retain g 0;
	drop g;
	%if %length(&group) > 0 %then %do;
		if first._gp_ then g+1;
		%end;
	%else %if %scan(&pvalue,2) > 0 %then %do;
		if first.pvalue then g+1;
		%end;
	%else %do;
		g=1;
		%end;
		
   length function color comment $8;
   xsys='2'; ysys='2'; comment='ellipse';
   if first.pvalue then function='POLY';
                   else function='POLYCONT';
   color=scan("&colors", g);
   %if %length(%scan(&line, 2)) %then %do;
	   line=input(scan("&line", g), 3.);
	   %end;
	%else %do;
	   line = &line;
	   %end;
   %if %length(%scan(&width, 2)) %then %do;
	   size=input(scan("&width", g), 3.);
	   %end;
	%else %do;
	   size = &width;
	   %end;
   if size>0 then output;
   if eof then call symput('NGROUP',put(g,best.));
run;
 /*----------------------------*
  |  Crosses at Mean +- StdErr |
  *----------------------------*/
%if &annoadd ^= NONE %then %do;
proc summary data=&data nway;
   &where;
   class &group;
   var &x &y;
	%if %length(&weight) > 0 %then %do;
	freq &weight;
	%end;
   output out=_sumry_ mean=mx my &std=sx sy;
run;
data _bars_;
   set _sumry_ end=eof;
*   where _freq_>1;
   %if &group ^= %str() %then %str(by &group;);
   length function color comment $8;
   retain g 1;
   drop _freq_ _type_ mx my sx sy g;
   xsys='2'; ysys='2';
 
   color=scan("&colors",g);
	%if %index(&annoadd,MEAN) > 0 %then %do;
   line=3;
   %if %length(%scan(&width, 2)) %then %do;
	   size=input(scan("&width", g), 3.);
	   %end;
	%else %do;
	   size = &width;
	   %end;
   comment = 'bar';
   x = mx-sx; y=my;      function='MOVE'; output;
   x = mx+sx;            function='DRAW'; output;
   x = mx   ; y=my-sy;   function='MOVE'; output;
              y=my+sy;   function='DRAW'; output;
	%end;
 
 *-- Write group label (convert numeric &group to character);
   %if &group ^= %str() and %index(&annoadd,GPLABEL) > 0 %then %do;
      length text $16;
	  %if %length(&gpfmt) = 0 %then %do;
	      text = left(trim(&group));
	  	%end;
	%else %do;
	      text = left(trim(put(&group, &gpfmt)));
		%end;
      position='3';
      size = &htext;
      x = mx+.2*sx   ; y=my+.2*sy;
	  comment = 'label';
      function='LABEL'; output;
   %end;
   %if &group ^= %str() %then %do;
      if last.&group then g+1;
   %end;
run;
 
data &out;
   set &out _bars_;
	%if %length(&z)>0 %then %do;
		zsys='2';
		z=&z;
		%end;
proc datasets lib=work memtype=data nolist nowarn;
   delete _sumry_ _bars_;
%end; /* annoadd ^= NONE */

%if %length(&inanno) %then %do;
data &out;
   set &out &inanno ;		
	%end;
 
%if %upcase(&plot)=YES %then %do;

   %gensym(n=&ngroup, h=&hsym, interp=&i, colors=&colors, symbols=&symbols, width=&iwidth );

   %if %length(&haxis)=0 %then %do;
      axis99 offset=(3);
      %let haxis=axis99;
      %end;
   %if %length(&vaxis)=0 %then %do;
      axis98 offset=(3) label=(a=90 r=0);
      %let vaxis=axis98;
      %end;

	%if &group = %str() %then %let group=1;
   proc gplot data=&data ;
	  &where;
      plot &y * &x = &group /
           annotate=&out
           nolegend frame
           vaxis=&vaxis vminor=0
           haxis=&haxis hminor=0
		   &plotopt
           name="&name";
      run; quit;
goptions reset=symbol;
%end;
%done: ;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%if &abort %then %put ERROR: The &me macro ended abnormally.;
%mend;
 
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
%macro gensym(
   n=1,
   start=1, 
   h=1.5,
   interp=none,
   line=1,
   symbols=%str(square triangle : $ = X _ Y),
   colors=BLACK RED GREEN BLUE BROWN ORANGE PURPLE YELLOW,
   ci=,
   font=,
   width=1,
   repeat=1,
   label=
   );

*options mprint symbolgen;
    %*--  symbols, colors, line styles, and interp are recycled as needed;
  %local chr col int lin k cic;
  %do k=&start %to %eval(&n + &start -1) ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0 
	      %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0 
	      %then %let colors = &colors &colors;
     %if %length(%scan(&interp, &k, %str( ))) = 0 
	      %then %let interp = &interp &interp;
     %if %length(%scan(&line,   &k, %str( ))) = 0 
	      %then %let line = &line &line;
     %if %length(%scan(&width,   &k, %str( ))) = 0 
	      %then %let width = &width &width;
     %if %length(&ci) 	%then %do;
         %if %length(%scan(&ci,  &k, %str( ))) = 0 
	           %then %let ci = &ci &ci;
	     %end;

	 %let chr =%scan(&symbols,&k, %str( ));
     %let col =%scan(&colors, &k, %str( ));
     %let int =%scan(&interp, &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     %let wid =%scan(&width,  &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));

	  %if &k=99 %then %let repeat=999;
     symbol&k
      %if %length(&font) 	  %then font=&font;
	  %if %length(&label) 	%then pointlabel=%str( (h=1 "#&label") );
	  height=&h value=&chr color=&col i=&int l=&lin w=&wid r=&repeat
	  %if %length(&cic) 	%then %str(ci=&cic);
	  ;
	%if &k=99 %then %goto done;
  %end;
*options nomprint nosymbolgen;
%done:
%mend;
