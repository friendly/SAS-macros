 /*-------------------------------------------------------------------*
  *    Name: contour.sas                                              *
  *   Title: Plot bivariate data ellipses                             *
        Doc: http://www.datavis.ca/sasmac/contour.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  8 Jun 1988 12:33:21                                     *
  * Revised: 10 Oct 2006 11:48:19                                     *
  * Version:  2.4                                                     *
  *    Observations with missing data are deleted (including .A-.Z)   *
  *    Fixed gensym, for more than 8 groups                           *
  *    Added line= option, selection of mean & group labels           *
  *    Added CLASS= synonym for GROUP=, reset SYMBOL at end           *
  *    Added WEIGHT= to weight observations                           *
  *    Added WHERE= to select observations                            *
  *    Changed default pvalue = 0.68 = 'standard ellipse'             *
  *    Added INANNO= for input annotate data set(s)                   *
  *    Fixed bug with weights when GROUP= is specified (or not)       *
  *    Fixed bug with annoadd=MEAN with weighted data                 *
  *    Added Z= for use with G3D                                      *
  *    Added HSYM=; changed GP to _GP_                                *
  *    Fixed bug with singular covariance matrices                    *
  *    Allow different LINE= values for multiple groups               *
  *    Allow different WIDTH= values for multiple groups              *
  *    Added GPFMT= to format group labels                            *
  *    Suppress notes; clean up _temp_ data sets                      *
  *    Handle error when a group has <2 observations                  *
  *    Inlined revised GENSYM                                         *
  *    Fixed bug with WHERE= (plotted all observations)               *
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/
 
%macro contour(
	data=_LAST_,           /* input data set                      */
	x=,                    /* X variable                          */
	y=,                    /* Y variable                          */
	z=,                    /* Z variable (for G3D)                */
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
	all=NO,                /* include contour for total sample?   */
	out=CONTOUR,           /* output annotate data set            */
	plot=YES,              /* plot the results?                   */
	haxis=,                /* AXIS statement for horizontal axis  */
	vaxis=,                /* AXIS statement for vertical axis    */
	i=none,                /* SYMBOL statement interpolate option */
	interp=,               /* (synonym for I=)                    */
	name=CONTOUR,          /* Name for graphic catalog entry      */
	gout=gseg,
	colors=RED BLUE GREEN BLACK PURPLE BROWN ORANGE YELLOW ,
	symbols=dot square circle + star -     plus   :      $     =,
	hsym=1.2,               /* Height of plot symbols              */
	htext=1.5
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

%let all = %upcase(&all);
%let annoadd = %upcase(&annoadd);
%if &x=%str() or &y=%str() %then %do;
   %put CONTOUR: X= and Y= variables must be specified;
   %goto DONE;
   %end;

%if %length(&class)>0 %then %let group=&class;
%if %length(&interp)>0 %then %let i=&interp;

%*-- Set up where clause to select observations;
%if %length(&where)>0 %then %let where=where (&where);
 
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
   mean = (wt#xx)[+,]/sum(wt);
   xx = xx - mean @ j(n,1,1);
 
   *-- Find principal axes of ellipses --;
   xx = xx` * diag(wt) * xx / (sum(wt)-1);
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
 
*-- Find contours for each group;
   run dogroups(x, y, wt, gp, { &pvalue} );
quit;
 
 /*-----------------------------------*
  |  Plot the contours using ANNOTATE |
  *-----------------------------------*/
goptions reset=symbol;
data &out;
   set contour end=eof;
   by _gp_ pvalue notsorted;
	retain g 0;
	drop g;
	if first._gp_ then g+1;
   length function color $8;
   xsys='2'; ysys='2';
   if first.pvalue then function='POLY';
                   else function='POLYCONT';
   color=scan("&colors",_gp_);
   %if %length(%scan(&line, 2)) %then %do;
	   line=input(scan("&line", _gp_), 3.);
	   %end;
	%else %do;
	   line = &line;
	   %end;
   %if %length(%scan(&width, 2)) %then %do;
	   size=input(scan("&width", _gp_), 3.);
	   %end;
	%else %do;
	   size = &width;
	   %end;
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
   length function color $8;
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
   set &inanno &out;		
	%end;
 
%if %upcase(&plot)=YES %then %do;

   %gensym(n=&ngroup, h=&hsym, interp=&i, colors=&colors, symbols=&symbols );

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
%mend contour;
 
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

*options mprint;
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
     %if %length(&ci) 	%then %do;
         %if %length(%scan(&ci,  &k, %str( ))) = 0 
	           %then %let ci = &ci &ci;
	     %end;

	 %let chr =%scan(&symbols,&k, %str( ));
     %let col =%scan(&colors, &k, %str( ));
     %let int =%scan(&interp, &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     %let cic =%scan(&ci,     &k, %str( ));

	  %if &k=99 %then %let repeat=999;
     symbol&k
      %if %length(&font) 	  %then font=&font;
	  %if %length(&label) 	%then pointlabel=%str( (h=1 "#&label") );
	  height=&h value=&chr color=&col i=&int l=&lin w=&width r=&repeat
	  %if %length(&cic) 	%then %str(ci=&cic);
	  ;
	%if &k=99 %then %goto done;
  %end;
*options nomprint;
%done:
%mend;
