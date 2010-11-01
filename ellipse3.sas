%macro ellipse3(
	data=_LAST_,           /* input data set                      */
	x=,                    /* X variable                          */
	y=,                    /* Y variable                          */
	z=,                    /* Z variable                          */
	where=,                /* WHERE clause to select observations */
	weight=,               /* numeric weight for observations     */
	group=,                /* Group variable (optional)           */
	class=,                /* syn for GROUP=                      */
	pvalue= .5,           /* Confidence coefficient (1-alpha)    */
	line=5,                /* line style for ellipse              */
	annoadd=MEAN GPLABEL,  /* additional annotations              */
	std=STDERR,            /* error bar metric: STD or STDERR     */
	points=40,             /* points on each contour              */
	all=NO,                /* include contour for total sample?   */
	colors=BLACK RED BLUE GREEN PURPLE YELLOW BROWN ORANGE,
	out=ell3d
	);
	
%let all = %upcase(&all);
%let annoadd = %upcase(&annoadd);
%if &x=%str() or &y=%str() or &z=%str() %then %do;
   %put ELLIPSE3: The X=, Y= and Z= variables must all be specified;
   %goto DONE;
   %end;

%*-- Set up where clause to select observations;
%if %length(&where)>0 %then %let where=where (&where);
 
proc iml;
start ellipse3(c, x, y, z, wt, npoints, pvalues, formean);
  /*----------------------------------------------------------------*
   |  Computes elliptical contours for a scatterplot                |
   |   C       returns the contours as consecutive pairs of columns |
   |   X,Y,z   coordinates of the points                            |
   |   NPOINTS scalar giving number of points around a contour      |
   |   PVALUES column vector of confidence coefficients             |
   |   FORMEAN 0=contours for observations, 1=contours for means    |
   *----------------------------------------------------------------*/
 
   xx = x||y||z;
   n  = nrow(x);
	if type(wt) ^='N' then wt = j(n,1);
   *-- Correct for the mean --;
   mean = (wt#xx)[+,]/sum(wt);
   xx = xx - mean @ j(n,1,1);
 
   *-- Find principal axes of ellipses --;
   xx = xx` * diag(wt) * xx / (sum(wt)-1);
   print 'Variance-Covariance Matrix', xx[f=12.3];
   call eigen(v, e, xx);
 
   *-- Set contour levels --;
   con =  3*finv(pvalues,3,int(sum(wt))-1,0);
   if formean=1 then c = c / (sum(wt)-1) ;
   * print 'Contour values',pvalues con;

	z = j(npoints, 1, 0); 
	m = mean @ j(3#npoints,1);
	r = circle(npoints);

	c = (r     || z) //
		 (r[,1] || z || r[,2]) //
		 (  z   || r         ) ;
	c = c * diag(con # v);
	c = c + m;
*   print c;

	seg = t(1:3) @ j(npoints,1);
	c = c || seg;
	finish;

start circle(npoints);
   *-- Parameterize the ellipse by angles around unit circle --;
   t = ( (1:npoints) - 1) # atan(1)#8/(npoints-1);
   s = sin(t);
   c = cos(t);
	r = t(s // c);
	return( r );
	finish;

start ellip2d(v, e, mean, c, npoints );
   a = sqrt(c*v[ 1 ] );
   b = sqrt(c*v[ 2 ] );
 
   *-- Parameterize the ellipse by angles around unit circle --;
   t = ( (1:npoints) - 1) # atan(1)#8/(npoints-1);
   s = sin(t);
   t = cos(t);
   s = s` * a;
   t = t` * b;

   *-- Form contour points --;
	s = e*(shape(s,1) // shape(t,1));
   s = t( s ) ;
*   s = s + mean @ j(1,npoints,1) ) ;
	return(s);
	finish;
 
start dogroups(x, y, z, wt, gp, pvalue);
   d  = design(gp);
   %if &all=YES %then %do;
      d = d || j(nrow(x),1,1);
   %end;
   do group = 1 to ncol(d);
      * print group;
      *-- select observations in each group;
      col = d[, group ];
      xg  = x[ loc(col), ];
      yg  = y[ loc(col), ];
      zg  = z[ loc(col), ];
		wg  = wt[loc(col), ];

      *-- Find ellipse boundary ;
		formean=0;
		run ellipse3(xyzg, xg, yg, zg, wg, &points, pvalue, formean);
*		print xyzg;
      nr = nrow(xyzg);
 
      *-- Output contour data for this group;
      cnames = { X Y Z SEG GP };
		xyzgp = xyzg  || j(nr,1,group);
		if group=1
			then create contour from xyzgp [colname=cnames];
		append from xyzgp;
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

*-- Get input data: X, Y, Z, GP;
   use &data;
   read all var {&x} into x [colname=lx] &where;
   read all var {&y} into y [colname=ly] &where;
   read all var {&z} into z [colname=lz] &where;
	xyz = x || y || z;
	run nomiss(xyz, ' ', kept);
%if %length(&weight) > 0 %then %do;
   read all var {&weight} into wt [colname=lg] &where;
	if ncol(keep) ^= nrow(xyz) then wt = wt[kept];
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
	x = xyz[,1];
	y = xyz[,2];
	z = xyz[,3];
	if ncol(keep) ^= nrow(xyz) then gp = gp[kept];
   close &data;
 
/*
	formean=0;
	run ellipse3(c, x, y, z, wt, &points, &pvalue, formean);
	lc = lx || ly || lz;
	print c[colname=lc f=best6.2];
*/

*-- Find contours for each group;
   run dogroups(x, y, z, wt, gp, { &pvalue} );

quit;

*proc print;
 /*-----------------------------------*
  |  Plot the contours using ANNOTATE |
  *-----------------------------------*/
data &out;
   set contour end=eof;
   by seg gp notsorted;
	retain g 0;
	drop g;
	if first.gp then g+1;
   length function color $8;
   xsys='2'; ysys='2';
   if first.seg then function='POLY';
               else function='POLYCONT';
   color=scan("&colors",gp);
   line = &line;
   if eof then call symput('NGROUP',put(g,best.));
run;
*proc print;

 /*----------------------------*
  |  Crosses at Mean +- StdErr |
  *----------------------------*/
*macro dumdum;
%if &annoadd ^= NONE %then %do;
proc summary data=&data nway;
   class &group;
   var &x &y &z;
   output out=sumry mean=mx my mz &std=sx sy sz;
run;

data bars;
   set sumry end=eof;
   %if &group ^= %str() %then %str(by &group;);
   length function color $8;
   retain g 1;
   drop _freq_ _type_ mx my mz sx sy sz g;
   xsys='2'; ysys='2'; zsys='2';
 
   color=scan("&colors",g);
	%if %index(&annoadd,MEAN) > 0 %then %do;
   line=3;
   x = mx-sx; y=my;    z=mz;   function='MOVE'; output;
   x = mx+sx;                  function='DRAW'; output;
   x = mx   ; y=my-sy; z=mz;   function='MOVE'; output;
              y=my+sy;         function='DRAW'; output;
	x = mx   ; y=my;   z=mz-sz; function='MOVE'; output;
	           y=my;   z=mz+sz; function='DRAW'; output;
	%end;
 
 *-- Write group label (convert numeric &group to character);
   %if &group ^= %str() and %index(&annoadd,GPLABEL) > 0 %then %do;
      length text $16;
      text = left(&group);
      position='3';
      size = 1.4;
      x = mx+.2*sx   ; y=my+.2*sy;  z=mz+.2*sz
      function='LABEL'; output;
   %end;
   %if &group ^= %str() %then %do;
      if last.&group then g+1;
   %end;
run;
proc print data=bars;
 
data &out;
   set &out bars;
%end; /* annoadd ^= NONE */
*mend;

proc g3d data=&out;
*	plot y * x = z/
		xticknum=2 yticknum=2 zticknum=2
		nolabel
		anno=&out;
	scatter y * x = z/
		xticknum=2 yticknum=2 zticknum=2
		nolabel size=0.01 noneedle
		anno=&out;

%done:
%mend;