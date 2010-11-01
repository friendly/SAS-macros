 /*--------------------------------------------------------------*
  *    Name: condmat.sas                                         *
  *   Title: Scatterplot matrix of conditional independence plots*
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 13 Jan 2000 09:24:08                                *
  * Revised: 13 Jan 2000 09:24:08                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The CONDMAT macro

=Usage:

 The CONDMAT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%condmat();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        The name of the variable to be analyzed

* NAMES=      Alternative variable names

* GROUP=      Grouping variable (plot symbol)

* SYMBOLS=     [Default: SYMBOLS=%STR(TRIANGLE PLUS SQUARE CIRCLE  $ X _ Y)]

* COLORS=     BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE
                

 =*/
%macro condmat(
	data=_last_,
	var=,
	names=,                /* Alternative variable names         */
	group=,                /* grouping variable (plot symbol)    */
	symbols=%str(triangle plus square circle  $ X _ Y),
	colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE
	);


 /*-- need extra work space, modules take quite a bit of space --*/
proc iml worksize=500;

 /*--------------------*/
 /*-- define modules --*/
 /*--------------------*/

 /*-- MODULE : compute ellipses --*/
START CONTOUR(C,X,Y,NPOINTS,PVALUES);

    /*-- this routine computes contours for a scatter plot --------*/
    /*-- c returns the contours as consecutive pairs of columns ---*/
    /*-- x and y are the x and y coordinates of the points --------*/
    /*-- npoints is the number of points in a contour -------------*/
    /*-- pvalues is a column vector of contour probabilities ------*/
    /*--  the number of contours is controled by the ncol(pvalue) -*/

    xx=x||y; n=nrow(x);
    /* correct for the mean */
    mean=xx[+,]/n;
    xx=xx-mean@j(n,1,1);

    /* find principle axes of ellipses */
    xx=xx`*xx/n;
    call eigen(v,e,xx);

    /* set contour levels */
    c=-2*log(1-pvalues);
    a=sqrt(c*v[1]); b=sqrt(c*v[2]);

    /* parameterize the ellipse by angle */
    T=( (1:NPOINTS) - {1})#ATAN(1)#8/(NPOINTS-1);
    S=SIN(T); T=COS(T);
    S=S`*A; T=T`*B;

    /* form contour points */
    s =((e*(shape(s,1)//shape(t,1)))+mean`@j(1,npoints*ncol(c),1) )`;
    c=shape( s , npoints ) ;

    /* returned as ncol pairs of columns for contours */
	finish;

 /*-- MODULE : draw contour curves --*/
START GCONTOUR(T1, T2, vp);

    RUN CONTOUR(T12, T1, T2, 30, {.5});
    CALL GDRAW(view(T12[,1],vp[,1]), view(T12[,2],vp[,2]),,'libr');
*    CALL GPOINT(T1,T2,,'RED');
FINISH;


*-----Find partial residuals for y vs. x[,i] variable-----;
*start partial(x, y, i, symbol, color, vp );
start partial(y1, y2, x, symbol, color, vp);
   k = ncol(x);
   n = nrow(x);

   k1= k + 1;                  *-- number of predictors;
	unit=j(n,1);
   xx = unit || x;     *-- add column of 1s;
 
	*  run reg( y,  x, b, res, yhat, hat, rstudent, mse );
      run reg( y1, xx, by, ry, fy, hy, sry, msey );
      run reg( y2, xx, bx, rx, fx, hx, srx, msex );
      run reg( ry,(unit|| rx), b, r, f, h, sr, mse );
      uv = uv || ry ||rx;
		run gxyplot(rx,ry, symbol,color, vp);

		*-- Draw regression line from slope;
		xs = min(rx) // 0 // max(rx);
		ys = b[2,] # xs;
		run xyview(xs, ys, vp, xx, yy);
print b xs ys xx yy;
		if abs(b[2])>.05 then call gdraw(xx, yy, 3, 'libr');
      call gpoint(xx[2],yy[2],1,'black',6);        *-- mark the mean;

*		call gshow;
*   print "Partial Residuals", uv[ colname=uvname format=8.3];
finish;  /* end of partial */

start gxyplot (x,y, symbol, color, vp);
 
*	run xyview(x, y, vp, xx, yy);	
	call gpoint (view(x,vp[,1]), view(y,vp[,2]), symbol, color);
	call gcontour(x, y, vp);  
finish;

*-- Translate world coords to viewport;
*   X - a vector or 2 col matrix of world coords;
*   VP- 2-vector or 2 col matrix of device coords;
start view(x, vp);
	/*---find world---*/
	world = min(x) // max(x);
	
	/*---find  range---*/
	leng = abs(world[2,] - world[1,]);
	
	psize = vp[2,]-vp[1,]; 
	xv = vp[1,] + (x - world[1,]) # (psize/leng);
	return(xv);
	finish;
	
start xyview(x, y, vp, xx, yy);
	/*---find world---*/
	world = min(x) || min(y) // (max(x) || max(y));
	
	/*---find x and y ranges---*/
	leng = abs(world[2,] - world[1,]);
	
	psize = vp[2,]-vp[1,]; 
	xx = vp[1,1] + (x - world[1,1]) # (psize[1]/leng[1]);
	yy = vp[1,2] + (y - world[1,2]) # (psize[2]/leng[2]);
	finish;
	
*----- module to fit one regression ----------;
start reg (y, x, b, res, yhat, h, rstudent, mse);
	n = nrow(x);
	p = ncol(x);
	xpx = x` * x;
	xpy = x` * y;
	xpxi= inv(xpx);
	b   = xpxi * xpy;
	yhat= x * b;
	res = y - yhat;
	h   = vecdiag(x * xpxi * x`);
	sse = ssq(res);
	mse = sse/(n-p);
	sisq= j(n,1,sse) - (res##2) / (1-h);
	sisq= sisq / (n-p-1);
	rstudent = res / sqrt( sisq # (1-h) );
finish;
 

 /*-- MODULE : do conditional scatterplot matrix --*/
start condmat(data, group, vname)
	global(colors, symbols);
	
	if type(colors) = 'U' then colors=1:8;
	if type(symbols)= 'U' then symbols=1:8;
	
   call gopen('scatter');
   nv      = ncol(vname);
   if (nv = 1) then nv = nrow(vname);

   cellwid = int(90/nv);
   dist = 0.1 * cellwid;
   width = cellwid - 2*dist;
   xstart = int((90 -cellwid * nv)/2) + 5;
   xgrid = ((0:nv)#cellwid + xstart)`;
	call ggrid(xgrid, xgrid);
	

   xstart = xstart + dist;
	ystart = xgrid[nv] + dist;

   /*-- label variables ---*/
	hlab = nv;
   call gset("height", hlab);
   call gstrlen(len, vname);
	if max(len)> cellwid then do;
		hlab = hlab # .95 # cellwid / max(len);
		call gset("height", hlab);
		call gstrlen(len, vname);
	end;
/*
   WHERE = XGRID[1:NV] + (CELLWID-LEN)/2;
   CALL GSCRIPT(WHERE, 0, VNAME) ;
   LEN2 = LEN[NV-(0:NV-1)];
   WHERE = XGRID[1:NV] + (CELLWID-LEN2)/2;
   CALL GSCRIPT(4,WHERE, VNAME[NV - (0:NV-1)],90);
*/
   /*-- First viewport --*/
   VP = (XSTART || YSTART) // ((XSTART || YSTART) + WIDTH) ;

   /*-- Since the characters are scaled to the viewport (which
        is inversely porportional to the number of variables),
        enlarge it proportional to the number of variables --*/
   HT = 2;  CALL GSET("HEIGHT", HT);
	
	col = j(nrow(data),1, colors[1]);
	sym = j(nrow(data),1, symbols[1]);
	if nrow(group)>1 then do;
		d = design(group);
		do i=1 to ncol(d);
			l = loc(d[,i]);
			col[l] = colors[i];
			sym[l] = symbols[i];
			end;
		end;
*	print group col sym d;
		
   DO I=1 TO NV;
		others = remove(1:nv, i);
		WX = XGRID[I] + ((CELLWID-LEN)/2)[I];
		WY = xgrid[1]  + cellwid * (nv-i+.5) - hlab/2;
		CALL GSCRIPT(WX, WY, VNAME[I],,, hlab) ;
      DO J=1 TO NV;
         CALL GPORTSTK(VP);
			k = j - (j>i);
         IF (I=J) THEN do; 
			*RUN GBXWHSKR(DATA[,I], HT);
			end;
          else do;
			 rest = remove(others, loc(others=j));
			 print "Var:" I  j  rest;
			 run partial(data[,i], data[,j], data[,rest], sym, col, vp);
			 end;
         /*-- onto the next viewport --*/
         VP[,1] = VP[,1] + CELLWID;
         CALL GPORTPOP;
      end;
      vp =  (xstart // xstart + width) || (vp[,2] - cellwid);
   end;

   call gshow;
finish;

start str2vec(string);
	*-- String to character vector;
   free out;
   i=1;
   sub = scan(string,i,' ');
   do while(sub ^=' ');
      out = out || sub;
      i = i+1;
      sub = scan(string,i,' ');
   end;
	return(out);
	finish;


	/*-- load graphics --*/
	call gstart;
 /*-- Placement of text are based on the character height. The
     IML modules defined here assume percent as the unit of
     character height for device independent control. --*/
	GOPTIONS GUNIT=PCT;


   use &data;
	read all var{&var} into vars[c=vname];
	%if %length(&names)
		%then %do;
	   vname = str2vec("&names");          *-- Preserve case;
		%end;
	%else %do;
	   vname = str2vec("&var");          *-- Preserve case of var names;
		%end;

	%if &group ^= %str() %then %do;
		read all var {&group} into group [colname=lg] ;
		%end;
	%else %do;
		group = 1;
		%end;


	symbols={&symbols};
	colors = {&colors};
	run condmat(vars, group, vname);

quit;

%mend;
