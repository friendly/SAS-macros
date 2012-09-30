 /*--------------------------------------------------------------*
  *    Name: corrgram.sas                                        *
  *   Title: Draw a correlogram                                  *
        Doc: http://www.datavis.ca/sasmac/corrgram.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 24 Nov 2000 07:10:52                                *
  * Revised: 21 Sep 2012 17:32:36                                *
  * Version: 1.2-3                                               *
  *  1.1 Added FILL=L type for parametric ellipse                *
  *  1.2 Added FONT=  (suggestions from Ray.Lindsay@abare.gov.au)*
  *      Fixed problem with TITLE=                               *
  *      Added output of global macro variable VORDER            *
  *      Fixed buglet with var=a1-a8 (thx: Ian Wakeling)         *
  *   Try to handle longer variable names (thx: Bill Raynor)     *
  *   Updated for V 9.3 where CORR is builtin (thx: Bill Raynor) *
  *   Fixed minor warning with vname_                            * 
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The CORRGRAM macro produces a schematic display of a correlation matrix,
 called a correlogram.  In this display, variables are permuted so that
 ``similar'' variables are positioned adjacently, and cells of a
 matrix are shaded or filled to show the correlation value.

=Usage:

 The CORRGRAM macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%corrgram(data=baseball, var=_numeric_, fill=C E C);

 The input dataset may contain raw data (no missing values, yet),
 or a correlation matrix (type='CORR') produced by PROC CORR or
 some other SAS procedure.
  
==Parameters:

* DATA=       Name of input data set.  [Default: DATA=_LAST_]

* VAR=        List of variables whose correlations are to be displayed.
              You may use an explicit, blank separated list of variables
              (the case of variable names will be preserved), or any of
              the SAS abbreviations, such as X1-X15, height--strength,
              or _NUMERIC_ (variable labels will apprear in upper case).

* TYPE=       Type of input data: DATA or CORR.  If not specified, the
              program tries to guess, by examining the type attribute
              of the input data set.

* FILL=       How to fill the cells in the lower triangle, diagonal 
              and the upper triangle.  Should be three characters, chosen from
              S= shade (with color proportional to the correlation), E= empty, 
              N= numeric value, C= circle, B= bar, or L=eLlipse. 
              [Default: FILL=S E S]

* ORDERBY=    PCs used to order the variables, either one or two integers
              (eigenvector numbers) or one of the strings ALPHA, DATA, or
			  empty (ORDERBY=).  
			  If two integers, V1, V2 are specified, the arctangent of ratios, 
              V2/V1 determine the variable order.  If ORDERBY=ALPHA, the
              variables are arranged in alphabetic order.  If 
			  ORDERBY=DATA or is empty, variables are arranged in their
			  order in the dataset.
              [Default: ORDERBY=1 2]

* OPTIONS=    One or more options: RINV displays R^{-1} rather than R.
              TRANS transposes the data set, and calculates correlations
              among observations, rather than variables.

* PARTIAL=    List of one or more variables to be partialed out.

* COLORS=     Colors for positive and negative values [Default: COLORS=BLUE RED]

* HLABEL=     Height of variable labels

* ALABEL=     Angles for variable labels on the vertical and horizontal
              axes.

* CMIN=       Minimum abssolute value for correlation to be shaded

* TITLE=      Optional plot title, or NONE, for no title.  Under V7+, the macro
              uses the TITLE1 unless TITLE=NONE has been specified.
                
* FONT=       Font for text labels [Default: FONT=HWPSL009 for PS,
              otherwise, SWISS]

==Side-effects

 For use with other programs, the macro creates a global macro variable,
 VORDER, a blank-separated string of the names of the variables, as
 reordered by the macro.
 
 =*/

%global vorder;
%macro corrgram(
    data=_last_,      /* name of input data set                          */
    var=,             /* list of variables                               */
    id=,              /* observation id variable                         */
    type=,            /* Type of input data: DATA or CORR                */
    fill=S E S,       /* how to fill the cells in lower tri, diagonal,   */
                      /* upper triangle; S=shade, E=empty, N = value     */
    orderby=1 2,      /* How used to order the variables?                */
    options=,
    display = R R,    /* (not yet used) */
    partial=,         /* list of variables to partial out                */
    colors=blue red,  /* colors for positive and negative values         */
    cgrid=gray,       /* color for grid lines                            */
    hlabel=,          /* height of variable labels                       */
    alabel=0 90,      /* angles for variable labels (H, V)               */
    cmin=0,           /* minimum abs. value for correlation to be shaded */
    diags=3,          /* number of diagonal lines in shaded cells        */
    title=,           /* plot title                                      */
	font=,            /* Font for text labels                            */
    verbose=
    );

%let abort=0;
%if %length(&var)=0 
   %then %do;
      %put ERROR: The VAR= parameter must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%let display = %upcase(&display);
%if %length(%qscan(&display,2))=0 %then
	%let display = &display &display;

%if %sysevalf(&sysver >=7 and %qupcase(&title)=AUTO) %then %do;
    data _null_;
        set sashelp.vtitle(obs=1);
        call symput('title', trim(text));
    %end;

%let type=%upcase(&type);
%let options=%upcase(&options);
%if %upcase(&data)=_LAST_ %then %let data=&syslast;

%* --- Transform variable lists (X1-X10) into expanded form for IML ---;
%if %index(&var,-) >0 or 
 	"%upcase(&var)"="_NUM_" or 
 	"%upcase(&var)"="_NUMERIC_"  %then
 %do;
 %let ovar = &var;
 data _null_;
    set &data (obs=1);
       %*-- convert shorthand variable list to long form;
     length 
		%if &sysver>=7	%then _vname_ $ 32  _vlist_ $ 1000;
                        %else _vname_ $ 8  _vlist_ $ 200;
						;
     array _xx_ &var;
     _vname_ = ' ';
	  i=0;
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
		  i+1;
     end;
     call symput( 'VAR', trim(left(_vlist_)) );
     call symput( 'NV', trim(put(i,2.0)) );
	  put "NOTE:  Variable list (&ovar) expanded to VAR=" _vlist_;
  run;
  %if &nv=0 %then %do;
    %put ERROR: No variables were found in the VAR=&ovar list;
    %goto DONE;
  		%end;
  %end;

%*-- Determine type of input dataset, if not specified;
%if %length(&type)=0 %then %do;
	proc contents data=&data noprint out=_cont_;
	data _null_;
		set _cont_(obs=1);
		if typemem=' ' then typemem='DATA';
		call symput('type', typemem);
	run;
	%put CORRGRAM: TYPE= not specified. Assuming TYPE=&TYPE;
	%end;

%if &type=CORR %then %do;
	data _corr_;
		set &data;
		where (_type_)='CORR';
	%let data=_corr_;
	*proc print;
	%end;
	
proc iml worksize=8000;
goptions gunit=pct;
start corrgram(c, vnames)
        global(colors, filltype, htext, title, verbose, font);

   if type(colors) ^= 'C'   then colors= {&colors};
   if type(filltype) ^= 'C' then filltype = {RGB RGB};
   if type(verbose) ^= 'N'  then verbose=0;

	*-- Set default font based on device driver name;
   if type(font ) ^= 'C' then do;
		call execute('device  = upcase("&sysdevic");');
		if index(device,'PS') > 0 then
         font= 'hwpsl009';         /* Helvetica for PS drivers */
		else /* if device='WIN' then */
			font = 'SWISS';
		end;

	call gstart;
	if title ^= ' ' then
	call gopen('corrgrm',0,title);
	else call gopen('corrgrm',0,'blank');

	upper = choose(title=' ', 100, 105);
	call gwindow ({-10 -10} || upper || upper);
	if nrow(vnames) = 1 then vnames = t(vnames);
	nv = nrow(vnames);

*   cellwid = int(100/nv);
   cellwid = 100/nv;
   if type(htext) ^= 'N'    then htext = cellwid/3;

	*-- calculate grid locations;
   xstart = int((100 -cellwid * nv)/2);
   xgrid = t((0:nv)#cellwid + xstart);
   call ggrid(xgrid, xgrid, 1, "&cgrid");

	bottom = xgrid[1:nv] || xgrid[nv:1];
	center = bottom + cellwid/2;

	corr = char(int(100 # c),3,0);
	*-- calculate scaled colors;
	crange = &cmin // 1;
	scolor = scolor(c, crange, {0, 1});
	if verbose then print scolor;

   box = { 0 0, 1 0, 1 1, 0 1} # cellwid;

	if title^=' ' then do;
		call gset("height", 3);
		call gstrlen(len,title);
		call gscript(50-len/2, 101, title, 0, 0, , font);
		end;

	fill = {&fill};
	fill = substr(fill,1,1);
	diags = &diags;
   call gset("height", htext);
	do i=1 to nv;
		do j=1 to nv;
			act = fill[2 - sign(i-j)];
			if act = 'E' then ;
			else if act = 'N' then do;              /* Number */
				run dovalue(corr,i,j,c,center,colors);
				end;
			else if act = 'S' then do;              /* Shade */
				*-- shift box to bottom-left corner;
				poly = box + repeat((bottom[j,1] || bottom[i,2]),4, 1);
*				clr = scolor[i,j];
				run doshade(c[i,j], poly, scolor[i,j], diags);
				end;
			else if act = 'C' then do;
				call gpie(center[j,1], center[i,2], cellwid/2, 0, 360, "&cgrid",, 'empty');
				if c[i,j] < 0 then do;
					a1 = 90;
					a2 = mod(90 + abs(c[i,j] # 360), 360);
					end;
				else do;
					a1 = mod(90 - abs(c[i,j] # 360), 360);
					a2 = 90;
					end;
				call gpie(center[j,1], center[i,2], cellwid/2, a1, a2, scolor[i,j],, 'solid');
				call gpie(center[j,1], center[i,2], cellwid/2, a1, a2, "&cgrid",, 'empty');
				end;
			else if act = 'B' then do;
*           run dobar(c[i,j], scolor[i,j]);
*				start dobar(c, color);    **** too many vars ;
				poly = { 0 0, 1 0, 1 1, 0 1};
				if abs(c[i,j])<0.001 then poly[,2] = j(4,1, 0.5);
				else if c[i,j] < 0 then do;    *-- bar from bottom up;
					poly[{3 4},2] = j(2,1,abs(c[i,j]));
					end;
				else do;                  *-- bar from top down;
					poly[{1 2},2] = j(2,1,1-c[i,j]);
					end;
				poly = poly # cellwid;
				poly = poly + repeat((bottom[j,1] || bottom[i,2]),4, 1);
				call gpoly(poly[,1], poly[,2], 1, "&cgrid", 'solid', scolor[i,j]);
				end;
				*	finish;
			else if act = 'L' then do;
*				run ellipse;
				if abs(c[i,j])>.99 then do;
					d = 1+(2*(c[j]<0));
					poly = { 0 0, 1 1, 1 0, 0 1}[d:d+1,];
					poly = poly # cellwid;
					poly = poly + repeat((bottom[j,1] || bottom[i,2]),2, 1);
					call gdraw(poly[,1], poly[,2], 1, scolor[i,j]);
					end;
				else do;
					d = arcos(c[i,j]);
					a = t(do(0, 2*3.14159, (2*3.14159)/24));
					poly = cos(a+d/2) || cos(a-d/2);
					poly = poly # cellwid/2;
					poly = poly + repeat((center[j,1] || center[i,2]),24+1, 1);
					call gpoly(poly[,1], poly[,2], 1, "&cgrid", 'solid', scolor[i,j]);
					end;
				end;
			end;
		end;

   /*-- label the variables ---*/
	%if %length(&hlabel)=0 %then %do;
		htry = htext # {1.1 1 .9 .8};
		ok=0;
		do i=1 to ncol(htry) until(ok);
			ht = htry[i];
			call gstrlen(len, vnames, ht);
			if max(len) < cellwid then ok=1;
			end;	
		* print htext cellwid '-->' ht, len;
		%end;
	%else %do;
		ht = &hlabel;
		call gstrlen(len, vnames, ht);
		%end;

	angle={ &alabel };
	if all(angle={0 90}) & max(len) > 1.5 # cellwid
		then angle= angle + 25 # {-1 -1};
	htsave = ht;
	call gset("height", ht);

	*-- horizontal labels;
	ang = angle[1];
	clabel='black';
	if ang ^=. then do;
		if (ang = 0) then do;
			wy = -ht;
			if max(len) > cellwid then wy = wy || (wy-ht-0.5);
			wy = shape(wy, 1,  nv);
			wx = xgrid[1:nv] + (cellwid-len)/2;
			end;
		else if (ang = 90) then do;
			ht = htsave # (10/max(len));
			wx = xgrid[1:nv] + (cellwid+ht)/2;
			wy = -10;
			end;
		else do;
			wx = xgrid[1:nv] + (cellwid-len)/2;
			wy = -(1+htext/2);
			wy = -(0.5+ht);
			end;
		call gscript(wx, wy, vnames, angle[1], , ht, font, clabel);
		end;	

	*-- vertical labels;
   len = len[nv-(0:nv-1)];
	ang = angle[2];
	if (ang = 90) then do;
		wx = -0.5;
		if max(len) > cellwid then wx = wx || (wx-ht-0.5);
		wx = shape(wx, 1,  nv);
		wy = xgrid[1:nv] + (cellwid-len)/2;
		end;
	else if (ang = 0) then do;
		ht = htsave # (10/max(len));
		wx = -10;
		wy = xgrid[1:nv] + (cellwid-ht)/2;
		print ang ht;
		end;
	else do;
		wx = -6;
		radian = 180 / 3.14159;
		wx = -(0.5 + (max(len) # cos(ang/radian))); *print wx;
		wy = xgrid[1:nv] + (cellwid-len)/2;
		end;
   call gscript(wx, wy, vnames[nv - (0:nv-1)], angle[2], , ht, font, clabel);

   call gshow;
	finish;

*-- draw the matrix value in the middle of the cell;
start dovalue(corr,i,j,c,center,colors) global(htext);
	call gstrlen(len, corr[i,j]);
	if i=j
		then clr='black';
		else clr = colors[1+(c[i,j]<0)];
	call gscript(center[j,1]-len/2, center[i,2]-htext/2,
				corr[i,j],,,,,clr);
	finish;

*-- draw shded box in color 'color' with 'diags' white lines ;
start doshade(c, poly, color, diags);
	call gpoly(poly[,1], poly[,2], 1, "&cgrid", 'solid', color);
	if diags > 0 then do;
		clr = 'white';
		if c < 0 then do;
			from = poly[2,];
			to  =  poly[4,];
			if diags > 1 then do;
				from = from // (poly[{3 4},])[:,] // (poly[{1 4},])[:,];
				to  =  to   // (poly[{3 2},])[:,] // (poly[{1 2},])[:,];
				end;
			call gdrawl(from, to, 1, clr);
			end;
		else do;
			from = poly[1,];
			to  =  poly[3,];
			if diags > 2 then do;
				from = from // (poly[{1 2},])[:,] // (poly[{1 4},])[:,];
				to  =  to   // (poly[{3 2},])[:,] // (poly[{3 4},])[:,];
				end;
			call gdrawl(from, to, 1, clr);
			end;
		end;
	finish;

/*
start ellipse;
	d = arcos(c[i,j]);
	a = do(0, 2*3.14159, (2*3.14159)/30);
	poly = cos(a+d/2) || cos(a-d/2);
	poly = poly # cellwid;
	poly = poly + repeat((bottom[j,1] || bottom[i,2]),4, 1);
	call gpoly(poly[,1], poly[,2], 1, "&cgrid", 'solid', scolor[i,j]);
	finish;

start dobar;
	poly = { 0 0, 1 0, 1 1, 0 1};
	if c[i,j] < 0 then do;    *-- bar from bottom up;
		poly[{3 4},2] = j(2,1,abs(c[i,j]));
		end;
	else do;                  *-- bar from top down;
		poly[{1 2},2] = j(2,1,1-c[i,j]);
		end;
	poly = poly # cellwid;
	poly = poly + repeat((bottom[j,1] || bottom[i,2]),4, 1);
	call gpoly(poly[,1], poly[,2], 1, "&cgrid", 'solid', scolor[i,j]);
	end;
	finish;
*/

%macro corr;
*-- corr()  defined in SAS 9.3;
%if %sysevalf(&sysver  < 9.3) %then %do; 
start corr (x);
  d = x - repeat(x[:,], nrow(x), 1);
  xpx=t(d)*d;                      * crossproduct;
  v = vecdiag(xpx);                * diagonal values;
  v = 1/sqrt(choose(v=0,.,v));     * account for constants;
  v = choose(v=.,0,v);
  corr= diag(v) * xpx * diag(v);   * correlation matrix;
  return (corr);
finish;
%end;
%mend;
%corr;

start rinv(r);
	*-- Matrix of all partial correlations is - R^{-1}, scaled to corr matrix;
    ri = inv(r);
    s = vecdiag(ri);
    s = diag(sqrt(1/s));
    ri = s * ri * s;
	 n = nrow(r);
	 ri = ri # (2#I(n) - J(n,n));
    return (ri);
    finish;

start partial(r, list);
	x = list;
	y = remove(1:nrow(r), x);
	ri = r[y,y] - r[y,x] * inv(r[x,x]) * r[x,y];
	s = vecdiag(ri);
	s = diag(sqrt(1/s));
	ri = s * ri * s;
	ri = block( r[x,x], ri);
	order = rank(x||y);
	*print x y, order;
	tmp = ri;
	ri[order,order] = tmp;
	return (ri);
	finish;

start center(r);
	*-- Matrix of scalar-products derived from R;
	ri = repeat(r[,:], 1, ncol(r));
	rj = repeat(r[:,], nrow(r), 1);
	d = r - ri - rj + r[:,:];	
	return (d);
	finish;

/*  Translate a character matrix MAT to a numeric matrix of
    the indices of each element in a vector of NAMES.
	 Returns a numeric matrix of the same shape as MAT  */
start name2num(mat, names);
	new = j(nrow(mat), ncol(mat), 0);
	do i=1 to nrow(mat);
		do j=1 to ncol(mat);
			l = loc(trim(upcase(mat[i,j])) = upcase(names));
			if type(l)^='U' then new[i,j] = l;
			end;
		end;
	return(new);
	finish;

start scolor(value, vs, cs)
    global (colors, filltype, verbose);
    /*-----------------------------------------------------------------
    | args:
    |   value - matrix of values, for which scaled colors are computed.
    |           Positive values use colors[1], negative use colors[2] for hue
    |   vs    - an ordered vector of value ranges
    |   cs    - an ordered vector of color intensities in [0,1]
    | returns:
    |    a matrix of color names corresponding to values
    |-----------------------------------------------------------------*/

        *-- scale values to [0,1], using vs and cs as piecewise-linear;
    dar = j( nrow(value), ncol(value), 0);
    do i=1 to nrow(value);
        do j= 1 to ncol(value);
        v = abs(value[i,j]);
        p = loc(v <= vs);
        if ncol(p) > 0 then do;
            if p[1] > 1 then do;
                p = (p[1]-1) || p[1];;
                m = (cs[p[2]]-cs[p[1]]) / (vs[p[2]]-vs[p[1]]);
                dar[i,j] =  cs[p[1]] + (v -vs[ p[1]]) # m;
                end;
            else dar[i,j] = cs[1];
            end;
        else do;
            dar[i,j] =  cs[nrow(cs)];
            end;
        end;
    end;

    *-- map scaled intensity from [0, 1] --> [255, 0];
    dark = int(255# (1-dar));
    result = j( nrow(value)#ncol(value), 1, '        ');
    do k=1 to 2;
        ftype = filltype[k];
        if k=1
            then p = loc( value >= 0);
            else p = loc( value < 0 );

        if ncol(p) > 0 then do;
            d = dark[p];
            if ftype='GRAY' then do;
                res = 'GRAY'+hex(d) ;
                end;

            else if ftype='RGB' then do;
                hue = {RED GREEN BLUE CYAN MAGENTA YELLOW GRAY};
                *-- Hue matrix: cols are R, G, B.  1=>255, 0=> scaled value;
                hmat = I(3) // (1 - I(3)) // J(1,3);
                hrow = hmat[loc(colors[k]=hue),];
                res = 'CX';
                do i=1 to 3;
                    if hrow[i] = 1
                        then res = res + 'FF';
                        else res = res + hex(d);
                    end;
                end;

            else do;  /* ftype='HLS' */
                hue = {RED GREEN BLUE MAGENTA CYAN YELLOW,
                        '070' '100' '001' '03C' '12C' '0B4'  };
                col = loc(hue[1,]=colors[k]);
                if ncol(col)=0 then col=4;
                hval= hue[2,col];
*               dark = int(255 - 255*dar*.75);
                res = compress('H'+hval+hex(d)+'FF');
                end;
            result[t(p),] = res;
            end; /* if ncol(p)>0 */
    end;
    result = shape(result, nrow(value), ncol(value));

    * print value vs cs ftype '-->' dar dark result;
    return( result );
    finish;

start hex(num);
	*-- Convert 0<num<256 to hex;
	chars = cshape('0123456789ABCDEF',16,1,1);
	num = int((num <> 0) >< 255);
	h = rowcat(chars[1+floor(num/16),] || chars[1+mod(num,16),]);
	return(h);
	finish;

*-- Reorder the rows and columns of a square symmetric matrix by
    one eigen vector, or the angles formed by two eigen vectors;
start pcorder(c, vnames, by);
	vec = eigvec(c);
	angle=0;
	*print vec[r=vnames];
	sign = sign(by);
	by = abs(by);
	if ncol(by) = 1
		then order = rank(sign # vec[,by]);
		else do;
			pi = 3.1415926;
			angle = choose(vec[,by[1]]=0, pi, atan(vec[,by[2]]/vec[,by[1]]));
			angle = choose(vec[,by[1]]>0, angle, angle+pi);
			angle = (180 / pi) * angle;
			order = rank(sign[1] # angle);
			tmp = angle;
			angle[order] = tmp;
			end;

	tmp = c;
	c[order,order] = tmp;
	tmp = vec;
	vec[order,] = tmp;
	vec = vec[,1:2];
	tmp = vnames;
	vnames[order] = tmp;
	print order vec[r=vnames c={v1 v2} f=6.3] angle[f=7.3];
	finish;

*-- Arrange a square matrix by alpha-order of variable names
    (unique() subterfuge suggested by Rick Wicklin);
start aorder(c, vnames);
*	order = rank(vnames);   /* rank() doesnt work for character vars */
	tmp = unique( upcase(vnames) );
	order = j(1,ncol(vnames));
	do i = 1 to ncol(vnames);
		order[i] = loc( upcase(vnames) = tmp[i] );
		end;

	c = c[order,order];	
	vnames = vnames[order];
	finish;


start testdata(data, vnames, nv, nobs);
    data = normal(j(nobs,nv,0));
    call execute("vnames = 'x1':'x"+left(char(nv,3))+"';");
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

start symput(name, val);
   *-- Create a macro variable from a char/numeric scalar or column vector;
   if type(val) ='N'
      then value = trim(left(char(val)));
      else value = val;
	if nrow(value)>1 then value = compbl(rowcat(t(value)+' '));
   call execute('%let ', name, '=', value, ';');
   finish;


%*-- Main routine;

%if %length(&verbose) %then %do;
	verbose = {&verbose};
	%end;

%if &data=@TEST@ %then %do;
    run testdata(data, vnames, &var, 50);
    %end;
%else %do;
    use &data;
    read all var {&var} into data;
   vnames = str2vec("&var");          *-- Preserve case of var names;
    %end;

%if %index(&options,TRANS) %then %do;
	%if %length(&id) %then %do;
		read all var {&id} into id;
		%end;
	%else %do;
		id = char(1:nrow(data),3,0);
		%end;
	vnames = id;
	data = t(data);
	%end;

%if &type=DATA %then %do;
    data = corr(data);
    %end;
%else %do;
*	print data[r=vnames c=vnames];
	%end;

/*
%if %index(&options,RINV) %then %do;
    data = rinv(data);
    %end;
*/
%if %length(&partial) %then %do;
	list = {&partial};
	if type(list) = 'C' then
		list = name2num(list, vnames);
	data = partial(data,list);
	*print 'Partial correlations' list, data[r=vnames c=vnames f=7.2];
	%end;

%if %index(&options,CENTER) %then %do;
    data = center(data);
    %end;

%if %length(&orderby) %then %do;
	%if %upcase(&orderby)=ALPHA %then %do;
		run aorder(data, vnames);
		%end;
	%if %upcase(&orderby)=DATA | %upcase(&orderby)=NONE %then %do;
		%end;
	%else %do;
		orderby = {&orderby};
		%if %index(&options,RINV) %then %do;
			nv = nrow(data);
	*		orderby = nv - orderby +1;
			%end;
		run pcorder(data, vnames, orderby);
		%end;
	%end;

%if %index(&options,RINV) %then %do;
    data = rinv(data);
    %end;

	title = "&title";
	%if %length(&font)   %then %str(font={&font};) ;

run corrgram(data, vnames);
*-- Return variable order into a macro variable;
*vorder = compbl(rowcat( t(vnames) + ' '));
run symput('vorder', vnames);

quit; run;

%done:
%if &abort %then %put ERROR: The CORRGRAM macro ended abnormally.;
%mend;
