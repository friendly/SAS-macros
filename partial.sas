 /*-------------------------------------------------------------------*
  *    Name: partial.sas                                              *
  *   Title: Macro for partial regression residual plots              *
        Doc: http://www.datavis.ca/sasmac/partial.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  23 Jan 1989 16:11:15                                    *
  * Revised:  30 Nov 2007 13:57:04                                    *
  * Version:  1.8-1                                                   *
  *   Fixed awful plot scaling                                        *
  *   More efficient computation of partial residuals (Velleman and   *
  *   Welsch, 1981)                                                   *
  *   Fixed selection of plots to produce. Added conf intervals       *
  *   Added partial residual plots (C+R, added variable)              *
  *   Added sanity checks for required arguments                      *
  * 1.8 Remove missing data (including .A-.Z)                         *
  *   Fixed buglet with upcase names. Fixed APPEND FROM               *
  *   Added X= and Y= as synonyms for XVAR= and YVAR=                 *
  *   PLOTS=ALL gives all plots including intercept                   *
  *                                                                   *
  *      From ``The SAS System for Statistical Graphics''             *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/
%macro partial(
    data = _LAST_,  /* name of input data set               */
    yvar=,          /* name of dependent variable           */
    xvar=,          /* list of independent variables        */
	x=,             /* synonym for XVAR=                    */
	y=,             /* synonym for YVAR=                    */
    id=,            /* ID variable (char or numeric)        */
    label=INFL,     /* label ALL, NONE, or INFLuential obs  */
    out=,           /* output data set: partial residuals   */
    htext=1.5,      /* height of text in plots              */
	hsym=1.5,       /* height of points in plots            */
    font=,          /* font used in plots                   */
    symbol=dot,     /* point plotting symbol                */ 
    plots=&xvar,    /* which partial plots to produce       */
    type=PARTREG,   /* PARTREG or PARTRES                   */
    conf=,
    print=coeff infl,  /* what to print                     */
    gout=gseg,      /* name of graphic catalog              */
    name=PARTIAL);  /* name of graphic catalog entries      */
 

%let abort=0;
%if %length(&xvar)=0 %then %let xvar=&x;
%if %length(&yvar)=0 %then %let yvar=&y;

%let label = %upcase(&label);
%let plots = %upcase(&plots);
%let type = %upcase(&type);

%if %length(&yvar)=0 or %length(xvar)=0 %then %do;
	%put ERROR: The YVAR= (or Y=) and XVAR= (or X=) parameters must be specified;
	%let abort=1;
	%goto DONE;
	%end;

%if %length(&font)=0 %then %do;
	%if %index(%upcase(&sysdevic),PS)>0
		%then %let font=hwpsl009;
		%else %let font=swiss;
	%end;

*options nonotes;
*-- Parse variables list if it contains special lists;
%if %index(&xvar,-) > 0 or "&xvar"="_NUMERIC_" %then %do;
 data _null_;
 set &data (obs=1);
        %*  convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &xvar;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
     call symput( 'xvar', trim(_vlist_) );
     put 'NOTE: XVAR= list translated to: XVAR=' _vlist_;
 RUN;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%end;

%let print=%upcase(&print);

proc iml;
 
 /*-------------------------------------------------------------*/
 /*-- Sets the range of data allowing for p1% of space below, --*/
 /*-- and p2% of space above. ----------------------------------*/
 /*-------------------------------------------------------------*/
start rngset(w, s, p1, p2);
   call gscale(s,s[1:2], s[3]);
   w[1] = s[2] - s[1];         /*-- rescaled range ------------*/
   s[3] = w[1]/s[3];           /*-- # of interval -------------*/
   w[2] = w[1]/(1-p1-p2);      /*-- expand range --------------*/
   w[1] = s[1] - p1*w[2];      /*-- move lower bound ----------*/
   w[2] = s[2] + p2*w[2];      /*-- move upper edge -----------*/
finish;
 
 
 /*-------------------------------------------------------------*/
 /*-- Uses IML graphics subsystem to get plot XVAR by YVAR -----*/
 /*-------------------------------------------------------------*/
start gxyplot(x,y, label, symbol,symcol, title)
      global(xticks, yticks, caxis, ctext, ftext, htext, xscale, yscale);

   if type(xticks)='U' then xticks=5;
	if type(yticks)='U' then yticks=5;
	if type(caxis)='U' then caxis='BLACK';
	if type(ctext)='U' then ctext='BLACK';
	if type(ftext)='U' then ftext="&font";
	if type(htext)='U' then htext=&htext;

   /*-- set window --------------------------------------------*/
   w = {0 0, 100 100};  origin = {0,0};
   xscale = x[><] // x[<>] // xticks;
   run rngset( origin, xscale, .12, .05);
   w[,1] = origin;
   yscale = y[><] // y[<>] // yticks;
   run rngset( origin, yscale, .12, .05);
   w[,2] = origin;   origin = xscale[1] || yscale[1];
   call gwindow(w);

	if all(mod(xscale,1)=0) then xfmt='6.0';
								   else xfmt='7.1';
	if all(mod(yscale,1)=0) then yfmt='6.0';
								   else yfmt='7.1';
 
   /*-- draw axes and axis labels -----------------------------*/
   call gxaxis(origin,xscale[2]-xscale[1],xscale[3],,,xfmt,,,
               caxis,'N');
   call gyaxis(origin,yscale[2]-yscale[1],yscale[3],,,yfmt,,,
               caxis,'N');

	xl =xscale[1] || xscale[2] || xscale[2];
	yl =yscale[2] || yscale[2] || yscale[1];
	call gdraw(xl, yl, , caxis);
	
	call gstrlen(len, label[1]);
	cx = (xscale[2]+xscale[1]-len)/2;
   call gscript(cx, 2, label[1],0,0,htext,,,w[,1] || {0,100});

   call gstrlen(len, label[2]);
*  call gscript(origin[1]-len/2, 88, label[2],,,,,,w[,1] || {0,100});
*   call gscript(5, 85, label[2],270,90,,,,{0,100} || {0,100});
   call gscript(5, 50, label[2],90,0,htext,,,{0,100} || {0,100});

   /*-- x by y scatter plot -----------------------------------*/
   call gpoint(x, y, symbol, symcol, &hsym);
 
   /*-- do title ----------------------------------------------*/
   if (substr(title,1,1) ^= ' ') then do;
      call gstrlen(len, title, htext, ftext);
		cx = (xscale[2]+xscale[1]-len)/2;
      call gscript(cx,(w[2,2]+yscale[2])/2,
                  title,0,0,htext,ftext,ctext);
      end;
finish;
 
 
*-----Find partial residuals for each variable-----;
start partial(x, y, names, obs, uv, uvname ) global(xscale, yscale);
   k = ncol(x);
   n = nrow(x);
   yname = names[,k+1];
   k1= k + 1;                  *-- number of predictors;
   x = j( n , 1 , 1) || x;     *-- add column of 1s;
   name1 = { 'INTCEPT'};
   names = name1 || names[,1:k];
 
   run reg( y,  x, b, res, yhat, hat, rstudent, mse, cmat );
	%if %index(&print,COEF) %then %do;
		print "Full regression";
		print "Regression weights" , b[ rowname=names ];
		%end;
   lev = hat > 2*k1/n;
   flag = (lev>0) | (abs(rstudent) > 2);
	* print obs hat[f=8.4] lev flag;

   if any( flag ) then do;
      l = loc(flag)`;
		ol = obs[l];
      xl=  x[l,2:k1] || hat[l] || rstudent[l];
		cl = names[2:k1] // {'Leverage', 'RStudent'};
	%if %index(&print,COEF) %then %do;
      print "High leverage or large residual points",
				 XL [ r=ol c=cl ];
		%end;
   end;
 
   do i = 1 to k1;
      name = names[,i];
      reset noname;
*		others = remove(1:k1, i);
*      run reg( y,    x[, others], by, ry, fy, hy, sry, msey, cmat );
*      run reg( x[,i],x[, others], bx, rx, fx, hx, srx, msex, cmat );

		%if &type=PARTREG
			%then %do; rx = cmat[,i] / ssq( cmat[,i] ); pre='V'; %end;
			%else %do; rx = x[,i];  pre=''; %end;
		
		ry = (rx * b[i]) + res;
      uv = uv || ry ||rx;
		hi = rx ##2 / ssq( rx );        /* partial leverage */
      uvname = uvname || concat({'U'},name)
                      || concat(pre,name);
 
*      if i>1 then do;	*-- Skip plot for intercept;
		if "&plots"="ALL" | index("&plots", upcase(trim(name))) > 0 then do;
		%if &type=PARTREG %then %do;
			pre = 'Partial ';
			des = "Partial regression plot for &data";
			%end;
		%else %do;
			pre = '';
			des = "Partial residual plot for &data";
			%end;
       /**--------------------------------**
        | Start IML graphics               |
        **--------------------------------**/
       %if %sysevalf(&sysver  < 6) %then %do;
          %let lib=%scan(&gout,1,'.');
          %let cat=%scan(&gout,2,'.');
          %if &cat=%str() %then %do;
              %let cat=&lib;
              %let lib=work;
              %end;
          call gstart gout={&lib &cat}
                name="&name" + char(i,1,0);
                descript=des;
       %end;
       %else %do;     /* Version 6+ */
          call gstart("&gout");
          call gopen("&name" + char(i,1,0), 0,des);
       %end;

         labels = concat( pre, name )  ||
                  concat( {"Partial "}, yname ) ;
			call gset('height', &htext);
			title=' ';
			symbol="&symbol";
			run gxyplot(rx,ry, labels, symbol,'black', title);
 
         *-- Draw regression line from slope;
         xs = xscale[{1 2}];
         ys = b[i] * xs;
         call gdraw(xs, ys, 3, 'BLUE');
         call gpoint(0,0,1,'black',6);        *-- mark the mean;
 
         *-- Mark influential points and large residuals;
         %if &label ^= NONE %then %do;
            outy = ry[ loc(flag) ];
            outx = rx[ loc(flag) ];
            outl = obs[ loc(flag) ];
            call gpoint(outx, outy,,'RED');
            call gtext(outx,outy,outl,'RED');
         %end;
         %if &label = ALL %then %do;
            outy = ry[ loc(^flag) ];
            outx = rx[ loc(^flag) ];
            outl = obs[ loc(^flag) ];
            call gtext(outx,outy,outl,'black');
         %end;

		%if %length(&conf)>0 %then %do;
			ifact = ( "%substr(&conf,1,1)" = 'I' );  *-- Individual? --;
			conf = num(substr("&conf  ",2));
			if conf=. then conf=0.95;
			
			*-- Draw confidence limits for plot ## should work, but dont trust;
			t = rx;
			rx[rank(rx)]=t;
			t = tinv(conf, n-k1);
			stderr= sqrt((ifact + hat)#mse);
			lower = b[i]#rx - t # stderr;
			upper = b[i]#rx + t # stderr;
			call gdraw(rx, lower, 34, 'GREEN');
			call gdraw(rx, upper, 34, 'GREEN');
			%end;
		
         call gshow;
      end;
   end;
   *print "Partial Residuals", uv[ colname=uvname format=8.3];
finish;  /* end of partial */
 
*----- module to fit one regression ----------;
start reg (y, x, b, res, yhat, h, rstudent, mse, c);
	n = nrow(x);
	p = ncol(x);
	xpx = x` * x;
	xpy = x` * y;
	xpxi= inv(xpx);
	b   = xpxi * xpy;         /* regression coefficients */
	yhat= x * b;
	res = y - yhat;
*	h   = vecdiag(x * xpxi * x`);
	/*---compute leverage---*/
	c = x*xpxi;              /* catcher matrix */
	h = (c#x)[,+];           /* leverage; faster than vecdiag(x*xpxi*x`) */
	sse = ssq(res);
	mse = sse/(n-p);
	sisq= j(n,1,sse) - (res##2) / (1-h);
	sisq= sisq / (n-p-1);
	rstudent = res / sqrt( sisq # (1-h) );
finish;
 
start nomiss(y, X, obsnames);
   *-- Remove rows with any missing data from matrix and obsnames;
   *   (pass ' ' for obsnames if no row labels are present);
	matrix = y || X;
   miss = loc(matrix <=.Z);
   if type(miss)='U'
      then return;           /* no missing data */
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
            end;
         end;
			y = matrix[,1:ncol(y)];
			X = matrix[,(1+ncol(y)):ncol(matrix)];
finish;

*-----read the data and prepare partial regression plots----;
     use &data;
     read all var{&xvar} into  x[ colname=xname ];
     %if &id ^= %str() %then %do;
        read all var{&id} into  obs;
		  if type(obs) = 'N' then obs = trim(left(char(obs,4,0)));
		  else obs=trim(left(obs));
     %end;
     %else %do;
        obs = char(1:nrow(x),3,0);
     %end;
     read all var{&yvar } into  y[ colname=yname ];
	  run nomiss(y, X, obs);
     names = xname || yname;
     run partial(x, y, names, obs, uv, uvname);
     %if &out ^= %str() %then %do;
	  	  nv = char(ncol(xname),2,0);
		  call symput("nv",nv);
        create &out from uv[colname=uvname];
        append from uv;
     %end;
quit;

*-- Add variable labels to partial variables;
%if &out ^=%str() %then %do;
*put nv = &nv;
data &out;
	set &out;
	label
		UINTCEPT = 'Partial Intercept'
		VINTCEPT = 'Partial Intercept'
	%do i = 1 %to &nv;
		%let xname = %scan(&xvar,&i);
		%let xn = &xname;
		%if %length(&xname)>7 %then %let xname=%substr(&xname,1,7);
		%if &type=PARTREG %then %do;
			U&xname = "Partial &yvar"
			V&xname = "Partial &xn"
		%end;
		%else %do;
			U&xname = "PartRes &yvar"
			V&xname = "&xn"
		%end;
	%end; %str(;)

%if %index(&print,CORR) %then %do;
proc corr data=&out nosimple;
   var u: ;
	with v: ;
	%end;
%end;

%done:
	%if &abort %then %put ERROR: The PARTIAL macro ended abnormally.;
	options notes;
%mend PARTIAL;
