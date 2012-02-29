 /*-------------------------------------------------------------------*
  *    Name: resline.sas                                              *
  *   Title: Fit a resistant line to XY data, and find powers         *
  *          to straighten the relation.                              *
        Doc: http://www.datavis.ca/sasmac/resline.html          
  * Usage (minimal syntax):                                           *
  *     %resline(data= , x= , y= )                                    *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  17 Sep 1991 22:33:49                                    *
  * Revised:  15 May 2006 09:12:35                                    *
  * Version:  2.3                                                     *
  *  - Revised so ID variable in output data set has same name as in  *
  *    the input data set                                             *
  *  - Delete missing data (including .A-.Z) Updated for V7+          *
  * 2.3 Added POWERS= to set the list of powers used in RST           *
  *     Added TRANS= powers of X Y                                    *
  *     Applied variable labels to output data set                    *
  *     Added GPLOT= for graphic plots                                *
  *     Cleaned up temp data sets. Fixed APPEND FROM                  *
  *                                                                   *
  *-------------------------------------------------------------------*/
%macro resline(
        data=_LAST_,      /* data set to be analyzed             */
        x=, y=,           /* names of X and Y variables          */
        id=,              /* observation id variable (char)      */
        ends = .5,        /* greatest range of either end 1/3    */
        plot=FIT ,        /* printer plots to show               */
		gplot=FIT RESID,
        rst=YES,          /* calculate ratio of slopes table     */
        powers = -1 -.5 0 .5 1 2,  /* list of powers to consider */
        trans =,
		  lohi=4,           /* Number of low/hi residuals to show  */
        print=YES,        /* print data, fit, residuals          */
        out=_FIT_,        /* name of output data set             */
        copy=,            /* vars to copy to ODS                 */
        outsum=_SUMVAL_); /* name of output summary values       */

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let rst=%upcase(&rst);
%let plot=%upcase(&plot);
%let gplot=%upcase(&gplot);

%let trans=%upcase(&trans);
%if &x=%str() or &y=%str() %then %do;
    %put ERROR:  Both X= and Y= must be specified.;
    %goto done;
    %end;

proc iml;
start resline(xy, vars, id, fr) global (outsum, rst, dorst);
   *-- Fit a resistant line to XY data;
   reset noname;
   X = XY[, 1 ];
   Y = XY[, 2 ];
   if nrow(id)>1 then id=shape(id,nrow(xy),1);

   *-- Sort XY and ID by X;
   t = xy;
   xy[ rank(x),] = t;
   t = id;
   id[ rank(x),] = t;
   x = xy[,1];
   y = xy[,2];

   if type(outsum) = 'U' then outsum=1;
   run fitline(xy, s, a, b, fit, residual);
   print 'Parameters of fitted resistant line',
         b[c='slope'] a[c='intercept'];
   fr = fit || residual;
   cl = {'Fit' 'Residual'};
    %if %upcase(&print)=YES %then %do;
        print 'Data, Fitted values and Residuals';
       print XY [r=id c=vars] FR [c=cl format=8.3];
        %end;

   n = nrow(XY);

    %if %index(&PLOT,FIT) > 0 %then %do;
        plot = S // XY // ( X || FIT ) ;
        title= 'Legend:  * Data   + Fitted values   # Summary points';
        sym = repeat( '#',3,1 ) //        /* summary points */
                repeat( '*',n,1 ) //        /* data points */
                repeat( '+',n,1 ) ;         /* fitted line */
        call pgraf(plot,sym,vars[1], vars[2],title);
        %end;

    %if %index(&PLOT,RES) > 0 %then %do;
        plot = (x || residual) // ( x || j(n,1,0));
        sym = repeat( '*',n,1 ) //
                repeat( '-',n,1 ) ;
        call pgraf(plot,sym,vars[1], 'RESIDUAL');
        %end;

    %if %index(&RST,YES) > 0 %then %do;
        if dorst >0 then run rst(S);
        %end;

    %if %index(&PLOT,DIAG) > 0 %then %do;
        outsum=0;
        run diagplot(xy, a, b);
        %end;
    finish;

start fitline(xy, s, a, b, fit, res);
   run sumvals(xy,s,ns);
   b = (s[3,2]-s[1,2]) /
       (s[3,1]-s[1,1]) ;
   a =(s[,2] - b*s[,1]) [:,];
   fit = a + b*xy[,1];
   res = xy[,2] - fit;
    finish;

start sumvals(xy,sumval,ns) global (outsum);
   *-- Summary values for an XY-batch (already sorted by X);
   n = nrow(XY);
   n1= floor(n/3) + (mod(n,3)=2);     *-- number in lower/upper third;
   n3= floor(n/3) + (mod(n,3)=2);     *-- number in lower/upper third;
   flags = shape(' ',3,1);

   *-- Do not let either end third cover more than half of range;
   xrange = xy[n,1] - xy[1,1];
   mid = xy[1,1] + &ends # xrange;
   if xy[n1,1] > mid then do;
      n1 = max(2,sum(xy[,1] < mid));
      flags[1] = 'R';
      end;
   mid = xy[n,1] - &ends # xrange;
   if XY[n-n3+1,1] < mid then do;
      n3 = max(2,sum(xy[,1] > mid));
      flags[3] = 'R';
      end;
   *-- adjust for equal values which straddle summary x value;
   if any(XY[n1,1] = XY[(n1+1):n,1]) then
      do;
        n1 = max(2, sum(xy[,1] < xy[n1,1] ));
        flags[1]=compress(flags[1]+'=');
      * print 'Equal X values in lower third, n1=' n1;
      end;
   if any(XY[n-n3+1,1] = XY[1:(n-n3),1]) then
      do;
        n3 = max(2, sum(xy[,1] > xy[n-n3+1,1] ));
        flags[3]=compress(flags[3]+'=');
      * print 'Equal X values in upper third, n3=' n3;
      end;
   run medians(XY[ 1:n1, ],SL);
   run medians(XY[ n1+1:n-n3, ],SM);
   run medians(XY[ n-n3+1:n, ],SH);

   SUMVAL = SL // SM // SH;
   ns = n1 // (n-n1-n3) // n3;
   third = {'Low','Mid','High'};
   cl = {x y};
   print "Summary Values",,sumval [ r=third c=cl format=8.3]
                               ns [ c={'n'} format=5.0]
                            flags [ c={' '}];
   if any(flags^=' ') then
      print "('R' -> half-range rule;  '=' -> equal X value rule)";

   %if &outsum ^= %str() %then %do;
       if outsum>0 then do;
			out = sumval || ns;
			cl = {"&x" "&y" "N"};
			create &outsum from out[c=cl r=third];
			append from out[r=third];
			end;
   %end;
finish;

start medians(d,med);
   *-- Median of each column of a matrix;
   n = nrow(D);
   c = ncol(D);
   Med = J(1,c,0);
   m = floor( ((n+1)/2) || ((n+2)/2) );
   do i = 1 to c;
       t = d[,i];
      rt = rank(t);
      t[rt,]=d[,i];
      med[,i] = (t[m,])[+,]/2;
      end;
finish;

start rst(s) global(rst);
   if any(s < 0 ) then do;
       wh = any(s[,1]<0) + 2#(any(s[,2]<0));
        which = {'X' 'Y' 'X and Y'}[wh];
      print 'Ratio of slopes table cannot be computed for'
            'negative summary values.  Add a constant to' which;
      return;
      end;

   *-- Calculate ratio of slopes table from summary values in S;
   powers = {&powers};
   np = ncol(powers);
   do ix = 1 to np;
   p = powers[ix];
   do iy = 1 to np;
      q = powers[iy];
      sl2 = (power(S[3,2],q)-power(S[2,2],q)) /
            (power(S[3,1],p)-power(S[2,1],p));
      sl1 = (power(S[2,2],q)-power(S[1,2],q)) /
            (power(S[2,1],p)-power(S[1,1],p));
      rst = rst || (sl2/sl1);
      end; end;
   rst = shape(rst,np,np);
   labl= char(powers,4,1);
   if nrow(loc(powers=0))>0 then labl[loc(powers=0)]  ='log';
   if nrow(loc(powers=0.5))>0 then labl[loc(powers=0.5)]='sqrt';
   if nrow(loc(powers=1))>0 then labl[loc(powers=1)]  ='raw';
   print '----- Ratio of Slopes table ------',
         'Rows are powers of X, columns are powers of Y';
   print rst[r=labl c=labl f=8.3];

   *-- Find slope ratios closest to 1 (log closest to 0);
   lrst = abs(log10(rst));
   ranks = rank(lrst);
   do l=1 to 5;
      loc = loc(ranks=l);
      r   = ceil(loc/np);
      c   = loc - (r-1)#np;
      best= best // (labl[r] || labl[c]);
      ratio = ratio // rst[r,c];
      end;
   logs= log10(ratio);
   print '------- 5 Best powers -------',
         best[c={'Power of X' 'Power of Y'}]
         ratio[c={'Slope Ratio'} f=12.3]
         logs[c={'log Ratio'} f=10.3];
finish;

start power(x,p);
   if x<0      then return(.);
   if p>0      then return(x##p);
   else if p=0 then return(log10(x));
   else             return(-x##p);
finish;

start pname(name, p);
    l = length(name);
    if p=0 then return ('log'+substr(name,1,5));
    else if p=0.5 then return ('sqrt'+substr(name,1,4));
   else return(name + '##' +char(p));
finish;

start diagplot(xy,a,b);
   *-- diagnostic plot for transformation to straightness;
   *   1-slope of yt on xt ==> power for y;
   *-- Source: Stoto & Emerson, JASA, 1982;
   run medians(xy,m);
   n = nrow(xy);
   xt = b##2 # ( xy[,1] - m[1] )##2 / (2 # m[2]);
   yt = xy[,2] - m[2] - b#( xy[,1] - m[1] );
   plot = xt || yt;
   t = plot;
   plot[ rank(xt),] = t;
   print 'Diagnostic Plot';
   run fitline(plot,sp,int,slope,fp,rp);
   power= round(1-slope,.5);
   Print 'Slope and suggested power for Y from diagnostic plot',
         slope[c={slope}] power[c={power}];
   plot = plot //(plot[,1]||fp ) ;
   sym = repeat( 'x',n,1 ) //        /* data points */
         repeat( '+',n,1 ) ;         /* fitted line */
   title='Diagnostic plot: 1-slope = power for Y';
   call pgraf(plot,sym,'X transform','Y transform',title);
finish;

start nomiss(matrix, obsnames);
   *-- Remove rows with any missing data from matrix and obsnames;
   *   (pass ' ' for obsnames if no row labels are present);
   miss = loc(matrix <=.Z);
   if type(miss)='U'
      then return;           /* no missing data */
      else do;
         nr = nrow(matrix);
         nc = ncol(matrix);
         rows= 1+floor((miss-1)/nc);
         rows= unique(rows);
		 if ncol(rows) = nrow(matrix) then do;
		 	print 'ERROR:  There are no non-missing observations';
			return;
		 	end;
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
finish;

/*---------------*
 |  Main routine |
 *---------------*/
   use &data;
    vars = contents();
    ok = any(vars = upcase("&x")) & any(vars = upcase("&y"));
    if ok=0 then do;
        file log;
        put "ERROR: Either the X (&x) or Y (&y) variable(s) were not found "
            " in the dataset &data.";
        call push('quit;');
    end;

   %if &id = %str() %then %do;
      read all var{&x &y} into xy[ c=vars ];
      id='OBS'+char(1:nrow(xy),3)`;
   %end;
   %else %do;
      read all var{&x &y} into xy[ c=vars r=&id ];
      id = &id;
   %end;

   run nomiss(xy, id);
	dorst=0;
	if substr("&rst",1,1)='Y' then dorst=1;
   run resline(xy, vars, id, fr);

    %if %length(&trans) and %substr(&rst,1,1)=Y %then %do;
        opx = {%scan(&trans &trans,1)};  px=opx;
        opy = {%scan(&trans &trans,2)};  py=opy;
        powers = {&powers};
        if type(px) ='N'
            then rst = rst[loc(powers=px),];
            else px = {&powers};
        if type(py) ='N'
            then rst = rst[,loc(powers=py)];
            else py = {&powers};
        reset name;
        *print 'Selecting power from', px py rst;

        npx = nrow(rst);
        npy = ncol(rst);
        ranks = rank(abs(log10(rst)));
		  loc = loc(ranks=1);
		  r   = ceil(loc/npy);
		  c   = loc - (r-1)#npx;

        if type(opx) ='C' then px = px[r];
        if type(opy) ='C' then py = py[c];
        print "Best powers are" px py;

        txy = power(xy[,1], px) || power(xy[,2], py);
        tvar = vars;
        tvar[1] = pname(vars[1], px) ;
        tvar[2] = pname(vars[2], py) ;
        outsum=0; dorst=0;
       run resline(txy, tvar, id, tfr);
        %end;


   *-- Create output data set;
    *options notes;
    &x = xy[,1];
    &y = xy[,2];
    fit = fr[,1];
    residual = fr[,2];
    %if %length(&id) %then %do;
        &id = id;
        %end;
   create &out var{&id &x &y fit residual};
   append;

quit;

%* Merge with input data, to set variable labels;
data &out;
   merge &data(keep=&x &y &id &copy where=(&x^=. & &y^=.))
         &out ;
   label fit = "Fitted &y"
         residual='Residual'
	;
   run; quit;

%let cleanup=;
*-- Find lowest and highest residuals;
%if &lohi > 0 %then %do;
%put RESLINE: Finding the &lohi Highest/Lowest residuals...;

	%hilo(in=&out, out=_dn_, var=residual, id=&id, dir=DN, num=&lohi);
	%hilo(in=&out, out=_up_, var=residual, id=&id, dir=UP, num=&lohi);
	%let cleanup=_rank_;
	
	data _hilo_;
		set _dn_(in=indn) _up_(in=inup);
	proc sort;
		by descending residual;
	proc print;
		id &id;
	%let cleanup=&cleanup _hilo_ _dn_ _up_;
	%end;
%if %index(&GPLOT,FIT) > 0 %then %do;
	data _anno_;
		set &outsum;
		retain xsys ysys '2' function 'symbol' size 2 text 'dot';
		x = &x;
		y = &y;
	run;
	
	proc transpose data=_sumval_ out=_ns_ prefix=n;
		var n;
 
	data _null_;
		%*-- find boundaries between thirds for vertical lines;
		set _ns_(keep=n1-n3);
		retain n1-n3;

		obs=n1;     link findobs;
		call symput('h1', left(put(h,best8.2)));
		obs=n1+n2;  link findobs;
		call symput('h2', left(put(h,best8.2)));
		stop;

	findobs:
		          set &out point=obs; x1=&x;
		obs+1   ; set &out point=obs; x2=&x;
		h = (x1+x2)/2;
		return;
	run;

	proc gplot data=&out;
   plot &y  * &x = 1
        fit * &x = 2 / 
		   overlay
			anno=_anno_ href=(&h1 &h2) lhref=34 chref=blue hm=1 vm=1
			vaxis=axis1;
		symbol1 v=circle cv=black;
		symbol2 v=none i=join line=1  c=black;
		axis1 label=(a=90);
	run; quit;
	%let cleanup=&cleanup _anno_ _ns_;
	%end;

%if %index(&GPLOT,RESID) > 0 %then %do;

	%if &lohi > 0 %then %do;
		%label(data=_hilo_, x=&x, y=residual, text=&id, out=_label_, pos=-, size=1);
		%let anno=anno=_label_;
		%end;
	proc gplot data=&out;
   plot residual * &x = 1 / 
			vref=0 lvref=34 cvref=blue hm=1 vm=1 &anno 
			vaxis=axis1;
		symbol1 v=circle cv=black;
		axis1 label=(a=90) offset=(2);
	run; quit;
	%let cleanup=&cleanup _label_;
	%end;

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
%if %length(&cleanup) %then %do;
options nonotes;
proc datasets nofs nolist library=work memtype=(data);
    delete &cleanup;
	 run; quit;
%end;

%done: 
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;

 /* ---------------------------------------------------------------*/

%macro hilo(in=,          /* input data set                        */
            out=,         /* output data set                       */
            var=col1,     /* variable to analyze                   */
            id=,          /* id variable                           */
            num=,         /* number of extreme observations wanted */
            dir=up        /* which extreme: UP or DN               */
                );
%let dir=%upcase(&dir);
%if &dir=UP
    %then %do;
       %let pre=LO;
       %let order=;
    %end;
    %else %do;
       %let pre=HI;
       %let order=DESCENDING;
    %end;

*-- Find lowest or highest num  observations;
proc rank data=&in out=_rank_ ties=low &ORDER;
	where (&var > .Z);
   var &var;
   ranks _depth_;
proc sort data=_rank_ out=&out;
   by  _depth_;
	where _depth_ <= &num;
*proc print;
%mend;
