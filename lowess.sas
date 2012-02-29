 /*-------------------------------------------------------------------*
  *    Name: lowess.sas                                               *
  *   Title: LOcally Weighted robust Scatterplot Smoothing            *
        Doc: http://www.datavis.ca/sasmac/lowess.html             
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  21 Apr 1989 09:21:55                                    *
  * Revised:  26 Jan 2005 16:05:02                                    *
  * Version:  2.2                                                     *
  *  Fixed bug when nobs not divisible by step                        *
  *  Add:  Filter out missing data                                    *
  *        Fit local quadratic as well as linear smooth               *
  *        Output annotate= data set                                  *
  *  1.7   Allow variable X step -- for large data sets               *
  *  1.7c  Step < 1 --> calculate step = int(n/100)                   *
  *  1.8   ANOVA table, hat-values. Allow numeric ID var              *
  *  1.9   Allow to suppress robustness (for binary data)             *
  *  2.0   Added IN= ADS.  Fixed step=0 bug.                          *
  *  2.1   Added HAXIS/VAXIS controls.  Fixed IN= with GPLOT=Y        *
  *        Re-written to use PROC LOESS if running in Ver 7+          *
  *        (but only with 1 X variable for now). Fixed res bug.       *
  *        -handle special missing values (.A-.Z)                     *
  *        Handle SILENT=Y with proc loess                            *
  *  2.2   Handle multiple X variables with proc loess (no plots)     *
  *        Added FREQ= variable for grouped data                      *
  *        Made F= optional for V8+ to use AICC to set smoothing      *
  *        Added OPTIONS= for PROC LOESS                              *
  *        Added WIDTH= to control line thickness                     *
  *                                                                   *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/
%macro LOWESS(
       data=_LAST_,        /* name of input data set               */
       out=SMOOTH,         /* name of output data set              */
       x = X,              /* name of independent variable         */
       y = Y,              /* name of Y variable to be smoothed    */
       id=,                /* optional row ID variable             */
	   freq=,              /* frequency variable                   */
       f = .50,            /* lowess window width                  */
       p = 1,              /* 1=linear fit, 2=quadratic            */
       iter=2,             /* total number of iterations           */
       robust=1,           /* do robustness step?                  */
       step=1,             /* step for successive x values         */
       plot=NO,            /* draw the plot => gplot & pplot       */
       gplot=YES,          /* draw the plot?                       */
       pplot=NO,           /* draw a printer plot?                 */
	   interp=none,        /* plot interpolation option            */
       symbol=circle,      /* plotting symbol for points           */
       htext=1.5,          /* text height for plot labels          */
       hsym=1.5,           /* point symbol height                  */
       colors=BLACK RED gray,   /* colors for points, smooth, CI   */
       line=1,             /* line style for smooth                */
	   width=3,            /* line width for smooth                */
       outanno=,           /* name of output annotate dataset      */
       in=,                /* name of input annotate dataset       */
       silent=NO,          /* suppress printed output              */
       copy=,              /* vars to copy to ODS / ADS            */
       subset=1,           /* expr. to subset input observatns     */
       clm=,               /* alpha level for CI [V7+ only]        */
	   options=,           /* other LOESS options                  */
       haxis=,             /* AXIS statement for horizontal axis   */
       vaxis=,             /* and for vertical axis                */
	   proctest=&sysver>7, /* use PROCTEST=0 to revert to V6:IML   */
       name=LOWESS,        /* name for graphic catalog entry       */
		 gout=);

%let abort=0;
%if &x=%str() or &y=%str() %then %do;
   %put LOWESS: X= and Y= variables must be specified;
    %let abort=1;
   %goto DONE;
   %end;

%if %length(&gout)>0 %then
	%let gout=GOUT=&gout;

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


proc sort data=&data out=_sorted_;
   by &x;
*   where ((&x > .Z) & (&y > .Z));
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%* if "&subset" ^= "1" %then %do;
    data _sorted_;
        set _sorted_;
		  array vars{*} &x &y;
        if (&subset) and nmiss(of &x &y)=0;
    %* end;

%if %length(&freq) %then %do;
	%*-- Make the dataset ungrouped;
    data _sorted_;
        set _sorted_;
		drop _i_;
		do _i_=1 to &freq;
			output;
			end;	
	%end;

%if &proctest %then %do;
	%if %upcase(&silent) ^= NO %then %do;  
	%put LOWESS: Running PROC LOESS... MODEL &y=&x ...;
	%end;
    *-- Use proc loess for V7+ [This code is still experimental];
    %if &robust=0 %then %let iter=1;
	%if %upcase(&silent) ^= NO %then %do;  %* suppress printed output;
		ods listing close;
		%end;
    proc loess data=_sorted_;
        %if %length(&id) %then %do;
           id &id;
           %end;
        model &y = &x /
			&options
        	 %if %length(&f) %then %do;
				smooth=&f 
        		%end;
				degree=&p iterations=&iter residual
		  		scale=SD
        %if %length(&clm) %then %do;           %* add confidence limits;
            clm alpha=&clm
            %end;
         ;
        ods output OutputStatistics=&out(drop=&x);
		 run;
	%if %upcase(&silent) ^= NO %then %do;  %* restore printed output;
		ods listing;
		%end;

*options notes;
    data &out;
      merge _sorted_(keep=&x &y &id &copy) &out;
      drop Pred DepVar 
        	 %if %length(&f) %then %do;
			  SmoothingParameter 
				%end;
	  ;
      _yhat_ = pred;
      _resid_ = residual;
		label _yhat_ = "Smoothed &y"
				_resid_='Residual';
	run;
   *proc contents data=&out;
	*proc print;
    %end;

%else %do;   /* &sysver <7  */
%put LOWESS: Running PROC IML...;
proc iml worksize=10000;
start lowess( x, y, f, iter, step, yhat, res, delta, hat);
   n = nrow(x);
   if n < 2 then do;
      yhat = y;
      return;
      end;
   q = round( f * n);      * # nearest neighbors;
   q = min( q, n );        * if f>1;

   res  = y;
   yhat = J(n,1,.);
    yhat = y;
   delta= J(n,1,1);        * robustness weights;
   if iter <= 0 then iter=1;
   do it = 1 to iter;
        hat = J(n,1,0);
      do i = 1 to n by step;
         dist = abs(x[,1] - x[i,1]);  * distance to each other pt;
         r = rank( dist );
         s = r; s[r]=1:n;
         near =  s[1:q] ;             * find the q nearest;
         nx = x [ near,];
         ny = y [ near ];
         d  = dist[ near[q] ];        * distance to q-th nearest;
         if f > 1 then d = d#f;       * handle f>1;
         if d > 0 then do;
            u = abs( nx[,1] - x[i,1] ) / d ;
            wts = (u < 1) # (1 - u##3) ## 3; * neighborhood wts;
            wts = delta[ near ] # wts;
            if sum(wts[2:q]) > .0001 then do;
               run wls( nx, ny, wts, b, i, h );
                    k = i : min(i+step-1,n);      * obs indices for fit;
               yhat[k] = (j(ncol(k),1) || x[k,]) * b;      * smoothed value(s);
                    hat[k] = hat[k] + h[k-i+1];
               end;
            else yhat[k] = y[k];
         end;
         else do;
            yhat[i] = ny [+] /q;
         end;
/* -- stmts used to generate an example of how lowess works... ignore
		if it=1 & i=80 then do;
			xi = x[i,];
			xr = min(nx[,1]) || max(nx[,1]); 
			print xi, xr, b, (yhat[i]), d;
			end;
*/
      end;

        * print 'dfh' (sum(hat));
        res = y - yhat;
        %if &robust ^= 0 %then %do;
      run robust(res,delta);
        %end;
   end;

    *-- Calculate analysis of variance table;
  %if &silent = NO %then %do;
    dfh = round(sum(hat), .001);   %*-- trace(L);
    sse  = ssq(res);
    dfe = n- dfh;
    mse = sse/dfe;
    ssh = ssq(y-y[:]) - sse;
    msh = ssh/dfh;
    run ols(y, x, b, r, ss);
    nl = (ssh-ss[1,1]) || (dfh-ss[1,2]);
    nonlin = nl || (nl[1]/nl[2]) || ( (nl[1]/nl[2])/mse );
    source = (ssh || dfh || msh || (msh/mse)) // nonlin //
            (sse || dfe || mse || .) // ss;
	 pr = j(5,1,.);
	 pr[1,] = 1-probf(source[1,4], source[1,2], source[3,2]);
	 pr[2,] = 1-probf(source[2,4], source[2,2], source[3,2]);
	 pr[4,] = 1-probf(source[4,4], source[4,2], source[5,2]);
    cl = {'SS' 'df' 'MS' 'F'};
    rl = {'Lowess fit' 'Nonlinearity  ' '  Error' 'Linear fit' '  Error'};
    print "Lowess parameters: f=&f p=&p";
    print "Analysis of Variance Table for &y",
          source[r=rl c=cl] pr[f=7.4];
    %end;
   finish;

start WLS( X, Y, W, b, i, h );      *-- Weighted least squares;
   xx = j(nrow(x), 1, 1) || x;
   xpx = xx` * diag( w ) * xx;
   xpy = xx` * diag( w ) * y;
   if abs(det(xpx)) > .00001
      then do;
            xpxi = inv(xpx);
            b   = xpxi * xpy;
            c = xx*xpxi;              /* catcher matrix */
            h = (c#xx)[,+];           /* leverage; */
        end;
      else do;
         b = (y[loc(w^=0)])[:] // j(ncol(x)-1,1,0);
         print 'Singular matrix for observation', i;
      end;
finish;

*----- module to fit ols regression ----------;
start ols (y, x, b, res, ss);
   n = nrow(x);
   xx = j(nrow(x), 1, 1) || x;
   p = ncol(xx);
   xpx = xx` * xx;
   xpy = xx` * y;
   xpxi= inv(xpx);
   b   = xpxi * xpy;
   yhat= xx * b;
   res = y - yhat;
   sse = ssq(res);
    ssh = ssq(yhat - yhat[:]);
   mse = sse / (n-p);
    msh = ssh / (p-1);
    ss = (ssh || (p-1) || msh || (msh/mse)) // (sse || (n-p) || mse || .);
    finish;

start median( w, m);         * calculate median ;
   n = nrow( W );
   R = rank( W );
   i = int((n+1)/2);
   i =  i || n-i+1;
   m = w[ r[i] ];
   m = .5 # m[+];
   finish;

start robust( r, wts);       * calculate robustness weights;
    run median(abs(r), m);
     if m=0 then do;
        wts = j(nrow(r), 1);
        return;
        end;
    w = r / (6 # m);         * bisquare function;
    wts = (abs(w) < 1) # (1 - w##2) ## 2;
   finish;


*-- Main routine;
  use _sorted_;
  read all var{&x &y} into xy[ c=vars ];
   %if &id = %str() %then %do;
      id='OBS'+compress(char(1:nrow(xy),4))`;
   %end;
   %else %do;
      read all var{&id} into id;
   %end;
  close _sorted_;

  x = xy[,1];
  y = xy[,2];
  if &p > 1 then x = x || x##2;

  *-- Make the step size depend on n;
  n = nrow(xy);
  step = &step;
  if step < 1 then do;
    if n>100 then step = int(n/100);
             else step = 1;
  end;
  run lowess(x, y, &f, &iter, step, yhat, res, weight, hat);

*-- Output results to data set &out ;
  xys =    yhat || res || weight || hat;
  cname = {'_YHAT_' '_RESID_' '_WEIGHT_' '_HAT_' };

  create &out from xys [ colname=cname ];
  append from xys;

  %if &silent = NO %then %do;
        xyres =x[,1] || y || xys;
        cname = vars || cname;
        if type(id)='N' then id=char(id,6);
        print "Data, smoothed fit, residuals and weights",
                xyres[ colname=cname r=id f=best6.];
  %end;

quit;
 /*--------------------------------------------*
  | Merge data with smoothed results.          |
  | (In a data step to retain variable labels) |
  *--------------------------------------------*/
options notes;
data &out;
   merge _sorted_(keep=&x &y &id &copy)
         &out ;
   label _yhat_ = "Smoothed &y"
         _weight_='Lowess weight'
            _hat_ = 'Leverage';
   run; quit;

%end;  /* &sysver < 7 */

%let x1=%scan(&x, 1, %str( ));
%let x2=%scan(&x, 2, %str( ));
%let c1 = %scan(&colors &colors, 1);
%let c2 = %scan(&colors &colors, 2);
%let c3 = %scan(&colors &colors, 3);

%if %length(&x2)>0 %then %do;
	%put LOWESS:  The LOWESS macro does not produce plots for multiple Xs.;
	%put %str(        ) Try PROC G3D or GCONTOUR.;
	%end;
%else %do;
	%if %upcase(&PLOT)=YES or %upcase(&GPLOT)=YES %then %do;

   %if %length(&haxis)=0 %then %do;
       axis2   label=(h=&htext)          value=(h=&htext);
      %let haxis=axis2;
      %end;
   %if %length(&vaxis)=0 %then %do;
       axis1   label=(h=&htext a=90 r=0) value=(h=&htext);
      %let vaxis=axis1;
      %end;

proc gplot data=&out &GOUT ;
  plot &y     * &x = 1
       _yhat_ * &x = 2
       %if %length(&clm)>0 and &sysver>=7 %then %do;
       LowerCL * &x = 3
       UpperCL * &x = 3
       %end;

       / overlay frame
            vaxis=&vaxis haxis=&haxis
         hminor=1 vminor=1
        %if %length(&in)>0 %then %do;
            anno=&in
            %end;
         name="&name"
            des="Lowess Plot of &y * &x" ;
  symbol1 v=&symbol  h=&hsym  i=&interp c=&c1;
  symbol2 v=none i=join c=&c2 w=&width l=&line;
  symbol3 v=none i=join c=&c3          l=&line;
  run; quit;
  goptions reset=symbol;
	%end;
%end;
%if %upcase(&PLOT)=YES or %upcase(&PPLOT)=YES %then %do;
proc plot data=&out ;
  plot &y     * &x = '*'
       _yhat_ * &x = '@'
       %if %length(&clm)>0 and &sysver>=7 %then %do;
       LowerCL * &x = -
       UpperCL * &x = -
          %end;
       / overlay ;
run;
%end;

%*-- Create an output annotate dataset? ---;
%if %length(&outanno) > 0 %then %do;
data &outanno;
   set &out(keep=&id &x _yhat_ &copy);
    drop _yhat_;
    retain xsys ysys '2';
    length function color $8;
    if _n_=1 then function='MOVE    ';
        else function='DRAW    ';
    x = &x;
    y = _yhat_;
    color = upcase("&c2");
    line = &line;
	size=&width;

%if %length(&in)>0 %then %do;
    data &outanno;
        set &in &outanno;
    run; quit;
    %end;

%end;

%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%if &abort %then %put ERROR: The LOWESS macro ended abnormally.;
%mend LOWESS;
