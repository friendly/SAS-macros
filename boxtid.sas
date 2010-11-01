 /*-----------------------------------------------------------------*
  |     Name: boxtid.sas                                            |
  |    Title: Power transformations by Box-Tidwell method           |
         Doc: http://www.datavis.ca/sasmac/boxtid.html        
  | ----------------------------------------------------------------|
  |    Procs: iml reg plot gplot                                    |
  |  Macdefs: boxtid                                                |
  |  Macrefs: gskip                                                 |
  | Datasets: _cvar_ _part_ _anno_                                  |
  |  Modules: boxtid ols regcoef power symput                       |
  | ----------------------------------------------------------------|
  |   Author: Michael Friendly <friendly@yorku.ca>                  |
  |  Created: 1 Dec 1997 11:39:52                                   |
  |  Revised: 15 May 2006 09:21:33                                  |
  |  Version: 1.2                                                   |
  |  - Handle missing (.A-.Z)                                       |
  |  - Fixed bugs with no XTRANS=, etc.                             |
  |  - Added GPLOT=TRANS                                            |
  |  - Fixed bugs with yvar/resp syn., numeric ID=                  |
  *-----------------------------------------------------------------*/
 /*=
=Description:

 The BOXTID macro finds power transformations for some or all of the
 predictors in a regression model using the Box-Tidwell method.  In
 addition, it can produce plots showing the influence of individual
 observations on the selection of powers. These are partial residual
 plots for the constructed variables X * log X.  Observations with
 large studentized residuals or large Cook's distances are labeled
 automatically.

 As a convenience, an output data set containing the optimally
 transformed variables is also produced. 

=Usage:
 
 The BOXTID macro takes 14 keyword arguments.  You must specify either
 the RESP= or YVAR= parameter, and the names of all predictors (XVAR=).
 For example:
 
	%boxtid(data=angell, yvar=moralint,
        xvar=hetero mobility, id=city);


==Parameters:

* DATA=_last_        Name of input data set

* RESP=              The name of the response variable

* YVAR=              Response variable (synonym for RESP=)

* XVAR=              Names of the predictors in the model.  This must
                     be a simple list of variable names, i.e., lists
							like X1-X10 are not allowed.  ALL XVARs MUST be
							strictly POSITIVE.

* XTRANS=            Variables to be transformed: names or indices.
                     If XVAR=X1 X2 X3 X7 X9, you may specify either
							XTRANS=X3 X7 X9 or XTRANS=3 4 5 for the same effect.
                     If not specified, all variables in the XVAR= list
							are transformed.

* PREFIX=t_          Prefix for names of transformed variables.  If the
                     X variables are X1 X2 X3, the output data set will
							contain T_X1, T_X2, T_X3 when the PREFIX=T_.

* ID=                Name of an ID variable, used as a point label in
                     influence plots.

* OUT=boxtid         Name of the output data set

* ROUND=0.5          Round powers.  The estimated power for each predictor
                     is rounded to the nearest ROUND= unit in constructing
							the transformed variables.

* MAXIT=15           Maximum number of iterations for Box-Tidwell

* CONVERGE=0.001     Convergence criterion.  The process stops when the
                     largest change in an estimated power is less than
							the CONVERGE= value, or when MAXIT iterations would
							be exceeded.

* PPLOT=             Specifies printer plots, if any to be produced.
                     Either or both of the keywords TRANS and INFL.    

* GPLOT=TRANS INFL   Specifies high-res plots, if any to be produced.
                     Either or both of the keywords TRANS and INFL.    

* QUIET=N            Y or N.  QUIET=Y suppresses printout of the iteration
                     history.
=Limitations:

 =*/
 
%macro boxtid(
   data=_last_,     /* name of input data set */
   resp=,           /* response variable */
   yvar=,           /* response variable */
   xvar=,           /* predictors */
   xtrans=,         /* variables to be transformed: names or indices */
	prefix=t_,       /* prefix for names of transformed variables */
   id=,             /* name of ID variable */
   out=boxtid,      /* name of output data set */
   round=0.5,       /* round powers */
   maxit=15,        /* maximum number of iterations */
   converge=0.005,  /* convergence criterion */
   pplot=,          /* printer plots? */
   gplot=TRANS INFL, /* high-res plots? */
	symbol=square,   /* plotting symbol in INFL plots */
	quiet=N
       );

	%*-- Reset required global options;
	%if &sysver >= 7 %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let pplot=%upcase(&pplot);
%let gplot=%upcase(&gplot);
%let quiet=%upcase(%substr(&quiet,1,1));
%if %length(&xvar)>0 %then %let model=&xvar;

%if (%length(&resp) = 0) %then %do;
	%if %length(&yvar)>0 %then %let resp=&yvar;
	%else %do;
		%put ERROR: BOXTID: RESP= or YVAR= must be specified;
		%goto exit;
		%end;
	%end;

 /*
 /  Get the number of non-missing observations; quick exit if none.
 /  Drop missing, and non-positive Xs
 /-------------------------------------------------------------------*/
   data _nomiss_; 
		set &data;
		array vars{*} &resp &xvar;
		drop _OK_ _i_;
		_OK_=1;
		do _i_=1 to dim(vars);
			if vars(_i_) <= .Z then _OK_=0;
			if _i_>1 & vars(_i_) <= 0 then _OK_=0;
			end;
      if _OK_;

   proc contents noprint data=_nomiss_ out=_NOBS_;
   data _null_; set _NOBS_;
      call symput('NOBS',trim(left(put(nobs,best20.))));
   run;
   %if (&nobs <= 0) %then %do;
		%put ERROR: There are no non-missing values of the variables;
		%goto exit;
		%end;

%put BOXTID: &nobs observations retained for Box-Tidwell:;
%put %str(        )[all(&xvar) > 0, &resp non-missing];
%put BOXTID: Finding power transformations...;
proc iml;
 
start boxtid(y,x,names,xt, gamma) global(name);
   %if &xtrans = %str() %then %do;  %*-- Do them all if xtrans not specified;
       ind = 1:ncol(x);
       x1 = x;
       free x2;
       name = names;
       %end;
   %else %do;
      t = upcase({&xtrans});
      if type(t) = 'N'
         then ind = t;
         else do i=1 to ncol(t);
				l = loc( t[i] = upcase(names) );
            if l>0 then ind = ind || l;
            end;
       x1 = x[,ind];
       name = names[,ind];
		 if ncol(ind) < ncol(x) then do;
			x2 = x[,remove((1:ncol(x)),ind)];
			names = name || names[,remove((1:ncol(x)),ind)];
		 	end;
		else do;
			free x2;
			end;
       %end;

*show x1 x2;
 
   n =nrow(x1);
   n1=ncol(x1);
   gamma=j(1,n1);
   t=1+(1:n1);            *-- coefficients to select;
   g=2;
   do it = 1 to  &maxit while( max(abs(1-g)) > &converge );
      *-- transform the x1 variables;
      xt = power(x1, gamma);
      xlx = xt # log(xt);
      xr1 = j(n,1) || xt  || x2;
      xr2 = j(n,1) || xlx || xt || x2;

      run regcoef(y, xr1, b);     *-- fit y ~ xr1, coeff->b;
      run regcoef(y, xr2, c);     *-- fit y ~ xr2, coeff->c;
	  if any(b=.) then goto endit;
      g = 1 + (c[t] / b[t])`;
      crit = crit // max(abs(1-g));
      gamma = gamma # g;
      hist = hist // ( gamma );
	print 'G:' g 'Gamma:' gamma;
     end;
endit:

	%if &quiet = N %then %do;
	it = t(1:nrow(hist));
   hlab = 'Iteration' || name;
   print 'Iteration History: Transformation Powers', 
      it[c='Iteration' f=3.] hist[c=name f=7.4] crit[c='Criterion' f=7.4];
		%end;

   xr2 = j(n,1) || (x1#log(x1)) || x1 || x2;
   run ols(y, xr2, c, res, mse, covb);
   se = (vecdiag(covb))[t];
   z  = c[t] / sqrt(se);
   gamma = gamma`;
   p = 2# (1-probnorm(abs(z)));
   result = gamma || se || z || p;
   label  = {'Power' 'StdErr' 'Score Z' 'Prob>|Z|'};
   print 'Score tests for power transformations', result[r=name c=label f=7.4];

   %if %length(&round)>0 %then %do;
      r = &round;
      gamma = round(gamma, r);
      print "Powers rounded to the nearest &round", gamma[r=name];
	   %end;
      xt = power(x1, gamma);

   %if %index(&pplot,TRANS)>0 %then %do;
   do i=1 to ncol(xt);
      xy = xt[,i] || y;
      sym = substr(&id,1,3);
      xl = name[i];
      call pgraf(xy, sym, xl, yname,'Transformation plot');
      end;
   %end;

   xt = xt; * || x2;
   finish;
 
*----- module to fit one regression ----------;
start ols (y, x, b, res, mse, covb);
   n = nrow(x);
   p = ncol(x);
   xpx = x` * x;
   xpy = x` * y;
   xpxi= inv(xpx);
   b   = xpxi * xpy;
   yhat= x * b;
   res = y - yhat;
   sse = ssq(res);
   mse = sse / (n-p);
   covb = mse#xpxi;
   finish;
 
 
start regcoef(y, x, b);
   xpx = x` * x;
   xpy = x` * y;
   det = det(xpx);
/*
print 'Det:' det;
   on_error =  {
   "if det < 1E-10 then do;  b=xpy # .;",
   "  call push(on_error); resume; end;" };
   push(on_error);
*/
	if abs(det) > 1E-8 
    	then b = solve(xpx, xpy);
		else do;
			b = xpy # .;
			print "det(X'X) is" det "  Iterations terminated";
			end;
   finish;
 
start power(x, p);
   do c=1 to ncol(x);
      pow = p[c];
      if pow=1 then xt = xt || x[,c];
         else if abs(pow)<.001 then xt = xt || log(x[,c]);
*         else xt = xt || ( ((x[,c]##pow)-1)/pow );
         else xt = xt || ((x[,c]##pow));
      end;
   return (xt);
   finish;

start symput(name, val);
   *-- Create a macro variable from a char/numeric scalar;
   if type(val) ='N'
      then value = trim(char(val));
      else value = val;
   call execute('%let ', name, '=', value, ';');
   finish;



*-----read the data and prepare partial regression plots----;
     use _nomiss_;
     %if %length(&id)>0 %then %do;
        read all var{&xvar} into  x[ colname=xname ];
	     read all var{&id} into &id;
		  if type(&id) = 'N' 
		  	then _id_ = char(&id);
			else _id_ = &id;
     %end;
     %else %do;
        read all var{&xvar} into  x[ colname=xname ];
        _id_ = char(1:nrow(x),8,0);
     %end;
     %let id = _id_;
     read all var{&resp } into  y[ colname=yname ];
     names = xname || yname;
     n = nrow(x);
   reset noname;
	*show &id;
   run boxtid(y, x, xname, xt, gamma);

	*-- Labels for transformed variables--;
	plist = {-1 -0.5 0 0.5 1 2 3};
	plabl = {'Inverse' 'InvSqrt' 'Log' 'Sqrt' '' 'Squared' 'Cubed'};
	do i=1 to nrow(gamma);
		run symput('p'+char(i,1), gamma[i]);
		if (any(gamma[i] = plist)) then do;
			plab = plabl[ loc(gamma[i] = plist) ];
			run symput('l'+char(i,1), plab);
			*print (gamma[i]) plab;
			end;
		else run symput('l'+char(i,1), char(gamma[i])+' power of');
		end;

*   print 'Transformed data', xt[r=&id c=xname] y[c=yname];
     %if &out ^= %str() %then %do;
        nv = char(nrow(gamma),2,0);
        run symput("nv",nv);
        xt = y || x || xt;
        xname = "&resp" || xname || ("&prefix"+substr(name,1,6));
        create &out from xt[r=_id_ colname=xname];
        append from xt[r=_id_];
     %end;
quit;run;

%put BOXTID: &nv variables have been transformed.;

%if %length(&xtrans)=0 %then %let xtrans=&xvar; 
%*-- Add labels for transformed variables;
data &out;
	merge _nomiss_ &out;
	label
	%do i=1 %to &nv;
   %let xt = %substr(%scan(&xtrans,&i),1,6);
		&prefix.&xt = "&&l&i &xt"
		%end;
		;
run;

%if %index(&gplot,TRANS)>0 %then %do;
%put BOXTID: Plotting the transformations...;
	%do i=1 %to &nv;
	   %let xt = %substr(%scan(&xtrans,&i),1,6);
		proc gplot data=&out;
			plot &resp * &prefix.&xt /
         vaxis=axis1 vminor=1 hminor=1 frame
         ;
      axis1 label=(a=90) ;
      	symbol i=rl h=1.5 v=&symbol c=black ci=red;
		%end;
	%end;

%*-- Calculate constructed variables for partial-regression plots;
%put BOXTID: Calculating the constructed variables for INFL plots...;
data _cvar_;
   set &out;
   array xlx{&nv} xlx1 - xlx&nv;
   array x{&nv} &xtrans;
   do i=1 to &nv;
      xlx(i) = x(i) * log( x(i) + (x(i)^=0) );
      end;
	label
	%do i=1 %to &nv;
		xlx&i = "XlogX %scan(&xtrans,&i)"
		%end;
		;

*-- Find influence statistics for CV plot;
   proc reg data=_cvar_ outest=_parm_ noprint;
     id &id;
    m0: model &resp=&model xlx1 - xlx&nv;
        output out=m0 rstudent=_resid_ cookd=_infl_ h=_hat_;
*   proc print data=_parm_;

%do i=1 %to &nv;
   %let xli = xlx&i;
   %let xt = %scan(&xtrans,&i);
   %let xlother=;
   %do j=1 %to &nv;
      %if &i ^= &j %then %let xlother = &xlother xlx&j;
      %end;
   %put BOXTID: Constructed variable plot for &xt: xli=&xli  xlother=&xlother;
   
   proc reg data=_cvar_ noprint;
     id &id;
    m1: model &resp=&model &xlother ;
        output out=m1 r=_resy_;     * y ~ X + XlX(other);
    m2: model &xli =&model &xlother ;
        output out=m2 r=_resx_;     * X(k)log X(k) ~ X + XlX(other);
   
   data _part_;
      keep _resy_ _resx_ &id _infl_ _resid_;
      merge m0 m1 m2;
      label _resy_ ="Partial &resp"
            _resx_ ="Partial Constructed Variable (&xt)";

   %if %index(&pplot,INFL)>0 %then %do;
      proc plot data=_part_;
         plot _resy_  * _resx_ = &id / vref=0;
      run;quit;
   %end;

	%if %index(&gplot,INFL)>0 %then %do;
	data _anno_;
		set _part_ nobs=n;
		length text $16 color $8;
		if _n_=1 then do;
			xsys='1'; ysys='1';
			x=3; y=97; position='6';
			text = "BoxTid power: &&p&i";
			function='LABEL'; output;
			end;

		if  abs(_resid_) > 3 | _infl_> 4/(n-1) then do;
			xsys='2'; ysys='2';
			x = _resx_; y=_resy_ ;
			function='LABEL'; color='red';
			if y > 0 then position='B';
						else position='E';
			text=&id; output;
			end;
		run;

   proc gplot data=_part_;
      plot _resy_  * _resx_ /
         vaxis=axis1 vminor=1 hminor=1 frame
         vref=0 lvref=34 anno=_anno_ ;
      axis1 label=(a=90) ;
      symbol i=rl h=1.5 v=&symbol c=black ci=red;
   run;quit;
	%gskip;
	%end;  /* if INFL */
%end; /* i=1 %to &nv */

%if %index(&gplot,INFL)>0 %then %do;
   proc datasets nolist;
      delete _cvar_ _part_ _anno_;
		run; quit;
	%end;  /* if INFL */


%exit:
	%*-- Restore global options;
	%if &sysver >= 7 %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
