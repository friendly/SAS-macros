 /*-------------------------------------------------------------------*
  *    Name: dfbeta.sas                                               *
  *   Title: IML macro program for calculating dfbeta statistics      *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  23 Jan 1989 16:11:15                                    *
  * Revised:  15 May 2006 09:24:37                                    *
  * Version:  1.5                                                     *
  *-------------------------------------------------------------------*/
%macro dfbeta(
    data = _LAST_,  /* name of input data set               */
    yvar =,         /* name of dependent variable           */
    xvar =,         /* list of independent variables        */
    id =,           /* ID variable (char or numeric)        */
    label=INFL,     /* label ALL, NONE, or INFLuential obs  */
    out =dfbeta,    /* output data set: partial residuals   */
    htext=1.5,      /* height of text in plots              */
    symbol=star,    /* point plotting symbol                */ 
    plots=&xvar,    /* which partial plots to produce       */
    gout=gseg,      /* name of graphic catalog              */
    name=DFBETA);   /* name of graphic catalog entries      */
 
%let label = %upcase(&label);
%let plots = %upcase(&plots);
proc iml;
  
 
*-----Find dfbetas for each variable-----;
start dfbeta(x, y, names, obs, dfb, uvname );
   k = ncol(x);
   n = nrow(x);
   yname = names[,k+1];
   k1= k + 1;                  *-- number of predictors;
   x = j( n , 1 , 1) || x;     *-- add column of 1s;
   name1 = { 'INTCEPT'};
   names = name1 || names[,1:k];
 
   run reg( y,  x, b, res, yhat, hat, rstudent, mse, dfb );
   print "Full regression";
   print "Regression weights" , b[ rowname=names ];
   lev = hat > 2*k1/n;
   flag = lev | (abs(rstudent) > 2);
   if any( flag ) then do;
      l = loc(flag)`;
		ol = obs[l];
      xl=  x[l,2:k1] || hat[l] || rstudent[l];
		cl = names[2:k1] // {'Leverage', 'RStudent'};
      Print "High leverage or large residual points",
				 XL [ rowname=ol c=cl ];
   end;
 
	free uvname;
   do i = 1 to k1;
      name = names[,i];
      reset noname;
      uvname = uvname || concat({'D'},name);
		end;	 
   print "DFBETAS", dfb[ rowname=obs c=uvname format=9.4];
finish;  /* end of dfbeta */
 
*----- module to fit one regression ----------;
start reg (y, x, b, res, yhat, h, rstudent, mse, dfbeta);
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
	q = diag ( 1/sqrt(vecdiag(xpxi)) );
	dfbeta = (x * xpxi * q) # repeat((rstudent/sqrt((1-h))) ,1,p) ;
finish;
 
*-----read the data and prepare partial regression plots----;
     use &data;
     read all var{&xvar} into  x[ colname=xname ];
     %if &id ^= %str() %then %do;
        read all var{&id} into  obs;
		  if type(obs) = 'N' then obs = trim(left(char(obs,4,0)));
     %end;
     %else %do;
        obs = char(1:nrow(x),3,0);
     %end;
     read all var{&yvar } into  y[ colname=yname ];
     names = xname || yname;
     run dfbeta(x, y, names, obs, dfb, uvname);
     %if &out ^= %str() %then %do;
	  	  nv = char(ncol(xname),2,0);
		  call symput("nv",nv);
        create &out from dfb[rowname=obs colname=uvname];
        append from dfb[rowname=obs];
     %end;
quit;

proc print;

*-- Add variable labels to dfbeta variables;
%if &out ^=%str() %then %do;
%put nv = &nv;
data &out;
	set &out;
	label
	%do i = 1 %to &nv;
		%let xname = %scan(&xvar,&i);
		%let xn = &xname;
		%if %length(&xname)>7 %then %let xname=%substr(&xname,1,7);
		D&xname = "DFBETA &xvar"
	%end; %str(;)
%end;

%mend;
