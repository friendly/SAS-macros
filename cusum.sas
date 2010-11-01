*---------------------------CUSUM------------------------------*
| This IML routine computes recursive residuals and outputs    |
| their cumulative sum and ss (called cusum and cusumss) to a  |
| sas data set to be plotted.  The plot should show if the     |
| regression parameters change over time.                      |
| From: Brown,Durbin, and Evans, "Techniques for testing the   |
|       constancy of regression relationships over time",      |
|       JRSS B V37, P149 (1975).                               |
*------------------------------------------------JPS--25OCT79--*;

%macro cusum(
	data=_last_,   /* name of input data set */
	yvar=,         /* response variable */
	xvars=,        /* predictor variables */
	out=cusum      /* name of output data set */
	);

proc iml;
   use &data;
   read all var {&xvars} into x[ colname=xname ];
   read all var {&yvar}  into y[ colname=yname ];
   close &data;
   *--------cusum----------;
   n=nrow(x);
   p=ncol(x);
   x = j(n,1,1) || x;
   p1=p+1;
 
   *---first p+1 observations---;
   z=x[ 1:p1, ];
   yy=y[ 1:p1, ];
   xy=t(z)*yy;
   ixpx=inv(t(z)*z);
   b=ixpx*xy;
   r=y[ p1, ]-z[ p1, ]*b;
 
   *---recursion until n---;
   start recurse;
      do i=p+2 to n;
         xi=x[ i, ]; yi=y[ i, ];
         xy=xy+t(xi)*yi;
         ixpx=invupdt(ixpx,xi);     /* update inverse matrix */
         b=ixpx*xy;                 /* new coefficients      */
         r=r//(yi-xi*b);            /* current residual      */
      end;
   finish;
   run recurse;
 
   *---print estimates---;
   print "Final parameter estimates",  b /;
   print "Inverse XPX matrix", ixpx /;
 
   *---form lower triangle of ones---;
   m=n-p;
   tri= (1:m)`*repeat(1,1,m)  >= repeat(1,m,1)*(1:m) ;
 
   *---form csum and csumss---;
   cusum=tri*r;
   cusumss=tri*(r#r);
 
   *---output to dataset---;
   rnames=xname || yname || { n  residual   cusum   cusumss  };
   x= x[ p1:n, 2:p1 ] || y[ p1:n ];
   x= x || (p1:n)` || r || cusum || cusumss;
   print  x  [ colname=rnames ] /;
   create &out from x [ colname=rnames ];
   append from x;
quit;

data &out;
	set &out;
	label
		residual='Recursive residual'
		cusum='CUSUM value'
		cusumss ='Cumulative SS';

%mend;
