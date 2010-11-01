%macro bctrans(data=,
   out=bctrans,
   var=,
	newvar=,
	add=0,
   min=-2,
	max=2,
	step=0.2);

proc iml;
   use &data;

   read all var {&var} into x;
   n=nrow(x);
   one=j(n,1,1);
	if any(x<=0)
		then x = x+min(x)+.001;
   lnx=log(x);
   sumlog=sum(lnx);

start loglike;
	free mat;
   do lam = &min to &max by &step;
      lambda=round(lam,.01);
      if lambda = 0
         then xl=log(x);
         else xl=((x##lambda) - one)/lambda;
      mean=xl[:];
      d=xl-mean;
      ss=ssq(d)/n;
      l=-.5#n#log(ss)+((lambda-1)*sumlog);
		mat = mat // (lambda || l);
		end;
   finish;

   run loglike;

   print "Lambdas and their l(lambda) values",
         mat [format=8.3] ;
   cl = {lambda loglike};
   create lambdas from mat[colname=cl];
   append from mat;
   quit;

proc plot data=lambdas nolegend;
   plot loglike*lambda;
   title 'Lambda vs. loglike(lambda) values';
   run;
   quit;

proc sort data=lambdas;
   by descending loglike;
   run;

data &out;
   set lambdas;
   if _n_>1 then delete;
   run;

proc print data=&out;
title 'Highest lambda and l(lambda) value';
run;


proc iml;
use &data;
read all var {&var} into old;
use &out;
read all var {lambda} into power;
if power=0
   then new=log(old);
   else new=old##power;
create final from new;
append from new;
quit;

data final;
	set final;
	rename col1=&var;
	run;

proc univariate normal plot data=final;
	title 'Normality Assessment for';
	title2 'Power-Transformed Variable';
	run;
%mend bctrans;
