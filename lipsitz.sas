/*
from Lipsitz, Ibrahim & Molenberghs,
Box-Cox transformation in the analysis of longitudinal data
with incomplete responses
*/

%macro contrast(c=,title=);
do;
	if contrast = 0 then goto bottom;
		
	nc= ncol(contrast);
		
	r= nrow(variable);
		
	contrast=contrast[1,2:nc];
		
	nr=(nc-1)/(r);
	nr=round(nr);
		
	contrast=shape(contrast,nr,r);
		
	gsq=(contrast*estimate)`*inv(contrast*vb*contrast`)*
			(contrast*estimate);
	cont_est = contrast*estimate;
	var_cont = contrast*vb*contrast`;
		
	df=nrow(contrast);
		
	p=1-probchi(gsq,df);
	&c=contrast;
		
	print, {contrast is &title  };
	print , &c;
	print, gsq df p;
		
	print, {'estimate of contrast is'  };
	print, cont_est var_cont;
		
	bottom:
		stop;
	end;
%mend contrast;
 


%*inc '~stuart/BoxCox/macros/transmax2.mac';

%macro jack(dataset=,yvar=,xvar=,classvar=,idvar=,timevar=,
        corr_=un,beg_g=-.9,end_g=1.8,gr1=.3,gr2=.06,
        gr3=.012,gr4=.0024,c1_beta=,title1_b=,c2_beta=,title2_b=,
	out=beta_j);

data datazz;
   set &dataset;
run;

%transmax(data=datazz,print=no,
            y= &yvar,
            x= &xvar,
	    class= &classvar,
           id= &idvar,
         time= &timevar,
     beg_grid= &beg_g,
	 end_grid= &end_g,
      grid1= &gr1,
      grid2= &gr2,
      grid3= &gr3,
      grid4= &gr4,
     outdata=outb);

proc sort data=datazz;
  by &idvar;
run;

data datazz;
  set datazz end=f;
  by &idvar;
  if first.&idvar then clus_n+1;
  if f then call symput('nclus',clus_n);
  run;


/* Compute jacknife estimates          */

%do i=1 %to &nclus;

data data_i;
 set datazz;
where clus_n ne &i;
run;

		   
%transmax(data=data_i,print=no,
            y= &yvar,
            x= &xvar,
	    class= &classvar,
           id= &idvar,
         time= &timevar,
     beg_grid= &beg_g,
	 end_grid= &end_g,
      grid1= &gr1,
      grid2= &gr2,
      grid3= &gr3,
      grid4= &gr4,
     outdata=out_bi);		   
  
proc datasets;
 delete data_i ;
run;

proc iml worksize=5000;

 reset nolog noprint;

/*initial marginal parameters */

   USE out_bi;
   READ ALL INTO BETA_i;

   USE outb;
   READ ALL INTO BETA;
   
   use out_bi;
    READ ALL VAR _CHAR_ INTO names;
	
 names = names` ;

diff = (beta_i - beta)`;

create diff_b from diff /*[colname=names] */;
append from diff;
close diff_b;

beta_i = beta_i`;

create beta_ii from beta_i /* [colname=names] */;
append from beta_i;
close beta_ii;

quit;


proc append base=betas data=diff_b force;
run;

proc append base=&out data=beta_ii force;
run;

proc datasets;
 delete diff_b beta_i beta_ii;
run;

%end;

proc univariate data = &out;
run;

proc corr nocorr sscp data=betas out=var_b(type=sscp drop=intercep)
      noprint;
run;

data var_b (drop=_type_ _name_) ;
 set var_b;
  if ( (_type_='SSCP') and (_name_ ne 'INTERCEP') );
run;



proc iml;

 reset nolog noprint;

   USE outb;
   READ ALL INTO BETA;

     use var_b;
     read all into var_b;
	 
   use outb;
    READ ALL VAR _CHAR_ INTO variable;


print, { 'Parameters of Box-Cox' };
print, { 'STANDARD ERRORS FROM JACKKNIFE' };

vb=(&nclus-1)/(&nclus)#var_b;

     sebeta=sqrt(vecdiag(vb));      *vector of estimated
                                       standard errors of
                                       beta;


     z=beta/sebeta;                   *z-statistics;


     zsq=z#z;

     p=1-probchi(zsq,1);              *two-sided p-value;

estimate=beta;
se_est=sebeta;

     print, variable estimate se_est z p;

   contrast= { 0  &c1_beta };
   %contrast(c=c1_beta,title=&title1_b);
 
   contrast= { 0  &c2_beta };
   %contrast(c=c2_beta,title=&title2_b);
 
quit;


%mend jack;

%macro transmax(data=_last_,class=,y=y,x=x,id=id,print=yes,time=,
        corr=un,
		  beg_grid=-.9,
		  end_grid=1.8,
		  grid1=.3,
		  grid2=.06,
        grid3=.012,
		  grid4=.0024,
		  outdata=);

data grid;
  do l=&beg_grid to &end_grid by &grid1 ;
      output;
   end;
 run;

data _null_;
 set grid;
 call symput('ngrid',_n_);
 run;


%do j=1 %to &ngrid;

/*
proc printto new log="/tmp/junk.log";
run;
*/

data add(keep= one l );
 set grid;
 if _n_ = &j ;
 one=1;
 run;

DATA zzONE ;
 set &data;
 if &y > .;
 one = 1;
run;

DATA zzONE ;
 set zzONE ;
 merge zzone add;
 by one;
run;

DATA zzONE ;
 set zzONE ;

lambda = l ;
*   if (-.01 < l < .01) then lambda=.01;
*   y_l = (&y **lambda - 1)/lambda ;
if (l ne 0) then y_l = (&y **lambda - 1)/lambda ;
if (l = 0) then y_l = log( &y ) ;
if (l ne 0) then extra=log(abs( &y **(lambda - 1) ) ) ;
if (l = 0) then extra= log(abs( 1/&y ) ) ;
run;

proc means data=zzONE noprint;
  var extra ;
 output out=new  sum =  sum_e ;
 run;

%global _disk_;
%let _disk_ = on;
%global _print_;
%let _print_ = off;

data ll;
 ll1=.;
run;

proc mixed METHOD= ML data=zzone ;
 class &time &class ;
 model y_l =  &x / SOLUTION ;
 repeated &time / type=&corr subject=&id;
MAKE 'FITTING' OUT=LL;
run;

data ll(drop=DESCR);
 set ll;
 if DESCR = 'Log Likelihood';
 ll1=VALUE;
run;

%let _print_ = on;

data new;
 merge new ll;
 run;

data new;
 set new;
 ll = ll1 + sum_e;
run;

data new(drop=one l);
 merge add new;
   lambda = l ;
*  if (-.01 < l < .01) then lambda=.01;
run;

proc append base=lls data=new force;
run;

proc datasets;
  delete add zzONE new ll ;
run;

%end;

proc sort data=lls ;
 by descending ll;
run;

data lls;
 set lls;
 if _n_=1;
run;

data lls;
 set lls;
 bg = lambda - &grid1;
 call symput('bg',bg);
 eg = lambda + &grid1;
 call symput('eg',eg);
 run;

proc datasets;
  delete lls grid maxl;
run;

/* smaller grid search */


data grid;
  do l=&bg to &eg by &grid2 ;
      output;
   end;
 run;

data _null_;
 set grid;
 call symput('ngrid',_n_);
 run;

%do j=1 %to &ngrid;

data add(keep= one l );
 set grid;
 if _n_ = &j ;
 one=1;
 run;

DATA zzONE ;
 set &data;
 if &y > .;
 one = 1;
run;

DATA zzONE ;
 set zzONE ;
 merge zzone add;
 by one;
run;

DATA zzONE ;
 set zzONE ;

lambda = l ;
*   if (-.01 < l < .01) then lambda=.01;
*   y_l = (&y **lambda - 1)/lambda ;
if (l ne 0) then y_l = (&y **lambda - 1)/lambda ;
if (l = 0) then y_l = log( &y ) ;
if (l ne 0) then extra=log(abs( &y **(lambda - 1) ) ) ;
if (l = 0) then extra= log(abs( 1/&y ) ) ;
run;

proc means data=zzONE noprint;
  var extra ;
 output out=new  sum =  sum_e ;
 run;

%global _disk_;
%let _disk_ = on;
%global _print_;
%let _print_ = off;

data ll;
 ll1=.;
run;

proc mixed METHOD= ML data=zzone ;
 class &time &class ;
 model y_l =  &x / SOLUTION ;
 repeated &time / type=&corr subject=&id;
MAKE 'FITTING' OUT=LL;
run;

data ll(drop=DESCR);
 set ll;
 if DESCR = 'Log Likelihood';
 ll1=VALUE;
run;

%let _print_ = on;

data new;
 merge new ll;
 run;

data new;
 set new;
 ll = ll1 + sum_e;
run;

data new(drop=one l);
 merge add new;
   lambda = l ;
*  if (-.01 < l < .01) then lambda=.01;
run;

proc append base=lls data=new force;
run;

proc datasets;
  delete add zzONE new ll ;
run;

%end;

proc sort data=lls ;
 by descending ll;
run;

data lls;
 set lls;
 if _n_=1;
run;

data lls;
 set lls;
 bg = lambda - &grid2;
 call symput('bg',bg);
 eg = lambda + &grid2;
 call symput('eg',eg);
 run;

proc datasets;
  delete lls grid ;
run;

/* smaller grid search */


data grid;
  do l=&bg to &eg by &grid3 ;
      output;
   end;
 run;

data _null_;
 set grid;
 call symput('ngrid',_n_);
 run;

%do j=1 %to &ngrid;

data add(keep= one l );
 set grid;
 if _n_ = &j ;
 one=1;
 run;

DATA zzONE ;
 set &data;
 if &y > .;
 one = 1;
run;

DATA zzONE ;
 set zzONE ;
 merge zzone add;
 by one;
run;

DATA zzONE ;
 set zzONE ;

lambda = l ;
*   if (-.01 < l < .01) then lambda=.01;
*   y_l = (&y **lambda - 1)/lambda ;
if (l ne 0) then y_l = (&y **lambda - 1)/lambda ;
if (l = 0) then y_l = log( &y ) ;
if (l ne 0) then extra=log(abs( &y **(lambda - 1) ) ) ;
if (l = 0) then extra= log(abs( 1/&y ) ) ;
run;

proc means data=zzONE noprint;
  var extra ;
 output out=new  sum =  sum_e ;
 run;

%global _disk_;
%let _disk_ = on;
%global _print_;
%let _print_ = off;

data ll;
 ll1=.;
run;

proc mixed METHOD= ML data=zzone ;
 class &time &class ;
 model y_l =  &x / SOLUTION ;
 repeated &time / type=&corr subject=&id;
MAKE 'FITTING' OUT=LL;
run;

data ll(drop=DESCR);
 set ll;
 if DESCR = 'Log Likelihood';
 ll1=VALUE;
run;

%let _print_ = on;

data new;
 merge new ll;
 run;

data new;
 set new;
 ll = ll1 + sum_e;
run;

data new(drop=one l);
 merge add new;
   lambda = l ;
*  if (-.01 < l < .01) then lambda=.01;
run;

proc append base=lls data=new force;
run;

proc datasets;
  delete add zzONE new ll ;
run;

%end;

proc sort data=lls ;
 by descending ll;
run;

data lls;
 set lls;
 if _n_=1;
run;

data lls;
 set lls;
 bg = lambda - &grid3;
 call symput('bg',bg);
 eg = lambda + &grid3;
 call symput('eg',eg);
 run;

proc datasets;
  delete lls grid ;
run;

/* smaller grid search */


data grid;
  do l=&bg to &eg by &grid4 ;
      output;
   end;
 run;

data _null_;
 set grid;
 call symput('ngrid',_n_);
 run;

%do j=1 %to &ngrid;

data add(keep= one l );
 set grid;
 if _n_ = &j ;
 one=1;
 run;

DATA zzONE ;
 set &data;
 if &y > .;
 one = 1;
run;

DATA zzONE ;
 set zzONE ;
 merge zzone add;
 by one;
run;

DATA zzONE ;
 set zzONE ;

lambda = l ;
*   if (-.01 < l < .01) then lambda=.01;
*   y_l = (&y **lambda - 1)/lambda ;
if (l ne 0) then y_l = (&y **lambda - 1)/lambda ;
if (l = 0) then y_l = log( &y ) ;
if (l ne 0) then extra=log(abs( &y **(lambda - 1) ) ) ;
if (l = 0) then extra= log(abs( 1/&y ) ) ;
run;

proc means data=zzONE noprint;
  var extra ;
 output out=new  sum =  sum_e ;
 run;

%global _disk_;
%let _disk_ = on;
%global _print_;
%let _print_ = off;

data ll;
 ll1=.;
run;

proc mixed METHOD= ML data=zzone ;
 class &time &class ;
 model y_l =  &x / SOLUTION ;
 repeated &time / type=&corr subject=&id;
MAKE 'FITTING' OUT=LL;
run;

data ll(drop=DESCR);
 set ll;
 if DESCR = 'Log Likelihood';
 ll1=VALUE;
run;

%let _print_ = on;

data new;
 merge new ll;
 run;

data new;
 set new;
 ll = ll1 + sum_e;
run;

data new(drop=one l);
 merge add new;
   lambda = l ;
*  if (-.01 < l < .01) then lambda=.01;
run;

proc append base=lls data=new force;
run;

proc datasets;
  delete add zzONE new ll ;
run;

%end;

proc sort data=lls ;
 by descending ll;
run;

data lls;
 set lls;
 if _n_=1;
run;

data lls;
 set lls;
 call symput('lambda',lambda);
 run;

proc datasets;
  delete lls grid ;
run;

options nocenter ls=70;

DATA zzONE ;
 set &data;
 if &y > .;
lambda = &lambda ;
if (lambda ne 0) then y_l = (&y **lambda - 1)/lambda ;
if (lambda = 0) then y_l = log( &y ) ;
run;

 %global _disk_;
 %let _disk_ = on;
 %global _print_;
 %let _print_ = off;

proc mixed METHOD= ML data=zzone ;
 class &time &class ;
 model y_l =  &x / SOLUTION ;
 repeated &time / type=&corr subject=&id;
make 'SOLUTIONF' out=esti;
run;

  %let _print_ = on;

data add;
 _effect_='LAMBDA';
 _est_=&lambda ;
 _df_=1;
run;

data esti;
 set esti add;
run;


%if &print^=no %then %do;
proc print label data=esti;
run;
%end;

%if &outdata^= %then %do;
data &outdata (keep=_effect_ _est_);
    set esti;
  if _df_ =. then delete;
run;
%end;

proc datasets;
 delete esti add;
run;


%mend;

