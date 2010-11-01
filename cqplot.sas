 /*-------------------------------------------------------------------*
  *    Name: cqplot.sas                                               *
  *   Title: Macro for ChiSquare quantile-comparison plots            *
        Doc: http://www.datavis.ca/sasmac/cqplot.html           
  *                                                                   *
  *  minimal syntax: %cqplot (data=dataset,var=variables);            *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
  * Created:  11 Nov 1994 21:43:25                                    *
  * Revised:  09 Dec 2003 12:14:06                                    *
  * Version:  1.2                                                     *
  *   - Cleaned up temp datasets                                      *
  *   - Removed LH= parameter:  now control via GOPTIONS HTEXT=       *
  *-------------------------------------------------------------------*/
 /*
  * $Revision: 1.2 $
  * $Log:	cqplot.sas,v $
  * Revision 1.2  96/11/05  16:51:11  friendly
  * Added RCS ident section
  */
%macro cqplot (
   data=_LAST_,    /* input data set                                 */
   var=,           /* variables to be plotted                        */
   id=,            /* ID variable for point labels                   */
   nvar=,          /* number of variables in the VAR= list           */
   dsq=,           /* Sq distance from centroid, if already computed */
   pplot=NO,       /* Printer plots?                                 */
   gplot=YES,      /* Hi-res plots?                                  */
   out=cqplot,     /* output data set                                */
   stderr=YES,     /* plot std errors around curves?                 */
   stdmult=2,      /*  std error multiplier                          */
   detrend=YES,    /* plot detrended version?                        */
   label=&dsq>_upper_,   /* which observations are labeled?          */
/*   lh=1.5,          height for axis labels                         */
   anno=,          /* name of input annotate data set                */
   annod=,         /* annotate dataset for detrended plot            */
   name=CQPLOT,    /* name of graphic catalog entries                */
   gout=);         /* name of graphic catalog                        */

%let stderr=%substr(%UPCASE(&stderr),1,1);
%let detrend=%substr(%UPCASE(&detrend),1,1);
%let pplot=%substr(%upcase(&pplot),1,1);
%let gplot=%substr(%upcase(&gplot),1,1);
%if &gout^=%str()  %then %let gout=GOUT=&gout;
 
%local cleanup;
%let cleanup=;
%if &dsq=%str() %then %do;
	%if %length(&nvar)=0 %then %do;
	data _null_;
		if 0 then set &data;
		array _var{*} &var;
		call symput('nvar',trim(left(put(dim(_var),12.))));
	%end;

	*-- Find squared Mahalanobis distances via standardized principal comps;
	proc princomp std noprint data=&data out=_prin_;
		var &var;
	data cqplot;
		set _prin_;
		dsq = uss(of prin1-prin&nvar);    /* Mahalanobis D**2 */
		drop prin1 - prin&nvar;
	
	%let dsq=dsq;
	%let in=cqplot;
	
	%if %length(&id) %then %do;
	data cqplot;
		merge &data(keep=&id) cqplot;
	%end;
	%let cleanup=_prin_;
%end;

%else %do;
	%if %length(&nvar)=0 %then %do;
		%put ERROR:  You must specify NVAR= with DSQ=;
	%end;
	%let in=&data;
%end; 

proc sort data=&in;
	by &dsq;

proc univariate noprint data=&in;        * find n, median and hinge-spread;
	where (&dsq ^=.);
   var &dsq;
   output out=_stats_ n=nobs qrange=hspr;
%let cleanup= &cleanup _stats_;
  
data &out;
   set &in;
   drop hspr nobs g scale df;
	retain scale df;
	if _n_ = 1 then do;
		set _stats_;
		df = &nvar;
   	scale = hspr / (cinv(.75, df) - cinv(.25, df));
	end;
   _p_=(_n_ - .5)/nobs;                 * cumulative prob.;
   _z_=cinv(_p_,df);                    * ChiSquare Quantile;
   g = (_z_**(-1+df/2))/(exp(_z_/2)*gamma(df/2)*(sqrt(2)**df));
   _se_ = (scale/g)*sqrt(_p_*(1-_p_)/nobs);
   _resid_ = &dsq - _z_;                * deviation from normal;
   _lower_ = _z_ - &stdmult*_se_;       * +/- SEs around fitted line;
   _upper_ = _z_ + &stdmult*_se_;
   _reslo_  = -&stdmult*_se_;
   _reshi_   = &stdmult*_se_;
  label _z_='ChiSquare Quantile'
  			&dsq = 'Squared Distance'
        _resid_='Deviation From ChiSquare';
  run;

%if %length(&cleanup) %then %do;
proc datasets nofs nolist;
   delete &cleanup;
	run; quit;
	%end;

%if &pplot=Y %then %do;
 proc plot;
   plot &dsq * _z_='*'
        _z_ * _z_='-'
  %if &stderr=Y %then %do;
        _lower_ * _z_='+'
        _upper_ * _z_='+'  
		  %end;
		                     / overlay;       * observed and fitted values;
%if &detrend=Y %then %do;
   plot _resid_ * _z_='*'
  %if &stderr=Y %then %do;
        _reslo_ * _z_='+'
        _reshi_ * _z_='+' / overlay 
		  %end;
		                              vref=0; * deviation from fitted line;
   run;
	%end;
%end;

*proc print;
%if &gplot=Y %then %do;
	%if %length(&id)>0 %then %do;
		%label(data=&out, out=_label_, x=_z_, y=&dsq,
/*			 subset=&dsq>_upper_, */
			subset=&label,
			 pos=4, xoff=-.2, text=&id);
		%if &anno^=%str() %then %do;
			data _label_;
				set _label_ &anno;
			%end;
		%let anno=ANNOTATE=_label_;
	%end;
	%else %if &anno^=%str() %then %do;
		%let anno=ANNOTATE=&anno;
	%end;
%if &annod^=%str()  %then %let annod=ANNOTATE=&annod;

proc gplot data=&out &gout ;
  plot &dsq     * _z_= 1
       _z_ * _z_= 2
  %if &stderr=Y %then %do;
       _lower_ * _z_= 3
       _upper_ * _z_= 3 %end;
       / overlay frame &anno
         vaxis=axis1 haxis=axis2
         hminor=1 vminor=1
         name="&name" 
			des="cqplot of &var";
%if &detrend=Y %then %do;
  plot _resid_ * _z_= 1
  %if &stderr=Y %then %do;
       _reslo_ * _z_= 3
       _reshi_ * _z_= 3 %end;
       / overlay &annod
         vaxis=axis1 haxis=axis2
         vref=0 frame
         hminor=1 vminor=1
         name="&name.1"
			des="detrended cqplot";
%end;
/*
%let vh=1;           *-- value height;
%if &lh >= 1.5 %then %let vh=1.5;
%if &lh >= 2.0 %then %let vh=1.8;
*/
  symbol1 v=dot  h=1.1 i=none c=black l=1;
  symbol2 v=none   i=join  c=blue  l=3 w=2;
  symbol3 v=none   i=join  c=red   l=20;
*  axis1  label=(a=90 r=0 h=&lh) value=(h=&vh);
*  axis2  label=(h=&lh) value=(h=&vh);
  axis1  label=(a=90 r=0);
  axis2;
run; quit;
%end;

%done:
%mend;
