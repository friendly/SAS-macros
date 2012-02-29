 /*-------------------------------------------------------------------*
  *    Name: distplot.sas                                             *
  *   Title: Plots for discrete distributions                         *
        Doc: http://www.datavis.ca/sasmac/distplot.html            
  *                                                                   *
  *  Hoaglin & Tukey, Checking the shape of discrete distributions.   *
  *   In Hoaglin, Mosteller & Tukey (Eds.), Exploring data tables,    *
  *   trends, and shapes, NY: Wiley, 1985.                            *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  19 Mar 1991 08:48:26                                    *
  * Revised:  21 Jan 2009 12:05:30                                    *
  * Version:  1.4-1                                                   *
  *  1.1  Plot y * &count so label will not be required               *
  *       Allow for 0 frequencies                                     *
  *  1.2  Added indicated parameter change plots                      *
  *       Fixed bugs in ngebin and geometric                          *
  *       Fixed validvarname for V7+                                  *
  *  1.3  Fixed minor bugs re multiple plots                          *
  *  1.4  Fixed V9 change to INTERCEPT; fixed buglet                  *
  * Requires: %words %label %gskip                                    *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/

 /*=
=Description:
 
 The DISTPLOT macro constructs plots of a discrete distribution
 designed to diagnose whether the data follows one of the standard
 distributions: the Poisson, Binomial, Negative Binomial, Geometric,
 or Log Series, specified by the DIST= parameter.  The usual
 (PLOT=DIST) plot is constructed so that the points lie along a
 straight line when the data follows that distribution.  An influence
 plot (PLOT=INFL) shows the influence of each observation on the
 choice of the distribution parameter(s).

=Usage:

 The DISTPLOT macro is called with keyword parameters.
 You must specify the distribution to be fit (DIST=). and the
 COUNT= and FREQ= variables.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%distplot(data=queues, count=women, freq=queues, dist=binomial,
	    parm=0.435);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* COUNT=      Basic count variable

* FREQ=       Number of occurrences of count

* LABEL=      Horizontal (count) label

* DIST=       Name of distribution, one of POISSON, BINOMIAL, 
              GEOMETRIC, LOGSERIES, or NEGBIN.

* PARM=       Trial value of the distribution parameter(s) to level the 
              plot.  For the Binomial distribution, PARM=p, the binomial
				  probability of success; for the Poisson, PARM=lambda,
				  the Poisson mean.  For the Geometric and Negative binomial,
				  PARM=p

* Z=          Multiplier for error bars in the PLOT=DIST plot.
              [Default: Z=1.96]

* PLOT=       What to plot: DIST and/or INFL [Default: PLOT=DIST]

* HTEXT=      Height of text labels in the plots [Default: HTEXT=1.4]

* OUT=        The name of the output data set [Default: OUT=DISTPLOT]

* NAME=       Name of the graphics catalog entry [Default: NAME=DISTPLT]

 =*/
%macro distplot(
     data=_last_,       /* name of input data set                   */
     count=,            /* basic count variable                     */
     freq=,             /* number of occurrences of count           */
     label=,            /* Horizontal (count) label                 */
     dist=,             /* name of distribution                     */
     parm=,             /* trial value of parm(s) to level the plot */
     z=1.96,            /* multiplier for error bars                */
     plot=DIST,         /* What to plot: DIST and/or INFL           */
	  htext=1.4,         /* Height of text labels                    */
	  out=distplot,      /* name of the output data set              */
     name=distplt       /* graphics catalog entry                   */
	  );
 
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=V7;
		%end;
	%else %do;
	   options nonotes;
		%end;
	%*-- Handle V9 changes;
	%if %sysevalf(&sysver  >= 9) %then %do;
		%let intercep = intercept;
		%end;
	%else %do;
		%let intercep = intercep;
		%end;

%*if &label=%str() %then %let label=&count;
%let plot=%upcase(&plot);
%let dist=%upcase(&dist);
%if %length(&z)=0 %then %let z=0;

%*-- Determine if any parameters were passed;
%global parm1 parm2;
%let parm1=0;
%let parm2=0;
%let nparm = %words(&parm, root=parm);
%* put nparm= &nparm parm1=&parm1 parm2=&parm2;

proc means data=&data N sum sumwgt mean var noprint vardef=weight;
   var &count;
   weight &freq;
   output out=_sum_ sumwgt=N sum=sum mean=_mean_ var=_var_ max=_max_;

data &out;
   set &data nobs=_cells_;
   if _n_=1 then set _sum_(drop=_type_ _freq_);
	drop kf k;
   k = &count;
   nk= &freq;                     * n(k);
	kf= gamma(k+1);                * k!  ;
  
   *-- centered value n*(k), Hoaglin & Tukey, Eqn 9 ;
   p = nk / N;
   if nk >=2 then nkc = nk - .67 - .8*p;
             else nkc = exp(-1);

	%if &dist=POISSON %then %do;
		*-- if levelling the plot, subtract centering value;
		%if &nparm = 0
				%then %str( level =  0; );
				%else %str( level = &parm1 - k * log( &parm1 ); );
	   y =  log(kf * nk / N) + level ;
	   yc=  log(kf * nkc/ N) + level ;
		parm = _mean_;       *-- estimate of lambda;
		call symput('eparm', left(put(parm, 6.3)));
	   phat = exp(-_mean_) * _mean_**&count / kf;
		if nk<=1 then do;
			ylo = log(kf*nkc/N) - 2.677 + level;
			yhi = log(kf*nkc/N) + 2.717 - 2.3/N + level;
			h = (yhi-ylo)/2;
			end;
		%end;
	%else %if &dist=BINOMIAL %then %do;
		%if &nparm = 0
				%then %str( level =  0; );
				%else %do;
					level = -(_max_ * log (1-&parm1) + k*log(&parm1/(1-&parm1)));
					%end;
		bnk = gamma(_max_+1) / ( gamma(k+1)*gamma(_max_-k+1) );
		y =  log(nk / (N * bnk)) + level ;
		yc=  log(nkc/ (N * bnk)) + level ;

		parm = _mean_ / _max_;     *-- estimate of p;
		call symput('eparm', left(put(parm, 6.3)));
		phat = bnk * (parm**k) * parm**(_max_-k);

		if nk<=1 then do;
			ylo = log(nkc/(N * bnk)) - 2.677 + level;
			yhi = log(nkc/(N * bnk)) + 2.717 - 2.3/N + level;
			h = (yhi-ylo)/2;
			end;
		%end;
	%else %if &dist=NEGBIN %then %do;
		%if &nparm=0 %then %do;
			parmn = _mean_**2 / (_var_-_mean_);     *-- n, moment est;
			parm  = _mean_/_var_;                   *-- p, moment est;
			level = 0;
			%end;
		%else %if &nparm=1 %then %do;
			parmn = &parm1;
			parm = _mean_/_var_;                    *-- p, moment est;
			level = -(parmn * log(parm) + k*log(1-parm));
			%end;
		%else %do;
			parmn = &parm1;
			parm = &parm2;
			level = -(parmn * log(parm) + k*log(1-parm));
			%end;
*		parm  = (_var_/_mean_**2) -1;           *-- p, moment est;
		bnk = gamma(parmn+k) / (gamma(k+1) * gamma(parmn));
		phat = bnk * (parm**parmn) * (1-parm)**k;
		y =  log(nk / (N * bnk)) + level ;
		yc=  log(nkc/ (N * bnk)) + level ;

		if nk<=1 then do;
			ylo = log(nkc/(N * bnk)) - 2.677 + level;
			yhi = log(nkc/(N * bnk)) + 2.717 - 2.3/N + level;
			h = (yhi-ylo)/2;
			end;
		%end;
	%else %if &dist=GEOMETRIC %then %do;
		%if &nparm = 0
			%then %str( level =  0; );
			%else %str( level = -(log(&parm1) + k*log(1-&parm1)););
		y =  log(nk / N ) + level ;
		yc=  log(nkc/ N ) + level ;

		parm = 1/_mean_;
		call symput('eparm', left(put(parm, 6.3)));
		phat = parm*(1-parm)**(k-1);

		if nk<=1 then do;
			ylo = log(nkc/ N ) - 2.677 + level;
			yhi = log(nkc/ N ) + 2.717 - 2.3/N + level;
			h = (yhi-ylo)/2;
			end;
		%end;
	%else %if &dist=LOGSERIES %then %do;
		%if &nparm = 0
				%then %str( level =  0; );
		y =  log(k * nk / N ) + level ;
		yc=  log(k * nkc/ N ) + level ;

		*Birch estimator;
		parm = 1 - (1 / (1 + ((5/3)- log(mean)/16)*(mean-1)+2)*log(mean));
		call symput('eparm', left(put(parm, 6.3)));
		if nk<=1 then do;
			ylo = log(k * nkc/ N ) - 2.677 + level;
			yhi = log(k * nkc/ N ) + 2.717 - 2.3/N + level;
			h = (yhi-ylo)/2;
			end;
		%end;
 
   *-- half-length of confidence interval for log(eta-k) [Eqn 10];
	if nk>1 then do;
		h = &z * sqrt( (1-p) / ( nk-((.47+.25*p)*sqrt(nk)) ) );
		ylo = yc - h;
		yhi = yc + h;
		end;

	*-- Estimated prob and expected frequency;
   exp  = N * phat;

	*-- Leverage and apparent parameter values (p.402);

   %if &parm1 = 0
		%then %str(lev = (&count / _mean_) - 1;);
		%else %str(lev = (&count / &parm1) - 1;);
	hc = sign(lev) * lev / h;
	vc = sign(lev) * (log(nkc) - log(exp)) / h;
	slope = vc / hc;   *-- (lambda - lambda0);
	label y = 'Count metameter'
		hc = 'Scaled Leverage'
		vc = 'Relative parameter change'
		slope = 'Parameter change';


proc print data=&out;
   id &count;
*   var nk y nkc yc h ylo yhi lev hc vc;
   sum nk nkc;
*   format y yc h yhi ylo 6.3 lev hc vc 6.2;

/* 
*-- Calculate goodness of fit chisquare;
data fit;
   set &out;
   chisq= (nk - exp)**2 / exp;
proc print data=fit;
   id &count;
   var nk p phat exp chisq;
   sum nk exp chisq;
*/

*-- Find slope, intercept of line;
proc reg data=&out outest=_parms_ noprint;
   model y =  &count;
data _stats_;        *-- Annotate data set to label the plot;
   set _parms_ (keep=&count &intercep);
   set _sum_   (keep=_mean_);
	b = &count;
	a = &intercep;
   drop &count &intercep ek a b;
   length text $30 function $8;
   xsys='1'; ysys='1';
   x=15;
	*-- set label y location based on slope;
	if &count > 0
		then y=96;
		else y=16; 
   function = 'LABEL';
   size = &htext;
   color = 'RED';
   position='3'; text ='slope(b) = '||left(put(b,f6.3)); output;
   position='6'; text ='intercept= '||left(put(a,f6.3)); output;

   y=y-6;
	%if &dist=POISSON %then %do;
		ek = exp(b - &parm1);
		position='6'; text ="lambda:  mean = &eparm"; output;
		position='9'; text ='       exp(b) = '||put(ek,5.3); output;
	%end;
	%else %if &dist=BINOMIAL %then %do;
		%if &parm1>0 %then 
			%str( b = b - log (&parm1/(1-&parm1)); );
		ek = exp(b)/(1+exp(b));
		position='6'; text ="p:      mean/n = &eparm"; output;
		position='9'; text ='   e(b)/1+e(b) = '||put(ek,5.3); output;
	%end;
	%else %if &dist=NEGBIN %then %do;
		%if &parm1>0 %then 
			%str( b = b - log (1-&parm1); );
		ek = 1 - exp(b);
		en = a / log(ek);
		position='6'; text ='n:  a/log(p) = '||put(en,5.3); output; 
		position='9'; text ='p:    1-e(b) = '||put(ek,5.3); output;
	%end;
	%else %if &dist=GEOMETRIC %then %do;
		%if &parm1>0 %then 
			%str( b = b - log (1-&parm1); );
		ek = 1 - exp(b);
		en = exp(a);
		position='3'; text ="p:   1/mean = &eparm";        output; 
		position='6'; text ='p:   1-e(b) = '||put(ek,5.3); output;
*		y = y-4;
		position='9'; text ='p:   e(a)   = '||put(en,5.3); output; 
	%end;
	%else %if &dist=LOGSERIES %then %do;
		%if &parm1>0 %then 
			%str( b = b - log (&parm1); );
		ek = exp(b);
/*		position='6'; text ='p:   e(a)   = '||put(en,5.3); output;   */
		position='9'; text ='p:   1-e(b) = '||put(ek,5.3); output;
	%end;

%let order=;
%if &z > 0 %then %do;
data _conf_;
   set &out;
   drop yc;
   xsys='2'; ysys='2';
   x = &count;    line=33;
   y = yc;   function='MOVE  '; output;
   text='+'; function='SYMBOL'; output;
   y = yhi;  function='DRAW  '; output;
   y = yc;   function='MOVE  '; output;
   y = ylo;  function='DRAW  '; output;
data _stats_;
   set _stats_ _conf_;

*-- find range of confidence limits to set y axis extrema;
proc means data=_conf_ noprint;
   var y;
   output out=_range_ min=min max=max;
data _null_;
   set _range_;
	inc = 1;
	if (max-min)>10 then inc=2;
   min = inc * floor(min/inc);
   max = inc * ceil(max/inc);
   call symput('MIN', left(put(min,3.)));
   call symput('MAX', left(put(max,3.)));
   call symput('INC', left(put(inc,3.)));
run;
%let order = order=(&min to &max by &inc);
%end; /* %if &z */

*-- Poissonness-style distribution plot;
proc gplot data=&out;
   plot y * &count / anno=_stats_ vaxis=axis1 haxis=axis2 hminor=0 vminor=1
		name="&name"
		des="Distribution plot of &count";
   symbol v=- h=2 i=rl c=black;
   axis1 &order
         label=(a=90 r=0
          'Count metameter')
         value=(h=&htext);
   axis2 offset=(3) minor=none
	%if %length(&label) %then %do;
         label=("k  (&label)")
			%end;
         value=(h=&htext);
	run; quit;

*-- Indicated parameter change (infl) plot;
	%if %index(&plot,INFL) %then %do;
	%label(data=&out, y=vc,    x=hc, text=put(&count,3.), pos=6,
		out=_anno1_, size=);
	*-- Draw lines from origin to each point;
	data _lines_;
		set &out(keep=hc vc);
		xsys='2'; ysys ='2';
		x=0;  y=0;  function='move    '; output;
		x=hc; y=vc; function='draw    '; output;
	data _anno1_;
		set _anno1_ _lines_;

	%label(data=&out, y=slope, x=hc, text=left(put(&count,2.)),
		out=anno2, size=);
	%gskip;
	symbol v=- h=2 color=black i=none;
	axis1 label=(a=90);

	proc gplot data=&out;
		plot vc * hc  /
			hzero vref=0 lvref=33 anno=_anno1_ vaxis=axis1 hminor=1 vminor=1
			name="&name"
			des="Parameter change plot of &count";
		run; quit;
	%gskip;
	proc gplot data=&out;
		bubble slope * hc =hc / 
			vref=0 lvref=33 anno=anno2 vaxis=axis1  hminor=1 vminor=1
			bsize=40 bcolor=red bscale=radius
			name="&name"
			des="Parameter change plot of &count";

	run; quit;
%end;
*-- Clean up datasets no longer needed;
proc datasets lib=work memtype=data nolist nowarn;
   delete _sum_ _stats_ _conf_ _parms_;
	run; quit;

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;
